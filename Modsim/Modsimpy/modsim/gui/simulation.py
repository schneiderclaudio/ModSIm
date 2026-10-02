"""Simulation helpers for the ModSIM GUI.

This module wires the job model, the legacy-file writers and the Fortran
engine together for the three "Simulate" features:

* output-format handling (``FORMAT.OUT`` load/write),
* repetitive simulation (parameter sweeps over a ``Repeat.out`` file),
* liberation models (``LJUBAMD`` / ``BETAAMD``).

The engine entry points are invoked through :class:`ModsimEngine`; a missing
engine library surfaces as :class:`EngineLoadError` from any function that
constructs the engine.
"""

from __future__ import annotations

import itertools
import os
from dataclasses import dataclass
from typing import List, Optional, Sequence

from ..engine.engine_bridge import EngineLoadError, ModsimEngine
from ..io import readers, writers
from ..models.job import FormatOutFile, Job

#: Number of sweep levels the engine supports (Repeat.out flag positions 1..5).
MAX_SWEEP_LEVELS = 5


@dataclass
class SweepLevel:
    """A single repetitive-simulation level.

    ``level`` is the engine's 1-based level number (1..5); ``unit_number`` is
    the ``.cur`` unit's ``number``; ``param_index`` is the 0-based index into
    the unit's ``params`` list that the sweep varies.
    """

    level: int
    unit_number: int
    param_index: int
    param_name: str
    start: float
    end: float
    step: float


@dataclass
class SweepConfig:
    """The complete repetitive-simulation configuration."""

    levels: List[SweepLevel]


@dataclass
class SweepRun:
    """The result of one sweep combination (one engine run)."""

    values_line: str
    cum_out: int
    inord_result: int
    simop_result: int


# ---------------------------------------------------------------------------
# Output format
# ---------------------------------------------------------------------------
def load_format_out(job: Job, job_path: str) -> Optional[FormatOutFile]:
    """Load ``FORMAT.OUT`` from ``job_path`` into ``job.format_out``.

    Returns the parsed format when the file exists (and attaches it to the
    job), otherwise ``None``.
    """
    path = os.path.join(job_path, "FORMAT.OUT")
    if not os.path.exists(path):
        return None
    job.format_out = readers.read_format_out(path)
    return job.format_out


def write_format_out_for_run(job: Job, job_path: str) -> None:
    """Write the job's ``FORMAT.OUT`` into ``job_path`` before a run.

    No-op when the job has no output format configured.
    """
    if job.format_out is not None:
        writers.write_format_out(job.format_out, os.path.join(job_path, "FORMAT.OUT"))


# ---------------------------------------------------------------------------
# Liberation models
# ---------------------------------------------------------------------------
def run_liberation(job: Job, job_path: str, model: str, params: Sequence[float]) -> int:
    """Run a liberation model on ``job`` in ``job_path``.

    ``model`` selects the engine entry point: ``"ljubamd"`` runs the
    Ljubljana (Andrews-Mika) model, ``"betaamd"`` the Beta (Austin breakage
    function) model with ``params`` (exactly four floats).  Returns the engine
    result code (-1 on success).  Raises :class:`ValueError` for an unknown
    model and :class:`EngineLoadError` when the engine library is missing.
    """
    # The liberation models open CURRDATA.SYD in the job directory
    # (LJUBAMD.FOR:40, BETAAMD.FOR:47); mirror the VB6 GUI's FileCopy of the
    # job's .sid file so the engine reads the current system data.
    writers.write_currdat_syd(job, job_path)

    engine = ModsimEngine()
    name = job.name
    if model == "ljubamd":
        return engine.ljubamd(job_path, name)
    if model == "betaamd":
        return engine.betaamd(job_path, name, params)
    raise ValueError(f"unknown liberation model: {model!r}")


# ---------------------------------------------------------------------------
# Repetitive simulation (parameter sweep)
# ---------------------------------------------------------------------------
def _float_range(start: float, end: float, step: float):
    """Yield ``start``..``end`` inclusive in ``step`` increments.

    ``end`` is included with a small tolerance (1e-9) so floating-point
    arithmetic cannot drop the last value.  ``step`` may be negative (counting
    down from ``start``).  A zero ``step`` yields only ``[start]``; a step
    pointing away from ``end`` yields nothing.
    """
    if step == 0:
        yield start
        return
    if step > 0:
        if start > end:
            return
        value = start
        while value <= end + 1e-9:
            yield value
            value += step
    else:
        if start < end:
            return
        value = start
        while value >= end - 1e-9:
            yield value
            value += step


def run_sweep(job: Job, job_path: str, config: SweepConfig) -> List[SweepRun]:
    """Run a repetitive simulation (parameter sweep) for ``job``.

    Levels are sorted ascending by level number.  For every combination of
    per-level values the corresponding unit parameter is written into the
    job's ``.cur`` data, the job is persisted to ``job_path`` and the engine's
    ``INORDCALC`` / ``SIMOP`` pair is invoked with cumulative output enabled.
    The first successful run writes the ``Repeat.out`` column headings; later
    runs append (``heads_written``).

    Returns one :class:`SweepRun` per combination, in generation order.  When
    the engine reports the data-input tear error (exit code 13) the sweep is
    aborted and ``TEARS.OUT`` is reset.
    """
    if job.cur is None:
        raise ValueError(
            "Job has no .cur data section; cannot run a parameter sweep."
        )

    sorted_levels = sorted(config.levels, key=lambda level: level.level)
    active_levels = {level.level for level in sorted_levels}
    flag_code = "".join(
        "T" if pos in active_levels else "F" for pos in range(1, MAX_SWEEP_LEVELS + 1)
    )

    sequences = [_float_range(l.start, l.end, l.step) for l in sorted_levels]
    engine = ModsimEngine()
    cum_out = 0
    heads_written = False
    runs: List[SweepRun] = []

    for combo in itertools.product(*sequences):
        for level, value in zip(sorted_levels, combo):
            unit = next(
                (u for u in job.cur.units if u.number == level.unit_number), None
            )
            if unit is None:
                raise ValueError(
                    f"Level {level.level}: no unit {level.unit_number} found in "
                    f"the job's .cur data."
                )
            unit.params[level.param_index] = writers._fmt_e(value)
        # Force the .cur writer to regenerate from the structured (edited)
        # parameters instead of emitting the stale raw lines.
        job.cur.raw_lines = None

        cum_out += 1
        values_line = " ".join(str(v) for v in combo)

        # Repeat.out tells the engine which levels are active, which unit
        # parameters they vary, and the current sweep values (SIMOP.FOR:378).
        writers.write_repeat_out(
            os.path.join(job_path, "Repeat.out"),
            flag_code,
            heads_written,
            [(l.level, l.unit_number, l.param_name) for l in sorted_levels],
            values_line,
        )

        # Persist the job so the engine reads the current parameters.
        writers.write_job_directory(job, job_path)
        writers.write_tears_out(job, job_path)
        writers.write_datt_dat(job, job_path)
        writers.write_simop_dat(job, job_path)

        inord = engine.inordcalc(job_path)
        simop, _ = engine.simop(job_path, cum_out)

        if inord == -1 and simop == -1:
            # This run wrote the cumulative column headings; subsequent runs
            # may append without re-emitting the level header lines.
            heads_written = True

        runs.append(SweepRun(values_line, cum_out, inord, simop))

        if inord == 13:
            # Data-input tear error: reset TEARS.OUT so the next run starts
            # with no tear streams, and stop the sweep.
            writers.reset_tears_out(job_path)
            break

    return runs
