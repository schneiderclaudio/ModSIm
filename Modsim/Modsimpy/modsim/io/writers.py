"""Writers for ModSIM legacy job files.

Each ``write_*`` function writes one file type back to disk in the legacy
format.  When the corresponding model object carries ``raw_lines`` (which it
always does after a read), those lines are written verbatim, guaranteeing a
byte-identical round trip.  For programmatically-created objects (no
``raw_lines``) the writer regenerates the file from the structured fields.
"""

from __future__ import annotations

import math
import os
from typing import List, Optional, Tuple

from ..models.job import (
    AmdFile,
    CurFile,
    DistFile,
    FormatOutFile,
    Job,
    JobFlagsFile,
    LjuFile,
    MatFile,
    MopFile,
    SidFile,
    SizFile,
    SydFile,
    TeaFile,
    TrnFile,
)

# Extension (lower-cased) used when writing each file type.
_EXTENSIONS = {
    "job": "JOB",
    "syd": "syd",
    "siz": "siz",
    "gcd": "gcd",
    "scd": "scd",
    "mat": "mat",
    "mop": "mop",
    "tea": "TEA",
    "sid": "sid",
    "cur": "cur",
    "trn": "TRN",
    "lju": "lju",
    "amd": "amd",
}


def _write_lines(path: str, lines: List[str]) -> None:
    """Write lines to ``path`` using the legacy CRLF line ending."""
    with open(path, "w", encoding="ascii", newline="") as fh:
        fh.write("\r\n".join(lines))
        fh.write("\r\n")


def _fmt_e(value: float) -> str:
    """Format a float in the legacy ``E``-notation (e.g. ``1.2780E+2``).

    Mantissa has four decimal places and the exponent carries a sign with no
    leading zero.  This mirrors the Fortran fixed-format output used by the
    engine.
    """
    if value == 0.0:
        return "0.0000E+0"
    exp = int(math.floor(math.log10(abs(value)) + 1e-12))
    mant = value / (10.0 ** exp)
    if abs(mant) >= 10.0 - 1e-12:
        mant /= 10.0
        exp += 1
    sign = "+" if exp >= 0 else "-"
    return f"{mant:.4f}E{sign}{abs(exp)}"


def _fmt_f(value: float) -> str:
    """Format a float with four decimal places (e.g. ``100.0000``)."""
    return f"{value:.4f}"


# ---------------------------------------------------------------------------
# .JOB
# ---------------------------------------------------------------------------
def write_job(file_obj: JobFlagsFile, path: str) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    lines = [f'"Job name","{file_obj.job_name}"']
    for flag in file_obj.flags:
        lines.append(f'"{flag.key}",#{"TRUE" if flag.value else "FALSE"}#')
    _write_lines(path, lines)


# ---------------------------------------------------------------------------
# .syd
# ---------------------------------------------------------------------------
def write_syd(file_obj: SydFile, path: str) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    lines = [file_obj.job_name, f"{file_obj.unit_count:2d} "]
    for u in file_obj.units:
        lines.append(
            f"{u.number:2d} {u.type:3d} {u.kind} {u.in_stream:3d} {u.out_stream:3d} "
        )
    _write_lines(path, lines)


# ---------------------------------------------------------------------------
# .siz
# ---------------------------------------------------------------------------
def write_siz(file_obj: SizFile, path: str) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    lines = [file_obj.job_name]
    for s in file_obj.streams:
        lines.append(f"Stream     {s.num1:3d} {s.num2:3d} {s.label:<30}")
        if s.kind == "feedrate":
            lines.append(
                f"Feedrate  {_fmt_e(float(s.feedrate or 0.0))} "
                f"{_fmt_f(float(s.feedrate_pct or 0.0))}"
            )
        elif s.kind == "water":
            lines.append(
                f"Water feed {_fmt_e(float(s.water or 0.0))} "
                f"{_fmt_f(float(s.water_pct or 0.0))}"
            )
        elif s.kind == "size":
            lines.append(f"Size dist  {len(s.size_points):3d}")
            for p in s.size_points:
                lines.append(f"{_fmt_e(float(p.size))} {_fmt_f(float(p.cum_pct))}")
    lines.append("END OF FILE")
    _write_lines(path, lines)


# ---------------------------------------------------------------------------
# .gcd / .scd
# ---------------------------------------------------------------------------
def _write_dist(file_obj: DistFile, path: str, has_bounds: bool) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    lines = [f"Number of data streams{file_obj.stream_count:8d} "]
    for s in file_obj.streams:
        lines.append(f"Stream     {s.num1:3d} {s.num2:3d} {s.label:<30}")
        lines.append(f"{file_obj.range_label}{len(s.ranges):8d} ")
        for r in s.ranges:
            lines.append(f"{r.index:5d}")
            if has_bounds:
                lines.append(" ".join(r.bounds) + " ")
            lines.append(f"{len(r.values):5d}")
            for start in range(0, len(r.values), 8):
                lines.append(" ".join(r.values[start : start + 8]) + " ")
    lines.append("END OF FILE")
    _write_lines(path, lines)


def write_gcd(file_obj: DistFile, path: str) -> None:
    _write_dist(file_obj, path, has_bounds=True)


def write_scd(file_obj: DistFile, path: str) -> None:
    _write_dist(file_obj, path, has_bounds=False)


# ---------------------------------------------------------------------------
# .mat / .mop
# ---------------------------------------------------------------------------
def write_mat(file_obj: MatFile, path: str) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    _write_lines(path, [file_obj.flag_string])


def write_mop(file_obj: MopFile, path: str) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    _write_lines(path, [file_obj.flag_string])


# ---------------------------------------------------------------------------
# .cur
# ---------------------------------------------------------------------------
def _render_cur_lines(file_obj: CurFile) -> List[str]:
    """Render the lines of a ``.cur`` file from its structured data."""
    lines: List[str] = []
    for u in file_obj.units:
        lines.append(f"TYPE {u.number:3d} {u.model} {u.noparam:3d} {u.unit_id:3d}")
        # The engine reads exactly ``NoPARAM`` values, so the file must carry
        # exactly ``noparam`` parameters: pad with zeroes when short, truncate
        # when long.
        params = list(u.params)
        if len(params) < u.noparam:
            params += ["0.0000E+0"] * (u.noparam - len(params))
        else:
            params = params[: u.noparam]
        for start in range(0, len(params), 8):
            lines.append(" ".join(params[start : start + 8]) + " ")
    if file_obj.outc:
        lines.append("OUTC")
        lines.extend(file_obj.outc)
    lines.append("STOP")
    return lines


def write_cur(file_obj: CurFile, path: str) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    _write_lines(path, _render_cur_lines(file_obj))


# ---------------------------------------------------------------------------
# .TEA / .sid / .TRN
# ---------------------------------------------------------------------------
def write_tea(file_obj: TeaFile, path: str) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    raise NotImplementedError(
        "Regenerating .TEA from structured data is not supported; "
        "read the file first so raw_lines are available."
    )


def write_sid(file_obj: SidFile, path: str) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    raise NotImplementedError(
        "Regenerating .sid from structured data is not supported; "
        "read the file first so raw_lines are available."
    )


def write_trn(file_obj: TrnFile, path: str) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    raise NotImplementedError(
        "Regenerating .TRN from structured data is not supported; "
        "read the file first so raw_lines are available."
    )


# ---------------------------------------------------------------------------
# .lju / .amd (liberation transfer coefficients)
# ---------------------------------------------------------------------------
def write_lju(file_obj: LjuFile, path: str) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    raise NotImplementedError(
        "Regenerating .lju from structured data is not supported; "
        "read the file first so raw_lines are available."
    )


def write_amd(file_obj: AmdFile, path: str) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    raise NotImplementedError(
        "Regenerating .amd from structured data is not supported; "
        "read the file first so raw_lines are available."
    )


# ---------------------------------------------------------------------------
# Directory
# ---------------------------------------------------------------------------
def write_job_directory(job: Job, path: str) -> None:
    """Write every file in ``job`` to ``path`` in the legacy format.

    ``path`` is created if it does not exist.  Only the file types present in
    the job are written.
    """
    os.makedirs(path, exist_ok=True)
    for file_obj in job.files():
        ext = _EXTENSIONS[file_obj.name]
        writer = _WRITERS[file_obj.name]
        writer(file_obj, os.path.join(path, f"{job.name}.{ext}"))


#: Fallback record written to ``TEARS.OUT`` when the job has no ``.TEA`` file.
#: The legacy VB6 GUI writes this record when no tear data exists
#: (``Print #41, "LAST 0 0 0 0"``, MDIMod.frm:388-390) so the engine's tear
#: reading phase sees an explicit end-of-tear-data marker instead of hitting
#: end-of-file and aborting with the data-input tear error.
TEARS_RESET_LINES = ["LAST 0 0 0 0"]


def write_tears_out(job: Job, path: str) -> None:
    """Write the job's tear data to ``TEARS.OUT`` in ``path``.

    The engine reads ``TEARS.OUT`` during the data-input phase whenever the
    job's ``DATT.DAT`` contains a ``TEAR`` keyword.  The legacy VB6 GUI creates
    it by copying the job's ``.TEA`` file into the job directory before running
    (``FileCopy JobFileName & ".TEA", JobPath & "TEARS.OUT"``); mirror that
    behaviour here so the engine does not create an empty file and abort with
    an end-of-file read error.

    When the job has no ``.TEA`` file (or it has no raw lines), the VB6
    fallback record ``LAST 0 0 0 0`` is written instead
    (``Print #41, "LAST 0 0 0 0"``, MDIMod.frm:388-390), so the engine reads an
    explicit end-of-tear-data marker rather than an empty file.
    """
    if job.tea is not None and job.tea.raw_lines is not None:
        _write_lines(os.path.join(path, "TEARS.OUT"), job.tea.raw_lines)
        return
    _write_lines(os.path.join(path, "TEARS.OUT"), TEARS_RESET_LINES)


def reset_tears_out(path: str) -> None:
    """Reset the job's ``TEARS.OUT`` to the tear-data reset record.

    The legacy VB6 GUI clears tear data by writing ``LAST 0 0 0 0`` to
    ``TEARS.OUT`` (MDIMod.frm:388-390).  The engine then reads an explicit
    end-of-tear-data marker on the next run, which avoids the data-input tear
    error (exit code 13) caused by a stale or unreadable ``TEARS.OUT``.
    """
    _write_lines(os.path.join(path, "TEARS.OUT"), TEARS_RESET_LINES)


def write_datt_dat(job: Job, path: str) -> None:
    """Write the job's ``DATT.DAT`` to ``path``.

    The engine reads ``DATT.DAT`` during the data-input phase.  The legacy VB6
    GUI builds it by concatenating the system/plant data (``CURRDATA.SYD``, a
    copy of the job's ``.sid``) with the run data (``CURRDATA.RUN``, a copy of
    the job's ``.cur``) via ``FileConCat`` (see ``SYSDAT.BAS``).  Mirror that
    behaviour here so the engine reads the job's current unit parameters
    instead of a stale ``DATT.DAT`` left in the job directory (which can cause
    a singular convergence matrix and exit code 34).

    No-op when the job has no ``.sid`` or ``.cur`` file, since there is then no
    data to concatenate.  The ``.cur`` portion is rendered from the structured
    unit data even after a parameter-edit dialog has cleared its ``raw_lines``,
    so edited parameters always reach the engine.
    """
    if job.sid is None or job.cur is None:
        return
    if job.sid.raw_lines is None:
        # The .sid is never regenerated programmatically; without raw lines
        # there is nothing authoritative to concatenate.
        return
    cur_lines = (
        job.cur.raw_lines
        if job.cur.raw_lines is not None
        else _render_cur_lines(job.cur)
    )
    _write_lines(
        os.path.join(path, "DATT.DAT"),
        list(job.sid.raw_lines) + list(cur_lines),
    )


def write_simop_dat(job: Job, path: str) -> None:
    """Write the job's ``SIMOP.DAT`` to ``path``.

    The engine reads ``SIMOP.DAT`` during the SIMOP (output-format) phase.  The
    legacy VB6 GUI creates it by copying the job's ``.mop`` file into the job
    directory before running
    (``FileCopy JobFileName & ".mop", RTrim(JobPath) & "SIMOP.DAT"``); mirror
    that behaviour here so the engine's SIMOP phase receives the job's
    output-format data instead of a stale copy.

    When the job has no ``.mop`` file there is nothing to provide and no file
    is written.  When the ``.mop`` has been edited programmatically (no
    ``raw_lines``), the writer regenerates the file from ``flag_string``.
    """
    if job.mop is None:
        return
    lines = (
        job.mop.raw_lines
        if job.mop.raw_lines is not None
        else [job.mop.flag_string]
    )
    _write_lines(os.path.join(path, "SIMOP.DAT"), lines)


def write_liberation_files(job: Job, path: str) -> None:
    """Stage the job's liberation transfer-coefficient files for a run.

    The engine's mill model reads ``LJUBAMD.DAT`` / ``BETAAMD.DAT`` from the
    job directory when a mill has the Ljubljana / Beta liberation model enabled
    (``MILLMODS.FOR`` ``CONDBF``).  Those files are not shipped — the legacy
    VB6 GUI creates them before every run by copying the job's ``.lju`` /
    ``.amd`` files (``MDIMod.frm:309-319``).  Mirror that here so a
    liberation-enabled job does not fail with ``Error calculating conditional
    breakage function`` (exit 44) or a singular convergence matrix (exit 34).

    No-op when a job has no ``.lju`` / ``.amd`` file.
    """
    if job.lju is not None and job.lju.raw_lines is not None:
        _write_lines(os.path.join(path, "LJUBAMD.DAT"), job.lju.raw_lines)
    if job.amd is not None and job.amd.raw_lines is not None:
        _write_lines(os.path.join(path, "BETAAMD.DAT"), job.amd.raw_lines)


# ---------------------------------------------------------------------------
# FORMAT.OUT / CURRDATA.SYD / Repeat.out (fixed-name run files)
# ---------------------------------------------------------------------------
def write_format_out(format_out: FormatOutFile, path: str) -> None:
    """Write a ``FORMAT.OUT`` file.

    ``FORMAT.OUT`` holds the output-format options for a run.  The engine
    reads it during the SIMOP phase (SIMOP.FOR:298-374): the flags line is
    read as ``FORMAT(I1,L1,I1,4L1,I1)``, metal names as ``7A8`` and the
    size/accumulation stream lists as ``(20I4)``.  The layout mirrors the
    VB6 dialog writer ``OUTFORMAT.FRM`` (``CmdAccept_Click``).

    Regeneration follows the engine's conditional reads: with no metals
    (``num_metals == 0``) the mineral count and rows are omitted (the engine
    reads ``NOMIN`` only inside ``IF(NOMET > 0)``, SIMOP.FOR:332-340), and a
    size/accumulation flag is written as ``"T"`` only when its stream list is
    non-empty (otherwise ``"F"``, matching SIMOP.FOR:348-371).

    When ``format_out.raw_lines`` is present they are written verbatim;
    otherwise the file is regenerated from the structured fields.
    """
    if format_out.raw_lines is not None:
        _write_lines(path, format_out.raw_lines)
        return
    lines: List[str] = []
    lines.append(
        f"{format_out.solid_units}"
        f"{'T' if format_out.show_water else 'F'}"
        f"{format_out.water_units}"
        f"{'T' if format_out.show_pct_solids else 'F'}"
        f"{'T' if format_out.show_yield else 'F'}"
        f"{'T' if format_out.show_minerals else 'F'}"
        f"{'T' if format_out.show_metals else 'F'}"
        f"{format_out.metal_units}"
        f"{'T' if format_out.coal_flag else 'F'}"
    )
    lines.append(str(format_out.num_metals))
    if format_out.num_metals > 0:
        names = format_out.metal_names[: format_out.num_metals]
        lines.append("".join(name.ljust(8) for name in names))
        # The engine reads NOMIN and the mineral rows only inside
        # IF(NOMET > 0) (SIMOP.FOR:332-340); with no metals there is nothing
        # to grade, so skip straight to the size-distribution flag line.
        lines.append(str(format_out.num_minerals))
        for row_idx in range(format_out.num_minerals):
            row = (
                format_out.minmetal[row_idx]
                if row_idx < len(format_out.minmetal)
                else []
            )
            values = [float(v) for v in row[: format_out.num_metals]]
            values += [0.0] * (format_out.num_metals - len(values))
            lines.append(" ".join(_fmt_e(v) for v in values))
    if format_out.size_flag and len(format_out.size_streams) > 0:
        lines.append("T")
        lines.append(str(len(format_out.size_streams)))
        lines.append("".join(f"{s:4d}" for s in format_out.size_streams))
        lines.append(str(format_out.icode))
    else:
        lines.append("F")
    if format_out.accumulate_flag and len(format_out.accumulate_streams) > 0:
        lines.append("T")
        lines.append(str(len(format_out.accumulate_streams)))
        lines.append("".join(f"{s:4d}" for s in format_out.accumulate_streams))
    else:
        lines.append("F")
    _write_lines(path, lines)


def write_currdat_syd(job: Job, path: str) -> None:
    """Write the job's system/plant data to ``CURRDATA.SYD`` in ``path``.

    The engine's liberation models open ``CURRDATA.SYD`` in the job directory
    (LJUBAMD.FOR:40, BETAAMD.FOR:47).  The legacy VB6 GUI creates it by
    copying the job's ``.sid`` file into the job directory
    (``FileCopy JobFileName & ".sid", JobPath & "CURRDATA.SYD"``,
    MDIMod.frm:288); mirror that here.

    No-op when the job has no ``.sid`` file (or it has no raw lines).
    """
    if job.sid is None or job.sid.raw_lines is None:
        return
    _write_lines(os.path.join(path, "CURRDATA.SYD"), job.sid.raw_lines)


def write_repeat_out(
    path: str,
    flag_code: str,
    heads_written: bool,
    levels: List[Tuple[int, int, str]],
    values_line: str,
) -> None:
    """Write a ``Repeat.out`` file for repetitive simulations.

    The engine reads ``Repeat.out`` during the SIMOP phase when cumulative
    output is requested (SIMOP.FOR:378-397): line 1 is the 5-character flag
    code ``(A5)``, line 2 the heads-written flag ``(A1)``, then for each
    active level a ``READ(*,*) Level, RepUnitNumber`` line followed by an
    ``(A80)`` parameter-name line, and finally the ``(A80)`` sweep-values
    line.

    ``flag_code`` must be exactly 5 characters (``'T'`` marks an active
    level).  ``levels`` lists the ``(level, unit_number, param_name)``
    entries for the active levels in ascending level order; they are emitted
    only when ``heads_written`` is ``False``.  ``values_line`` is the
    ``(A80)`` hold-parameters / sweep-values line.
    """
    lines: List[str] = [flag_code, "T" if heads_written else "F"]
    if not heads_written:
        for level, unit_number, param_name in levels:
            lines.append(f"{level} {unit_number}")
            lines.append(param_name)
    lines.append(values_line)
    _write_lines(path, lines)


_WRITERS = {
    "job": write_job,
    "syd": write_syd,
    "siz": write_siz,
    "gcd": write_gcd,
    "scd": write_scd,
    "mat": write_mat,
    "mop": write_mop,
    "tea": write_tea,
    "sid": write_sid,
    "cur": write_cur,
    "trn": write_trn,
    "lju": write_lju,
    "amd": write_amd,
}
