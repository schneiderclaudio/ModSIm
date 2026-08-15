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
from typing import List, Optional

from ..models.job import (
    CurFile,
    DistFile,
    Job,
    JobFlagsFile,
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
def _write_dist(file_obj: DistFile, path: str) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    lines = [f"Number of data streams{file_obj.stream_count:8d} "]
    for s in file_obj.streams:
        lines.append(f"Stream     {s.num1:3d} {s.num2:3d} {s.label:<30}")
        lines.append(f"{file_obj.range_label}{len(s.ranges):8d} ")
        for r in s.ranges:
            lines.append(f"{r.index:5d}")
            lines.append(" ".join(r.bounds) + " ")
            lines.append(f"{len(r.values):5d}")
            for start in range(0, len(r.values), 8):
                lines.append(" ".join(r.values[start : start + 8]) + " ")
    lines.append("END OF FILE")
    _write_lines(path, lines)


def write_gcd(file_obj: DistFile, path: str) -> None:
    _write_dist(file_obj, path)


def write_scd(file_obj: DistFile, path: str) -> None:
    _write_dist(file_obj, path)


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
def write_cur(file_obj: CurFile, path: str) -> None:
    if file_obj.raw_lines is not None:
        _write_lines(path, file_obj.raw_lines)
        return
    lines: List[str] = []
    for u in file_obj.units:
        lines.append(f"TYPE {u.number:3d} {u.model} {u.in_stream:3d} {u.out_stream:3d}")
        for start in range(0, len(u.params), 8):
            lines.append(" ".join(u.params[start : start + 8]) + " ")
    if file_obj.outc:
        lines.append("OUTC")
        lines.extend(file_obj.outc)
    lines.append("STOP")
    _write_lines(path, lines)


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


def write_tears_out(job: Job, path: str) -> None:
    """Write the job's tear data to ``TEARS.OUT`` in ``path``.

    The engine reads ``TEARS.OUT`` during the data-input phase whenever the
    job's ``DATT.DAT`` contains a ``TEAR`` keyword.  The legacy VB6 GUI creates
    it by copying the job's ``.TEA`` file into the job directory before running
    (``FileCopy JobFileName & ".TEA", JobPath & "TEARS.OUT"``); mirror that
    behaviour here so the engine does not create an empty file and abort with
    an end-of-file read error.

    No-op when the job has no ``.TEA`` file (or it has no raw lines), since
    there is then no tear data to provide.
    """
    if job.tea is None or job.tea.raw_lines is None:
        return
    _write_lines(os.path.join(path, "TEARS.OUT"), job.tea.raw_lines)


def write_datt_dat(job: Job, path: str) -> None:
    """Write the job's ``DATT.DAT`` to ``path``.

    The engine reads ``DATT.DAT`` during the data-input phase.  The legacy VB6
    GUI builds it by concatenating the system/plant data (``CURRDATA.SYD``, a
    copy of the job's ``.sid``) with the run data (``CURRDATA.RUN``, a copy of
    the job's ``.cur``) via ``FileConCat`` (see ``SYSDAT.BAS``).  Mirror that
    behaviour here so the engine reads the job's current unit parameters
    instead of a stale ``DATT.DAT`` left in the job directory (which can cause
    a singular convergence matrix and exit code 34).

    No-op when the job has no ``.sid`` or ``.cur`` file (or they have no raw
    lines), since there is then no data to concatenate.
    """
    if job.sid is None or job.sid.raw_lines is None:
        return
    if job.cur is None or job.cur.raw_lines is None:
        return
    _write_lines(
        os.path.join(path, "DATT.DAT"),
        list(job.sid.raw_lines) + list(job.cur.raw_lines),
    )


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
}
