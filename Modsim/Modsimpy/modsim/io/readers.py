"""Readers for ModSIM legacy job files.

Each ``read_*`` function parses one file type into the corresponding model
object from :mod:`modsim.models.job`, preserving the exact ``raw_lines`` so a
subsequent write is byte-identical.  :func:`read_job_directory` parses every
recognised file in a job directory into a :class:`~modsim.models.job.Job`.
"""

from __future__ import annotations

import os
from typing import List, Optional

from ..models.job import (
    AmdFile,
    CurFile,
    CurUnit,
    DistFile,
    DistStream,
    FormatOutFile,
    GradeRange,
    Job,
    JobFile,
    JobFlag,
    JobFlagsFile,
    LjuFile,
    MatFile,
    MopFile,
    SidFile,
    SizFile,
    SizStream,
    SizePoint,
    SydFile,
    TeaFile,
    TrnFile,
    Unit,
)

# The set of file extensions (lower-cased) handled by this module.
HANDLED_EXTENSIONS = {
    "job",
    "syd",
    "siz",
    "gcd",
    "scd",
    "mat",
    "mop",
    "tea",
    "sid",
    "cur",
    "trn",
    "lju",
    "amd",
}


def _read_lines(path: str) -> List[str]:
    """Read a file as ASCII, returning its lines without terminators."""
    with open(path, "r", encoding="ascii", newline="") as fh:
        content = fh.read()
    return content.splitlines()


def _stream_label(line: str) -> str:
    """Extract the label from a ``Stream ...`` line (starts at column 19)."""
    return line[19:].rstrip()


# ---------------------------------------------------------------------------
# .JOB
# ---------------------------------------------------------------------------
def read_job(path: str) -> JobFlagsFile:
    """Parse a ``.JOB`` file (CSV-ish ``"Key",#BOOL#`` lines)."""
    lines = _read_lines(path)
    job_name = ""
    flags: List[JobFlag] = []
    for line in lines:
        stripped = line.strip()
        if not stripped or not stripped.startswith('"'):
            continue
        key, _, value = stripped[1:].partition('",')
        value = value.strip().strip('"')
        if value.startswith("#"):
            flags.append(JobFlag(key=key, value=value.strip("#").upper() == "TRUE"))
        else:
            job_name = value
    return JobFlagsFile(name="job", raw_lines=lines, job_name=job_name, flags=flags)


# ---------------------------------------------------------------------------
# .syd
# ---------------------------------------------------------------------------
def read_syd(path: str) -> SydFile:
    """Parse a ``.syd`` file (unit list)."""
    lines = _read_lines(path)
    job_name = lines[0].strip() if lines else ""
    unit_count = int(lines[1].strip()) if len(lines) > 1 else 0
    units: List[Unit] = []
    for line in lines[2:]:
        parts = line.split()
        if len(parts) >= 5:
            units.append(
                Unit(
                    number=int(parts[0]),
                    type=int(parts[1]),
                    kind=parts[2],
                    in_stream=int(parts[3]),
                    out_stream=int(parts[4]),
                )
            )
    return SydFile(
        name="syd",
        raw_lines=lines,
        job_name=job_name,
        unit_count=unit_count,
        units=units,
    )


# ---------------------------------------------------------------------------
# .siz
# ---------------------------------------------------------------------------
def read_siz(path: str) -> SizFile:
    """Parse a ``.siz`` file (stream size distributions)."""
    lines = _read_lines(path)
    job_name = lines[0].strip() if lines else ""
    streams: List[SizStream] = []
    i = 1
    while i < len(lines):
        line = lines[i]
        if line.startswith("Stream"):
            parts = line.split()
            num1 = int(parts[1])
            num2 = int(parts[2])
            label = _stream_label(line)
            i += 1
            if i >= len(lines):
                break
            nxt = lines[i]
            if nxt.startswith("Feedrate"):
                fp = nxt.split()
                streams.append(
                    SizStream(
                        num1=num1,
                        num2=num2,
                        label=label,
                        kind="feedrate",
                        feedrate=fp[1],
                        feedrate_pct=fp[2] if len(fp) > 2 else None,
                    )
                )
                i += 1
            elif nxt.startswith("Water feed"):
                wp = nxt.split()
                streams.append(
                    SizStream(
                        num1=num1,
                        num2=num2,
                        label=label,
                        kind="water",
                        water=wp[2],
                        water_pct=wp[3] if len(wp) > 3 else None,
                    )
                )
                i += 1
            elif nxt.startswith("Size dist"):
                n = int(nxt.split()[2])
                points: List[SizePoint] = []
                for j in range(n):
                    sp = lines[i + 1 + j].split()
                    points.append(SizePoint(size=sp[0], cum_pct=sp[1]))
                streams.append(
                    SizStream(
                        num1=num1,
                        num2=num2,
                        label=label,
                        kind="size",
                        size_points=points,
                    )
                )
                i += 1 + n
            else:
                i += 1
        else:
            i += 1
    return SizFile(name="siz", raw_lines=lines, job_name=job_name, streams=streams)


# ---------------------------------------------------------------------------
# .gcd / .scd
# ---------------------------------------------------------------------------
def _read_dist(path: str, name: str, range_label: str, has_bounds: bool) -> DistFile:
    """Parse a ``.gcd``/``.scd`` file.

    ``has_bounds`` selects the per-range layout: ``.gcd`` blocks are
    ``index / nmin nmax / count / values`` while ``.scd`` blocks are
    ``index / count / values`` (no bounds line).
    """
    lines = _read_lines(path)
    stream_count = 0
    streams: List[DistStream] = []
    i = 0
    if lines and lines[0].startswith("Number of data streams"):
        stream_count = int(lines[0].split()[-1])
        i = 1
    while i < len(lines):
        line = lines[i]
        if line.startswith("Stream"):
            parts = line.split()
            num1 = int(parts[1])
            num2 = int(parts[2])
            label = _stream_label(line)
            i += 1
            n = 0
            if i < len(lines) and lines[i].startswith("Number of"):
                n = int(lines[i].split()[-1])
                i += 1
            ranges: List[GradeRange] = []
            for _ in range(n):
                index = int(lines[i].strip())
                i += 1
                if has_bounds:
                    bounds = lines[i].split()
                    i += 1
                else:
                    bounds = []
                count = int(lines[i].strip())
                i += 1
                values: List[str] = []
                while len(values) < count and i < len(lines):
                    values.extend(lines[i].split())
                    i += 1
                ranges.append(GradeRange(index=index, bounds=bounds, values=values))
            streams.append(DistStream(num1=num1, num2=num2, label=label, ranges=ranges))
        else:
            i += 1
    return DistFile(
        name=name,
        raw_lines=lines,
        stream_count=stream_count,
        streams=streams,
        range_label=range_label,
    )


def read_gcd(path: str) -> DistFile:
    """Parse a ``.gcd`` file (grade-class distributions)."""
    return _read_dist(path, "gcd", "Number of grade ranges", has_bounds=True)


def read_scd(path: str) -> DistFile:
    """Parse a ``.scd`` file (S-class distributions)."""
    return _read_dist(path, "scd", "Number of S-ranges", has_bounds=False)


# ---------------------------------------------------------------------------
# .mat / .mop
# ---------------------------------------------------------------------------
def read_mat(path: str) -> MatFile:
    """Parse a ``.mat`` file (cryptic flag lines)."""
    lines = _read_lines(path)
    flag_string = lines[0].strip() if lines else ""
    return MatFile(name="mat", raw_lines=lines, flag_string=flag_string)


def read_mop(path: str) -> MopFile:
    """Parse a ``.mop`` file (single flag string)."""
    lines = _read_lines(path)
    flag_string = lines[0].strip() if lines else ""
    return MopFile(name="mop", raw_lines=lines, flag_string=flag_string)


# ---------------------------------------------------------------------------
# .TEA
# ---------------------------------------------------------------------------
def read_tea(path: str) -> TeaFile:
    """Parse a ``.TEA`` file (stream/mineral counts)."""
    lines = _read_lines(path)
    stream_count = 0
    mineral_count = 0
    if lines and lines[0].startswith("STRM"):
        parts = lines[0].split()
        if len(parts) >= 3:
            stream_count = int(parts[1])
            mineral_count = int(parts[2])
    return TeaFile(
        name="tea",
        raw_lines=lines,
        stream_count=stream_count,
        mineral_count=mineral_count,
    )


# ---------------------------------------------------------------------------
# .sid
# ---------------------------------------------------------------------------
def read_sid(path: str) -> SidFile:
    """Parse a ``.sid`` file (system/plant/run data)."""
    lines = _read_lines(path)
    job_name = ""
    for line in lines:
        if line.strip() and not line.startswith(("SYSTEM DATA", "PLANT DATA", "RUN DATA")):
            job_name = line.strip()
            break
    return SidFile(name="sid", raw_lines=lines, job_name=job_name)


# ---------------------------------------------------------------------------
# .cur
# ---------------------------------------------------------------------------
def read_cur(path: str) -> CurFile:
    """Parse a ``.cur`` file (unit parameters)."""
    lines = _read_lines(path)
    units: List[CurUnit] = []
    outc: List[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        if line.startswith("TYPE"):
            parts = line.split()
            unit = CurUnit(
                number=int(parts[1]),
                model=parts[2],
                noparam=int(parts[3]),
                unit_id=int(parts[4]),
            )
            i += 1
            params: List[str] = []
            while (
                len(params) < unit.noparam
                and i < len(lines)
                and not lines[i].startswith(("TYPE", "OUTC", "STOP"))
            ):
                params.extend(lines[i].split())
                i += 1
            unit.params = params[: unit.noparam]
            units.append(unit)
        elif line.startswith("OUTC"):
            i += 1
            while i < len(lines) and not lines[i].startswith("STOP"):
                outc.append(lines[i])
                i += 1
        else:
            i += 1
    return CurFile(name="cur", raw_lines=lines, units=units, outc=outc)


# ---------------------------------------------------------------------------
# .TRN
# ---------------------------------------------------------------------------
def read_trn(path: str) -> TrnFile:
    """Parse a ``.TRN`` file (flowsheet layout)."""
    lines = _read_lines(path)
    job_name = lines[1].strip() if len(lines) > 1 else ""
    return TrnFile(name="trn", raw_lines=lines, job_name=job_name)


# ---------------------------------------------------------------------------
# .lju / .amd (liberation transfer coefficients)
# ---------------------------------------------------------------------------
def read_lju(path: str) -> LjuFile:
    """Read a ``.lju`` file (Ljubljana Andrews-Mika transfer coefficients).

    The file is opaque: ``raw_lines`` is authoritative and it is staged
    verbatim to ``LJUBAMD.DAT`` before a run.
    """
    return LjuFile(name="lju", raw_lines=_read_lines(path))


def read_amd(path: str) -> AmdFile:
    """Read a ``.amd`` file (Beta-function Andrews-Mika transfer coefficients).

    Opaque; staged verbatim to ``BETAAMD.DAT`` before a run.
    """
    return AmdFile(name="amd", raw_lines=_read_lines(path))


# ---------------------------------------------------------------------------
# FORMAT.OUT (fixed-name run file)
# ---------------------------------------------------------------------------
def read_format_out(path: str) -> FormatOutFile:
    """Parse a ``FORMAT.OUT`` file (output-format options).

    ``FORMAT.OUT`` is a fixed-name run file (not part of the
    ``<name>.<ext>`` job-file round trip).  The layout mirrors what the engine
    reads in SIMOP.FOR:298-374 — flags line ``FORMAT(I1,L1,I1,4L1,I1)``,
    metal names as 8-char fields (``7A8``) and stream lists as 4-char fields
    (``20I4``) — and what the VB6 ``OUTFORMAT.FRM`` dialog writes.

    Parsing is lenient: on any anomaly the raw lines are preserved and
    whatever fields could be parsed are returned (no exception is raised), so
    a subsequent write still round-trips the file byte-identically.
    """
    lines = _read_lines(path)
    fmt = FormatOutFile(name="format_out", raw_lines=lines)
    i = 0
    if i >= len(lines):
        return fmt
    try:
        flags = lines[i]
        i += 1
        fmt.solid_units = int(flags[0])
        fmt.show_water = flags[1] == "T"
        fmt.water_units = int(flags[2])
        fmt.show_pct_solids = flags[3] == "T"
        fmt.show_yield = flags[4] == "T"
        fmt.show_minerals = flags[5] == "T"
        fmt.show_metals = flags[6] == "T"
        fmt.metal_units = int(flags[7])
        fmt.coal_flag = flags[8] == "T"
    except (IndexError, ValueError):
        return fmt
    if i >= len(lines):
        return fmt
    try:
        fmt.num_metals = int(lines[i].strip())
        i += 1
    except ValueError:
        return fmt
    if fmt.num_metals > 0:
        if i >= len(lines):
            return fmt
        names_line = lines[i]
        i += 1
        fmt.metal_names = [
            names_line[j : j + 8].strip()
            for j in range(0, len(names_line), 8)
            if names_line[j : j + 8].strip()
        ][: fmt.num_metals]
    if i >= len(lines):
        return fmt
    # The engine reads NOMIN and the mineral rows only inside IF(NOMET > 0)
    # (SIMOP.FOR:332-340); with no metals the next line is the size flag.
    if fmt.num_metals > 0:
        try:
            fmt.num_minerals = int(lines[i].strip())
            i += 1
        except ValueError:
            return fmt
        rows: List[List[float]] = []
        for _ in range(fmt.num_minerals):
            if i >= len(lines):
                break
            values = lines[i].split()
            i += 1
            row: List[float] = []
            for value in values[: fmt.num_metals]:
                try:
                    row.append(float(value))
                except ValueError:
                    continue
            rows.append(row)
        fmt.minmetal = rows
    if i >= len(lines):
        return fmt
    # Size-distribution streams.
    if lines[i].strip() == "T":
        i += 1
        fmt.size_flag = True
        if i >= len(lines):
            return fmt
        try:
            count = int(lines[i].strip())
            i += 1
        except ValueError:
            return fmt
        if count > 0:
            if i >= len(lines):
                return fmt
            streams_line = lines[i]
            i += 1
            fmt.size_streams = [
                int(streams_line[j : j + 4].strip())
                for j in range(0, len(streams_line), 4)
                if streams_line[j : j + 4].strip()
            ][:count]
            if i >= len(lines):
                return fmt
            try:
                fmt.icode = int(lines[i].strip())
                i += 1
            except ValueError:
                return fmt
    else:
        fmt.size_flag = False
        i += 1
    if i >= len(lines):
        return fmt
    # Accumulation streams.
    if lines[i].strip() == "T":
        i += 1
        fmt.accumulate_flag = True
        if i >= len(lines):
            return fmt
        try:
            count = int(lines[i].strip())
            i += 1
        except ValueError:
            return fmt
        if count > 0:
            if i >= len(lines):
                return fmt
            streams_line = lines[i]
            i += 1
            fmt.accumulate_streams = [
                int(streams_line[j : j + 4].strip())
                for j in range(0, len(streams_line), 4)
                if streams_line[j : j + 4].strip()
            ][:count]
    else:
        fmt.accumulate_flag = False
    return fmt


# ---------------------------------------------------------------------------
# Directory
# ---------------------------------------------------------------------------
def read_job_directory(path: str, name: Optional[str] = None) -> Job:
    """Parse every recognised job file for ``name`` in ``path`` into a Job.

    A ModSIM job is a set of ``<name>.<ext>`` files (``Bougainville.JOB``,
    ``Bougainville.syd``, ...) that may share a directory with other jobs.
    ``name`` is the job base name; when omitted it is inferred from the single
    ``.JOB`` file in the directory (an error is raised if that is ambiguous).

    Files that are absent are simply skipped (a job need not contain every
    file type).
    """
    if name is None:
        name = _infer_job_name(path)
    # Real job files use mixed case (``Bougainville.JOB``, ``Bougainville.TEA``,
    # ``Bougainville.TRN``); resolve names case-insensitively so jobs read
    # correctly on case-sensitive filesystems.
    actual = {f.lower(): f for f in os.listdir(path)}
    job = Job()
    for ext, reader in _READERS.items():
        real = actual.get(f"{name.lower()}.{ext}")
        if real is None:
            continue
        parsed = reader(os.path.join(path, real))
        _attach(job, parsed)
    job.name = _job_name(job) or name
    return job


def _infer_job_name(path: str) -> str:
    """Infer the job base name from the ``.JOB`` files in ``path``."""
    jobs = [f for f in os.listdir(path) if f.lower().endswith(".job")]
    if len(jobs) == 1:
        return os.path.splitext(jobs[0])[0]
    if not jobs:
        raise ValueError(f"no .JOB file found in {path!r}")
    raise ValueError(
        f"multiple .JOB files found in {path!r}; specify the job name explicitly"
    )


def _attach(job: Job, parsed: JobFile) -> None:
    """Attach a parsed file object to the job by its type."""
    if isinstance(parsed, JobFlagsFile):
        job.job = parsed
    elif isinstance(parsed, SydFile):
        job.syd = parsed
    elif isinstance(parsed, SizFile):
        job.siz = parsed
    elif isinstance(parsed, DistFile):
        if parsed.name == "gcd":
            job.gcd = parsed
        else:
            job.scd = parsed
    elif isinstance(parsed, MatFile):
        job.mat = parsed
    elif isinstance(parsed, MopFile):
        job.mop = parsed
    elif isinstance(parsed, TeaFile):
        job.tea = parsed
    elif isinstance(parsed, SidFile):
        job.sid = parsed
    elif isinstance(parsed, CurFile):
        job.cur = parsed
    elif isinstance(parsed, TrnFile):
        job.trn = parsed
    elif isinstance(parsed, LjuFile):
        job.lju = parsed
    elif isinstance(parsed, AmdFile):
        job.amd = parsed


def _job_name(job: Job) -> str:
    """Derive the job name, preferring ``.JOB`` then ``.syd``."""
    if job.job and job.job.job_name:
        return job.job.job_name
    if job.syd and job.syd.job_name:
        return job.syd.job_name
    if job.sid and job.sid.job_name:
        return job.sid.job_name
    return ""


_READERS = {
    "job": read_job,
    "syd": read_syd,
    "siz": read_siz,
    "gcd": read_gcd,
    "scd": read_scd,
    "mat": read_mat,
    "mop": read_mop,
    "tea": read_tea,
    "sid": read_sid,
    "cur": read_cur,
    "trn": read_trn,
    "lju": read_lju,
    "amd": read_amd,
}
