"""In-memory model for ModSIM legacy job files.

A ModSIM "job" is a directory of legacy text files (``.JOB``, ``.syd``,
``.siz``, ``.gcd``, ``.mat``, ``.scd``, ``.sid``, ``.cur``, ``.mop``,
``.TEA``, ``.TRN``).  This module defines the dataclasses used to represent a
job in memory.

Round-trip fidelity
-------------------
The legacy formats are produced by Fortran fixed-format ``WRITE`` statements
with precise column spacing and ``E``-notation numbers.  Reproducing every
byte from parsed floats is fragile (floating point rounding, exponent edge
cases).  To guarantee that a job read from disk and written back unchanged is
byte-identical, every :class:`JobFile` retains the exact ``raw_lines`` it was
read from.  Writers emit those lines verbatim when present, and only fall back
to regenerating from the structured fields for programmatically-created jobs.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple


@dataclass
class JobFlag:
    """A single ``"Key",#BOOL#`` entry from a ``.JOB`` file."""

    key: str
    value: bool


@dataclass
class Unit:
    """A unit (equipment item) from a ``.syd`` file.

    ``kind`` is one of ``F`` (feed), ``W`` (water) or ``O`` (other).
    """

    number: int
    type: int
    kind: str
    in_stream: int
    out_stream: int


@dataclass
class SizePoint:
    """A single ``size cumPct`` row from a ``.siz`` size distribution."""

    size: str
    cum_pct: str


@dataclass
class SizStream:
    """A stream entry from a ``.siz`` file.

    ``kind`` is one of ``"feedrate"``, ``"water"`` or ``"size"``.  Numeric
    values are kept as their original strings so they can be written back
    without loss.
    """

    num1: int
    num2: int
    label: str
    kind: str
    feedrate: Optional[str] = None
    feedrate_pct: Optional[str] = None
    water: Optional[str] = None
    water_pct: Optional[str] = None
    size_points: List[SizePoint] = field(default_factory=list)


@dataclass
class GradeRange:
    """A grade (or S-) range block from a ``.gcd`` / ``.scd`` file."""

    index: int
    bounds: List[str] = field(default_factory=list)
    values: List[str] = field(default_factory=list)


@dataclass
class DistStream:
    """A stream entry from a ``.gcd`` / ``.scd`` file."""

    num1: int
    num2: int
    label: str
    ranges: List[GradeRange] = field(default_factory=list)


@dataclass
class CurUnit:
    """A unit entry from a ``.cur`` file (``TYPE`` block)."""

    number: int
    model: str
    in_stream: int
    out_stream: int
    params: List[str] = field(default_factory=list)


@dataclass
class JobFile:
    """Base class for a single legacy job file.

    ``raw_lines`` holds the exact lines read from disk (without line
    terminators).  When present, writers emit these verbatim to guarantee a
    byte-identical round trip.
    """

    name: str
    raw_lines: Optional[List[str]] = None


@dataclass
class JobFlagsFile(JobFile):
    """Parsed ``.JOB`` file."""

    job_name: str = ""
    flags: List[JobFlag] = field(default_factory=list)


@dataclass
class SydFile(JobFile):
    """Parsed ``.syd`` file."""

    job_name: str = ""
    unit_count: int = 0
    units: List[Unit] = field(default_factory=list)


@dataclass
class SizFile(JobFile):
    """Parsed ``.siz`` file."""

    job_name: str = ""
    streams: List[SizStream] = field(default_factory=list)


@dataclass
class DistFile(JobFile):
    """Parsed ``.gcd`` or ``.scd`` file.

    ``range_label`` is ``"Number of grade ranges"`` for ``.gcd`` and
    ``"Number of S-ranges"`` for ``.scd``.
    """

    job_name: str = ""
    stream_count: int = 0
    streams: List[DistStream] = field(default_factory=list)
    range_label: str = ""


@dataclass
class MatFile(JobFile):
    """Parsed ``.mat`` file (cryptic flag lines)."""

    flag_string: str = ""


@dataclass
class MopFile(JobFile):
    """Parsed ``.mop`` file (single flag string)."""

    flag_string: str = ""


@dataclass
class TeaFile(JobFile):
    """Parsed ``.TEA`` file (stream/mineral counts)."""

    stream_count: int = 0
    mineral_count: int = 0


@dataclass
class SidFile(JobFile):
    """Parsed ``.sid`` file (system/plant/run data)."""

    job_name: str = ""


@dataclass
class CurFile(JobFile):
    """Parsed ``.cur`` file."""

    units: List[CurUnit] = field(default_factory=list)
    outc: List[str] = field(default_factory=list)


@dataclass
class TrnFile(JobFile):
    """Parsed ``.TRN`` file (flowsheet layout).

    ``raw_lines`` remains authoritative for a byte-identical round trip.  The
    structured ``unit_positions`` / ``stream_polylines`` fields hold the
    on-canvas layout so the flowsheet canvas can read and update it; when the
    layout changes the caller regenerates ``raw_lines`` from these (see
    :mod:`modsim.gui.canvas.trn_layout`).
    """

    job_name: str = ""
    unit_positions: Dict[int, Tuple[float, float]] = field(default_factory=dict)
    stream_polylines: Dict[int, List[Tuple[float, float]]] = field(default_factory=dict)


@dataclass
class Job:
    """In-memory representation of a complete ModSIM job directory.

    Each attribute holds the parsed file for that type, or ``None`` when the
    job directory does not contain that file.  ``name`` is the job name taken
    from the ``.JOB`` file (falling back to ``.syd``).
    """

    name: str = ""
    job: Optional[JobFlagsFile] = None
    syd: Optional[SydFile] = None
    siz: Optional[SizFile] = None
    gcd: Optional[DistFile] = None
    scd: Optional[DistFile] = None
    mat: Optional[MatFile] = None
    mop: Optional[MopFile] = None
    tea: Optional[TeaFile] = None
    sid: Optional[SidFile] = None
    cur: Optional[CurFile] = None
    trn: Optional[TrnFile] = None

    def files(self) -> List[JobFile]:
        """Return the non-``None`` job files in a stable order."""
        return [
            f
            for f in (
                self.job,
                self.syd,
                self.siz,
                self.gcd,
                self.scd,
                self.mat,
                self.mop,
                self.tea,
                self.sid,
                self.cur,
                self.trn,
            )
            if f is not None
        ]
