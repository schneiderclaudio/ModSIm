"""Parser for ModSIM engine result files.

The Fortran engine writes a variety of result files into the job directory
after a simulation completes.  The exact format varies by engine version and
by which report options were enabled, so this module implements a *robust*
parser that recognises several common layouts and extracts the structured
data the GUI needs for graphing:

* stream size distributions (size vs cumulative % passing),
* liberation spectra (unconditional and conditional-on-size),
* stream grade/recovery style data (flowrates, % solids, yields).

The parser is deliberately tolerant: it scans every ``.OUT`` / ``.DAT`` /
``.TXT`` file in the job directory, detects which layout each file uses, and
skips files it cannot make sense of (e.g. binary Fortran unformatted files)
without raising.  The result is a :class:`Results` object holding plain
dataclasses that the plotting layer consumes.
"""

from __future__ import annotations

import os
import re
from dataclasses import dataclass, field
from typing import Dict, List, Optional

# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------


@dataclass
class SizeDistribution:
    """A stream's size distribution: representative size vs cumulative passing.

    ``sizes`` are in metres; ``cum_passing`` is the cumulative % passing
    (0-100).  Both lists are parallel and sorted by decreasing size.
    """

    stream: int
    label: str = ""
    sizes: List[float] = field(default_factory=list)
    cum_passing: List[float] = field(default_factory=list)

    def points(self) -> List[tuple]:
        """Return ``(size, cum_passing)`` pairs for plotting."""
        return list(zip(self.sizes, self.cum_passing))


@dataclass
class LiberationSpectrum:
    """A liberation spectrum for a stream.

    ``classes`` are the liberation class indices (1-based); ``values`` are the
    mass fractions in each class.  ``kind`` is ``"unconditional"`` or
    ``"conditional"``.
    """

    stream: int
    kind: str
    label: str = ""
    classes: List[int] = field(default_factory=list)
    values: List[float] = field(default_factory=list)

    def points(self) -> List[tuple]:
        """Return ``(class_index, value)`` pairs for plotting."""
        return list(zip(self.classes, self.values))


@dataclass
class StreamData:
    """Summary properties of a stream (flowrates, % solids, yield)."""

    stream: int
    label: str = ""
    solid_flow: Optional[float] = None  # tonne/hr
    water_flow: Optional[float] = None  # m^3/hr
    slurry_flow: Optional[float] = None  # kg/s
    slurry_vol_flow: Optional[float] = None  # m^3/s
    pct_solids_mass: Optional[float] = None
    pct_solids_vol: Optional[float] = None
    yield_solids: Optional[float] = None


@dataclass
class Results:
    """Structured results parsed from a job directory."""

    job_dir: str
    size_distributions: List[SizeDistribution] = field(default_factory=list)
    liberation_spectra: List[LiberationSpectrum] = field(default_factory=list)
    streams: List[StreamData] = field(default_factory=list)
    sources: List[str] = field(default_factory=list)

    def size_distribution(self, stream: int) -> Optional[SizeDistribution]:
        """Return the size distribution for ``stream`` or ``None``."""
        for sd in self.size_distributions:
            if sd.stream == stream:
                return sd
        return None

    def liberation_spectra_for(self, stream: int) -> List[LiberationSpectrum]:
        """Return all liberation spectra belonging to ``stream``."""
        return [ls for ls in self.liberation_spectra if ls.stream == stream]

    def stream(self, number: int) -> Optional[StreamData]:
        """Return the :class:`StreamData` for ``number`` or ``None``."""
        for s in self.streams:
            if s.stream == number:
                return s
        return None


# ---------------------------------------------------------------------------
# Numeric helpers
# ---------------------------------------------------------------------------

_FLOAT_RE = re.compile(
    r"[-+]?(?:\d+\.?\d*|\.\d+)(?:[eEdD][-+]?\d+)?"
)


def _to_float(token: str) -> Optional[float]:
    """Convert a token to float, tolerating Fortran ``D`` exponents."""
    if token is None:
        return None
    token = token.strip().replace("D", "E").replace("d", "e")
    try:
        return float(token)
    except ValueError:
        return None


def _read_text(path: str) -> List[str]:
    """Read a file as text, tolerating a leading UTF-8 BOM and CRLF."""
    with open(path, "rb") as fh:
        raw = fh.read()
    # Strip a UTF-8 BOM if present.
    if raw.startswith(b"\xef\xbb\xbf"):
        raw = raw[3:]
    try:
        text = raw.decode("utf-8")
    except UnicodeDecodeError:
        text = raw.decode("latin-1")
    return text.splitlines()


def _is_text(path: str) -> bool:
    """Heuristic: a file is text if it contains no NUL bytes in the first KB."""
    with open(path, "rb") as fh:
        head = fh.read(1024)
    return b"\x00" not in head


# ---------------------------------------------------------------------------
# Format-specific parsers
# ---------------------------------------------------------------------------


def _parse_streamprops(lines: List[str], results: Results) -> bool:
    """Parse the ``STREAMPROPS.TXT`` layout.

    Layout (per stream)::

        Stream number:                 1
         Bougainville
        Solid flowrate:                460.08  tonne/hr
        Water flowrate:                  0.00 m^3/hr
        ...
        Simulated size distribution
        Number of size classes:       25
        80% passing size:             0.648E-02 m
        ...
               Size      % passing
               meters
              0.168E-01     99.85
              ...
    """
    i = 0
    n = len(lines)
    found = False
    while i < n:
        line = lines[i]
        m = re.match(r"\s*Stream number:\s*(\d+)", line)
        if not m:
            i += 1
            continue
        found = True
        stream_no = int(m.group(1))
        label = ""
        if i + 1 < n:
            label = lines[i + 1].strip()
        sd = SizeDistribution(stream=stream_no, label=label)
        sd_data = StreamData(stream=stream_no, label=label)
        i += 1
        # Scan the block until the next "Stream number:" line.
        while i < n and not re.match(r"\s*Stream number:\s*\d+", lines[i]):
            cur = lines[i]
            fm = re.match(r"\s*Solid flowrate:\s*([-\d.EeDd+]+)", cur)
            if fm:
                sd_data.solid_flow = _to_float(fm.group(1))
            wm = re.match(r"\s*Water flowrate:\s*([-\d.EeDd+]+)", cur)
            if wm:
                sd_data.water_flow = _to_float(wm.group(1))
            sm = re.match(r"\s*Slurry flowrate:\s*([-\d.EeDd+]+)", cur)
            if sm:
                sd_data.slurry_flow = _to_float(sm.group(1))
            vm = re.match(r"\s*Slurry volumetric flowrate:\s*([-\d.EeDd+]+)", cur)
            if vm:
                sd_data.slurry_vol_flow = _to_float(vm.group(1))
            pm = re.match(r"\s*Percent solids by mass:\s*([-\d.EeDd+]+)", cur)
            if pm:
                sd_data.pct_solids_mass = _to_float(pm.group(1))
            pv = re.match(r"\s*Percent solids by volume:\s*([-\d.EeDd+]+)", cur)
            if pv:
                sd_data.pct_solids_vol = _to_float(pv.group(1))
            ym = re.match(r"\s*Yield of solids:\s*([-\d.EeDd+]+)", cur)
            if ym:
                sd_data.yield_solids = _to_float(ym.group(1))
            if "Simulated size distribution" in cur:
                # Advance to the data rows: skip "Number of size classes",
                # the passing-size lines and the "Size % passing" header.
                while i < n and not re.match(r"\s*Size\s+% passing", lines[i]):
                    i += 1
                i += 1  # skip "meters"
                while i < n:
                    row = lines[i].split()
                    if len(row) >= 2:
                        size = _to_float(row[0])
                        pct = _to_float(row[1])
                        if size is not None and pct is not None:
                            sd.sizes.append(size)
                            sd.cum_passing.append(pct)
                        i += 1
                    else:
                        break
                continue
            i += 1
        if sd.sizes:
            results.size_distributions.append(sd)
        results.streams.append(sd_data)
    return found


def _parse_opgraph(lines: List[str], results: Results) -> bool:
    """Parse the ``OPGRAPH.DAT`` layout.

    Layout::

        Bougainville
        Number of classes   25    1    1
        Number of streams     9
        ...
        Representative sizes
           <n floats>
        Mesh sizes
           <n floats>
        Stream    1
           <n floats>   (cumulative passing, fraction 0-1)
        ...
    """
    n_classes = 0
    n_streams = 0
    rep_sizes: List[float] = []
    i = 0
    n = len(lines)
    found = False
    while i < n:
        line = lines[i]
        m = re.match(r"\s*Number of classes\s+(\d+)", line)
        if m:
            n_classes = int(m.group(1))
            found = True
        m = re.match(r"\s*Number of streams\s+(\d+)", line)
        if m:
            n_streams = int(m.group(1))
        if "Representative sizes" in line:
            i += 1
            rep_sizes = _read_floats_until(lines, i, n_classes)
            i += len(rep_sizes)
            continue
        m = re.match(r"\s*Stream\s+(\d+)\s*$", line)
        if m and n_classes:
            stream_no = int(m.group(1))
            i += 1
            vals = _read_floats_until(lines, i, n_classes)
            i += len(vals)
            if vals:
                sd = SizeDistribution(stream=stream_no)
                sd.sizes = list(rep_sizes)
                # OPGRAPH stores cumulative passing as a fraction (0-1).
                sd.cum_passing = [v * 100.0 for v in vals]
                results.size_distributions.append(sd)
            continue
        i += 1
    return found


def _read_floats_until(lines: List[str], start: int, count: int) -> List[float]:
    """Read exactly ``count`` floats starting at ``start`` across lines."""
    out: List[float] = []
    i = start
    while len(out) < count and i < len(lines):
        for token in lines[i].split():
            val = _to_float(token)
            if val is not None:
                out.append(val)
                if len(out) >= count:
                    break
        i += 1
    return out[:count]


def _parse_libdisp(lines: List[str], results: Results) -> bool:
    """Parse the ``LIBDISPM.DAT`` layout (liberation spectra).

    Layout::

            1
        Unconditional spectrum for stream    6
          1 0.00201
          2 0.01499
          ...
        Conditional-on-size spectra for stream    6
          1 1.00000
          ...
    """
    i = 0
    n = len(lines)
    found = False
    while i < n:
        line = lines[i]
        m = re.match(
            r"\s*(Unconditional|Conditional-on-size) spectr(?:um|a) for stream\s+(\d+)",
            line,
        )
        if not m:
            i += 1
            continue
        found = True
        kind = "unconditional" if m.group(1).startswith("Unconditional") else "conditional"
        stream_no = int(m.group(2))
        ls = LiberationSpectrum(stream=stream_no, kind=kind)
        i += 1
        while i < n:
            row = lines[i].split()
            if len(row) >= 2:
                cls = _to_float(row[0])
                val = _to_float(row[1])
                if cls is not None and val is not None:
                    ls.classes.append(int(cls))
                    ls.values.append(val)
                    i += 1
                else:
                    # A non-numeric first token ends this spectrum block.
                    break
            else:
                break
        results.liberation_spectra.append(ls)
    return found


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#: File extensions (lower-cased) that may hold engine results.
RESULT_EXTENSIONS = {"out", "dat", "txt"}

#: Ordered list of (name, parser) format detectors.
_FORMAT_PARSERS = [
    ("streamprops", _parse_streamprops),
    ("opgraph", _parse_opgraph),
    ("libdisp", _parse_libdisp),
]


def parse_results(job_dir: str) -> Results:
    """Parse engine result files in ``job_dir`` into a :class:`Results`.

    Every ``.OUT`` / ``.DAT`` / ``.TXT`` file in the directory is examined.
    Recognised layouts contribute size distributions, liberation spectra and
    stream data; unrecognised or binary files are skipped silently.  The
    returned :class:`Results` is always populated (possibly empty) and never
    raises for a missing or unreadable directory.
    """
    results = Results(job_dir=job_dir)
    if not os.path.isdir(job_dir):
        return results

    for fname in sorted(os.listdir(job_dir)):
        path = os.path.join(job_dir, fname)
        if not os.path.isfile(path):
            continue
        ext = os.path.splitext(fname)[1].lstrip(".").lower()
        if ext not in RESULT_EXTENSIONS:
            continue
        if not _is_text(path):
            continue
        try:
            lines = _read_text(path)
        except (OSError, UnicodeError):
            continue
        for name, parser in _FORMAT_PARSERS:
            try:
                if parser(list(lines), results):
                    results.sources.append(fname)
                    break
            except Exception:
                # A malformed file must never break the whole parse.
                continue
    return results
