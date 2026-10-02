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

The primary per-stream results file is ``OPDISP.DAT``, which the calculation
phase always writes (see ``CALC.FOR`` / ``CALC1.FOR``): it carries each
stream's solids/water flow and its size-class mass distribution.  The other
parsers (``STREAMPROPS.TXT``, ``OPGRAPH.DAT``, ``LIBDISPM.DAT``) remain
supported for job directories that still contain those files.
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


_INT_RE = re.compile(r"[+-]?\d+")


def _match_ints(line: str) -> Optional[List[int]]:
    """Return a line's whitespace-separated tokens as ints, or ``None``.

    ``None`` is returned when the line is empty or any token is not a plain
    integer (so float lines, text lines and mixed lines never match).
    """
    tokens = line.split()
    if not tokens:
        return None
    out: List[int] = []
    for tok in tokens:
        if _INT_RE.fullmatch(tok) is None:
            return None
        out.append(int(tok))
    return out


def _consume_floats(lines: List[str], start: int, count: int) -> tuple:
    """Read ``count`` floats starting at ``start``, across line breaks.

    Returns ``(floats, next_line_index)``; the list may be shorter than
    ``count`` when the lines run out first.
    """
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
    return out, i


def _consume_ints(lines: List[str], start: int, count: int) -> tuple:
    """Read ``count`` ints starting at ``start``, across line breaks.

    Returns ``(ints, next_line_index)``; the list may be shorter than
    ``count`` when a non-integer line is reached or the lines run out.
    """
    out: List[int] = []
    i = start
    while len(out) < count and i < len(lines):
        line = lines[i]
        if not line.strip():
            i += 1
            continue
        m = _match_ints(line)
        if m is None or not m:
            break
        take = m[: count - len(out)]
        out.extend(take)
        if len(take) < len(m):
            return out, i + 1
        i += 1
    return out, i


def _cumulative_passing_pct(class_mass: List[float]) -> List[float]:
    """Convert differential class masses to cumulative % passing.

    ``class_mass`` holds the mass in each size class ordered from largest to
    smallest (as the engine writes them).  Returns the cumulative percentage
    passing each class's representative size, matching the engine's
    ``% passing`` convention (last class ends at 0.0).
    """
    total = sum(class_mass)
    if total <= 0:
        return []
    out: List[float] = []
    passing = total
    for mass in class_mass:
        passing -= mass
        # Clamp tiny negative values produced by float rounding.
        out.append(max(0.0, 100.0 * passing / total))
    return out


def _parse_opdisp(lines: List[str], results: Results) -> bool:
    """Parse the ``OPDISP.DAT`` layout (per-stream flows and size data).

    ``OPDISP.DAT`` is written by the calculation phase and is the engine's
    primary per-stream results file.  Layout::

        Bougainville                   job name (A80)
           1   1  25   1               NPLA NMIN NDCM NGCM
        Ore                            mineral names (A4)
         1.000                         grade matrix GRDM (NGCM rows)
           2                           DIMPP (particle property count)
         2.700     50.00               PPROP (DIMPP values)
           1   2   1   5   9   1   1   NPLT NNOD NTT NU NSM NPINP NPOUT
           1                           PLINP (plant feed streams)
           7                           PLOUT (plant product streams)
          25   1   1                   NDC NGC NSC
         <NDC representative sizes>
         2 2   2   2                   stream block: I1 I2 (IND...) -> stream
         127.8     0.000               solids flow, water flow (I2 values)
           1   1  25                   K J II (size-class block header)
         <II class masses>             differential; converted to % passing
         1 1   9                       water-only stream block (I2 == 1)
         35.11                         water flow (single value)
         ...
         199   0                       end-of-data sentinel (I2 == 99)

    Each stream block is either a unit output (``I1 == 2``, header ``I1 I2
    stream unit``), a plant feed/output/tear (``I1 == 1``) or a water feed
    (``I2 == 1``).  The size-class masses are stored differentially and are
    converted to cumulative % passing for :class:`SizeDistribution`.
    """
    n = len(lines)
    found = False
    i = 0
    while i < n:
        # Locate the next plant header: NPLA NMIN NDCM NGCM.
        while i < n:
            m = _match_ints(lines[i])
            if (
                m is not None
                and len(m) == 4
                and 1 <= m[0] <= 50      # NPLA
                and 1 <= m[1] <= 50      # NMIN
                and 3 <= m[2] <= 200     # NDCM (size classes)
                and 1 <= m[3] <= 50      # NGCM
            ):
                break
            i += 1
        if i >= n:
            break
        end = _parse_opdisp_plant(lines, i, results)
        if end is None:
            i += 1
            continue
        found = True
        i = end
    return found


def _parse_opdisp_plant(lines: List[str], start: int, results: Results) -> Optional[int]:
    """Parse one plant's section of ``OPDISP.DAT`` starting at ``start``.

    Returns the index of the line after the plant's stream data, or ``None``
    when ``start`` is not actually an ``OPDISP.DAT`` plant header.
    """
    n = len(lines)
    i = start
    header = _match_ints(lines[i])
    if header is None or len(header) < 4:
        return None
    npla, nmin, ndcm, ngcm = header[:4]
    if not (
        1 <= npla <= 50
        and 1 <= nmin <= 50
        and 3 <= ndcm <= 200
        and 1 <= ngcm <= 50
    ):
        return None
    i += 1

    # Mineral names: NMIN A4 tokens, one record in practice.
    if i >= n:
        return None
    i += 1

    # Grade matrix GRDM is only written when FLGM != 0; a following line of
    # ints means it is absent and the next value is DIMPP.
    while i < n and not lines[i].strip():
        i += 1
    if i >= n:
        return None
    if _match_ints(lines[i]) is None:
        _, i = _consume_floats(lines, i, ngcm * nmin)
        while i < n and not lines[i].strip():
            i += 1
        if i >= n:
            return None

    dimpp_m = _match_ints(lines[i])
    if dimpp_m is None:
        return None
    dimpp = dimpp_m[0]
    i += 1

    # Particle properties (DIMPP values).
    _, i = _consume_floats(lines, i, dimpp)

    # Plant header: NPLT NNOD NTT NU NSM NPINP NPOUT.
    if i >= n:
        return None
    plant = _match_ints(lines[i])
    if plant is None or len(plant) < 7:
        return None
    nplt, nnod, ntt, nu, nsm, npinp, npout = plant[:7]
    i += 1

    # Plant feed / product stream lists.
    plinp, i = _consume_ints(lines, i, npinp)
    if len(plinp) < npinp:
        return None
    plout, i = _consume_ints(lines, i, npout)
    if len(plout) < npout:
        return None

    # NDC NGC NSC line.
    if i >= n:
        return None
    m = _match_ints(lines[i])
    if m is None or len(m) < 3:
        return None
    ndc, ngc, nsc = m[:3]
    if ndc != ndcm:
        return None
    i += 1

    # Representative sizes.
    sizes, i = _consume_floats(lines, i, ndc)
    if len(sizes) < ndc:
        return None

    # ---- Stream data section ----
    feed_solids = {s: 0.0 for s in plinp}
    my_streams: List[StreamData] = []
    while True:
        while i < n and not lines[i].strip():
            i += 1
        if i >= n:
            break
        m = _match_ints(lines[i])
        if m is None or len(m) < 2:
            break
        i1, i2 = m[0], m[1]
        if i2 == 99:  # end-of-data sentinel
            i += 1
            break
        ind = m[2:]
        if not ind:
            break
        stream = ind[0]
        i += 1

        # Flow line: I2 values; solids first, water last.  A single value
        # (I2 == 1) is a pure water feed (no solids).
        flows, i = _consume_floats(lines, i, i2)
        if len(flows) < i2:
            break
        if i2 >= 2:
            solid = flows[0]
            water = flows[-1]
        else:
            solid = 0.0
            water = flows[0]

        # NGC * NSC size-class blocks: "K J II" header + II class masses.
        dist: List[float] = []
        if i2 > 1:
            nblocks = max(1, ngc * nsc)
            for _ in range(nblocks):
                while i < n and not lines[i].strip():
                    i += 1
                if i >= n:
                    break
                dm = _match_ints(lines[i])
                if dm is None or len(dm) < 3:
                    break
                ii = dm[2]
                i += 1
                vals, i = _consume_floats(lines, i, ii)
                if len(vals) < ii:
                    break
                if not dist:
                    dist = [0.0] * ii
                for idx, val in enumerate(vals):
                    if idx < len(dist):
                        dist[idx] += val

        if stream in feed_solids:
            feed_solids[stream] = solid

        sd_data = StreamData(stream=stream)
        sd_data.solid_flow = solid
        sd_data.water_flow = water
        denom = solid + water
        if denom > 0:
            sd_data.pct_solids_mass = 100.0 * solid / denom
        my_streams.append(sd_data)
        results.streams.append(sd_data)

        if dist and len(dist) == ndc:
            cum = _cumulative_passing_pct(dist)
            if cum:
                sd = SizeDistribution(stream=stream)
                sd.sizes = list(sizes)
                sd.cum_passing = cum
                results.size_distributions.append(sd)

    # Yield of solids relative to the total plant-feed solids.
    feed_total = sum(feed_solids.values())
    if feed_total > 0:
        for sd_data in my_streams:
            if sd_data.solid_flow is not None:
                sd_data.yield_solids = 100.0 * sd_data.solid_flow / feed_total
    return i


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#: File extensions (lower-cased) that may hold engine results.
RESULT_EXTENSIONS = {"out", "dat", "txt"}

#: Ordered list of (name, parser) format detectors.
_FORMAT_PARSERS = [
    ("opdisp", _parse_opdisp),
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
