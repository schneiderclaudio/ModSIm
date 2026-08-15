"""Parsing and regeneration of the ModSIM ``.TRN`` flowsheet-layout file.

The ``.TRN`` file is the drawing file written by the legacy VB6 flowsheet
editor.  It records, among other things, the on-canvas position of every unit
and the polyline route of every stream.  This module provides a small,
Qt-free layer that:

* parses the raw lines of a ``.TRN`` file into a :class:`TrnLayout` (unit
  positions and stream polylines, plus the header/footer segments it needs to
  write the file back), and
* regenerates the raw lines from that structured layout so that moving a unit
  or re-routing a stream survives a save round-trip.

The engine keeps the bulk of the file byte-identical: only the unit-position
section, the stream section and the derived header counts are rebuilt; the
annotations/flyout/font footer is preserved verbatim.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple


@dataclass
class UnitRecord:
    """A single unit placed on the canvas.

    ``x``/``y`` are the on-canvas coordinates in the legacy drawing space.
    ``irr`` and ``id`` are carried through verbatim (they are not re-interpreted
    here).  ``extra_lines`` captures any trailing per-unit data lines that some
    equipment types append (e.g. ``NOCELL`` for type 6, ``CONVEY_X/Y`` for type
    62) so they survive a save round-trip.
    """

    number: int
    x: float
    y: float
    irr: int = 0
    id: int = 0
    extra_lines: List[str] = field(default_factory=list)


@dataclass
class StreamRecord:
    """A routed stream polyline.

    ``str_id`` is the stream number; ``points`` lists the intermediate corner
    points between the start and end of the polyline.
    """

    str_id: int
    start_x: float
    start_y: float
    end_x: float
    end_y: float
    points: List[Tuple[float, float]] = field(default_factory=list)

    @property
    def corners(self) -> int:
        return len(self.points)


@dataclass
class TrnLayout:
    """Structured view of a ``.TRN`` file.

    ``unit_blocks`` groups units by their equipment type code (matching the
    on-disk layout, where each type block is introduced by a ``<type> <flags>``
    line).  ``streams`` holds the routed stream polylines.
    """

    header: List[str] = field(default_factory=list)
    unit_blocks: List[Tuple[int, str, List[UnitRecord]]] = field(default_factory=list)
    streams: List[StreamRecord] = field(default_factory=list)
    footer: List[str] = field(default_factory=list)
    water_count: int = 0

    # ------------------------------------------------------------------
    # Accessors
    # ------------------------------------------------------------------
    def unit_positions(self) -> Dict[int, Tuple[float, float]]:
        """Return ``{unit_number: (x, y)}`` for every placed unit."""
        result: Dict[int, Tuple[float, float]] = {}
        for _type, _flags, records in self.unit_blocks:
            for rec in records:
                result[rec.number] = (rec.x, rec.y)
        return result

    def unit_numbers(self) -> List[int]:
        return [rec.number for _, _, records in self.unit_blocks for rec in records]

    def max_unit_number(self) -> int:
        nums = self.unit_numbers()
        return max(nums) if nums else 0

    def max_stream_id(self) -> int:
        ids = [s.str_id for s in self.streams]
        return max(ids) if ids else 0

    def set_unit_position(self, number: int, x: float, y: float) -> None:
        for _type, _flags, records in self.unit_blocks:
            for rec in records:
                if rec.number == number:
                    rec.x = x
                    rec.y = y
                    return
        # Not found: add to the first block (or a fresh block with type 0).
        if self.unit_blocks:
            self.unit_blocks[0][2].append(UnitRecord(number, x, y))
        else:
            self.unit_blocks.append((0, "T", [UnitRecord(number, x, y)]))

    def add_unit(self, number: int, x: float, y: float, type_code: Optional[int] = None) -> None:
        if type_code is None:
            type_code = self._next_type_code()
        for idx, (typ, flags, records) in enumerate(self.unit_blocks):
            if typ == type_code:
                self.unit_blocks[idx][2].append(UnitRecord(number, x, y))
                return
        self.unit_blocks.append((type_code, "T", [UnitRecord(number, x, y)]))

    def add_stream(
        self,
        str_id: int,
        start_x: float,
        start_y: float,
        end_x: float,
        end_y: float,
        points: Optional[List[Tuple[float, float]]] = None,
    ) -> None:
        self.streams.append(
            StreamRecord(str_id, start_x, start_y, end_x, end_y, list(points or []))
        )

    def _next_type_code(self) -> int:
        types = [typ for typ, _, _ in self.unit_blocks]
        code = 1
        while code in types:
            code += 1
        return code


# ---------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------
def parse_trn_layout(lines: List[str]) -> TrnLayout:
    """Parse the raw lines of a ``.TRN`` file into a :class:`TrnLayout`."""
    layout = TrnLayout()
    if not lines:
        return layout

    layout.header = list(lines[0:3])

    # Header line 2: NUNIT Stream Water
    parts = lines[2].split() if len(lines) > 2 else []
    stream_count = int(parts[1]) if len(parts) > 1 else 0
    layout.water_count = int(parts[2]) if len(parts) > 2 else 0

    # The 5x20 count matrix (lines 3..7) tells us how many units of each of
    # the 100 equipment type codes are present.
    counts: Dict[int, int] = {}
    global_idx = 1
    for row in lines[3:8]:
        for tok in row.split():
            counts[global_idx] = int(tok)
            global_idx += 1

    pos = 8
    stream_flags = ""
    if stream_count > 0 and pos < len(lines):
        stream_flags = lines[pos].strip()
        pos += 1
    if layout.water_count > 0 and pos < len(lines):
        pos += 1  # water flags line, not needed for layout

    # Unit blocks, in type-code order.
    for typ in range(1, 101):
        count = counts.get(typ, 0)
        if count <= 0:
            continue
        if pos >= len(lines):
            break
        hdr = lines[pos].split()
        pos += 1
        flags = hdr[1] if len(hdr) > 1 else ""
        records: List[UnitRecord] = []
        for _ in range(count):
            if pos >= len(lines):
                break
            up = lines[pos].split()
            pos += 1
            extra: List[str] = []
            if typ in (6, 62):
                # Type 6 appends a NOCELL line; type 62 (conveyor) appends
                # a CONVEY_X/CONVEY_Y line after each unit record.
                if pos < len(lines):
                    extra.append(lines[pos])
                    pos += 1
            records.append(
                UnitRecord(
                    number=int(up[0]),
                    x=float(up[1]),
                    y=float(up[2]),
                    irr=int(up[3]) if len(up) > 3 else 0,
                    id=int(up[4]) if len(up) > 4 else 0,
                    extra_lines=extra,
                )
            )
        layout.unit_blocks.append((typ, flags, records))

    # Stream polylines.
    for i in range(stream_count):
        if i >= len(stream_flags) or stream_flags[i] != "T":
            continue
        if pos >= len(lines):
            break
        corners = int(lines[pos].strip())
        pos += 1
        if pos >= len(lines):
            break
        sl = lines[pos].split()
        pos += 1
        rec = StreamRecord(
            str_id=int(sl[4]),
            start_x=float(sl[0]),
            start_y=float(sl[1]),
            end_x=float(sl[2]),
            end_y=float(sl[3]),
        )
        for _ in range(corners):
            if pos >= len(lines):
                break
            cl = lines[pos].split()
            pos += 1
            rec.points.append((float(cl[0]), float(cl[1])))
        layout.streams.append(rec)

    layout.footer = list(lines[pos:])
    return layout


# ---------------------------------------------------------------------------
# Regeneration
# ---------------------------------------------------------------------------
def regenerate_trn_lines(layout: TrnLayout) -> List[str]:
    """Rebuild the raw ``.TRN`` lines from a :class:`TrnLayout`.

    The header (first three lines) is regenerated so the derived counts stay in
    sync with the actual unit/stream data; the count matrix, flags and the
    unit/stream sections are rebuilt; the footer is preserved verbatim.
    """
    lines: List[str] = []

    # Header.
    if len(layout.header) >= 3:
        lines.append(layout.header[0])
        lines.append(layout.header[1])
        nunit = sum(len(records) for _, _, records in layout.unit_blocks)
        stream_count = len(layout.streams)
        lines.append(f"{nunit} {stream_count} {layout.water_count}")
    else:
        nunit = sum(len(records) for _, _, records in layout.unit_blocks)
        stream_count = len(layout.streams)
        lines.append(f"1 {nunit} {stream_count} {layout.water_count}")

    # Count matrix (5 rows of 20).
    counts: Dict[int, int] = {}
    for typ, _flags, records in layout.unit_blocks:
        counts[typ] = len(records)
    matrix: List[int] = [counts.get(i, 0) for i in range(1, 101)]
    for r in range(5):
        row = matrix[r * 20 : (r + 1) * 20]
        lines.append(" " + " ".join(str(v) for v in row) + " ")

    # Stream / water flags.
    lines.append("T" * stream_count)
    lines.append("F" * stream_count)

    # Unit section.
    for typ, flags, records in layout.unit_blocks:
        lines.append(f"{typ} {flags}")
        for rec in records:
            lines.append(f"{rec.number} {rec.x} {rec.y} {rec.irr} {rec.id}")
            for extra in rec.extra_lines:
                lines.append(extra)

    # Stream section.
    for rec in layout.streams:
        lines.append(f"{rec.corners}")
        lines.append(f"{rec.start_x} {rec.start_y} {rec.end_x} {rec.end_y} {rec.str_id}")
        for px, py in rec.points:
            lines.append(f"{px} {py}")

    # Footer verbatim.
    lines.extend(layout.footer)
    return lines
