"""The real flowsheet window: canvas + palette hosted in the GUI shell.

:class:`FlowsheetCanvasWindow` replaces the placeholder flowsheet content.  It
loads a :class:`~modsim.models.job.Job`'s units and streams onto a
:class:`FlowsheetScene`, lets the user add / move units and draw streams, and
keeps the job model's layout data (``.TRN`` positions and ``.syd`` stream
connections) in sync so a save round-trips the layout.
"""

from __future__ import annotations

from typing import Dict, List, Optional, Tuple

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QHBoxLayout,
    QInputDialog,
    QLabel,
    QMessageBox,
    QSplitter,
    QVBoxLayout,
    QWidget,
)

from ...models.job import Job, SydFile, TrnFile, Unit
from ..flowsheet import FlowsheetWindow
from ..dialogs.schema import SCHEMAS
from .canvas import FlowsheetCanvas, FlowsheetScene
from .items import StreamItem, UnitItem
from .palette import UnitPalette
from .trn_layout import TrnLayout, parse_trn_layout, regenerate_trn_lines

_KIND_LABELS = {"F": "Feed", "W": "Water"}


class FlowsheetCanvasWindow(FlowsheetWindow):
    """A flowsheet sub-window that hosts the canvas and the unit palette."""

    def __init__(self, job: Optional[Job] = None, parent: Optional[QWidget] = None) -> None:
        self._trn_layout: Optional[TrnLayout] = None
        self._loading = True
        super().__init__(job, parent)

    # ------------------------------------------------------------------
    # Construction
    # ------------------------------------------------------------------
    def _build_body(self) -> None:
        while self._layout.count():
            item = self._layout.takeAt(0)
            widget = item.widget()
            if widget is not None:
                widget.deleteLater()

        self.canvas = FlowsheetCanvas(self)
        self.palette = UnitPalette(self)

        splitter = QSplitter(Qt.Orientation.Horizontal, self)
        splitter.addWidget(self.palette)
        splitter.addWidget(self.canvas)
        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 1)
        splitter.setSizes([220, 900])

        hint = QLabel(
            "Drag equipment from the palette to add a unit.  "
            "Drag between a unit's output port and another unit's input port to "
            "draw a stream.  Double-click a unit to rename it.",
            self,
        )
        hint.setObjectName("canvasHint")
        hint.setWordWrap(True)

        self._layout.addWidget(splitter, 1)
        self._layout.addWidget(hint)

        self._connect_signals()
        self._load_job()

    def _connect_signals(self) -> None:
        scene: FlowsheetScene = self.canvas.scene
        scene.unit_label_changed.connect(self._edit_unit_label)
        scene.layout_changed.connect(self._on_layout_changed)

    # ------------------------------------------------------------------
    # Loading
    # ------------------------------------------------------------------
    def _load_job(self) -> None:
        job = self.job
        if job is None:
            return
        scene: FlowsheetScene = self.canvas.scene

        # Unit positions from the .TRN layout, if present.
        positions: Dict[int, Tuple[float, float]] = {}
        self._trn_layout = None
        if job.trn is not None and job.trn.raw_lines:
            layout = parse_trn_layout(job.trn.raw_lines)
            self._trn_layout = layout
            positions = layout.unit_positions()

        # Model codes from the .cur file, to pick friendly labels.
        model_codes: Dict[int, str] = {}
        if job.cur is not None:
            for cu in job.cur.units:
                model_codes[cu.number] = cu.model

        # Create a unit node for every unit in the .syd file.
        unit_items: Dict[int, UnitItem] = {}
        if job.syd is not None:
            for u in job.syd.units:
                label = self._label_for(u.number, u.kind, model_codes)
                pos = positions.get(u.number)
                item = scene.add_unit(
                    u.number,
                    label=label,
                    kind=u.kind,
                    model_code=model_codes.get(u.number, ""),
                    pos=pos,
                )
                unit_items[u.number] = item

        # Create stream connectors from the .syd unit connections.
        self._load_streams(scene, job, unit_items)
        self._seed_stream_numbers(scene)

        if not positions:
            scene.auto_arrange()
        self.canvas.fit_to_content()
        self._loading = False

    @staticmethod
    def _label_for(number: int, kind: str, model_codes: Dict[int, str]) -> str:
        code = model_codes.get(number, "")
        if code and code in SCHEMAS:
            return SCHEMAS[code].name
        return _KIND_LABELS.get(kind.upper(), f"Unit {number}")

    def _load_streams(
        self,
        scene: FlowsheetScene,
        job: Job,
        unit_items: Dict[int, UnitItem],
    ) -> None:
        if job.syd is None:
            return
        # Map each stream number to its source (out) and target (in) units.
        # A stream may merge several sources and/or split to several targets.
        out_units: Dict[int, List[int]] = {}
        in_units: Dict[int, List[int]] = {}
        for u in job.syd.units:
            if u.out_stream > 0:
                out_units.setdefault(u.out_stream, []).append(u.number)
            if u.in_stream > 0:
                in_units.setdefault(u.in_stream, []).append(u.number)

        for stream_no in sorted(set(out_units) | set(in_units)):
            for src in out_units.get(stream_no, []):
                for tgt in in_units.get(stream_no, []):
                    source = unit_items.get(src)
                    target = unit_items.get(tgt)
                    if source is None or target is None:
                        continue
                    stream = scene.create_stream(source, target)
                    if stream is not None:
                        stream.number = stream_no

    def _seed_stream_numbers(self, scene: FlowsheetScene) -> None:
        max_no = max((s.number for s in scene.streams), default=0)
        scene._next_stream_number = max_no + 1

    # ------------------------------------------------------------------
    # Editing
    # ------------------------------------------------------------------
    def _edit_unit_label(self, unit: UnitItem) -> None:
        text, ok = QInputDialog.getText(
            self,
            "Rename Unit",
            f"Label for unit {unit.number}:",
            text=unit.label,
        )
        if ok:
            unit.set_label(text)

    def _on_layout_changed(self) -> None:
        # Keep the job model in sync so a later save persists the layout.
        # Skip while the initial job is being loaded onto the scene.
        if self.job is not None and not self._loading:
            self.sync_layout_to_job()

    # ------------------------------------------------------------------
    # Persistence
    # ------------------------------------------------------------------
    def sync_layout_to_job(self) -> None:
        """Write the current canvas layout back onto the job model.

        Unit positions go to ``job.trn`` (regenerating its ``raw_lines``) and
        stream connections go to ``job.syd``'s units (regenerating its
        ``raw_lines``), so a subsequent ``write_job_directory`` persists them.
        """
        job = self.job
        if job is None:
            return
        scene: FlowsheetScene = self.canvas.scene
        positions, connections = scene.get_layout()
        self._sync_trn(positions, connections)
        self._sync_syd(positions, connections)

    def _sync_trn(
        self,
        positions: Dict[int, Tuple[float, float]],
        connections: List[Tuple[int, int]],
    ) -> None:
        layout = self._trn_layout
        if layout is None:
            layout = self._fresh_trn_layout()
            self._trn_layout = layout

        # Update unit positions (adds new units as needed).
        for number, (x, y) in positions.items():
            layout.set_unit_position(number, x, y)

        # Rebuild stream records from the current connections, preserving any
        # saved polyline for a stream whose number is unchanged.
        old_polylines = {s.str_id: s.points for s in layout.streams}
        layout.streams = []
        for src, tgt in connections:
            stream = self._stream_between(src, tgt)
            number = stream.number if stream is not None else 0
            start = self._unit_pos(src)
            end = self._unit_pos(tgt)
            layout.add_stream(number, start[0], start[1], end[0], end[1], old_polylines.get(number))

        # Persist structured data + regenerated raw lines.
        job = self.job
        if job.trn is None:
            job.trn = TrnFile(name="trn", job_name=job.name)
        job.trn.unit_positions = dict(positions)
        job.trn.stream_polylines = {s.str_id: s.points for s in layout.streams}
        job.trn.raw_lines = regenerate_trn_lines(layout)

    def _sync_syd(
        self,
        positions: Dict[int, Tuple[float, float]],
        connections: List[Tuple[int, int]],
    ) -> None:
        job = self.job
        if job.syd is None:
            job.syd = SydFile(name="syd", job_name=job.name)

        # Group stream numbers into their source and target unit sets (a stream
        # may merge several sources and split to several targets).
        stream_sources: Dict[int, set] = {}
        stream_targets: Dict[int, set] = {}
        for src, tgt in connections:
            stream = self._stream_between(src, tgt)
            number = stream.number if stream is not None else 0
            if not number:
                continue
            stream_sources.setdefault(number, set()).add(src)
            stream_targets.setdefault(number, set()).add(tgt)

        # Rebuild the unit list with updated connectivity.
        old = {u.number: u for u in job.syd.units}
        new_units: List[Unit] = []
        for number in sorted(set(positions) | set(old)):
            existing = old.get(number)
            in_stream = 0
            out_stream = 0
            for s, srcs in stream_sources.items():
                if number in srcs:
                    out_stream = s
            for s, tgts in stream_targets.items():
                if number in tgts:
                    in_stream = s
            new_units.append(
                Unit(
                    number=number,
                    type=existing.type if existing else 1,
                    kind=existing.kind if existing else "O",
                    in_stream=in_stream,
                    out_stream=out_stream,
                )
            )
        job.syd.units = new_units
        job.syd.unit_count = len(new_units)
        # Drop raw_lines so write_syd regenerates from the structured units.
        job.syd.raw_lines = None

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------
    def _stream_between(self, src: int, tgt: int) -> Optional[StreamItem]:
        for s in self.canvas.scene.streams:
            if s.source.number == src and s.target.number == tgt:
                return s
        return None

    def _unit_pos(self, number: int) -> Tuple[float, float]:
        item = self.canvas.scene.unit_by_number(number)
        if item is None:
            return (0.0, 0.0)
        p = item.pos()
        return (p.x(), p.y())

    @staticmethod
    def _fresh_trn_layout() -> TrnLayout:
        layout = TrnLayout()
        layout.header = ["1 12 5 16", "Untitled", "0 0 0"]
        layout.water_count = 0
        layout.footer = ["0", "0", "Arial", "8", "608 8", "1"]
        return layout

    # Convenience for the shell / tests.
    def save_layout(self) -> None:
        """Persist the current canvas layout onto the job model."""
        self.sync_layout_to_job()
