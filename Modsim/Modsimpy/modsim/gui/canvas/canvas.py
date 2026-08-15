"""The ModSIM flowsheet canvas: a ``QGraphicsView``/``QGraphicsScene`` pair.

:class:`FlowsheetScene` manages the unit nodes and stream connectors on a
``QGraphicsScene`` and handles the interactive behaviours: dragging unit types
in from the palette, moving units, drawing new streams between ports, and
editing unit labels.  :class:`FlowsheetCanvas` is the ``QGraphicsView`` that
hosts the scene.
"""

from __future__ import annotations

import math
from typing import Dict, List, Optional, Tuple

from PySide6.QtCore import QPointF, QRectF, Qt, Signal
from PySide6.QtGui import QBrush, QColor, QPainter, QPen
from PySide6.QtWidgets import (
    QGraphicsPathItem,
    QGraphicsScene,
    QGraphicsView,
)

from .items import UNIT_HEIGHT, UNIT_WIDTH, StreamItem, UnitItem

# MIME type used when dragging unit types from the palette onto the canvas.
UNIT_MIME = "application/x-modsim-unit"

# Default spacing used when auto-arranging units that have no saved position.
GRID_SPACING_X = 220.0
GRID_SPACING_Y = 160.0


class FlowsheetScene(QGraphicsScene):
    """Scene holding the units and streams of one flowsheet.

    Emits ``unit_added``, ``stream_created``, ``unit_label_changed`` and
    ``layout_changed`` so the host window can react (e.g. persist changes).
    """

    unit_added = Signal(object)
    stream_created = Signal(object)
    unit_label_changed = Signal(object)
    layout_changed = Signal()

    def __init__(self, parent=None) -> None:
        super().__init__(parent)
        self._units: Dict[int, UnitItem] = {}
        self._streams: List[StreamItem] = []
        self._next_stream_number = 1
        self._draw_source: Optional[UnitItem] = None
        self._draw_guide: Optional[QGraphicsPathItem] = None
        self.setBackgroundBrush(QBrush(QColor("#f2f4f7")))

    # ------------------------------------------------------------------
    # Accessors
    # ------------------------------------------------------------------
    @property
    def units(self) -> Dict[int, UnitItem]:
        return self._units

    @property
    def streams(self) -> List[StreamItem]:
        return list(self._streams)

    def unit_by_number(self, number: int) -> Optional[UnitItem]:
        return self._units.get(number)

    # ------------------------------------------------------------------
    # Units
    # ------------------------------------------------------------------
    def add_unit(
        self,
        number: int,
        label: str = "",
        kind: str = "O",
        model_code: str = "",
        pos: Optional[Tuple[float, float]] = None,
    ) -> UnitItem:
        """Create a unit node on the scene at ``pos`` (or auto-arranged)."""
        item = UnitItem(number, label=label, kind=kind, model_code=model_code)
        if pos is not None:
            item.setPos(pos[0], pos[1])
        else:
            item.setPos(self._auto_position())
        self.addItem(item)
        self._units[number] = item
        self._next_stream_number = max(self._next_stream_number, number + 1)
        self.unit_added.emit(item)
        self.layout_changed.emit()
        return item

    def remove_unit(self, number: int) -> None:
        item = self._units.pop(number, None)
        if item is None:
            return
        for stream in list(self._streams):
            if stream.source is item or stream.target is item:
                self.removeItem(stream)
                self._streams.remove(stream)
        self.removeItem(item)
        self.layout_changed.emit()

    def next_unit_number(self) -> int:
        used = set(self._units.keys())
        n = 1
        while n in used:
            n += 1
        return n

    def _auto_position(self) -> QPointF:
        cols = max(1, int(math.sqrt(max(len(self._units), 1))))
        idx = len(self._units)
        col = idx % cols
        row = idx // cols
        return QPointF(col * GRID_SPACING_X + 40, row * GRID_SPACING_Y + 40)

    # ------------------------------------------------------------------
    # Streams
    # ------------------------------------------------------------------
    def create_stream(self, source: UnitItem, target: UnitItem) -> Optional[StreamItem]:
        """Create a stream connector from ``source`` output to ``target`` input."""
        if source is target:
            return None
        number = self._next_stream_number
        self._next_stream_number += 1
        stream = StreamItem(source, target, number=number)
        self.addItem(stream)
        self._streams.append(stream)
        self.stream_created.emit(stream)
        self.layout_changed.emit()
        return stream

    def on_unit_moved(self, unit: UnitItem) -> None:
        """Re-route every stream attached to ``unit`` (called on move)."""
        for stream in self._streams:
            if stream.source is unit or stream.target is unit:
                stream.update_path()
        self.layout_changed.emit()

    # ------------------------------------------------------------------
    # Layout snapshot / restore
    # ------------------------------------------------------------------
    def get_layout(self) -> Tuple[Dict[int, Tuple[float, float]], List[Tuple[int, int]]]:
        """Return ``(unit_positions, connections)`` for persistence.

        ``unit_positions`` maps unit number to ``(x, y)``; ``connections`` is a
        list of ``(source_number, target_number)`` pairs.
        """
        positions: Dict[int, Tuple[float, float]] = {}
        for number, item in self._units.items():
            pos = item.pos()
            positions[number] = (pos.x(), pos.y())
        connections = [(s.source.number, s.target.number) for s in self._streams]
        return positions, connections

    def auto_arrange(self) -> None:
        """Lay out units that have no saved position in a grid."""
        cols = max(1, int(math.ceil(math.sqrt(len(self._units)))))
        for idx, (number, item) in enumerate(sorted(self._units.items())):
            item.setPos(
                (idx % cols) * GRID_SPACING_X + 40,
                (idx // cols) * GRID_SPACING_Y + 40,
            )
        self.layout_changed.emit()

    # ------------------------------------------------------------------
    # Drag & drop from the palette
    # ------------------------------------------------------------------
    def dragEnterEvent(self, event) -> None:  # noqa: N802
        if event.mimeData().hasFormat(UNIT_MIME):
            event.acceptProposedAction()
        else:
            super().dragEnterEvent(event)

    def dragMoveEvent(self, event) -> None:  # noqa: N802
        if event.mimeData().hasFormat(UNIT_MIME):
            event.acceptProposedAction()
        else:
            super().dragMoveEvent(event)

    def dropEvent(self, event) -> None:  # noqa: N802
        mime = event.mimeData()
        if not mime.hasFormat(UNIT_MIME):
            super().dropEvent(event)
            return
        model_code = bytes(mime.data(UNIT_MIME)).decode("utf-8", "replace")
        pos = event.scenePos()
        self.add_unit(
            self.next_unit_number(),
            label=model_code,
            kind="O",
            model_code=model_code,
            pos=(pos.x() - UNIT_WIDTH / 2, pos.y() - UNIT_HEIGHT / 2),
        )
        event.acceptProposedAction()

    # ------------------------------------------------------------------
    # Drawing a new stream by dragging between ports
    # ------------------------------------------------------------------
    def _unit_at(self, pos: QPointF) -> Optional[UnitItem]:
        for item in self.items(pos):
            if isinstance(item, UnitItem):
                return item
        return None

    def _near_port(self, unit: UnitItem, pos: QPointF, which: str) -> bool:
        port = unit.output_port_pos() if which == "out" else unit.input_port_pos()
        return (port - pos).manhattanLength() < 18.0

    def mousePressEvent(self, event) -> None:  # noqa: N802
        if event.button() == Qt.MouseButton.LeftButton:
            unit = self._unit_at(event.scenePos())
            if unit is not None and self._near_port(unit, event.scenePos(), "out"):
                self._draw_source = unit
                guide = QGraphicsPathItem()
                guide.setPen(QPen(QColor("#f5821f"), 2.0, Qt.PenStyle.DashLine))
                guide.setZValue(50)
                self.addItem(guide)
                self._draw_guide = guide
                event.accept()
                return
        super().mousePressEvent(event)

    def mouseMoveEvent(self, event) -> None:  # noqa: N802
        if self._draw_guide is not None and self._draw_source is not None:
            start = self._draw_source.output_port_pos()
            end = event.scenePos()
            path = self._draw_guide.path()
            path.moveTo(start)
            path.lineTo(end)
            self._draw_guide.setPath(path)
            event.accept()
            return
        super().mouseMoveEvent(event)

    def mouseReleaseEvent(self, event) -> None:  # noqa: N802
        if self._draw_guide is not None:
            source = self._draw_source
            target = self._unit_at(event.scenePos())
            if self._draw_guide is not None:
                self.removeItem(self._draw_guide)
            self._draw_guide = None
            self._draw_source = None
            if (
                source is not None
                and target is not None
                and target is not source
                and self._near_port(target, event.scenePos(), "in")
            ):
                self.create_stream(source, target)
            event.accept()
            return
        super().mouseReleaseEvent(event)

    def mouseDoubleClickEvent(self, event) -> None:  # noqa: N802
        unit = self._unit_at(event.scenePos())
        if unit is not None:
            self.unit_label_changed.emit(unit)
            event.accept()
            return
        super().mouseDoubleClickEvent(event)


class FlowsheetCanvas(QGraphicsView):
    """A ``QGraphicsView`` hosting a :class:`FlowsheetScene`."""

    def __init__(self, parent=None) -> None:
        super().__init__(parent)
        self._scene = FlowsheetScene(self)
        self.setScene(self._scene)
        self.setRenderHint(QPainter.RenderHint.Antialiasing, True)
        self.setDragMode(QGraphicsView.DragMode.RubberBandDrag)
        self.setViewportUpdateMode(QGraphicsView.ViewportUpdateMode.BoundingRectViewportUpdate)
        self.setTransformationAnchor(QGraphicsView.ViewportAnchor.AnchorUnderMouse)
        self.setAcceptDrops(True)
        self.setMouseTracking(True)
        # A little headroom around the content so units can be dropped anywhere.
        self._scene.setSceneRect(QRectF(-400, -400, 4000, 3000))

    @property
    def scene(self) -> FlowsheetScene:
        return self._scene

    def fit_to_content(self) -> None:
        """Zoom so all units fit comfortably in the view."""
        rect = self._scene.itemsBoundingRect()
        if rect.isNull():
            return
        self._scene.setSceneRect(rect.adjusted(-80, -80, 80, 80))
        self.fitInView(rect.adjusted(-80, -80, 80, 80), Qt.AspectRatioMode.KeepAspectRatio)

    def wheelEvent(self, event) -> None:  # noqa: N802
        """Zoom with the mouse wheel (Ctrl+wheel for finer control)."""
        factor = 1.15
        if event.modifiers() & Qt.KeyboardModifier.ControlModifier:
            factor = 1.03
        if event.angleDelta().y() > 0:
            self.scale(factor, factor)
        else:
            self.scale(1.0 / factor, 1.0 / factor)
