"""Graphical items for the ModSIM flowsheet canvas.

:class:`UnitItem` is a draggable node representing a single unit, drawn with a
number, a label and an input/output port.  :class:`StreamItem` is the connector
drawn between a source unit's output port and a target unit's input port; it
re-routes itself automatically whenever either unit moves.
"""

from __future__ import annotations

from typing import Optional

from PySide6.QtCore import QPointF, QRectF, Qt
from PySide6.QtGui import (
    QColor,
    QLinearGradient,
    QPainter,
    QPainterPath,
    QPen,
    QPolygonF,
    QBrush,
)
from PySide6.QtWidgets import QGraphicsItem, QGraphicsPathItem

# Dimensions of a unit node in scene coordinates.
UNIT_WIDTH = 120.0
UNIT_HEIGHT = 70.0
PORT_RADIUS = 6.0

# Colour of the node based on the unit kind (F = feed, W = water, O = other).
_KIND_COLOURS = {
    "F": QColor("#2e9e5b"),
    "W": QColor("#2f7fd1"),
    "O": QColor("#5b6b7a"),
}
_KIND_DEFAULT = QColor("#5b6b7a")


def _kind_colour(kind: str) -> QColor:
    return _KIND_COLOURS.get(kind.upper(), _KIND_DEFAULT)


class UnitItem(QGraphicsItem):
    """A draggable unit node with an input port and an output port."""

    def __init__(
        self,
        number: int,
        label: str = "",
        kind: str = "O",
        model_code: str = "",
        parent: Optional[QGraphicsItem] = None,
    ) -> None:
        super().__init__(parent)
        self.number = number
        self.label = label or f"Unit {number}"
        self.kind = kind.upper() if kind else "O"
        self.model_code = model_code
        self.setFlags(
            QGraphicsItem.GraphicsItemFlag.ItemIsMovable
            | QGraphicsItem.GraphicsItemFlag.ItemIsSelectable
            | QGraphicsItem.GraphicsItemFlag.ItemSendsGeometryChanges
        )
        self.setAcceptHoverEvents(True)
        self._hovered = False
        self.setZValue(10)

    # ------------------------------------------------------------------
    # Geometry
    # ------------------------------------------------------------------
    def boundingRect(self) -> QRectF:
        return QRectF(-PORT_RADIUS, -PORT_RADIUS, UNIT_WIDTH + 2 * PORT_RADIUS, UNIT_HEIGHT + 2 * PORT_RADIUS)

    def input_port_pos(self) -> QPointF:
        """Scene position of the input port (left edge)."""
        return self.mapToScene(QPointF(0.0, UNIT_HEIGHT / 2.0))

    def output_port_pos(self) -> QPointF:
        """Scene position of the output port (right edge)."""
        return self.mapToScene(QPointF(UNIT_WIDTH, UNIT_HEIGHT / 2.0))

    def set_label(self, text: str) -> None:
        self.label = (text or f"Unit {self.number}").strip()
        self.update()

    # ------------------------------------------------------------------
    # Interaction
    # ------------------------------------------------------------------
    def itemChange(self, change, value):  # noqa: N802 - Qt naming
        if change == QGraphicsItem.GraphicsItemChange.ItemPositionHasChanged:
            scene = self.scene()
            if scene is not None:
                notify = getattr(scene, "on_unit_moved", None)
                if notify is not None:
                    notify(self)
        return super().itemChange(change, value)

    def hoverEnterEvent(self, event) -> None:  # noqa: N802
        self._hovered = True
        self.update()
        super().hoverEnterEvent(event)

    def hoverLeaveEvent(self, event) -> None:  # noqa: N802
        self._hovered = False
        self.update()
        super().hoverLeaveEvent(event)

    # ------------------------------------------------------------------
    # Painting
    # ------------------------------------------------------------------
    def paint(self, painter: QPainter, option, widget=None) -> None:  # noqa: N802
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, True)

        body = QRectF(0.0, 0.0, UNIT_WIDTH, UNIT_HEIGHT)
        accent = _kind_colour(self.kind)

        # Body fill: subtle vertical gradient from a light face to a slate body.
        gradient = QLinearGradient(0.0, 0.0, 0.0, UNIT_HEIGHT)
        gradient.setColorAt(0.0, QColor("#ffffff"))
        gradient.setColorAt(1.0, QColor("#e8ebee"))
        painter.setBrush(QBrush(gradient))
        pen = QPen(QColor("#3a4450"), 1.5)
        if self.isSelected():
            pen.setColor(QColor("#1f6feb"))
            pen.setWidth(2.5)
        elif self._hovered:
            pen.setColor(QColor("#1f6feb"))
        painter.setPen(pen)
        painter.drawRoundedRect(body, 8.0, 8.0)

        # Header strip with the unit number.
        header = QRectF(0.0, 0.0, UNIT_WIDTH, 24.0)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(accent)
        path = QPainterPath()
        path.addRoundedRect(body, 8.0, 8.0)
        clip = QPainterPath()
        clip.addRect(header)
        painter.setClipPath(path.intersected(clip))
        painter.drawRect(header)
        painter.setClipping(False)

        # Number text.
        painter.setPen(QColor("#ffffff"))
        font = painter.font()
        font.setBold(True)
        font.setPointSize(9)
        painter.setFont(font)
        painter.drawText(header, Qt.AlignmentFlag.AlignCenter, str(self.number))

        # Label text.
        painter.setPen(QColor("#2b333c"))
        font = painter.font()
        font.setBold(False)
        font.setPointSize(8)
        painter.setFont(font)
        label_rect = QRectF(4.0, 26.0, UNIT_WIDTH - 8.0, UNIT_HEIGHT - 30.0)
        painter.drawText(
            label_rect,
            Qt.AlignmentFlag.AlignCenter | Qt.AlignmentFlag.AlignVCenter,
            self.label,
        )

        # Ports.
        self._paint_port(painter, QPointF(0.0, UNIT_HEIGHT / 2.0), accent, "in")
        self._paint_port(painter, QPointF(UNIT_WIDTH, UNIT_HEIGHT / 2.0), accent, "out")

    def _paint_port(self, painter: QPainter, center: QPointF, accent: QColor, _which: str) -> None:
        painter.setPen(QPen(QColor("#2b333c"), 1.2))
        painter.setBrush(QBrush(accent))
        painter.drawEllipse(center, PORT_RADIUS, PORT_RADIUS)


class StreamItem(QGraphicsPathItem):
    """A connector between a source unit's output port and a target unit's input port."""

    def __init__(
        self,
        source: UnitItem,
        target: UnitItem,
        number: int = 0,
        parent: Optional[QGraphicsItem] = None,
    ) -> None:
        super().__init__(parent)
        self.source = source
        self.target = target
        self.number = number
        self.setZValue(5)
        self.setFlag(QGraphicsItem.GraphicsItemFlag.ItemIsSelectable, True)
        self._build_path()

    def update_path(self) -> None:
        """Recompute the connector path (e.g. after a unit has moved)."""
        self._build_path()

    def _build_path(self) -> None:
        start = self.source.output_port_pos()
        end = self.target.input_port_pos()

        # Control points make the stream leave horizontally from the source and
        # enter horizontally into the target, giving a smooth orthogonal feel.
        dx = max(abs(end.x() - start.x()) * 0.5, 40.0)
        path = QPainterPath(start)
        path.cubicTo(
            QPointF(start.x() + dx, start.y()),
            QPointF(end.x() - dx, end.y()),
            end,
        )

        self.setPath(path)

        pen = QPen(QColor("#1f6feb"), 2.0)
        pen.setCapStyle(Qt.PenCapStyle.RoundCap)
        if self.isSelected():
            pen.setColor(QColor("#f5821f"))
            pen.setWidth(3.0)
        self.setPen(pen)

    def paint(self, painter: QPainter, option, widget=None) -> None:  # noqa: N802
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, True)
        super().paint(painter, option, widget)
        self._paint_arrow(painter)

    def _paint_arrow(self, painter: QPainter) -> None:
        end = self.target.input_port_pos()
        prev = self.path().pointAtPercent(0.95)
        direction = end - prev
        length = 14.0
        if direction.manhattanLength() < 1e-6:
            return
        direction /= (direction.x() ** 2 + direction.y() ** 2) ** 0.5
        normal = QPointF(-direction.y(), direction.x())
        tip = end
        base = end - direction * length
        left = base + normal * (length * 0.45)
        right = base - normal * (length * 0.45)
        arrow = QPolygonF([tip, left, right])
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QBrush(self.pen().color()))
        painter.drawPolygon(arrow)
