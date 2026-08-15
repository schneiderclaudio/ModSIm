"""A palette of unit types that can be dragged onto the flowsheet canvas.

The palette lists the equipment models registered in the dialogs schema
(:mod:`modsim.gui.dialogs.schema`).  Each entry can be dragged onto the canvas,
which drops a new unit of that model on the scene.
"""

from __future__ import annotations

from typing import Optional

from PySide6.QtCore import QMimeData, Qt
from PySide6.QtGui import QDrag, QFontMetrics, QPainter, QPixmap
from PySide6.QtWidgets import QLabel, QListWidget, QListWidgetItem, QVBoxLayout, QWidget

from ..dialogs.schema import SCHEMAS, list_schemas
from .canvas import UNIT_MIME


class PaletteList(QListWidget):
    """A ``QListWidget`` whose items drag ``UNIT_MIME`` data onto the canvas.

    ``QAbstractItemView`` starts a drag by calling its ``startDrag`` virtual.
    Overriding it here (on the list widget itself, which is what the user
    actually drags) makes the drag carry the unit model code; the default
    implementation would only produce plain-text MIME data the canvas ignores.
    """

    def startDrag(self, supported_actions) -> None:  # noqa: N802
        item = self.currentItem()
        if item is None:
            return
        code = item.data(Qt.ItemDataRole.UserRole)
        if code is None:
            return

        mime = QMimeData()
        mime.setData(UNIT_MIME, code.encode("utf-8"))

        drag = QDrag(self)
        drag.setMimeData(mime)
        drag.setPixmap(_drag_pixmap(item.text(), self.font()))
        drag.exec(Qt.DropAction.CopyAction)


def _drag_pixmap(text: str, font) -> QPixmap:
    """Render ``text`` into a small pixmap shown while the item is dragged."""
    metrics = QFontMetrics(font)
    pad = 6
    pixmap = QPixmap(
        metrics.horizontalAdvance(text) + 2 * pad,
        metrics.height() + 2 * pad,
    )
    pixmap.fill(Qt.GlobalColor.transparent)
    painter = QPainter(pixmap)
    painter.setFont(font)
    painter.setPen(Qt.GlobalColor.black)
    painter.drawText(pad, metrics.ascent() + pad, text)
    painter.end()
    return pixmap


class UnitPalette(QWidget):
    """A compact, dockable list of draggable unit types."""

    def __init__(self, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.setObjectName("unitPalette")

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        title = QLabel("Equipment", self)
        title.setObjectName("paletteTitle")
        title.setContentsMargins(10, 8, 10, 6)
        layout.addWidget(title)

        self.list = PaletteList(self)
        self.list.setObjectName("paletteList")
        self.list.setDragEnabled(True)
        self.list.setSelectionMode(QListWidget.SelectionMode.SingleSelection)
        self.list.setToolTip("Drag an equipment type onto the canvas to add a unit.")
        layout.addWidget(self.list, 1)

        self._populate()

    def _populate(self) -> None:
        for code in list_schemas():
            schema = SCHEMAS[code]
            item = QListWidgetItem(f"{code}  \u2014  {schema.name}")
            item.setData(Qt.ItemDataRole.UserRole, code)
            item.setToolTip(schema.description or schema.name)
            self.list.addItem(item)

    def _current_code(self) -> Optional[str]:
        item = self.list.currentItem()
        if item is None:
            return None
        return item.data(Qt.ItemDataRole.UserRole)
