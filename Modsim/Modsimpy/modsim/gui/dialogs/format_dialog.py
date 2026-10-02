"""Output-format (FORMAT.OUT) editor dialog for the ModSIM GUI.

``FORMAT.OUT`` is a fixed-name run file that controls how simulation results
are written: which quantity columns appear, the display units for solids /
water / metal content, optional metal and mineral grade columns, and optional
size-distribution and accumulation stream lists.

This dialog mirrors the legacy VB6 ``OUTFORMAT.FRM`` form.  It edits the
structured :class:`~modsim.models.job.FormatOutFile` fields in place -- no file
I/O and no engine calls.  On accept it mutates the instance passed to the
constructor, so the caller's ``FormatOutFile`` is already up to date after
``exec()`` returns ``QDialog.DialogCode.Accepted``.
"""

from __future__ import annotations

from typing import List, Optional

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QAbstractItemView,
    QButtonGroup,
    QCheckBox,
    QDialog,
    QDialogButtonBox,
    QGridLayout,
    QGroupBox,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QLineEdit,
    QListWidget,
    QMessageBox,
    QPushButton,
    QRadioButton,
    QSpinBox,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

from ...models.job import FormatOutFile

__all__ = ["FormatDialog"]

# Unit labels, in the order the VB6 form presents them (label index + 1 is the
# stored value).
SOLID_UNITS = [
    "kg/s",
    "tonnes/hr",
    "ktonnes/month",
    "Mtonnes/year",
    "short tons/hr",
    "short tons/day",
]
WATER_UNITS = ["kg/s", "tonnes/hr", "cub m/hr", "liters/min", "gal/min"]
METAL_UNITS = ["%", "g/t"]

MAX_METALS = 4
MAX_MINERALS = 7

# Dark theme matching the shell in main_window._apply_style.
_STYLESHEET = """
QDialog { background: #2b2b2b; }
QGroupBox {
    background: #333333;
    color: #e6e6e6;
    border: 1px solid #4a4a4a;
    border-radius: 6px;
    margin-top: 14px;
    padding-top: 4px;
}
QGroupBox::title {
    subcontrol-origin: margin;
    subcontrol-position: top left;
    left: 12px;
    padding: 0 4px;
    color: #9cdcfe;
    font-weight: 600;
}
QLabel { color: #e6e6e6; }
QCheckBox { color: #e6e6e6; spacing: 6px; }
QRadioButton { color: #e6e6e6; spacing: 6px; }
QSpinBox, QLineEdit {
    background: #2b2b2b;
    color: #e6e6e6;
    border: 1px solid #4a4a4a;
    border-radius: 4px;
    padding: 3px 6px;
    selection-background-color: #1f6feb;
}
QSpinBox:focus, QLineEdit:focus { border: 1px solid #1f6feb; }
QSpinBox:disabled, QLineEdit:disabled { color: #8a8a8a; }
QListWidget, QTableWidget {
    background: #2b2b2b;
    color: #e6e6e6;
    border: 1px solid #4a4a4a;
    border-radius: 4px;
    gridline-color: #3a3a3a;
}
QListWidget::item:selected, QTableWidget::item:selected {
    background: #1f6feb;
    color: #ffffff;
}
QHeaderView::section {
    background: #3a3a3a;
    color: #e6e6e6;
    border: none;
    border-right: 1px solid #4a4a4a;
    border-bottom: 1px solid #4a4a4a;
    padding: 3px 6px;
}
QPushButton {
    background: #3a3a3a;
    color: #e6e6e6;
    border: 1px solid #4a4a4a;
    border-radius: 4px;
    padding: 4px 14px;
}
QPushButton:hover { background: #4a4a4a; }
QPushButton:pressed { background: #1f6feb; }
QPushButton:disabled { color: #8a8a8a; background: #2e2e2e; }
QDialogButtonBox QPushButton { min-width: 88px; }
"""


class FormatDialog(QDialog):
    """A dialog for editing the ``FORMAT.OUT`` output-format options.

    The dialog is constructed from a :class:`FormatOutFile` and pre-populated
    with its current values.  On OK the values are read from the widgets and
    written back into that same instance (mutated in place); cancel leaves it
    unchanged.
    """

    def __init__(
        self, format_out: FormatOutFile, parent: Optional[QWidget] = None
    ) -> None:
        super().__init__(parent)
        self._format_out = format_out

        self.setWindowTitle("Output Format — FORMAT.OUT")
        self.setMinimumWidth(760)
        self.setStyleSheet(_STYLESHEET)

        self._build_ui()

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self) -> None:
        """Assemble the full form from the group builders."""
        fo = self._format_out

        root = QVBoxLayout(self)
        root.setSpacing(12)
        root.setContentsMargins(16, 16, 16, 16)

        desc = QLabel("Edit the options written to FORMAT.OUT.")
        desc.setWordWrap(True)
        desc.setStyleSheet("color: #8a8a8a;")
        root.addWidget(desc)

        # Group 1: quantities to display.
        root.addWidget(self._build_quantities_group())

        # Groups 2-4: unit choices, side by side.
        solid_box, self._solid_group = self._unit_group(
            "Units for solid flowrate",
            SOLID_UNITS,
            self._clamp(fo.solid_units - 1, 0, len(SOLID_UNITS) - 1),
            3,
        )
        water_box, self._water_group = self._unit_group(
            "Units for water flowrate",
            WATER_UNITS,
            self._clamp(fo.water_units - 1, 0, len(WATER_UNITS) - 1),
            3,
        )
        metal_box, self._metal_group = self._unit_group(
            "Units for metal content",
            METAL_UNITS,
            self._clamp(fo.metal_units - 1, 0, len(METAL_UNITS) - 1),
            2,
        )
        units_row = QHBoxLayout()
        units_row.setSpacing(12)
        units_row.addWidget(solid_box, 2)
        units_row.addWidget(water_box, 2)
        units_row.addWidget(metal_box, 1)
        root.addLayout(units_row)

        # Group 5: metals / minerals / element matrix.
        root.addWidget(self._build_metals_group())

        # Groups 6-7: size distributions and accumulation, side by side.
        (
            size_box,
            self._size_cb,
            self._size_list,
            self._size_spin,
            self._size_add,
            self._size_remove,
        ) = self._build_stream_group(
            "Size distributions",
            "Display size distributions of selected streams",
            "Show particle size distributions for the selected streams.",
        )
        (
            accumulate_box,
            self._accumulate_cb,
            self._accumulate_list,
            self._accumulate_spin,
            self._accumulate_add,
            self._accumulate_remove,
        ) = self._build_stream_group(
            "Accumulate data",
            "Accumulate data from selected streams",
            "Accumulate output data for the selected streams across "
            "consecutive simulations.",
        )
        for stream in fo.size_streams:
            self._size_list.addItem(str(int(stream)))
        for stream in fo.accumulate_streams:
            self._accumulate_list.addItem(str(int(stream)))
        self._size_cb.setChecked(fo.size_flag)
        self._accumulate_cb.setChecked(fo.accumulate_flag)
        self._set_stream_controls_enabled(
            self._size_list, self._size_spin, self._size_add, self._size_remove,
            fo.size_flag,
        )
        self._set_stream_controls_enabled(
            self._accumulate_list, self._accumulate_spin,
            self._accumulate_add, self._accumulate_remove,
            fo.accumulate_flag,
        )

        streams_row = QHBoxLayout()
        streams_row.setSpacing(12)
        streams_row.addWidget(size_box, 1)
        streams_row.addWidget(accumulate_box, 1)
        root.addLayout(streams_row)

        # Group 8: coal data flag.
        self._coal_cb = QCheckBox("Use format for coal data")
        self._coal_cb.setChecked(fo.coal_flag)
        self._coal_cb.setToolTip("Treat this output format as coal data.")
        root.addWidget(self._coal_cb)

        buttons = QDialogButtonBox(
            QDialogButtonBox.Ok | QDialogButtonBox.Cancel
        )
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        root.addWidget(buttons)

    def _build_quantities_group(self) -> QGroupBox:
        """Group 1: which quantity columns to display."""
        fo = self._format_out
        box = QGroupBox("Select quantities to display")
        grid = QGridLayout(box)
        grid.setHorizontalSpacing(16)
        grid.setVerticalSpacing(8)

        self._water_cb = QCheckBox("Water flowrate")
        self._pct_solids_cb = QCheckBox("% solids")
        self._yield_cb = QCheckBox("Yield of total solids")
        self._minerals_cb = QCheckBox(
            "Recovery and grade of individual minerals"
        )
        self._metals_cb = QCheckBox("Recovery and grade of individual metals")

        self._water_cb.setChecked(fo.show_water)
        self._pct_solids_cb.setChecked(fo.show_pct_solids)
        self._yield_cb.setChecked(fo.show_yield)
        self._minerals_cb.setChecked(fo.show_minerals)
        self._metals_cb.setChecked(fo.show_metals)

        grid.addWidget(self._water_cb, 0, 0)
        grid.addWidget(self._pct_solids_cb, 0, 1)
        grid.addWidget(self._yield_cb, 1, 0)
        grid.addWidget(self._minerals_cb, 1, 1)
        grid.addWidget(self._metals_cb, 2, 0, 1, 2)

        note = QLabel("Solids flowrate is always included in the output.")
        note.setStyleSheet("color: #8a8a8a;")
        grid.addWidget(note, 3, 0, 1, 2)

        return box

    def _unit_group(
        self, title: str, labels: List[str], current: int, cols: int
    ) -> tuple[QGroupBox, QButtonGroup]:
        """Build a radio-button group titled ``title``.

        ``current`` is the index of the initially checked option.
        """
        box = QGroupBox(title)
        grid = QGridLayout(box)
        grid.setHorizontalSpacing(12)
        grid.setVerticalSpacing(8)

        group = QButtonGroup(self)
        for i, label in enumerate(labels):
            radio = QRadioButton(label)
            group.addButton(radio, i)
            grid.addWidget(radio, i // cols, i % cols)

        if 0 <= current < len(labels):
            group.button(current).setChecked(True)
        return box, group

    def _build_metals_group(self) -> QGroupBox:
        """Group 5: metal names and the mineral-element matrix."""
        fo = self._format_out
        box = QGroupBox("Metals and elements")
        v = QVBoxLayout(box)
        v.setSpacing(8)

        # Count spinners.
        top = QHBoxLayout()
        top.setSpacing(8)
        top.addWidget(QLabel("Number of metals:"))
        self._num_metals_spin = QSpinBox()
        self._num_metals_spin.setRange(0, MAX_METALS)
        self._num_metals_spin.setToolTip(
            "Number of metals or elements to report (0 hides the section)."
        )
        top.addWidget(self._num_metals_spin)
        top.addSpacing(24)
        top.addWidget(QLabel("Number of minerals:"))
        self._num_minerals_spin = QSpinBox()
        self._num_minerals_spin.setRange(1, MAX_MINERALS)
        self._num_minerals_spin.setToolTip(
            "Number of mineral rows in the metal-element matrix."
        )
        top.addWidget(self._num_minerals_spin)
        top.addStretch(1)
        v.addLayout(top)

        # Metal names.
        names_row = QHBoxLayout()
        names_row.setSpacing(8)
        names_row.addWidget(QLabel("Metal names:"))
        self._metal_name_edits: List[QLineEdit] = []
        for i in range(MAX_METALS):
            edit = QLineEdit()
            edit.setPlaceholderText(f"Metal {i + 1}")
            edit.setToolTip("Name of the metal or element.")
            self._metal_name_edits.append(edit)
            names_row.addWidget(edit, 1)
        v.addLayout(names_row)

        # Mineral-element matrix (rows = minerals, columns = metals).
        self._matrix = QTableWidget(0, 0)
        self._matrix.setMinimumHeight(150)
        self._matrix.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
        self._matrix.verticalHeader().setDefaultSectionSize(26)
        self._matrix.setToolTip(
            "Mass fraction of each metal or element in each mineral (0 to 1)."
        )
        v.addWidget(self._matrix)

        # Populate from the model.
        num_metals = self._clamp(fo.num_metals, 0, MAX_METALS)
        if fo.metal_names:
            num_metals = max(num_metals, min(len(fo.metal_names), MAX_METALS))
        num_minerals = self._clamp(fo.num_minerals, 1, MAX_MINERALS)
        if fo.minmetal:
            num_minerals = max(num_minerals, min(len(fo.minmetal), MAX_MINERALS))

        self._num_metals_spin.setValue(num_metals)
        self._num_minerals_spin.setValue(num_minerals)

        for i in range(MAX_METALS):
            name = fo.metal_names[i] if i < len(fo.metal_names) else ""
            self._metal_name_edits[i].setText(name)

        self._resize_matrix(num_minerals, num_metals)
        for r in range(num_minerals):
            for c in range(num_metals):
                value = 0.0
                if r < len(fo.minmetal) and c < len(fo.minmetal[r]):
                    value = fo.minmetal[r][c]
                cell = self._matrix.item(r, c)
                if cell is not None:
                    cell.setText(self._fmt_float(value))
        self._refresh_matrix_headers()

        for i in range(MAX_METALS):
            self._metal_name_edits[i].setEnabled(i < num_metals)

        # Wire signals.
        self._num_metals_spin.valueChanged.connect(self._on_metals_count_changed)
        self._num_minerals_spin.valueChanged.connect(
            self._on_minerals_count_changed
        )
        for edit in self._metal_name_edits:
            edit.textChanged.connect(self._refresh_matrix_headers)

        return box

    def _build_stream_group(
        self, title: str, checkbox_label: str, tooltip: str
    ) -> tuple[QGroupBox, QCheckBox, QListWidget, QSpinBox, QPushButton, QPushButton]:
        """Build a checkbox + editable stream-number list group."""
        box = QGroupBox(title)
        v = QVBoxLayout(box)
        v.setSpacing(8)

        checkbox = QCheckBox(checkbox_label)
        checkbox.setToolTip(tooltip)
        v.addWidget(checkbox)

        list_widget = QListWidget()
        list_widget.setSelectionMode(QAbstractItemView.ExtendedSelection)
        list_widget.setToolTip(
            "Selected stream numbers. Select one and click Remove to delete it."
        )
        list_widget.setMinimumHeight(110)
        v.addWidget(list_widget)

        controls = QHBoxLayout()
        controls.setSpacing(8)
        spin = QSpinBox()
        spin.setRange(1, 9999)
        spin.setToolTip("Stream number to add.")
        add_btn = QPushButton("Add")
        remove_btn = QPushButton("Remove")
        controls.addWidget(QLabel("Stream:"))
        controls.addWidget(spin, 1)
        controls.addWidget(add_btn)
        controls.addWidget(remove_btn)
        v.addLayout(controls)

        add_btn.clicked.connect(lambda: self._add_stream(list_widget, spin))
        remove_btn.clicked.connect(lambda: self._remove_stream(list_widget))
        checkbox.toggled.connect(
            lambda on: self._set_stream_controls_enabled(
                list_widget, spin, add_btn, remove_btn, on
            )
        )

        return box, checkbox, list_widget, spin, add_btn, remove_btn

    # ------------------------------------------------------------------
    # Matrix handling
    # ------------------------------------------------------------------

    def _resize_matrix(self, rows: int, cols: int) -> None:
        """Resize the matrix, preserving values that survive the change."""
        old_rows = self._matrix.rowCount()
        old_cols = self._matrix.columnCount()
        snapshot: List[List[str]] = []
        for r in range(old_rows):
            row: List[str] = []
            for c in range(old_cols):
                cell = self._matrix.item(r, c)
                row.append(cell.text() if cell is not None else "")
            snapshot.append(row)

        self._matrix.blockSignals(True)
        self._matrix.setRowCount(rows)
        self._matrix.setColumnCount(cols)
        for r in range(rows):
            for c in range(cols):
                text = snapshot[r][c] if (r < old_rows and c < old_cols) else "0"
                self._matrix.setItem(r, c, self._make_cell(text))
        self._matrix.blockSignals(False)
        self._refresh_matrix_headers()

    def _refresh_matrix_headers(self, *_args) -> None:
        """Refresh the matrix headers from the metal name edits."""
        cols = self._matrix.columnCount()
        headers = []
        for c in range(cols):
            name = ""
            if c < len(self._metal_name_edits):
                name = self._metal_name_edits[c].text().strip()
            headers.append(name if name else f"Metal {c + 1}")
        self._matrix.setHorizontalHeaderLabels(headers)
        rows = self._matrix.rowCount()
        self._matrix.setVerticalHeaderLabels(
            [f"Mineral {r + 1}" for r in range(rows)]
        )

    def _make_cell(self, text: str) -> QTableWidgetItem:
        item = QTableWidgetItem(text)
        item.setTextAlignment(Qt.AlignRight | Qt.AlignVCenter)
        return item

    def _read_matrix(self) -> List[List[float]]:
        """Read the matrix cells into a ``num_minerals`` x ``num_metals`` list."""
        rows = self._matrix.rowCount()
        cols = self._matrix.columnCount()
        out: List[List[float]] = []
        for r in range(rows):
            row: List[float] = []
            for c in range(cols):
                item = self._matrix.item(r, c)
                text = item.text().strip() if item is not None else ""
                try:
                    row.append(float(text) if text else 0.0)
                except ValueError:
                    row.append(0.0)
            out.append(row)
        return out

    # ------------------------------------------------------------------
    # Stream list handling
    # ------------------------------------------------------------------

    @staticmethod
    def _set_stream_controls_enabled(
        list_widget: QListWidget,
        spin: QSpinBox,
        add_btn: QPushButton,
        remove_btn: QPushButton,
        enabled: bool,
    ) -> None:
        for widget in (list_widget, spin, add_btn, remove_btn):
            widget.setEnabled(enabled)

    @staticmethod
    def _add_stream(list_widget: QListWidget, spin: QSpinBox) -> None:
        text = str(spin.value())
        for i in range(list_widget.count()):
            if list_widget.item(i).text() == text:
                return  # already selected
        list_widget.addItem(text)

    @staticmethod
    def _remove_stream(list_widget: QListWidget) -> None:
        for item in list_widget.selectedItems():
            list_widget.takeItem(list_widget.row(item))

    @staticmethod
    def _streams_from_list(list_widget: QListWidget) -> List[int]:
        streams: List[int] = []
        for i in range(list_widget.count()):
            text = list_widget.item(i).text().strip()
            try:
                streams.append(int(text))
            except ValueError:
                continue
        return streams

    # ------------------------------------------------------------------
    # Slot handlers
    # ------------------------------------------------------------------

    def _on_metals_count_changed(self, value: int) -> None:
        for i, edit in enumerate(self._metal_name_edits):
            edit.setEnabled(i < value)
        self._resize_matrix(self._num_minerals_spin.value(), value)

    def _on_minerals_count_changed(self, value: int) -> None:
        self._resize_matrix(value, self._num_metals_spin.value())

    # ------------------------------------------------------------------
    # Accept / model mutation
    # ------------------------------------------------------------------

    def accept(self) -> None:
        """Validate, write widget values into ``format_out``, then accept."""
        if not self._validate():
            return
        self._write_to_model()
        super().accept()

    def _validate(self) -> bool:
        """Check the matrix cells are numeric fractions between 0 and 1."""
        for r in range(self._matrix.rowCount()):
            for c in range(self._matrix.columnCount()):
                item = self._matrix.item(r, c)
                text = item.text().strip() if item is not None else ""
                if not text:
                    continue
                try:
                    value = float(text)
                except ValueError:
                    QMessageBox.warning(
                        self,
                        "Invalid value",
                        f"Cell (mineral {r + 1}, metal {c + 1}) is not a "
                        "number.",
                    )
                    return False
                if value > 1.0:
                    QMessageBox.warning(
                        self,
                        "Invalid value",
                        f"Cell (mineral {r + 1}, metal {c + 1}) must be a "
                        "fraction between 0 and 1.",
                    )
                    return False
        return True

    def _write_to_model(self) -> None:
        """Copy widget values back into the passed-in ``FormatOutFile``."""
        fo = self._format_out
        fo.solid_units = self._solid_group.checkedId() + 1
        fo.water_units = self._water_group.checkedId() + 1
        fo.metal_units = self._metal_group.checkedId() + 1
        fo.show_water = self._water_cb.isChecked()
        fo.show_pct_solids = self._pct_solids_cb.isChecked()
        fo.show_yield = self._yield_cb.isChecked()
        fo.show_minerals = self._minerals_cb.isChecked()
        fo.show_metals = self._metals_cb.isChecked()
        fo.coal_flag = self._coal_cb.isChecked()
        fo.num_metals = self._num_metals_spin.value()
        fo.metal_names = [
            self._metal_name_edits[i].text().strip()
            for i in range(fo.num_metals)
        ]
        fo.num_minerals = self._num_minerals_spin.value()
        fo.minmetal = self._read_matrix()
        fo.size_flag = self._size_cb.isChecked()
        fo.size_streams = self._streams_from_list(self._size_list)
        fo.accumulate_flag = self._accumulate_cb.isChecked()
        fo.accumulate_streams = self._streams_from_list(self._accumulate_list)

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _clamp(value: int, lo: int, hi: int) -> int:
        return max(lo, min(hi, value))

    @staticmethod
    def _fmt_float(value: float) -> str:
        if value == 0:
            return "0"
        return format(value, "g")
