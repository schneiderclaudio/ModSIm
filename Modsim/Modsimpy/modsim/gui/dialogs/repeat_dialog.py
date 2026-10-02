"""Repetitive-simulation (parameter sweep) configuration dialog.

The user builds a list of sweep levels; each level varies one parameter of
one unit across ``start..end``.  On accept the dialog returns a
:class:`~modsim.gui.simulation.SweepConfig` describing every level.
"""

from __future__ import annotations

from typing import List, Optional

from PySide6.QtWidgets import (
    QComboBox,
    QDialog,
    QDialogButtonBox,
    QDoubleSpinBox,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QLineEdit,
    QMessageBox,
    QPushButton,
    QSpinBox,
    QTableWidget,
    QVBoxLayout,
    QWidget,
)

from ...models.job import CurUnit, Job
from ..simulation import SweepConfig, SweepLevel
from .schema import get_schema

#: Maximum number of sweep rows (the engine supports levels 1..5).
_MAX_LEVELS = 5


def _default_param_name(unit: CurUnit, param_index: int) -> str:
    """A sensible default name for ``unit``'s ``param_index``-th parameter.

    Uses the schema label when the unit's model is registered, falling back
    to a generic ``Parameter N`` name.
    """
    try:
        fields = get_schema(unit.model).fields
        if param_index < len(fields) and fields[param_index].label:
            return fields[param_index].label
    except KeyError:
        pass
    return f"Parameter {param_index + 1}"


class RepeatDialog(QDialog):
    """Modal dialog for configuring a parameter sweep.

    Each row of the table is one sweep level: the 1-based level number (1..5),
    the unit to vary (chosen from the job's ``.cur`` units), the 0-based
    parameter index within that unit, the parameter name, and the
    start/end/step range to sweep.
    """

    _HEADERS = ["Level", "Unit #", "Param #", "Param name", "Start", "End", "Step"]

    def __init__(self, job: Job, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self._job = job
        self.setWindowTitle("Repetitive Simulation")
        self.setMinimumWidth(760)

        layout = QVBoxLayout(self)

        hint = QLabel(
            "Each level sweeps one parameter of one unit over start..end. "
            "Levels run in ascending order and every combination is simulated; "
            "cumulative results accumulate in CumData.out in the job directory."
        )
        hint.setWordWrap(True)
        hint.setStyleSheet("color: #8a8a8a;")
        layout.addWidget(hint)

        self._table = QTableWidget(0, len(self._HEADERS), self)
        self._table.setHorizontalHeaderLabels(self._HEADERS)
        header = self._table.horizontalHeader()
        header.setSectionResizeMode(QHeaderView.ResizeToContents)
        header.setSectionResizeMode(3, QHeaderView.Stretch)
        layout.addWidget(self._table)

        buttons_row = QHBoxLayout()
        add_button = QPushButton("Add Level")
        remove_button = QPushButton("Remove Level")
        add_button.clicked.connect(self._add_row)
        remove_button.clicked.connect(self._remove_row)
        buttons_row.addWidget(add_button)
        buttons_row.addWidget(remove_button)
        buttons_row.addStretch(1)
        layout.addLayout(buttons_row)

        self._add_row()

        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

    # ------------------------------------------------------------------
    # Row construction
    # ------------------------------------------------------------------
    def _add_row(self) -> None:
        if self._table.rowCount() >= _MAX_LEVELS:
            return
        row = self._table.rowCount()
        level = self._next_free_level()
        self._table.insertRow(row)

        level_spin = QSpinBox()
        level_spin.setRange(1, _MAX_LEVELS)
        level_spin.setValue(level)
        self._table.setCellWidget(row, 0, level_spin)

        unit_combo = QComboBox()
        for unit in self._job.cur.units:
            unit_combo.addItem(f"{unit.number} ({unit.model})", unit.number)
        unit_combo.currentIndexChanged.connect(
            lambda _index, combo=unit_combo: self._sync_param_range(combo)
        )
        self._table.setCellWidget(row, 1, unit_combo)

        param_spin = QSpinBox()
        param_spin.setRange(0, 0)
        self._table.setCellWidget(row, 2, param_spin)

        name_edit = QLineEdit()
        unit = self._unit_for(unit_combo.currentData())
        if unit is not None:
            name_edit.setText(_default_param_name(unit, 0))
        self._table.setCellWidget(row, 3, name_edit)

        self._table.setCellWidget(row, 4, self._make_double_spin(0.0))
        self._table.setCellWidget(row, 5, self._make_double_spin(1.0))
        self._table.setCellWidget(row, 6, self._make_double_spin(0.5))

        self._sync_param_range(unit_combo)

    @staticmethod
    def _make_double_spin(value: float) -> QDoubleSpinBox:
        spin = QDoubleSpinBox()
        spin.setDecimals(4)
        spin.setRange(-1.0e9, 1.0e9)
        spin.setValue(value)
        return spin

    def _next_free_level(self) -> int:
        used = {
            self._table.cellWidget(r, 0).value()
            for r in range(self._table.rowCount())
        }
        for level in range(1, _MAX_LEVELS + 1):
            if level not in used:
                return level
        return _MAX_LEVELS

    def _remove_row(self) -> None:
        if self._table.rowCount() <= 1:
            return
        row = self._table.currentRow()
        if row < 0:
            row = self._table.rowCount() - 1
        self._table.removeRow(row)

    # ------------------------------------------------------------------
    # Unit helpers
    # ------------------------------------------------------------------
    def _unit_for(self, number: int) -> Optional[CurUnit]:
        for unit in self._job.cur.units:
            if unit.number == number:
                return unit
        return None

    def _sync_param_range(self, combo: QComboBox) -> None:
        """Clamp a row's parameter-index spin to its unit's ``noparam - 1``."""
        unit = self._unit_for(combo.currentData())
        maximum = max(unit.noparam - 1, 0) if unit is not None else 0
        for row in range(self._table.rowCount()):
            if self._table.cellWidget(row, 1) is combo:
                self._table.cellWidget(row, 2).setMaximum(maximum)
                return

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------
    def levels(self) -> List[SweepLevel]:
        """Collect the configured sweep levels from the table."""
        levels: List[SweepLevel] = []
        for row in range(self._table.rowCount()):
            unit_number = self._table.cellWidget(row, 1).currentData()
            param_index = self._table.cellWidget(row, 2).value()
            name_edit = self._table.cellWidget(row, 3)
            param_name = name_edit.text().strip() or f"Parameter {param_index + 1}"
            levels.append(
                SweepLevel(
                    level=self._table.cellWidget(row, 0).value(),
                    unit_number=unit_number,
                    param_index=param_index,
                    param_name=param_name,
                    start=self._table.cellWidget(row, 4).value(),
                    end=self._table.cellWidget(row, 5).value(),
                    step=self._table.cellWidget(row, 6).value(),
                )
            )
        return levels

    @staticmethod
    def get_config(
        job: Job, parent: Optional[QWidget] = None
    ) -> Optional[SweepConfig]:
        """Show the sweep-configuration dialog for ``job``.

        Returns a :class:`SweepConfig` when accepted, ``None`` when cancelled
        (or when the job has no ``.cur`` units to sweep).
        """
        if job.cur is None or not job.cur.units:
            QMessageBox.information(
                parent,
                "Repetitive Simulation",
                "The job has no units in its .cur data; nothing to sweep.",
            )
            return None
        dialog = RepeatDialog(job, parent)
        if dialog.exec() != QDialog.DialogCode.Accepted:
            return None
        return SweepConfig(levels=dialog.levels())
