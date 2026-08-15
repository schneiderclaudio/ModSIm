"""Schema-driven equipment parameter dialogs for the ModSIM GUI.

This module turns an :class:`~modsim.gui.dialogs.schema.EquipmentSchema` into a
``QDialog`` form (data-driven generation), validates entered values against the
schema bounds, and wires the dialog to read/write the corresponding ``.cur``
job-file data section.

The ``.cur`` file stores each unit's parameters as a flat list of strings
(``CurUnit.params``).  Field order in the schema maps positionally onto that
list: field ``i`` corresponds to ``params[i]``.  Loading parses each string
into the field's type; writing formats each value back into the list.
"""

from __future__ import annotations

import math
from typing import Any, Dict, List, Optional, Tuple

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QCheckBox,
    QComboBox,
    QDialog,
    QDialogButtonBox,
    QFormLayout,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QMessageBox,
    QVBoxLayout,
    QWidget,
)

from ...models.job import CurUnit, Job
from .schema import EquipmentSchema, Field, FieldType, get_schema

__all__ = [
    "EquipmentDialog",
    "generate_dialog",
    "validate_value",
    "validate_values",
    "load_values_from_job",
    "write_values_to_job",
    "edit_equipment",
]

# Style applied to an input flagged as out of bounds.
_ERROR_STYLE = "border: 2px solid #d32f2f; background-color: #fff0f0;"


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

def validate_value(field: Field, value: Any) -> Tuple[bool, Optional[str]]:
    """Validate ``value`` against ``field``'s type and bounds.

    Returns ``(ok, error)`` where ``error`` is a human-readable message when
    ``ok`` is ``False``.
    """
    if field.type is FieldType.BOOL:
        return True, None

    if field.type is FieldType.CHOICE:
        if field.choices and value not in field.choices:
            return False, f"Must be one of: {', '.join(field.choices)}"
        return True, None

    if field.type is FieldType.STR:
        return True, None

    # Numeric fields (FLOAT / INT).
    try:
        num = float(value)
    except (TypeError, ValueError):
        return False, "Must be a number"

    if field.type is FieldType.INT and not float(num).is_integer():
        return False, "Must be an integer"

    if field.min is not None and num < field.min:
        return False, f"Must be >= {field.min:g}"
    if field.max is not None and num > field.max:
        return False, f"Must be <= {field.max:g}"
    return True, None


def validate_values(
    schema: EquipmentSchema, values: Dict[str, Any]
) -> Dict[str, str]:
    """Validate every field in ``values`` against ``schema``.

    Returns a dict mapping each invalid field key to its error message.
    """
    errors: Dict[str, str] = {}
    for field in schema.fields:
        ok, msg = validate_value(field, values.get(field.key))
        if not ok:
            errors[field.key] = msg or "Invalid value"
    return errors


# ---------------------------------------------------------------------------
# Value <-> string helpers for the .cur parameter list
# ---------------------------------------------------------------------------

def _format_param(field: Field, value: Any) -> str:
    """Format ``value`` as the string stored in a ``.cur`` parameter list."""
    if field.type is FieldType.BOOL:
        return "1" if value else "0"
    if field.type is FieldType.CHOICE:
        return str(value)
    if field.type is FieldType.STR:
        return str(value)
    if field.type is FieldType.INT:
        return str(int(value))
    # FLOAT -- legacy E-notation with a signed exponent (matches the engine).
    return _fmt_e(float(value))


def _fmt_e(value: float) -> str:
    """Format a float in the legacy ``E``-notation (e.g. ``1.2780E+2``)."""
    if value == 0.0:
        return "0.0000E+0"
    exp = int(math.floor(math.log10(abs(value)) + 1e-12))
    mant = value / (10.0 ** exp)
    if abs(mant) >= 10.0 - 1e-12:
        mant /= 10.0
        exp += 1
    sign = "+" if exp >= 0 else "-"
    return f"{mant:.4f}E{sign}{abs(exp)}"


def _parse_param(field: Field, raw: str) -> Any:
    """Parse a ``.cur`` parameter string into ``field``'s type."""
    if field.type is FieldType.BOOL:
        return raw.strip() not in ("0", "0.0", "FALSE", "false", "")
    if field.type is FieldType.CHOICE:
        return raw.strip()
    if field.type is FieldType.STR:
        return raw.strip()
    try:
        num = float(raw)
    except ValueError:
        return raw.strip()
    if field.type is FieldType.INT:
        return int(round(num))
    return num


# ---------------------------------------------------------------------------
# Job wiring
# ---------------------------------------------------------------------------

def _find_unit(job: Job, unit_number: int) -> CurUnit:
    """Return the ``CurUnit`` with ``unit_number`` from ``job.cur``."""
    if job.cur is None:
        raise ValueError("Job has no .cur data section")
    for unit in job.cur.units:
        if unit.number == unit_number:
            return unit
    raise KeyError(f"No unit {unit_number} found in job .cur data")


def load_values_from_job(job: Job, unit_number: int) -> Dict[str, Any]:
    """Load the current parameter values for ``unit_number`` from ``job``.

    The unit's model code selects the schema; each ``.cur`` parameter string is
    parsed into the corresponding schema field's type.  Returns a dict keyed by
    field key.
    """
    unit = _find_unit(job, unit_number)
    schema = get_schema(unit.model)
    values: Dict[str, Any] = {}
    for i, field in enumerate(schema.fields):
        if i < len(unit.params):
            values[field.key] = _parse_param(field, unit.params[i])
        else:
            values[field.key] = field.default
    return values


def write_values_to_job(
    job: Job, unit_number: int, values: Dict[str, Any]
) -> None:
    """Write ``values`` back to the ``.cur`` data section for ``unit_number``.

    Values are formatted into the unit's flat parameter list in schema field
    order.  Because the ``.cur`` writer emits ``raw_lines`` verbatim when
    present, the file's ``raw_lines`` are cleared so the structured (edited)
    parameters are actually persisted on the next write.
    """
    unit = _find_unit(job, unit_number)
    schema = get_schema(unit.model)
    params: List[str] = []
    for field in schema.fields:
        params.append(_format_param(field, values.get(field.key, field.default)))
    unit.params = params
    if job.cur is not None:
        job.cur.raw_lines = None


# ---------------------------------------------------------------------------
# Dialog
# ---------------------------------------------------------------------------

class EquipmentDialog(QDialog):
    """A ``QDialog`` generated from an :class:`EquipmentSchema`.

    Each schema field becomes a labeled input widget appropriate to its type,
    with a unit suffix where a unit is defined.  On OK the dialog validates
    every field against the schema bounds; any out-of-bounds value is flagged
    with a red border and an error message, and the dialog is not accepted
    until corrected.
    """

    def __init__(
        self,
        schema: EquipmentSchema,
        values: Optional[Dict[str, Any]] = None,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self._schema = schema
        self._inputs: Dict[str, QWidget] = {}
        self._labels: Dict[str, QLabel] = {}

        self.setWindowTitle(f"{schema.name} ({schema.code})")
        self.setMinimumWidth(420)

        layout = QVBoxLayout(self)

        if schema.description:
            desc = QLabel(schema.description)
            desc.setWordWrap(True)
            desc.setStyleSheet("color: #666;")
            layout.addWidget(desc)

        form = QFormLayout()
        form.setFieldGrowthPolicy(QFormLayout.AllNonFixedFieldsGrow)
        for field in schema.fields:
            widget = self._build_widget(field, values)
            self._inputs[field.key] = widget
            label = QLabel(field.label)
            label.setToolTip(field.help or "")
            self._labels[field.key] = label
            form.addRow(label, widget)
        layout.addLayout(form)

        self._error_label = QLabel("")
        self._error_label.setStyleSheet("color: #d32f2f;")
        self._error_label.setWordWrap(True)
        self._error_label.hide()
        layout.addWidget(self._error_label)

        buttons = QDialogButtonBox(
            QDialogButtonBox.Ok | QDialogButtonBox.Cancel
        )
        buttons.accepted.connect(self._on_accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

    # -- widget construction ------------------------------------------------

    def _build_widget(
        self, field: Field, values: Optional[Dict[str, Any]]
    ) -> QWidget:
        current = values.get(field.key, field.default) if values else field.default

        if field.type is FieldType.BOOL:
            box = QCheckBox()
            box.setChecked(bool(current))
            return box

        if field.type is FieldType.CHOICE:
            combo = QComboBox()
            combo.addItems(field.choices or [])
            if current is not None and str(current) in (field.choices or []):
                combo.setCurrentText(str(current))
            return combo

        if field.type is FieldType.STR:
            edit = QLineEdit()
            edit.setText("" if current is None else str(current))
            return edit

        # Numeric (FLOAT / INT): use a line edit so out-of-bounds values can be
        # entered and then flagged by validation.
        edit = QLineEdit()
        if current is not None:
            edit.setText(str(current))
        edit.setAlignment(Qt.AlignRight)
        if field.unit:
            edit.setToolTip(f"Unit: {field.unit}")
        return edit

    def _widget_value(self, field: Field) -> Any:
        widget = self._inputs[field.key]
        if field.type is FieldType.BOOL:
            return widget.isChecked()
        if field.type is FieldType.CHOICE:
            return widget.currentText()
        if field.type is FieldType.STR:
            return widget.text()
        text = widget.text().strip()
        if text == "":
            return field.default
        if field.type is FieldType.INT:
            try:
                return int(float(text))
            except ValueError:
                return text
        try:
            return float(text)
        except ValueError:
            return text

    # -- validation ---------------------------------------------------------

    def _on_accept(self) -> None:
        errors = validate_values(self._schema, self.values())
        self._clear_errors()
        if errors:
            for key, msg in errors.items():
                self._flag_error(key, msg)
            self._error_label.setText(
                "Please correct the highlighted values."
            )
            self._error_label.show()
            return
        self.accept()

    def _flag_error(self, key: str, message: str) -> None:
        widget = self._inputs[key]
        widget.setStyleSheet(_ERROR_STYLE)
        label = self._labels[key]
        label.setToolTip(message)
        label.setStyleSheet("color: #d32f2f; font-weight: bold;")

    def _clear_errors(self) -> None:
        for key, widget in self._inputs.items():
            widget.setStyleSheet("")
            self._labels[key].setStyleSheet("")
            self._labels[key].setToolTip(self._schema_fields()[key].help or "")
        self._error_label.hide()

    def _schema_fields(self) -> Dict[str, Field]:
        return {f.key: f for f in self._schema.fields}

    # -- public API ---------------------------------------------------------

    def values(self) -> Dict[str, Any]:
        """Return the current field values keyed by field key."""
        return {f.key: self._widget_value(f) for f in self._schema.fields}


def generate_dialog(
    schema: EquipmentSchema,
    values: Optional[Dict[str, Any]] = None,
    parent: Optional[QWidget] = None,
) -> EquipmentDialog:
    """Build an :class:`EquipmentDialog` from ``schema``.

    ``values`` optionally pre-fills the form (e.g. values loaded from a job).
    """
    return EquipmentDialog(schema, values=values, parent=parent)


def edit_equipment(
    job: Job,
    unit_number: int,
    parent: Optional[QWidget] = None,
) -> bool:
    """Open the parameter dialog for ``unit_number`` and persist on accept.

    Loads the unit's current values from ``job``'s ``.cur`` data, shows the
    generated dialog, and on accept writes the validated values back to the
    job model.  Returns ``True`` if the user accepted (and values were
    written), ``False`` if cancelled.
    """
    unit = _find_unit(job, unit_number)
    schema = get_schema(unit.model)
    values = load_values_from_job(job, unit_number)
    dialog = generate_dialog(schema, values=values, parent=parent)
    if dialog.exec() == QDialog.Accepted:
        write_values_to_job(job, unit_number, dialog.values())
        return True
    return False
