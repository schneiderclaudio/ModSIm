"""Liberation-model selection dialog.

A modal dialog choosing between the Ljubljana (Andrews-Mika) diagram and the
Beta (Austin breakage function) diagram; the Beta model additionally takes
four float parameters.  On accept :meth:`LiberationDialog.get_request`
returns the engine model name plus the parameter list.
"""

from __future__ import annotations

from typing import List, Optional, Tuple

from PySide6.QtWidgets import (
    QButtonGroup,
    QDialog,
    QDialogButtonBox,
    QDoubleSpinBox,
    QFormLayout,
    QLabel,
    QRadioButton,
    QVBoxLayout,
    QWidget,
)


class LiberationDialog(QDialog):
    """Modal dialog choosing a liberation model and its parameters."""

    def __init__(self, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.setWindowTitle("Liberation Models")
        self.setMinimumWidth(420)

        layout = QVBoxLayout(self)

        hint = QLabel(
            "Compute a liberation diagram from the current job's system data "
            "(CURRDATA.SYD)."
        )
        hint.setWordWrap(True)
        hint.setStyleSheet("color: #8a8a8a;")
        layout.addWidget(hint)

        self._ljub_radio = QRadioButton("Ljubljana (Andrews-Mika) diagram")
        self._beta_radio = QRadioButton("Beta (Austin breakage function) diagram")
        group = QButtonGroup(self)
        group.addButton(self._ljub_radio)
        group.addButton(self._beta_radio)
        self._ljub_radio.setChecked(True)
        layout.addWidget(self._ljub_radio)
        layout.addWidget(self._beta_radio)

        form = QFormLayout()
        form.setFieldGrowthPolicy(QFormLayout.AllNonFixedFieldsGrow)
        self._beta_inputs: List[QDoubleSpinBox] = []
        for label in ("Beta", "Gamma", "Delta", "Phi"):
            spin = QDoubleSpinBox()
            spin.setDecimals(4)
            spin.setRange(-1.0e9, 1.0e9)
            spin.setValue(0.0)
            form.addRow(label, spin)
            self._beta_inputs.append(spin)
        layout.addLayout(form)

        self._ljub_radio.toggled.connect(self._update_enabled)
        self._beta_radio.toggled.connect(self._update_enabled)
        self._update_enabled()

        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

    # ------------------------------------------------------------------
    # Internal
    # ------------------------------------------------------------------
    def _update_enabled(self) -> None:
        enabled = self._beta_radio.isChecked()
        for spin in self._beta_inputs:
            spin.setEnabled(enabled)

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------
    def request(self) -> Tuple[str, List[float]]:
        """Return ``(model, params)`` for the currently selected model.

        ``model`` is ``"ljubamd"`` (no parameters) or ``"betaamd"`` with the
        four Beta/Gamma/Delta/Phi parameters.
        """
        if self._beta_radio.isChecked():
            return ("betaamd", [spin.value() for spin in self._beta_inputs])
        return ("ljubamd", [])

    @staticmethod
    def get_request(
        parent: Optional[QWidget] = None,
    ) -> Optional[Tuple[str, List[float]]]:
        """Show the liberation-model dialog.

        Returns ``(model, params)`` when accepted, ``None`` when cancelled.
        """
        dialog = LiberationDialog(parent)
        if dialog.exec() != QDialog.DialogCode.Accepted:
            return None
        return dialog.request()
