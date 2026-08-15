"""Flowsheet sub-window content.

This module defines the seam that Phase 5 (the flowsheet canvas) will fill in.
:class:`FlowsheetWindow` is the base widget that is hosted inside a
``QMdiSubWindow``; subclasses implement the actual rendering.  For now the
shell ships with :class:`PlaceholderFlowsheetWindow`, which shows the job name
and unit count so the shell is functional and testable without a canvas.
"""

from __future__ import annotations

from typing import Optional

from PySide6.QtCore import Qt
from PySide6.QtWidgets import QLabel, QVBoxLayout, QWidget

from ..models.job import Job


class FlowsheetWindow(QWidget):
    """Base widget for a flowsheet displayed in an MDI sub-window.

    Subclasses should call :meth:`_build_body` (or override :meth:`_build`) to
    lay out their content.  The title shown on the MDI sub-window title bar is
    taken from :attr:`display_title`.
    """

    def __init__(self, job: Optional[Job] = None, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.job = job
        self._layout = QVBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._build()

    @property
    def display_title(self) -> str:
        """Title shown on the MDI sub-window's title bar."""
        return self.job.name if self.job is not None else "Flowsheet"

    def set_job(self, job: Job) -> None:
        """Associate a job with this window and refresh its content."""
        self.job = job
        self._build()

    def _build(self) -> None:
        """Rebuild the window content for the current job. Override in subclasses."""
        self._build_body()

    def _build_body(self) -> None:
        """Lay out the window body for the current job. Override in subclasses."""


class PlaceholderFlowsheetWindow(FlowsheetWindow):
    """Temporary flowsheet content until the Phase 5 canvas lands.

    Displays the job name and its unit count, with a short note that the
    visual flowsheet editor is not available yet.
    """

    def _build_body(self) -> None:
        # Clear any previous content so set_job() rebuilds cleanly.
        while self._layout.count():
            item = self._layout.takeAt(0)
            widget = item.widget()
            if widget is not None:
                widget.deleteLater()

        job = self.job
        unit_count = job.syd.unit_count if job is not None and job.syd else 0

        container = QWidget(self)
        container.setObjectName("placeholderCard")
        outer = QVBoxLayout(container)
        outer.setContentsMargins(24, 24, 24, 24)
        outer.setSpacing(8)

        name = QLabel(job.name if job is not None else "Untitled", container)
        name.setObjectName("jobTitle")
        name.setAlignment(Qt.AlignmentFlag.AlignCenter)

        units = QLabel(
            f"{unit_count} unit{'s' if unit_count != 1 else ''}",
            container,
        )
        units.setObjectName("jobMeta")
        units.setAlignment(Qt.AlignmentFlag.AlignCenter)

        note = QLabel(
            "The visual flowsheet editor is not available yet.\n"
            "This placeholder will be replaced by the flowsheet canvas.",
            container,
        )
        note.setObjectName("jobNote")
        note.setAlignment(Qt.AlignmentFlag.AlignCenter)
        note.setWordWrap(True)

        outer.addStretch(1)
        outer.addWidget(name)
        outer.addWidget(units)
        outer.addWidget(note)
        outer.addStretch(1)

        self._layout.addWidget(container)
