"""Application entry point for the ModSIM PySide6 GUI.

:func:`run` creates a :class:`QApplication`, shows the main window, and runs
the event loop.  This is imported by :mod:`modsim.__main__` and exposed as the
``modsim`` console script.
"""

from __future__ import annotations

import sys
from typing import Optional, Sequence

from PySide6.QtWidgets import QApplication

from .main_window import MainWindow


def run(argv: Optional[Sequence[str]] = None) -> int:
    """Create the application, show the main window, and run the event loop.

    ``argv`` is the list of command-line arguments (without the program name).
    Returns the application exit code.
    """
    app = QApplication(list(argv) if argv is not None else sys.argv)
    app.setApplicationName("ModSIM")
    app.setOrganizationName("ModSIM")

    window = MainWindow()
    window.show()
    return app.exec()
