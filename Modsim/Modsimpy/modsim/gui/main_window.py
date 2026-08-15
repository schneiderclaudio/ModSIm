"""Main window for the ModSIM MDI application shell.

This is a PySide6 ``QMainWindow`` hosting a ``QMdiArea`` as its central
widget.  It provides the File / Window / Help menus, a toolbar with common
actions, and a status bar that reports the current job and simulation state.

Open and Save are wired to the job-file-I/O module (:mod:`modsim.io.readers`
and :mod:`modsim.io.writers`).
"""

from __future__ import annotations

import os
from typing import Optional

from PySide6.QtCore import Qt
from PySide6.QtGui import QAction, QKeySequence
from PySide6.QtWidgets import (
    QFileDialog,
    QInputDialog,
    QLabel,
    QMainWindow,
    QMdiArea,
    QMdiSubWindow,
    QMessageBox,
    QToolBar,
    QWidget,
)

from ..engine.engine_bridge import EngineLoadError, ModsimEngine
from ..io.readers import read_job_directory
from ..io.writers import write_datt_dat, write_job_directory, write_tears_out
from ..models.job import Job
from .flowsheet import FlowsheetWindow
from .canvas.flowsheet_window import FlowsheetCanvasWindow
from .plotting.parser import parse_results
from .plotting.plots import ResultsViewer

# Simulation states reported in the status bar.
SIMULATION_READY = "Ready"
SIMULATION_RUNNING = "Running"
SIMULATION_COMPLETE = "Complete"
SIMULATION_ERROR = "Error"


class MainWindow(QMainWindow):
    """MDI application shell for ModSIM."""

    def __init__(self, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.setWindowTitle("ModSIM")
        self.resize(1200, 800)

        # Current job state.
        self._job: Optional[Job] = None
        self._job_path: Optional[str] = None
        self._simulation_state: str = SIMULATION_READY

        self._mdi = QMdiArea()
        self._mdi.setViewMode(QMdiArea.ViewMode.SubWindowView)
        self.setCentralWidget(self._mdi)

        self._build_actions()
        self._build_menus()
        self._build_toolbar()
        self._build_statusbar()
        self._apply_style()

    # ------------------------------------------------------------------
    # Construction
    # ------------------------------------------------------------------
    def _build_actions(self) -> None:
        self.act_new = QAction("&New", self)
        self.act_new.setShortcut(QKeySequence.StandardKey.New)
        self.act_new.setStatusTip("Create a new empty job")
        self.act_new.triggered.connect(self.new_job)

        self.act_open = QAction("&Open...", self)
        self.act_open.setShortcut(QKeySequence.StandardKey.Open)
        self.act_open.setStatusTip("Open a job directory")
        # `triggered` emits a bool (the checked state); drop it so `open_job`'s
        # first positional parameter stays `directory` rather than a bool.
        self.act_open.triggered.connect(lambda checked=False: self.open_job())

        self.act_save = QAction("&Save", self)
        self.act_save.setShortcut(QKeySequence.StandardKey.Save)
        self.act_save.setStatusTip("Save the current job")
        self.act_save.triggered.connect(self.save_job)

        self.act_save_as = QAction("Save &As...", self)
        self.act_save_as.setShortcut(QKeySequence.StandardKey.SaveAs)
        self.act_save_as.setStatusTip("Save the current job to a new directory")
        self.act_save_as.triggered.connect(self.save_job_as)

        self.act_exit = QAction("E&xit", self)
        self.act_exit.setShortcut(QKeySequence.StandardKey.Quit)
        self.act_exit.setStatusTip("Exit ModSIM")
        self.act_exit.triggered.connect(self.close)

        self.act_run = QAction("&Run", self)
        self.act_run.setStatusTip("Run the simulation")
        self.act_run.triggered.connect(self.run_simulation)

        self.act_cascade = QAction("&Cascade", self)
        self.act_cascade.setStatusTip("Arrange sub-windows in a cascade")
        self.act_cascade.triggered.connect(self._mdi.cascadeSubWindows)

        self.act_tile = QAction("&Tile", self)
        self.act_tile.setStatusTip("Tile the sub-windows")
        self.act_tile.triggered.connect(self._mdi.tileSubWindows)

        self.act_arrange = QAction("&Arrange Icons", self)
        self.act_arrange.setStatusTip("Arrange the minimized sub-window icons")
        self.act_arrange.triggered.connect(self._arrange_icons)

        self.act_about = QAction("&About ModSIM", self)
        self.act_about.setStatusTip("About ModSIM")
        self.act_about.triggered.connect(self._show_about)

    def _build_menus(self) -> None:
        menu_bar = self.menuBar()

        file_menu = menu_bar.addMenu("&File")
        file_menu.addAction(self.act_new)
        file_menu.addAction(self.act_open)
        file_menu.addAction(self.act_save)
        file_menu.addAction(self.act_save_as)
        file_menu.addSeparator()
        file_menu.addAction(self.act_exit)

        window_menu = menu_bar.addMenu("&Window")
        window_menu.addAction(self.act_cascade)
        window_menu.addAction(self.act_tile)
        window_menu.addAction(self.act_arrange)

        help_menu = menu_bar.addMenu("&Help")
        help_menu.addAction(self.act_about)

    def _build_toolbar(self) -> None:
        toolbar = QToolBar("Main Toolbar", self)
        toolbar.setMovable(False)
        toolbar.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
        toolbar.addAction(self.act_open)
        toolbar.addAction(self.act_save)
        toolbar.addSeparator()
        toolbar.addAction(self.act_run)
        self.addToolBar(toolbar)

    def _build_statusbar(self) -> None:
        status = self.statusBar()
        status.setSizeGripEnabled(True)

        self._job_label = QLabel("No job open", self)
        self._job_label.setObjectName("statusJob")
        self._state_label = QLabel(f"Simulation: {self._simulation_state}", self)
        self._state_label.setObjectName("statusState")

        status.addWidget(self._job_label, 1)
        status.addPermanentWidget(self._state_label)

    def _apply_style(self) -> None:
        self.setStyleSheet(
            """
            QMainWindow { background: #2b2b2b; }
            QMenuBar { background: #333333; color: #e6e6e6; }
            QMenuBar::item:selected { background: #4a4a4a; }
            QMenu { background: #333333; color: #e6e6e6; }
            QMenu::item:selected { background: #4a4a4a; }
            QToolBar { background: #333333; border: none; padding: 4px; spacing: 4px; }
            QToolButton { background: transparent; color: #e6e6e6; padding: 6px 10px; border-radius: 4px; }
            QToolButton:hover { background: #4a4a4a; }
            QStatusBar { background: #2b2b2b; color: #b8b8b8; }
            QStatusBar QLabel { color: #b8b8b8; padding: 0 6px; }
            QMdiArea { background: #232323; border: none; }
            QMdiSubWindow { background: #3a3a3a; }
            #placeholderCard { background: #3a3a3a; border: 1px solid #4a4a4a; border-radius: 8px; }
            #jobTitle { color: #ffffff; font-size: 20px; font-weight: 600; }
            #jobMeta { color: #9cdcfe; font-size: 15px; }
            #jobNote { color: #8a8a8a; font-size: 12px; }
            #unitPalette { background: #2e2e2e; border-right: 1px solid #3a3a3a; }
            #paletteTitle { color: #9cdcfe; font-size: 12px; font-weight: 600; letter-spacing: 1px; }
            #paletteList { background: #2e2e2e; color: #d6d6d6; border: none; outline: none; }
            #paletteList::item { padding: 6px 10px; border-bottom: 1px solid #383838; }
            #paletteList::item:selected { background: #1f6feb; color: #ffffff; }
            #paletteList::item:hover { background: #3a3a3a; }
            #canvasHint { color: #8a8a8a; background: #2e2e2e; padding: 4px 10px; font-size: 11px; }
            """
        )

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------
    @property
    def job(self) -> Optional[Job]:
        """The currently loaded job, or ``None``."""
        return self._job

    @property
    def job_path(self) -> Optional[str]:
        """The directory the current job was loaded from / saved to."""
        return self._job_path

    @property
    def simulation_state(self) -> str:
        return self._simulation_state

    def set_simulation_state(self, state: str, message: str = "") -> None:
        """Update the simulation state shown in the status bar."""
        self._simulation_state = state
        text = f"Simulation: {state}"
        if message:
            text = f"{text} - {message}"
        self._state_label.setText(text)
        self.statusBar().showMessage(message, 5000)

    def _update_status_job(self) -> None:
        if self._job is not None:
            self._job_label.setText(f"Job: {self._job.name}")
        else:
            self._job_label.setText("No job open")

    # ------------------------------------------------------------------
    # File actions
    # ------------------------------------------------------------------
    def new_job(self) -> None:
        """Create a new empty job and open a flowsheet window for it."""
        job = Job(name="Untitled")
        self._job = job
        self._job_path = None
        self._open_flowsheet(job)
        self._update_status_job()
        self.set_simulation_state(SIMULATION_READY, "New job created")

    def open_job(self, directory: Optional[str] = None, name: Optional[str] = None) -> Optional[Job]:
        """Open a job directory into the shell.

        When ``directory`` is omitted a folder picker is shown.  If the
        directory contains more than one job, the user is asked to choose
        which one (pass ``name`` to skip that prompt).  Returns the loaded
        :class:`~modsim.models.job.Job`, or ``None`` if cancelled or on error.
        """
        if directory is None:
            directory = QFileDialog.getExistingDirectory(
                self, "Open Job Directory"
            )
            if not directory:
                return None

        if name is None:
            candidates = self._job_names_in(directory)
            if len(candidates) > 1:
                name = self._choose_job_name(candidates)
                if name is None:
                    return None
            elif len(candidates) == 1:
                name = candidates[0]

        try:
            job = read_job_directory(directory, name=name)
        except Exception as exc:  # noqa: BLE001 - surface any parse failure
            QMessageBox.critical(self, "Open Job", f"Could not open the job:\n{exc}")
            return None

        self._job = job
        self._job_path = directory
        self._open_flowsheet(job)
        self._update_status_job()
        self.set_simulation_state(SIMULATION_READY, f"Opened {job.name}")
        return job

    @staticmethod
    def _job_names_in(directory: str):
        """Return the job base names (from ``.JOB`` files) in ``directory``."""
        try:
            entries = os.listdir(directory)
        except OSError:
            return []
        return sorted(
            os.path.splitext(f)[0]
            for f in entries
            if f.lower().endswith(".job")
        )

    def _choose_job_name(self, candidates):
        """Ask the user which job to open when a directory has several."""
        choice, ok = QInputDialog.getItem(
            self,
            "Select Job",
            "This directory contains more than one job. Choose one:",
            candidates,
            0,
            False,
        )
        return choice if ok else None

    def save_job(self) -> bool:
        """Save the current job to its directory. Returns success."""
        if self._job is None:
            QMessageBox.information(self, "Save Job", "No job is open to save.")
            return False

        if self._job_path is None:
            return self.save_job_as()

        try:
            write_job_directory(self._job, self._job_path)
        except Exception as exc:  # noqa: BLE001
            QMessageBox.critical(self, "Save Job", f"Could not save the job:\n{exc}")
            return False

        self.set_simulation_state(SIMULATION_READY, f"Saved {self._job.name}")
        return True

    def save_job_as(self) -> bool:
        """Save the current job to a directory chosen by the user."""
        if self._job is None:
            QMessageBox.information(self, "Save Job", "No job is open to save.")
            return False

        directory = QFileDialog.getExistingDirectory(
            self, "Save Job Directory"
        )
        if not directory:
            return False

        try:
            write_job_directory(self._job, directory)
        except Exception as exc:  # noqa: BLE001
            QMessageBox.critical(self, "Save Job", f"Could not save the job:\n{exc}")
            return False

        self._job_path = directory
        self.set_simulation_state(SIMULATION_READY, f"Saved {self._job.name}")
        return True

    # ------------------------------------------------------------------
    # Simulation
    # ------------------------------------------------------------------
    def run_simulation(self) -> None:
        """Run the current job's simulation via the engine bridge.

        Requires an open job with a saved directory.  The job is persisted to
        its directory first so the engine reads the latest edits, then the
        engine's ``INORDCALC`` and ``SIMOP`` entry points are invoked.  On
        success the result files are parsed and shown in a new MDI sub-window.
        Any engine failure is surfaced without crashing the GUI.
        """
        if self._job is None:
            QMessageBox.information(self, "Run", "Open a job before running.")
            return

        if self._job_path is None:
            # A job is open but has never been saved; ask the user to save it
            # to a directory so the engine has somewhere to read/write files.
            if not self.save_job_as():
                return

        assert self._job_path is not None
        job_path = self._job_path

        # Persist the current job (including flowsheet layout) so the engine
        # reads the latest edits.
        try:
            write_job_directory(self._job, job_path)
            # The engine reads TEARS.OUT (a copy of the job's .TEA) during the
            # data-input phase whenever DATT.DAT contains a TEAR keyword.
            write_tears_out(self._job, job_path)
            # The engine reads DATT.DAT (the job's .sid system/plant data
            # concatenated with its .cur run data) during the data-input
            # phase; generate it so the engine does not use a stale copy.
            write_datt_dat(self._job, job_path)
        except Exception as exc:  # noqa: BLE001 - surface any save failure
            QMessageBox.critical(
                self, "Run", f"Could not save the job before running:\n{exc}"
            )
            self.set_simulation_state(SIMULATION_ERROR, "Save failed")
            return

        self.set_simulation_state(SIMULATION_RUNNING, "Simulation running...")

        try:
            engine = ModsimEngine()
            engine.inordcalc(job_path)
            engine.simop(job_path)
        except EngineLoadError as exc:
            self.set_simulation_state(SIMULATION_ERROR, "Engine not found")
            QMessageBox.critical(
                self,
                "Run",
                "Could not load the ModSIM engine.\n\n"
                f"{exc}\n\n"
                "Set the MODSIM environment variable to the directory "
                "containing ModsimMain.dll, or build the DLL first.",
            )
            return
        except Exception as exc:  # noqa: BLE001 - engine runtime may fail
            self.set_simulation_state(SIMULATION_ERROR, "Simulation failed")
            QMessageBox.critical(
                self,
                "Run",
                "The simulation failed.\n\n"
                f"{exc}\n\n"
                "This may be an engine-side limitation (e.g. missing IMSL "
                "or a numerical convergence failure).",
            )
            return

        # Parse the result files and open a results viewer.
        try:
            results = parse_results(job_path)
        except Exception as exc:  # noqa: BLE001 - parsing must not crash
            self.set_simulation_state(SIMULATION_ERROR, "Results parse failed")
            QMessageBox.critical(
                self, "Run", f"Could not parse the simulation results:\n{exc}"
            )
            return

        viewer = ResultsViewer(results).widget
        sub = QMdiSubWindow()
        sub.setWidget(viewer)
        sub.setWindowTitle(f"{self._job.name} - Results")
        sub.resize(800, 600)
        self._mdi.addSubWindow(sub)
        sub.show()

        self.set_simulation_state(SIMULATION_COMPLETE, "Simulation complete")

    # ------------------------------------------------------------------
    # MDI helpers
    # ------------------------------------------------------------------
    def _open_flowsheet(self, job: Job) -> QMdiSubWindow:
        """Host a flowsheet window for ``job`` in a new MDI sub-window."""
        flowsheet: FlowsheetWindow = self.create_flowsheet_window(job)
        sub = QMdiSubWindow()
        sub.setWidget(flowsheet)
        sub.setWindowTitle(flowsheet.display_title)
        sub.resize(640, 480)
        self._mdi.addSubWindow(sub)
        sub.show()
        return sub

    def create_flowsheet_window(self, job: Job) -> FlowsheetWindow:
        """Factory hook for creating the flowsheet content widget.

        Phase 5 replaces this with a real canvas window.  Override this method
        (in a subclass) to swap in a different implementation.
        """
        return FlowsheetCanvasWindow(job)

    def _show_about(self) -> None:
        QMessageBox.about(
            self,
            "About ModSIM",
            "ModSIM\n"
            "Mineral processing plant simulator.\n\n"
            "This is the PySide6 desktop shell.",
        )

    def _arrange_icons(self) -> None:
        """Arrange minimized sub-windows in a row at the bottom of the MDI area."""
        minimized = [
            s for s in self._mdi.subWindowList() if s.isMinimized()
        ]
        if not minimized:
            return
        width = 0
        for sub in minimized:
            sub.showNormal()
            sub.resize(200, 120)
            sub.move(width, self._mdi.viewport().height() - 140)
            width += sub.width() + 8

