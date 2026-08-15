"""Smoke test for the ModSIM PySide6 MDI application shell.

Runs headless with ``QT_QPA_PLATFORM=offscreen`` so no display is required.
Verifies the main window structure (menu bar, MDI area, status bar) and that
opening a real job directory creates an MDI sub-window.
"""

import os
import sys
import tempfile
import unittest
from unittest import mock

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

# Allow running directly: python test_gui_shell.py
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from PySide6.QtCore import Qt  # noqa: E402
from PySide6.QtWidgets import QApplication, QMdiArea, QMdiSubWindow  # noqa: E402

from modsim.engine.engine_bridge import EngineLoadError  # noqa: E402
from modsim.gui.main_window import (  # noqa: E402
    SIMULATION_ERROR,
    SIMULATION_READY,
    MainWindow,
)

REPO_ROOT = os.path.dirname(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
)
JOBS_DIR = os.path.join(REPO_ROOT, "Modsim", "Jobs")
BOUGAINVILLE = os.path.join(JOBS_DIR, "Distribution jobs")


class GuiShellTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.app = QApplication.instance() or QApplication([])

    def setUp(self):
        self.window = MainWindow()

    def tearDown(self):
        self.window.close()

    def test_has_menus(self):
        menu_bar = self.window.menuBar()
        titles = [a.text() for a in menu_bar.actions()]
        self.assertIn("&File", titles)
        self.assertIn("&Window", titles)
        self.assertIn("&Help", titles)

    def test_has_mdi_area(self):
        self.assertIsInstance(self.window.centralWidget(), QMdiArea)

    def test_has_statusbar(self):
        self.assertIsNotNone(self.window.statusBar())
        self.assertIsNotNone(getattr(self.window, "_job_label", None))
        self.assertIsNotNone(getattr(self.window, "_state_label", None))

    def test_open_job_creates_subwindow(self):
        self.assertTrue(os.path.isdir(BOUGAINVILLE), "Bougainville job should exist")
        job = self.window.open_job(directory=BOUGAINVILLE, name="Bougainville")
        self.assertIsNotNone(job)
        self.assertEqual(job.name, "Bougainville")
        subs = self.window._mdi.subWindowList()
        self.assertEqual(len(subs), 1)
        self.assertIsInstance(subs[0], QMdiSubWindow)
        self.assertEqual(subs[0].windowTitle(), "Bougainville")
        # Status bar reflects the loaded job.
        self.assertEqual(self.window._job_label.text(), "Job: Bougainville")

    def test_save_job_writes_files(self):
        job = self.window.open_job(directory=BOUGAINVILLE, name="Bougainville")
        self.assertIsNotNone(job)
        with tempfile.TemporaryDirectory() as tmp:
            self.window._job_path = tmp
            self.assertTrue(self.window.save_job())
            self.assertTrue(os.path.isfile(os.path.join(tmp, "Bougainville.JOB")))
            self.assertTrue(os.path.isfile(os.path.join(tmp, "Bougainville.syd")))

    def test_simulation_state_updates(self):
        self.window.set_simulation_state("Running")
        self.assertIn("Running", self.window._state_label.text())
        self.window.set_simulation_state("Complete")
        self.assertIn("Complete", self.window._state_label.text())

    def test_run_simulation_no_job_is_graceful(self):
        # No job open: run_simulation must show an info box and return without
        # crashing, leaving the state at READY.
        with mock.patch(
            "modsim.gui.main_window.QMessageBox.information"
        ) as info:
            self.window.run_simulation()
            info.assert_called_once()
        self.assertEqual(self.window.simulation_state, SIMULATION_READY)

    def test_run_simulation_engine_not_found_sets_error(self):
        # A job is open with a saved path, but the engine cannot be loaded.
        # The bridge raises EngineLoadError; run_simulation must set the state
        # to ERROR rather than crashing.
        job = self.window.open_job(directory=BOUGAINVILLE, name="Bougainville")
        self.assertIsNotNone(job)
        with tempfile.TemporaryDirectory() as tmp:
            self.window._job_path = tmp
            with mock.patch(
                "modsim.gui.main_window.ModsimEngine",
                side_effect=EngineLoadError("no engine"),
            ), mock.patch(
                "modsim.gui.main_window.QMessageBox.critical"
            ):
                self.window.run_simulation()
        self.assertEqual(self.window.simulation_state, SIMULATION_ERROR)


if __name__ == "__main__":
    unittest.main()
