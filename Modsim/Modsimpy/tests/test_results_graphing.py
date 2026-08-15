"""Smoke tests for the ModSIM results graphing module.

These tests verify that:

1. :func:`modsim.gui.plotting.parse_results` parses real engine result files
   from a job directory into structured data (size distributions, liberation
   spectra, stream data).
2. Each plot builder returns a valid ``QWidget`` containing a Qt Charts chart
   when run with ``QT_QPA_PLATFORM=offscreen``.

The tests use the real ``Modsim/Jobs`` directory shipped with the repository,
so they exercise the actual engine output formats.
"""

import os
import sys
import unittest

# Force offscreen rendering before any Qt widget is created.
os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from modsim.gui.plotting import (  # noqa: E402
    ResultsViewer,
    liberation_spectra_plot,
    parse_results,
    size_distribution_plot,
    stream_plot,
)

#: The real job directory shipped with the repository.
JOBS_DIR = os.path.normpath(
    os.path.join(os.path.dirname(__file__), "..", "..", "..", "Modsim", "Jobs")
)

#: Whether the real job directory is present (skips tests gracefully if not).
JOBS_AVAILABLE = os.path.isdir(JOBS_DIR)


def _app():
    """Return the shared QApplication, creating it if needed."""
    from PySide6.QtWidgets import QApplication

    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


class ParseResultsTest(unittest.TestCase):
    """Parsing real engine result files must yield structured data."""

    @unittest.skipUnless(JOBS_AVAILABLE, f"jobs directory not found: {JOBS_DIR}")
    def test_parse_real_job_directory(self):
        results = parse_results(JOBS_DIR)
        self.assertIsInstance(results.size_distributions, list)
        self.assertIsInstance(results.liberation_spectra, list)
        self.assertIsInstance(results.streams, list)
        # The Bougainville job writes STREAMPROPS.TXT with many streams.
        self.assertGreater(len(results.size_distributions), 0)
        self.assertGreater(len(results.streams), 0)
        # Every size distribution must have parallel, non-empty data.
        for sd in results.size_distributions:
            self.assertEqual(len(sd.sizes), len(sd.cum_passing))
            self.assertGreater(len(sd.sizes), 0)
            self.assertTrue(all(s > 0 for s in sd.sizes))

    @unittest.skipUnless(JOBS_AVAILABLE, f"jobs directory not found: {JOBS_DIR}")
    def test_parse_liberation_spectra(self):
        results = parse_results(JOBS_DIR)
        # LIBDISPM.DAT is present in the jobs directory.
        self.assertGreater(len(results.liberation_spectra), 0)
        for ls in results.liberation_spectra:
            self.assertEqual(len(ls.classes), len(ls.values))
            self.assertIn(ls.kind, ("unconditional", "conditional"))

    def test_parse_missing_directory_returns_empty(self):
        results = parse_results(os.path.join("does", "not", "exist"))
        self.assertEqual(results.size_distributions, [])
        self.assertEqual(results.liberation_spectra, [])
        self.assertEqual(results.streams, [])


class PlotWidgetTest(unittest.TestCase):
    """Each plot builder must return a valid QWidget with a chart."""

    @classmethod
    def setUpClass(cls):
        _app()
        cls.results = parse_results(JOBS_DIR) if JOBS_AVAILABLE else None

    @unittest.skipUnless(JOBS_AVAILABLE, f"jobs directory not found: {JOBS_DIR}")
    def test_size_distribution_plot(self):
        from PySide6.QtCharts import QChartView
        from PySide6.QtWidgets import QWidget

        sd = self.results.size_distributions[0]
        widget = size_distribution_plot(sd)
        self.assertIsInstance(widget, QWidget)
        self.assertTrue(widget.findChild(QChartView) is not None)

    @unittest.skipUnless(JOBS_AVAILABLE, f"jobs directory not found: {JOBS_DIR}")
    def test_liberation_spectra_plot(self):
        from PySide6.QtCharts import QChartView
        from PySide6.QtWidgets import QWidget

        specs = self.results.liberation_spectra
        widget = liberation_spectra_plot(specs)
        self.assertIsInstance(widget, QWidget)
        self.assertTrue(widget.findChild(QChartView) is not None)

    @unittest.skipUnless(JOBS_AVAILABLE, f"jobs directory not found: {JOBS_DIR}")
    def test_stream_plot(self):
        from PySide6.QtWidgets import QWidget

        widget = stream_plot(self.results)
        self.assertIsInstance(widget, QWidget)

    @unittest.skipUnless(JOBS_AVAILABLE, f"jobs directory not found: {JOBS_DIR}")
    def test_results_viewer(self):
        from PySide6.QtWidgets import QTabWidget, QWidget

        viewer = ResultsViewer(self.results)
        self.assertIsInstance(viewer.widget, QWidget)
        self.assertIsInstance(viewer.widget, QTabWidget)
        self.assertGreater(viewer.widget.count(), 0)


if __name__ == "__main__":
    unittest.main(verbosity=2)
