"""Smoke test for the ModSIM flowsheet canvas.

Runs headless with ``QT_QPA_PLATFORM=offscreen``.  Loads a real job
(Bougainville) into a :class:`FlowsheetCanvasWindow`, then verifies that:

1. units and streams are rendered on the scene,
2. adding a unit from the palette creates a ``UnitItem``,
3. moving a unit updates its position and connected streams follow,
4. creating a stream between two units creates a ``StreamItem``,
5. saving the layout to a temp directory and reloading restores it.

Original fixture files are never modified; all writes go to temp directories.
"""

import os
import sys
import tempfile
import unittest

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from PySide6.QtCore import QMimeData, QPointF, Qt  # noqa: E402
from PySide6.QtWidgets import QApplication  # noqa: E402

from modsim.gui.canvas.canvas import UNIT_MIME, FlowsheetScene  # noqa: E402
from modsim.gui.canvas.flowsheet_window import FlowsheetCanvasWindow  # noqa: E402
from modsim.gui.canvas.items import StreamItem, UnitItem  # noqa: E402
from modsim.gui.canvas.palette import UnitPalette  # noqa: E402
from modsim.io.readers import read_job_directory  # noqa: E402
from modsim.io.writers import write_job_directory  # noqa: E402

REPO_ROOT = os.path.dirname(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
)
BOUGAINVILLE = os.path.join(REPO_ROOT, "Modsim", "Jobs", "Distribution jobs")


class FlowsheetCanvasTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.app = QApplication.instance() or QApplication([])
        cls.job = read_job_directory(BOUGAINVILLE, name="Bougainville")

    def setUp(self):
        # A fresh window per test so tests are independent.
        self.window = FlowsheetCanvasWindow(self.job)
        self.scene: FlowsheetScene = self.window.canvas.scene

    def tearDown(self):
        self.window.close()

    # ------------------------------------------------------------------
    def test_units_and_streams_rendered(self):
        self.assertGreaterEqual(len(self.scene.units), 5)
        self.assertGreater(len(self.scene.streams), 0)
        for item in self.scene.units.values():
            self.assertIsInstance(item, UnitItem)
        for stream in self.scene.streams:
            self.assertIsInstance(stream, StreamItem)
            self.assertIsNotNone(stream.path())

    def test_unit_positions_loaded_from_trn(self):
        # Unit 4 is positioned in the Bougainville .TRN file.
        unit = self.scene.unit_by_number(4)
        self.assertIsNotNone(unit)
        self.assertGreater(unit.pos().x(), 0)
        self.assertGreater(unit.pos().y(), 0)

    def test_add_unit_from_palette_creates_unit_item(self):
        palette = UnitPalette()
        self.assertGreater(palette.list.count(), 0)

        # Simulate dragging a unit type from the palette onto the scene.
        mime = QMimeData()
        mime.setData(UNIT_MIME, b"JAW1")
        before = len(self.scene.units)
        self.scene.dropEvent(_FakeDropEvent(mime, QPointF(300, 300)))
        self.assertEqual(len(self.scene.units), before + 1)
        new_number = max(self.scene.units)
        new_unit = self.scene.unit_by_number(new_number)
        self.assertIsInstance(new_unit, UnitItem)
        self.assertEqual(new_unit.model_code, "JAW1")

    def test_move_unit_updates_position_and_streams_follow(self):
        unit = self.scene.unit_by_number(4)
        attached = [s for s in self.scene.streams if s.source is unit or s.target is unit]
        self.assertGreater(len(attached), 0)
        old_paths = [s.path() for s in attached]

        unit.setPos(950.0, 950.0)
        self.assertEqual(unit.pos().x(), 950.0)
        self.assertEqual(unit.pos().y(), 950.0)

        # Connected streams must have re-routed to the new position.
        for stream, old in zip(attached, old_paths):
            self.assertNotEqual(stream.path(), old)

    def test_create_stream_between_units(self):
        unit_a = self.scene.unit_by_number(1)
        unit_b = self.scene.unit_by_number(8)
        before = len(self.scene.streams)
        stream = self.scene.create_stream(unit_a, unit_b)
        self.assertIsInstance(stream, StreamItem)
        self.assertEqual(len(self.scene.streams), before + 1)
        self.assertIs(stream.source, unit_a)
        self.assertIs(stream.target, unit_b)

    def test_layout_persistence_round_trip(self):
        scene = self.scene

        # Modify the layout: move a unit, add a new unit, connect it.
        moved = scene.unit_by_number(4)
        moved.setPos(999.0, 777.0)

        new_unit = scene.add_unit(
            scene.next_unit_number(),
            label="Jaw Crusher",
            kind="O",
            model_code="JAW1",
            pos=(400.0, 400.0),
        )
        # Connect the new unit to a unit that currently has no input stream
        # (unit 1 has in_stream 0), so the connection is representable.
        target = scene.unit_by_number(1)
        new_stream = scene.create_stream(new_unit, target)
        self.assertIsNotNone(new_stream)

        # Persist the layout onto the job model and save to a temp directory.
        self.window.sync_layout_to_job()
        with tempfile.TemporaryDirectory() as tmp:
            write_job_directory(self.job, tmp)

            # Reload from the temp directory into a fresh window.
            reloaded_job = read_job_directory(tmp, name="Bougainville")
            reload_window = FlowsheetCanvasWindow(reloaded_job)
            reload_scene = reload_window.canvas.scene
            try:
                # Units restore, including the newly added one.
                self.assertIn(moved.number, reload_scene.units)
                self.assertIn(new_unit.number, reload_scene.units)

                # The moved unit's position is restored.
                reloaded_moved = reload_scene.unit_by_number(moved.number)
                self.assertAlmostEqual(reloaded_moved.pos().x(), 999.0, places=1)
                self.assertAlmostEqual(reloaded_moved.pos().y(), 777.0, places=1)

                # The new stream survives the round trip.
                edges = [
                    (s.source.number, s.target.number) for s in reload_scene.streams
                ]
                self.assertIn((new_unit.number, target.number), edges)
                self.assertGreater(len(reload_scene.streams), 0)
            finally:
                reload_window.close()


class _FakeDropEvent:
    """Minimal stand-in for a QGraphicsSceneDropEvent used by the smoke test."""

    def __init__(self, mime: QMimeData, scene_pos: QPointF) -> None:
        self._mime = mime
        self._pos = scene_pos

    def mimeData(self):
        return self._mime

    def scenePos(self):
        return self._pos

    def acceptProposedAction(self):
        pass


if __name__ == "__main__":
    unittest.main()
