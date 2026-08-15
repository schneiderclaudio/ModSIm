"""Smoke tests for the schema-driven equipment parameter dialogs.

Covers the three core capabilities:
1. A dialog is generated from a schema with the right number of labeled inputs.
2. Validation rejects out-of-bounds values and accepts in-bounds values.
3. A dialog can load values from a real job's ``.cur`` data (Bougainville HFSU).

Run with the offscreen Qt platform so no display is required::

    QT_QPA_PLATFORM=offscreen python test_equipment_dialogs.py
"""

import os
import sys
import unittest

# Allow running directly: python test_equipment_dialogs.py
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtWidgets import QApplication, QDialog  # noqa: E402

from modsim.gui.dialogs import (  # noqa: E402
    EquipmentDialog,
    generate_dialog,
    get_schema,
    list_schemas,
    load_values_from_job,
    validate_value,
    validate_values,
    write_values_to_job,
)
from modsim.gui.dialogs.schema import FieldType  # noqa: E402
from modsim.io.readers import read_job_directory  # noqa: E402

REPO_ROOT = os.path.dirname(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
)
JOBS_DIR = os.path.join(REPO_ROOT, "Modsim", "Jobs")
BOUGAINVILLE = os.path.join(JOBS_DIR, "Distribution jobs")


class EquipmentDialogsTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.app = QApplication.instance() or QApplication([])

    # -- 1. Data-driven form generation -------------------------------------

    def test_dialog_generated_from_schema(self):
        schema = get_schema("CYCA")
        dialog = generate_dialog(schema)
        self.assertIsInstance(dialog, EquipmentDialog)
        # One labeled input per schema field.
        self.assertEqual(len(dialog._inputs), len(schema.fields))
        self.assertEqual(len(dialog._inputs), 5)
        # Every field key is present.
        self.assertEqual(set(dialog._inputs), set(schema.field_keys()))
        dialog.close()

    def test_schema_registry_has_expected_models(self):
        codes = set(list_schemas())
        for expected in ("HFSU", "CONV", "MIXR", "CYCA", "CRSH", "JAW1",
                         "GYRA", "SAGM", "SAGT", "FLTN", "FLTK", "SPLT",
                         "DMCY", "KYNC", "CONE"):
            self.assertIn(expected, codes, f"missing schema for {expected}")
        self.assertGreaterEqual(len(codes), 15)

    def test_choice_field_requires_choices(self):
        from modsim.gui.dialogs.schema import Field
        with self.assertRaises(ValueError):
            Field(key="x", label="X", type=FieldType.CHOICE, choices=None)

    # -- 2. Validation ------------------------------------------------------

    def test_validation_rejects_out_of_bounds(self):
        schema = get_schema("CYCA")
        # diameter bound is 0.01..2.0 m; 5.0 is out of bounds.
        ok, msg = validate_value(schema.fields[0], 5.0)
        self.assertFalse(ok)
        self.assertIsNotNone(msg)
        # in-bounds value passes.
        ok, msg = validate_value(schema.fields[0], 0.5)
        self.assertTrue(ok)

    def test_validation_rejects_non_numeric(self):
        schema = get_schema("CYCA")
        ok, msg = validate_value(schema.fields[0], "not-a-number")
        self.assertFalse(ok)

    def test_validate_values_collects_errors(self):
        schema = get_schema("CYCA")
        errors = validate_values(schema, {"diameter": 99.0, "apex_diameter": 0.1})
        self.assertIn("diameter", errors)
        self.assertNotIn("apex_diameter", errors)

    def test_dialog_flags_out_of_bounds_and_accepts_in_bounds(self):
        schema = get_schema("CYCA")
        dialog = generate_dialog(schema)
        # Out of bounds -> not accepted, error shown.
        dialog._inputs["diameter"].setText("99.0")
        dialog._on_accept()
        self.assertNotEqual(dialog.result(), QDialog.Accepted)
        self.assertFalse(dialog._error_label.isHidden())
        # Corrected -> accepted.
        dialog._inputs["diameter"].setText("0.5")
        dialog._on_accept()
        self.assertEqual(dialog.result(), QDialog.Accepted)
        dialog.close()

    # -- 3. Job .cur wiring -------------------------------------------------

    def test_load_values_from_real_job(self):
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        # Unit 1 is the HFSU model with 22 parameters.
        unit = job.cur.units[0]
        self.assertEqual(unit.model, "HFSU")
        values = load_values_from_job(job, unit.number)
        schema = get_schema("HFSU")
        self.assertEqual(set(values), set(schema.field_keys()))
        self.assertEqual(len(values), 22)
        # Values are parsed to floats.
        self.assertIsInstance(values["screen_width"], float)
        self.assertAlmostEqual(values["screen_width"], 0.62, places=4)

    def test_write_values_to_job_roundtrip(self):
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        unit = job.cur.units[0]
        values = load_values_from_job(job, unit.number)
        # Edit one value and write back.
        values["screen_width"] = 0.75
        write_values_to_job(job, unit.number, values)
        # Reload and confirm the edit persisted in the model.
        reloaded = load_values_from_job(job, unit.number)
        self.assertAlmostEqual(reloaded["screen_width"], 0.75, places=4)
        # The .cur raw_lines are cleared so the edit is persisted on write.
        self.assertIsNone(job.cur.raw_lines)


if __name__ == "__main__":
    unittest.main()
