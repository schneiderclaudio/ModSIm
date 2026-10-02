"""Smoke tests for the schema-driven equipment parameter dialogs.

Covers the three core capabilities:
1. A dialog is generated from a schema with the right number of labeled inputs.
2. Validation rejects out-of-bounds values and accepts in-bounds values.
3. A dialog can load values from a real job's ``.cur`` data (Bougainville HFSU),
   and write them back honouring the engine's ``NoPARAM`` parameter count.

Schema data-quality guarantees are also checked: every numeric field has a
non-``None`` default and every default lies within its own ``[min, max]``.

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
from modsim.gui.dialogs.schema import SCHEMAS, FieldType  # noqa: E402
from modsim.io.readers import read_job_directory  # noqa: E402
from modsim.models.job import CurFile, CurUnit, Job  # noqa: E402

REPO_ROOT = os.path.dirname(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
)
JOBS_DIR = os.path.join(REPO_ROOT, "Modsim", "Jobs")
BOUGAINVILLE = os.path.join(JOBS_DIR, "Distribution jobs")


def make_job_with_unit(
    model: str, noparam: int, params, unit_number: int = 1, unit_id: int = 1
) -> Job:
    """Build an in-memory job whose ``.cur`` holds one unit."""
    unit = CurUnit(
        number=unit_number,
        model=model,
        noparam=noparam,
        unit_id=unit_id,
        params=list(params),
    )
    job = Job()
    job.cur = CurFile(name="cur", units=[unit], raw_lines=None)
    return job


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

    def test_all_job_model_codes_have_schemas(self):
        """Every model used by a shipped job must have a schema.

        A unit whose model code has no schema renders as a generic
        ``Unit N`` label and cannot open a parameter dialog.  Scan the real
        job directories and fail on any code without a registered schema.
        """
        import re

        codes = set(list_schemas())
        missing: set = set()
        for root in ("Modsim", os.path.join("Modsim", "JobsRPK")):
            base = os.path.join(REPO_ROOT, root)
            if not os.path.isdir(base):
                continue
            for dirpath, _dirs, files in os.walk(base):
                for fname in files:
                    if not fname.lower().endswith(".cur"):
                        continue
                    with open(os.path.join(dirpath, fname), "r", encoding="ascii") as fh:
                        for line in fh:
                            m = re.match(r"\s*TYPE\s+\d+\s+(\S+)", line)
                            if m and m.group(1) not in codes:
                                missing.add(m.group(1))
        self.assertEqual(missing, set(),
                         f"job model codes without a schema: {sorted(missing)}")

    def test_legacy_screen_schema_fields(self):
        """A legacy screen model (SCRN) gains a named, editable schema."""
        schema = get_schema("SCRN")
        self.assertEqual(schema.name, "Screen")
        self.assertIn("Mesh size", (f.label for f in schema.fields))
        self.assertGreaterEqual(len(schema.fields), 6)
        # Every numeric field has a usable default (dialog never floats None).
        for f in schema.fields:
            self.assertIsNotNone(f.default)
            self.assertIsNone(f.min)
            self.assertIsNone(f.max)

    def test_edit_legacy_screen_unit(self):
        """A job unit with a legacy model code can be loaded and saved back."""
        job = make_job_with_unit(
            "SCRN",
            noparam=7,
            params=["1.0000E+0", "5.0000E-1", "2.0000E-3", "1.5000E+0",
                    "1.0000E+0", "9.5000E-1", "1.0000E+0"],
        )
        values = load_values_from_job(job, 1)
        self.assertEqual(len(values), 7)
        values["p1"] = 12.0
        write_values_to_job(job, 1, values)
        unit = job.cur.units[0]
        self.assertEqual(len(unit.params), unit.noparam)
        self.assertAlmostEqual(float(unit.params[0]), 12.0)

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
        # Unit 1 is the HFSU model with 22 parameters (NoPARAM column).
        unit = job.cur.units[0]
        self.assertEqual(unit.model, "HFSU")
        self.assertEqual(unit.noparam, 22)
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

    # -- 4. Schema data quality (A6/A7) --------------------------------------

    def test_no_none_defaults_for_numeric_fields(self):
        """No FLOAT/INT field in any schema may have a None default."""
        bad = []
        for schema in SCHEMAS.values():
            for f in schema.fields:
                if f.type in (FieldType.FLOAT, FieldType.INT) and f.default is None:
                    bad.append((schema.code, f.key))
        self.assertEqual(bad, [], f"numeric fields with default=None: {bad}")

    def test_defaults_within_bounds(self):
        """Every numeric default must lie within its own [min, max] bounds."""
        bad = []
        for schema in SCHEMAS.values():
            for f in schema.fields:
                if f.type not in (FieldType.FLOAT, FieldType.INT):
                    continue
                if f.default is None or f.min is None or f.max is None:
                    continue
                if not (f.min <= f.default <= f.max):
                    bad.append(
                        (schema.code, f.key, f.default, f.min, f.max)
                    )
        self.assertEqual(bad, [], f"defaults outside [min, max]: {bad}")

    def test_gmsu_choice_flags_are_int_fields(self):
        """The 13 GMSU option/type flags must be INT fields with default 0."""
        schema = get_schema("GMSU")
        keys = {
            "trunnion_bearing", "lubrication_type", "cooling_type",
            "drive_type", "gear_type", "shell_material", "liner_material",
            "ball_material", "feed_type", "discharge_type", "control_mode",
            "alarm_level", "shutdown_level",
        }
        by_key = {f.key: f for f in schema.fields}
        for key in keys:
            f = by_key[key]
            self.assertIs(f.type, FieldType.INT, f"{key} should be INT")
            self.assertEqual(f.default, 0)
            self.assertIsNotNone(f.min)
            self.assertIsNotNone(f.max)
            self.assertTrue(f.min <= f.default <= f.max)

    # -- 5. NoPARAM-authoritative read/write (A5) -----------------------------

    def test_load_values_maps_only_first_noparam_fields(self):
        """Fields at index >= noparam fall back to their schema default."""
        schema = get_schema("GMIL")
        noparam = 5
        params = ["1.0000E+0", "2.0000E+0", "3.0000E-1", "4.0000E+0", "5.0000E+1"]
        job = make_job_with_unit("GMIL", noparam=noparam, params=params)
        values = load_values_from_job(job, 1)
        self.assertEqual(len(values), len(schema.fields))
        # First noparam fields map positionally onto unit.params.
        self.assertAlmostEqual(values["diameter"], 1.0)
        self.assertAlmostEqual(values["ball_charge"], 0.3)
        self.assertAlmostEqual(values["power"], 50.0)
        # Fields at index >= noparam use the schema default.
        self.assertEqual(values["speed"], schema.fields[5].default)
        self.assertEqual(values["critical_speed"], schema.fields[6].default)
        self.assertEqual(values["mill_filling"], schema.fields[14].default)

    def test_write_values_pads_to_noparam(self):
        """Writing must pad the schema fields out to exactly NoPARAM values."""
        job = read_job_directory(JOBS_DIR, name="fosfertil793")
        unit = job.cur.units[1]
        self.assertEqual(unit.model, "GMSU")
        self.assertEqual(unit.noparam, 43)
        schema = get_schema("GMSU")
        self.assertLess(len(schema.fields), unit.noparam)
        values = load_values_from_job(job, unit.number)
        write_values_to_job(job, unit.number, values)
        # noparam stays authoritative and unchanged.
        self.assertEqual(unit.noparam, 43)
        self.assertEqual(len(unit.params), unit.noparam)
        # The missing trailing parameters are zero-padded in engine E-notation.
        self.assertEqual(unit.params[40:], ["0.0000E+0"] * 3)
        # A subsequent load still works and covers every schema field.
        reloaded = load_values_from_job(job, unit.number)
        self.assertEqual(set(reloaded), set(schema.field_keys()))

    def test_write_values_truncates_to_noparam(self):
        """Writing must truncate when the schema has more fields than NoPARAM."""
        schema = get_schema("GMIL")
        noparam = 5
        params = ["1.0000E+0"] * noparam
        job = make_job_with_unit("GMIL", noparam=noparam, params=params)
        values = {f.key: f.default for f in schema.fields}
        write_values_to_job(job, 1, values)
        unit = job.cur.units[0]
        self.assertEqual(unit.noparam, noparam)
        self.assertEqual(len(unit.params), noparam)
        # Reloading keeps a value for every schema field.
        reloaded = load_values_from_job(job, 1)
        self.assertEqual(set(reloaded), set(schema.field_keys()))

    def test_write_values_to_real_gmsu_job_does_not_raise(self):
        """Writing the GMSU unit from a real job must not raise (A7 crash)."""
        job = read_job_directory(JOBS_DIR, name="fosfertil793")
        unit = job.cur.units[1]
        self.assertEqual(unit.model, "GMSU")
        values = load_values_from_job(job, unit.number)
        # A7: previously this hit float(None) -> TypeError on the 13 INT flags.
        write_values_to_job(job, unit.number, values)
        self.assertEqual(len(unit.params), unit.noparam)
        self.assertEqual(unit.noparam, 43)


if __name__ == "__main__":
    unittest.main()
