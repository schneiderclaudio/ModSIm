"""Round-trip tests for the ModSIM legacy job-file I/O module.

Each test reads a real job directory, writes it to a temporary directory, and
verifies that every handled file is byte-identical to the original.  The
original fixture files are never modified.
"""

import filecmp
import os
import sys
import tempfile
import unittest

# Allow running directly: python test_job_io.py
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from modsim.io.readers import read_job_directory  # noqa: E402
from modsim.io.writers import (  # noqa: E402
    write_datt_dat,
    write_job_directory,
    write_tears_out,
)

# Extensions handled by the module (lower-cased).
HANDLED = {
    "job",
    "syd",
    "siz",
    "gcd",
    "scd",
    "mat",
    "mop",
    "tea",
    "sid",
    "cur",
    "trn",
}

REPO_ROOT = os.path.dirname(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
)
JOBS_DIR = os.path.join(REPO_ROOT, "Modsim", "Jobs")
JOBSRPK_DIR = os.path.join(REPO_ROOT, "Modsim", "JobsRPK")

# The Bougainville job files live directly in the "Distribution jobs" folder.
BOUGAINVILLE = os.path.join(JOBS_DIR, "Distribution jobs")
FAIRLANE = os.path.join(JOBSRPK_DIR, "Fairlane")


def _handled_files(job_dir, name):
    return [
        f
        for f in os.listdir(job_dir)
        if f.lower().startswith(name.lower() + ".")
        and f.lower().rsplit(".", 1)[-1] in HANDLED
    ]


class JobIORoundTripTest(unittest.TestCase):
    def _assert_roundtrip(self, src, name):
        with tempfile.TemporaryDirectory() as tmp:
            job = read_job_directory(src, name=name)
            self.assertTrue(job.name, "job name should be populated")
            write_job_directory(job, tmp)
            for fname in _handled_files(src, name):
                with self.subTest(file=fname):
                    self.assertTrue(
                        os.path.exists(os.path.join(tmp, fname)),
                        f"written file missing: {fname}",
                    )
                    self.assertTrue(
                        filecmp.cmp(
                            os.path.join(src, fname),
                            os.path.join(tmp, fname),
                            shallow=False,
                        ),
                        f"file not byte-identical: {fname}",
                    )

    def test_bougainville_roundtrip(self):
        self._assert_roundtrip(BOUGAINVILLE, "Bougainville")

    def test_fairlane_roundtrip(self):
        self._assert_roundtrip(FAIRLANE, "Fairlane")

    def test_job_name(self):
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        self.assertEqual(job.name, "Bougainville")
        self.assertIsNotNone(job.job)
        self.assertIsNotNone(job.syd)
        self.assertIsNotNone(job.siz)
        self.assertIsNotNone(job.gcd)
        self.assertIsNotNone(job.scd)
        self.assertIsNotNone(job.mat)
        self.assertIsNotNone(job.mop)
        self.assertIsNotNone(job.tea)
        self.assertIsNotNone(job.sid)
        self.assertIsNotNone(job.cur)
        self.assertIsNotNone(job.trn)

    def test_structured_parse(self):
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        # .syd units
        self.assertEqual(job.syd.unit_count, 9)
        self.assertEqual(len(job.syd.units), 9)
        self.assertEqual(job.syd.units[0].number, 1)
        self.assertEqual(job.syd.units[0].type, 7)
        self.assertEqual(job.syd.units[0].kind, "F")
        # .siz streams
        self.assertEqual(len(job.siz.streams), 8)
        feed = job.siz.streams[0]
        self.assertEqual(feed.kind, "feedrate")
        self.assertEqual(feed.feedrate, "1.2780E+2")
        self.assertEqual(len(feed.size_points), 0)
        size_stream = job.siz.streams[2]
        self.assertEqual(size_stream.kind, "size")
        self.assertEqual(len(size_stream.size_points), 9)
        # .cur units
        self.assertEqual(len(job.cur.units), 5)
        self.assertEqual(job.cur.units[0].model, "HFSU")
        # .JOB flags
        self.assertEqual(job.job.job_name, "Bougainville")
        self.assertTrue(any(f.key == "Flowsheet" and f.value for f in job.job.flags))

    def test_write_tears_out_matches_tea(self):
        """TEARS.OUT must be a byte-identical copy of the job's .TEA file."""
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        self.assertIsNotNone(job.tea)
        with tempfile.TemporaryDirectory() as tmp:
            write_tears_out(job, tmp)
            tears_path = os.path.join(tmp, "TEARS.OUT")
            self.assertTrue(
                os.path.exists(tears_path), "TEARS.OUT should be written"
            )
            self.assertTrue(
                filecmp.cmp(
                    os.path.join(BOUGAINVILLE, "Bougainville.TEA"),
                    tears_path,
                    shallow=False,
                ),
                "TEARS.OUT should be byte-identical to the .TEA file",
            )

    def test_write_tears_out_noop_without_tea(self):
        """A job without a .TEA file must not create TEARS.OUT."""
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        job.tea = None
        with tempfile.TemporaryDirectory() as tmp:
            write_tears_out(job, tmp)
            self.assertFalse(
                os.path.exists(os.path.join(tmp, "TEARS.OUT")),
                "TEARS.OUT should not be written when the job has no .TEA",
            )

    def test_write_datt_dat_matches_sid_plus_cur(self):
        """DATT.DAT must be the .sid (system/plant) + .cur (run) concatenation."""
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        assert job.sid is not None and job.sid.raw_lines is not None
        assert job.cur is not None and job.cur.raw_lines is not None
        expected = list(job.sid.raw_lines) + list(job.cur.raw_lines)
        with tempfile.TemporaryDirectory() as tmp:
            write_datt_dat(job, tmp)
            datt_path = os.path.join(tmp, "DATT.DAT")
            self.assertTrue(
                os.path.exists(datt_path), "DATT.DAT should be written"
            )
            with open(datt_path, "r", encoding="ascii", newline="") as fh:
                written = fh.read().splitlines()
            self.assertEqual(written, expected)
            # The generated DATT.DAT must carry the job's current unit
            # parameters (22 HFSU params), not a stale 15-param copy.
            self.assertIn("TYPE 001 HFSU 022 001", written)

    def test_write_datt_dat_noop_without_sid(self):
        """A job without a .sid file must not create DATT.DAT."""
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        job.sid = None
        with tempfile.TemporaryDirectory() as tmp:
            write_datt_dat(job, tmp)
            self.assertFalse(
                os.path.exists(os.path.join(tmp, "DATT.DAT")),
                "DATT.DAT should not be written when the job has no .sid",
            )

    def test_write_datt_dat_noop_without_cur(self):
        """A job without a .cur file must not create DATT.DAT."""
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        job.cur = None
        with tempfile.TemporaryDirectory() as tmp:
            write_datt_dat(job, tmp)
            self.assertFalse(
                os.path.exists(os.path.join(tmp, "DATT.DAT")),
                "DATT.DAT should not be written when the job has no .cur",
            )


if __name__ == "__main__":
    unittest.main()
