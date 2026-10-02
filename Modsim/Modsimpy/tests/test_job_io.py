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

from modsim.io.readers import (  # noqa: E402
    read_cur,
    read_format_out,
    read_job_directory,
    read_scd,
    read_trn,
)
from modsim.io.writers import (  # noqa: E402
    reset_tears_out,
    write_currdat_syd,
    write_datt_dat,
    write_format_out,
    write_job_directory,
    write_liberation_files,
    write_repeat_out,
    write_simop_dat,
    write_tears_out,
    write_trn,
)
from modsim.models.job import FormatOutFile  # noqa: E402

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
    "lju",
    "amd",
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

    def test_write_tears_out_without_tea_writes_reset_record(self):
        """A job without a .TEA file must write the VB6 fallback record.

        The legacy VB6 GUI writes ``LAST 0 0 0 0`` to TEARS.OUT when no .TEA
        exists (MDIMod.frm:388-390) so the engine reads an explicit
        end-of-tear-data marker instead of an empty file.
        """
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        job.tea = None
        with tempfile.TemporaryDirectory() as tmp:
            write_tears_out(job, tmp)
            tears_path = os.path.join(tmp, "TEARS.OUT")
            self.assertTrue(
                os.path.exists(tears_path), "TEARS.OUT should be written"
            )
            with open(tears_path, "r", encoding="ascii", newline="") as fh:
                written = fh.read().splitlines()
            self.assertEqual(written, ["LAST 0 0 0 0"])

    def test_write_tears_out_without_raw_lines_writes_reset_record(self):
        """A .TEA without raw lines must write the VB6 fallback record."""
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        self.assertIsNotNone(job.tea)
        assert job.tea is not None
        job.tea.raw_lines = None
        with tempfile.TemporaryDirectory() as tmp:
            write_tears_out(job, tmp)
            with open(
                os.path.join(tmp, "TEARS.OUT"), "r", encoding="ascii", newline=""
            ) as fh:
                written = fh.read().splitlines()
            self.assertEqual(written, ["LAST 0 0 0 0"])

    def test_reset_tears_out_writes_reset_record(self):
        """reset_tears_out must write the VB6 tear-reset record."""
        with tempfile.TemporaryDirectory() as tmp:
            reset_tears_out(tmp)
            tears_path = os.path.join(tmp, "TEARS.OUT")
            self.assertTrue(
                os.path.exists(tears_path), "TEARS.OUT should be written"
            )
            with open(tears_path, "r", encoding="ascii", newline="") as fh:
                written = fh.read().splitlines()
            self.assertEqual(written, ["LAST 0 0 0 0"])

    def test_write_simop_dat_matches_mop(self):
        """SIMOP.DAT must be a byte-identical copy of the job's .mop file."""
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        self.assertIsNotNone(job.mop)
        assert job.mop is not None and job.mop.raw_lines is not None
        expected = list(job.mop.raw_lines)
        with tempfile.TemporaryDirectory() as tmp:
            write_simop_dat(job, tmp)
            simop_path = os.path.join(tmp, "SIMOP.DAT")
            self.assertTrue(
                os.path.exists(simop_path), "SIMOP.DAT should be written"
            )
            with open(simop_path, "r", encoding="ascii", newline="") as fh:
                written = fh.read().splitlines()
            self.assertEqual(written, expected)

    def test_write_simop_dat_uses_flag_string_without_raw_lines(self):
        """SIMOP.DAT must fall back to flag_string when .mop has no raw lines."""
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        assert job.mop is not None and job.mop.raw_lines is not None
        job.mop.raw_lines = None
        with tempfile.TemporaryDirectory() as tmp:
            write_simop_dat(job, tmp)
            with open(
                os.path.join(tmp, "SIMOP.DAT"),
                "r",
                encoding="ascii",
                newline="",
            ) as fh:
                written = fh.read().splitlines()
            self.assertEqual(written, [job.mop.flag_string])

    def test_write_simop_dat_noop_without_mop(self):
        """A job without a .mop file must not create SIMOP.DAT."""
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        job.mop = None
        with tempfile.TemporaryDirectory() as tmp:
            write_simop_dat(job, tmp)
            self.assertFalse(
                os.path.exists(os.path.join(tmp, "SIMOP.DAT")),
                "SIMOP.DAT should not be written when the job has no .mop",
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

    def test_write_datt_dat_regenerates_cur_after_edit(self):
        """DATT.DAT must regenerate the .cur portion after a parameter edit.

        An equipment edit clears ``CurFile.raw_lines`` so ``write_cur``
        regenerates from the structured data; ``write_datt_dat`` must not skip
        writing DATT.DAT in that case or the engine would read a stale copy.
        """
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        assert job.sid is not None and job.sid.raw_lines is not None
        assert job.cur is not None and job.cur.raw_lines is not None
        job.cur.raw_lines = None  # a parameter edit leaves no raw lines
        with tempfile.TemporaryDirectory() as tmp:
            write_datt_dat(job, tmp)
            datt_path = os.path.join(tmp, "DATT.DAT")
            self.assertTrue(
                os.path.exists(datt_path),
                "DATT.DAT must still be written after a parameter edit",
            )
            with open(datt_path, "r", encoding="ascii", newline="") as fh:
                written = fh.read().splitlines()
            # The regenerated .cur data carries the job's current parameters
            # (22 HFSU params) rather than an earlier raw copy.
            hfsu = [l for l in written if l.startswith("TYPE") and "HFSU" in l]
            self.assertEqual(
                hfsu,
                ["TYPE   1 HFSU  22   1"],
                "DATT.DAT must carry the job's current .cur parameter count",
            )


    def test_write_format_out_regenerates_and_roundtrips(self):
        """A regenerated FORMAT.OUT must round-trip via read_format_out."""
        fmt = FormatOutFile(
            solid_units=2,
            water_units=3,
            metal_units=2,
            show_water=True,
            show_pct_solids=True,
            show_yield=False,
            show_minerals=True,
            show_metals=False,
            coal_flag=True,
            num_metals=2,
            metal_names=["Cu", "Fe"],
            num_minerals=2,
            minmetal=[[1.5, 2.5], [3.0, 4.0]],
            size_flag=True,
            size_streams=[1, 3, 5],
            icode=2,
            accumulate_flag=True,
            accumulate_streams=[2, 4],
        )
        with tempfile.TemporaryDirectory() as tmp:
            path = os.path.join(tmp, "FORMAT.OUT")
            write_format_out(fmt, path)
            parsed = read_format_out(path)
        self.assertEqual(parsed.name, "format_out")
        self.assertEqual(parsed.solid_units, 2)
        self.assertEqual(parsed.water_units, 3)
        self.assertEqual(parsed.metal_units, 2)
        self.assertTrue(parsed.show_water)
        self.assertTrue(parsed.show_pct_solids)
        self.assertFalse(parsed.show_yield)
        self.assertTrue(parsed.show_minerals)
        self.assertFalse(parsed.show_metals)
        self.assertTrue(parsed.coal_flag)
        self.assertEqual(parsed.num_metals, 2)
        self.assertEqual(parsed.metal_names, ["Cu", "Fe"])
        self.assertEqual(parsed.num_minerals, 2)
        self.assertEqual(parsed.minmetal, [[1.5, 2.5], [3.0, 4.0]])
        self.assertTrue(parsed.size_flag)
        self.assertEqual(parsed.size_streams, [1, 3, 5])
        self.assertEqual(parsed.icode, 2)
        self.assertTrue(parsed.accumulate_flag)
        self.assertEqual(parsed.accumulate_streams, [2, 4])

    def test_write_format_out_writes_raw_lines_verbatim(self):
        """A FORMAT.OUT read from disk must be written back byte-identical."""
        raw = ["2T3TFTF2T", "2", "Cu      Fe      ", "2", "1.5000E+0 2.5000E+0", "F", "F"]
        fmt = FormatOutFile(raw_lines=raw)
        with tempfile.TemporaryDirectory() as tmp:
            path = os.path.join(tmp, "FORMAT.OUT")
            write_format_out(fmt, path)
            with open(path, "r", encoding="ascii", newline="") as fh:
                written = fh.read().splitlines()
        self.assertEqual(written, raw)

    def test_write_format_out_pads_short_minmetal(self):
        """minmetal rows shorter than num_metals must be padded with 0.0."""
        fmt = FormatOutFile(
            num_metals=3,
            metal_names=["Cu", "Fe", "Zn"],
            num_minerals=2,
            minmetal=[[1.0]],
        )
        with tempfile.TemporaryDirectory() as tmp:
            path = os.path.join(tmp, "FORMAT.OUT")
            write_format_out(fmt, path)
            parsed = read_format_out(path)
        self.assertEqual(parsed.minmetal, [[1.0, 0.0, 0.0], [0.0, 0.0, 0.0]])

    def test_write_format_out_no_metals_skips_mineral_rows(self):
        """num_metals == 0 must skip num_minerals and mineral rows entirely.

        The engine reads NOMIN and the mineral rows only inside IF(NOMET > 0)
        (SIMOP.FOR:332-340); with no metals the next line after the metal
        count is the size-distribution flag.  The regenerated file must
        follow that layout and round-trip through read_format_out.
        """
        fmt = FormatOutFile(
            num_metals=0,
            num_minerals=3,
            minmetal=[[1.0], [2.0], [3.0]],
            size_flag=True,
            size_streams=[1, 3, 5],
            icode=2,
            accumulate_flag=True,
            accumulate_streams=[9],
        )
        with tempfile.TemporaryDirectory() as tmp:
            path = os.path.join(tmp, "FORMAT.OUT")
            write_format_out(fmt, path)
            with open(path, "r", encoding="ascii", newline="") as fh:
                written = fh.read().splitlines()
            parsed = read_format_out(path)
        # flags, "0" (num_metals), then straight to size flag/count/streams/icode
        self.assertEqual(written[1:], ["0", "T", "3", "   1   3   5", "2", "T", "1", "   9"])
        self.assertEqual(parsed.num_metals, 0)
        self.assertEqual(parsed.metal_names, [])
        self.assertEqual(parsed.minmetal, [])
        self.assertTrue(parsed.size_flag)
        self.assertEqual(parsed.size_streams, [1, 3, 5])
        self.assertEqual(parsed.icode, 2)
        self.assertTrue(parsed.accumulate_flag)
        self.assertEqual(parsed.accumulate_streams, [9])

    def test_write_format_out_empty_size_streams_emits_f(self):
        """size_flag with an empty stream list must emit 'F' (engine reads
        no streams and no icode when NSTRM == 0, SIMOP.FOR:352-356)."""
        fmt = FormatOutFile(
            num_metals=1,
            metal_names=["Cu"],
            size_flag=True,
            size_streams=[],
            icode=2,
        )
        with tempfile.TemporaryDirectory() as tmp:
            path = os.path.join(tmp, "FORMAT.OUT")
            write_format_out(fmt, path)
            with open(path, "r", encoding="ascii", newline="") as fh:
                written = fh.read().splitlines()
            parsed = read_format_out(path)
        self.assertEqual(written[-2:], ["F", "F"])
        self.assertFalse(parsed.size_flag)
        self.assertEqual(parsed.size_streams, [])
        self.assertFalse(parsed.accumulate_flag)
        self.assertEqual(parsed.accumulate_streams, [])

    def test_write_format_out_empty_accumulate_streams_emits_f(self):
        """accumulate_flag with an empty stream list must emit 'F' (engine
        reads no stream list when NAccStreams == 0, SIMOP.FOR:363-370)."""
        fmt = FormatOutFile(
            num_metals=1,
            metal_names=["Cu"],
            size_flag=True,
            size_streams=[7],
            icode=1,
            accumulate_flag=True,
            accumulate_streams=[],
        )
        with tempfile.TemporaryDirectory() as tmp:
            path = os.path.join(tmp, "FORMAT.OUT")
            write_format_out(fmt, path)
            with open(path, "r", encoding="ascii", newline="") as fh:
                written = fh.read().splitlines()
            parsed = read_format_out(path)
        self.assertEqual(written[-1], "F")
        self.assertTrue(parsed.size_flag)
        self.assertEqual(parsed.size_streams, [7])
        self.assertEqual(parsed.icode, 1)
        self.assertFalse(parsed.accumulate_flag)
        self.assertEqual(parsed.accumulate_streams, [])

    def test_write_currdat_syd_matches_sid(self):
        """CURRDATA.SYD must be a byte-identical copy of the job's .sid."""
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        assert job.sid is not None and job.sid.raw_lines is not None
        expected = list(job.sid.raw_lines)
        with tempfile.TemporaryDirectory() as tmp:
            write_currdat_syd(job, tmp)
            currdat_path = os.path.join(tmp, "CURRDATA.SYD")
            self.assertTrue(
                os.path.exists(currdat_path), "CURRDATA.SYD should be written"
            )
            with open(currdat_path, "r", encoding="ascii", newline="") as fh:
                written = fh.read().splitlines()
        self.assertEqual(written, expected)

    def test_write_currdat_syd_noop_without_sid(self):
        """A job without a .sid file must not create CURRDATA.SYD."""
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        job.sid = None
        with tempfile.TemporaryDirectory() as tmp:
            write_currdat_syd(job, tmp)
            self.assertFalse(
                os.path.exists(os.path.join(tmp, "CURRDATA.SYD")),
                "CURRDATA.SYD should not be written when the job has no .sid",
            )

    def test_write_currdat_syd_noop_without_raw_lines(self):
        """A .sid without raw lines must not create CURRDATA.SYD."""
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        assert job.sid is not None
        job.sid.raw_lines = None
        with tempfile.TemporaryDirectory() as tmp:
            write_currdat_syd(job, tmp)
            self.assertFalse(
                os.path.exists(os.path.join(tmp, "CURRDATA.SYD")),
                "CURRDATA.SYD should not be written without raw lines",
            )

    def test_write_liberation_files_stages_lju_amd(self):
        """LJUBAMD.DAT / BETAAMD.DAT must be byte-identical copies of .lju/.amd."""
        job = read_job_directory(FAIRLANE, name="Fairlane")
        assert job.lju is not None and job.lju.raw_lines is not None
        assert job.amd is not None and job.amd.raw_lines is not None
        with tempfile.TemporaryDirectory() as tmp:
            write_liberation_files(job, tmp)
            for src_attr, dst in (("lju", "LJUBAMD.DAT"), ("amd", "BETAAMD.DAT")):
                src = getattr(job, src_attr)
                dst_path = os.path.join(tmp, dst)
                self.assertTrue(
                    os.path.exists(dst_path), f"{dst} should be written"
                )
                with open(dst_path, "r", encoding="ascii", newline="") as fh:
                    written = fh.read().splitlines()
                self.assertEqual(
                    written, list(src.raw_lines), f"{dst} should match .{src_attr}"
                )

    def test_write_liberation_files_noop_without_files(self):
        """A job with no .lju/.amd must not create LJUBAMD.DAT / BETAAMD.DAT."""
        job = read_job_directory(BOUGAINVILLE, name="Bougainville")
        job.lju = None
        job.amd = None
        with tempfile.TemporaryDirectory() as tmp:
            write_liberation_files(job, tmp)
            self.assertFalse(os.path.exists(os.path.join(tmp, "LJUBAMD.DAT")))
            self.assertFalse(os.path.exists(os.path.join(tmp, "BETAAMD.DAT")))

    def test_write_repeat_out_emits_levels(self):
        """Repeat.out must carry flag code, 'F', level lines and values."""
        with tempfile.TemporaryDirectory() as tmp:
            path = os.path.join(tmp, "Repeat.out")
            write_repeat_out(
                path,
                flag_code="FTFFT",
                heads_written=False,
                levels=[(1, 2, "FEED"), (3, 5, "WATER")],
                values_line="Sweep values here",
            )
            with open(path, "r", encoding="ascii", newline="") as fh:
                written = fh.read().splitlines()
        self.assertEqual(
            written,
            ["FTFFT", "F", "1 2", "FEED", "3 5", "WATER", "Sweep values here"],
        )

    def test_write_repeat_out_heads_written_omits_levels(self):
        """When heads are already written, level lines must be omitted."""
        with tempfile.TemporaryDirectory() as tmp:
            path = os.path.join(tmp, "Repeat.out")
            write_repeat_out(
                path,
                flag_code="TTTTT",
                heads_written=True,
                levels=[(1, 2, "FEED")],
                values_line="1 2 3",
            )
            with open(path, "r", encoding="ascii", newline="") as fh:
                written = fh.read().splitlines()
        self.assertEqual(written, ["TTTTT", "T", "1 2 3"])

    def test_scd_with_ranges_parses(self):
        """A .scd (S-class distribution) must parse without a bounds line."""
        dist = read_scd(os.path.join(BOUGAINVILLE, "floatbank.scd"))
        self.assertEqual(dist.stream_count, 1)
        self.assertEqual(len(dist.streams), 1)
        self.assertEqual(dist.streams[0].num1, 1)
        self.assertEqual(dist.streams[0].num2, 3)
        self.assertEqual(len(dist.streams[0].ranges), 12)
        for r in dist.streams[0].ranges:
            self.assertEqual(r.bounds, [], ".scd ranges must have no bounds line")
            self.assertEqual(len(r.values), 3)
        # A regenerated .scd (raw_lines=None) must not emit a bounds line.
        dist.raw_lines = None
        with tempfile.TemporaryDirectory() as tmp:
            from modsim.io.writers import write_scd

            out = os.path.join(tmp, "floatbank.scd")
            write_scd(dist, out)
            with open(out, "r", encoding="ascii", newline="") as fh:
                written = fh.read().splitlines()
        self.assertIn("Number of S-ranges      12 ", written)
        self.assertEqual(written[3], "    1")
        self.assertEqual(written[4], "    3", "index must be followed by nmin, not bounds")
        self.assertEqual(written[5], "0.0000E+0 3.3000E-1 6.7000E-1 ")

    def test_zero_water_trn_roundtrips(self):
        """Bougainville.TRN (no water streams) must round-trip byte-identical."""
        trn = read_trn(os.path.join(BOUGAINVILLE, "Bougainville.TRN"))
        self.assertEqual(trn.job_name, "Bougainville")
        self.assertIsNotNone(trn.raw_lines)
        with tempfile.TemporaryDirectory() as tmp:
            out = os.path.join(tmp, "Bougainville.TRN")
            write_trn(trn, out)
            self.assertTrue(
                filecmp.cmp(
                    os.path.join(BOUGAINVILLE, "Bougainville.TRN"),
                    out,
                    shallow=False,
                ),
                "zero-water .TRN should be byte-identical after a round trip",
            )

    def test_cur_noparam_parsed_as_param_count(self):
        """The .cur TYPE line's NoPARAM column must be the param count."""
        cur = read_cur(os.path.join(BOUGAINVILLE, "Bougainville.cur"))
        self.assertEqual(len(cur.units), 5)
        hfsu = cur.units[0]
        self.assertEqual(hfsu.number, 1)
        self.assertEqual(hfsu.model, "HFSU")
        self.assertEqual(hfsu.noparam, 22)
        self.assertEqual(hfsu.unit_id, 1)
        self.assertEqual(len(hfsu.params), 22, "exactly NoPARAM values are read")
        conv = cur.units[1]
        self.assertEqual(conv.noparam, 6)
        self.assertEqual(len(conv.params), 6)
        # Zero-parameter units parse cleanly.
        self.assertEqual(cur.units[2].noparam, 0)
        self.assertEqual(cur.units[2].params, [])
        self.assertEqual(cur.units[2].unit_id, 3)


if __name__ == "__main__":
    unittest.main()
