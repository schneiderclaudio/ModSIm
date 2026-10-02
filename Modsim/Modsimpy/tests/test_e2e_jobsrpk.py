"""End-to-end smoke suite: run every ModSIM job in ``Modsim/JobsRPK``.

Each job is driven through the **real GUI** (headless, ``QT_QPA_PLATFORM=offscreen``)
and the **real Fortran engine**: the test opens the job the same way a user does
(File > Open) and triggers Simulate > Run, then validates that the engine wrote
parseable result files.

All the shared machinery (engine location, job discovery, the per-job subprocess
runner, and the pass/fail sweep contract) lives in ``_e2e_sweep.py``.  This file
only declares the JobsRPK job folder and its known-issues manifest.  See the
``_e2e_sweep`` docstring for the full pass/fail contract.

Use ``MODSIM_E2E_JOBS`` (comma-separated substrings) to run a subset, e.g.
(from ``Modsim/Modsimpy``)::

    MODSIM_E2E_JOBS=Demo,Cyclone python tests/test_e2e_jobsrpk.py
"""

from __future__ import annotations

import os
import unittest

# Force offscreen Qt before any Qt import (the shared module also does this).
os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from _e2e_sweep import E2ESweepMixin  # noqa: E402

_REPO_ROOT = os.path.dirname(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
)
_JOBSRPK_DIR = os.path.join(_REPO_ROOT, "Modsim", "JobsRPK")

#: Minimum number of jobs the sweep must discover (a sanity floor so a broken
#: scan cannot silently "pass" by running nothing).
MIN_DISCOVERED_JOBS = 80

#: Minimum number of jobs that must succeed today (measured baseline: 82/92
#: after the -O2 -fp:strict engine fix; see PYTHON_GUI_E2E_FAILURES_ADDENDUM.md).
#: Two jobs are flaky and may flip to a clean failure, so the floor leaves
#: headroom below the steady-state success count.
MIN_SUCCESSFUL_JOBS = 76

# ---------------------------------------------------------------------------
# Known-issue manifest (directory-relative job key -> reason).  Keyed by the
# directory-relative path so duplicate names across folders stay unambiguous.
# ---------------------------------------------------------------------------
#: Jobs that currently FAIL with a clean engine error code (34 singular matrix,
#: 36 mass balance, 44 ordering, 38, 16 ...) or a GUI-side save/open error.
KNOWN_FAILURES = {
    "./Bougainville": "singular convergence matrix (exit 34)",
    "./Fairlane": "singular convergence matrix (exit 34)",
    # ex1aV3 (x2), Bougainville, Fairlane, Kiruna, CCDTrial: genuine shipped-data
    # defects — fail identically under every compiler flag set tested (-O0,
    # -fp:strict, -Qsave). The former exit-36 "mass-balance" jobs below were
    # IFX -O2 fast-FP regressions; the engine is now built with -O2 -fp:strict
    # and they pass. See PYTHON_GUI_E2E_FAILURES_ADDENDUM.md.
    "./ex1aV3": "mass-balance error (exit 36)",
    "Baker Process/CCDTrial": "no-solids stream error (exit 16)",
    "Coal processing jobs/Batac jig with proximate analysis": "job open/save error before run",
    "CoalPrep/ex1aV3": "mass-balance error (exit 36)",
    "Kiruna Autogenous Mill/Kiruna primary circuit": "calculation-phase error (exit 38)",
    # Missing .sid/.cur: the GUI now refuses to run these instead of letting the
    # engine OPEN an empty DATT.DAT and abort with a Fortran EOF.
    "CoalPrep/ex5V3": "missing system/run data (.sid/.cur) - GUI refuses to run",
    "ConeOpt/Cone1": "missing system/run data (.sid/.cur) - GUI refuses to run",
    "Kennecott/Deepak data 2003/MILL": "missing run data (.cur) - GUI refuses to run",
}

#: Jobs that currently CRASH the engine (Fortran runtime abort / access
#: violation) and therefore cannot even report a clean error code.  Currently
#: empty: the jobs that used to EOF-abort (missing .sid/.cur) are now refused
#: cleanly by the GUI guard and live in KNOWN_FAILURES.
KNOWN_CRASHES = {}

#: Jobs whose outcome is non-deterministic in the current engine build: they
#: flip between success and a clean error code across runs (legacy uninitialised
#: state / a convergence boundary).  A *crash* here is still a regression.
FLAKY = {
    # Under the -O2 -fp:strict engine build (see
    # PYTHON_GUI_E2E_FAILURES_ADDENDUM.md) this job deterministically reports
    # calc-phase error 37 / 3; on other flag sets it alternated 34 / success,
    # so it sits on a numerical convergence boundary. Either outcome is
    # accepted here.
    "Kennecott/Kennecott SAG BM Line 4": "calc-phase error (37) - boundary job",
    # KennecottDeepak1 alternated mass-balance (36) / success on the old -O2
    # fast-FP build; with -fp:strict it succeeds 5/5 and is treated as a
    # passing job (kept out of the dict intentionally).
}


class JobsRpkE2ETest(E2ESweepMixin, unittest.TestCase):
    """Drive every JobsRPK job through the GUI + Fortran engine."""

    JOBS_DIR = _JOBSRPK_DIR
    SUITE_LABEL = "JobsRPK"
    MIN_DISCOVERED_JOBS = MIN_DISCOVERED_JOBS
    MIN_SUCCESSFUL_JOBS = MIN_SUCCESSFUL_JOBS
    KNOWN_FAILURES = KNOWN_FAILURES
    KNOWN_CRASHES = KNOWN_CRASHES
    FLAKY = FLAKY


if __name__ == "__main__":
    unittest.main(verbosity=2)
