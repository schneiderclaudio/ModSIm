"""End-to-end smoke suite: run every ModSIM job in ``Modsim/Jobs``.

This is the sibling of ``test_e2e_jobsrpk.py``: it drives every ``*.JOB`` in the
legacy ``Modsim/Jobs`` folder through the **real GUI** (headless,
``QT_QPA_PLATFORM=offscreen``) and the **real Fortran engine**, then asserts the
same regression contract -- open (File > Open), run (Simulate > Run), and
require parseable result files.

All shared machinery lives in ``_e2e_sweep.py``; this file only declares the
``Modsim/Jobs`` folder and its known-issues manifest.  See the ``_e2e_sweep``
docstring for the full pass/fail contract.

Use ``MODSIM_E2E_JOBS`` (comma-separated substrings) to run a subset, e.g.
(from ``Modsim/Modsimpy``)::

    MODSIM_E2E_JOBS=Demo,Cyclone python tests/test_e2e_jobs.py
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
_JOBS_DIR = os.path.join(_REPO_ROOT, "Modsim", "Jobs")

#: Minimum number of jobs the sweep must discover (a sanity floor so a broken
#: scan cannot silently "pass" by running nothing).
MIN_DISCOVERED_JOBS = 40

#: Minimum number of jobs that must succeed today (measured baseline: 34/45
#: after the -O2 -fp:strict engine fix; see PYTHON_GUI_E2E_FAILURES_ADDENDUM.md).
#: No flaky jobs have been observed in this folder so far, so the floor leaves
#: modest headroom below the steady-state success count.
MIN_SUCCESSFUL_JOBS = 31


# ---------------------------------------------------------------------------
# Known-issue manifest (directory-relative job key -> reason).  Keyed by the
# directory-relative path so duplicate names across folders stay unambiguous
# (e.g. the root ``Fairlane`` vs ``Distribution jobs/Fairlane``).
# ---------------------------------------------------------------------------
#: Jobs that currently FAIL with a clean engine error code (34 singular matrix,
#: 36 mass balance, 38, 16, 32 ...) or a GUI-side missing-data refusal.
KNOWN_FAILURES = {
    "./Bougainville": "singular convergence matrix (exit 34)",
    "./Cone": "missing run data (.cur) - GUI refuses to run",
    "./Cyclone": "error reading file in calculation phase (exit 32)",
    "./ELISTA05": "no-solids stream error (exit 16)",
    "./FAG_SAG": "calculation-phase error (exit 38)",
    "./Fairlane": "singular convergence matrix (exit 34)",
    "./SAG-CVRD": "mass-balance error (exit 36)",
    "./UNITS": "missing system data (.sid) - GUI refuses to run",
    "./ex1aV3": "mass-balance error (exit 36)",
    "./fosfertil793": "mass-balance error (exit 36)",
    "./jones test": "mass-balance error (exit 36)",
    # ./sag-cetem, ./sag-cetem_B11, ./sag-cetem_closed were exit-36 entries
    # removed after the -O2 -fp:strict engine fix (IFX fast-FP regression);
    # they pass now. See PYTHON_GUI_E2E_FAILURES_ADDENDUM.md.
}

#: Jobs that currently CRASH the engine (Fortran runtime abort / access
#: violation) and therefore cannot even report a clean error code.
KNOWN_CRASHES = {}

#: Jobs whose outcome is non-deterministic in the current engine build.
FLAKY = {}


class JobsE2ETest(E2ESweepMixin, unittest.TestCase):
    """Drive every Modsim/Jobs job through the GUI + Fortran engine."""

    JOBS_DIR = _JOBS_DIR
    SUITE_LABEL = "Jobs"
    MIN_DISCOVERED_JOBS = MIN_DISCOVERED_JOBS
    MIN_SUCCESSFUL_JOBS = MIN_SUCCESSFUL_JOBS
    KNOWN_FAILURES = KNOWN_FAILURES
    KNOWN_CRASHES = KNOWN_CRASHES
    FLAKY = FLAKY


if __name__ == "__main__":
    unittest.main(verbosity=2)
