"""Shared machinery for the ModSIM job-sweep E2E suites.

Both ``test_e2e_jobsrpk.py`` and ``test_e2e_jobs.py`` drive every ``*.JOB`` in
their respective job folder through the **real GUI** (headless,
``QT_QPA_PLATFORM=offscreen``) and the **real Fortran engine**, then assert a
regression contract over the current state of the shipped data.  Everything the
two suites have in common -- engine location, job discovery, the per-job
subprocess runner, and the pass/fail sweep test -- lives here, so each suite
only declares its job folder and its known-issues manifest.

Why a subprocess per job
------------------------
The legacy Fortran engine can abort the whole Python process on malformed input
(``forrtl: severe ...``), which a normal in-process test cannot survive.  Every
job therefore runs in its own subprocess (``_e2e_run_job.py``) so one bad job
cannot take down the rest of the sweep; the parent test collects the exit status
and the JSON result line.

Pass/fail contract
------------------
The suite is a *regression* contract over the current state of the shipped data:

* every job is executed;
* jobs not listed in ``KNOWN_FAILURES`` / ``KNOWN_CRASHES`` must succeed
  (engine returns -1/-1 AND ``OPDISP.DAT`` parses into stream/size data);
* jobs in ``KNOWN_FAILURES`` / ``KNOWN_CRASHES`` are asserted to stay in their
  currently-broken state (clean failure vs crash), so fixing one of them turns
  the suite red until it is moved to the passing set -- i.e. the manifest is the
  single source of truth for "this job is expected not to run";
* ``FLAKY`` jobs may pass or fail (legacy non-determinism) but must never
  crash.

A crash anywhere *outside* ``KNOWN_CRASHES`` is always a hard failure, as is a
clean failure outside ``KNOWN_FAILURES``.

Use ``MODSIM_E2E_JOBS`` (comma-separated substrings) to run a subset, e.g.
(from ``Modsim/Modsimpy``)::

    MODSIM_E2E_JOBS=Demo,Cyclone python tests/test_e2e_jobsrpk.py
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
import unittest

# Force offscreen Qt before any Qt import.
os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

_REPO_ROOT = os.path.dirname(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
)
_ENGINE_BUILD_DIR = os.path.join(_REPO_ROOT, "Modsim", "Modsimdl", "build")
_WORKER = os.path.join(os.path.dirname(os.path.abspath(__file__)), "_e2e_run_job.py")

#: Time budget per job subprocess (the engine is fast; this only guards hangs).
JOB_TIMEOUT_SECONDS = 180


def _engine_library_names():
    if sys.platform == "win32":
        return ("ModsimMain.dll",)
    if sys.platform == "darwin":
        return ("libmodsim.dylib",)
    return ("libmodsim.so",)


def _candidate_engine_dirs():
    """Yield engine build directories, newest-first (release before debug)."""
    for compiler in ("ifx", "gfortran"):
        for build in ("release", "debug"):
            yield os.path.join(_ENGINE_BUILD_DIR, compiler, build)


def locate_engine_dir() -> str | None:
    """Return the directory containing the engine library, or ``None``.

    Mirrors ``scripts/run.ps1``: prefer the IFX release tree, then debug, then
    gfortran.  An already-resolvable ``MODSIM`` env var wins (kept as-is by the
    worker's own resolution), but here we want a concrete directory to hand to
    the worker subprocess explicitly.
    """
    env_dir = os.environ.get("MODSIM")
    if env_dir and os.path.isdir(env_dir):
        for name in _engine_library_names():
            if os.path.isfile(os.path.join(env_dir, name)):
                return env_dir
    for candidate in _candidate_engine_dirs():
        for name in _engine_library_names():
            if os.path.isfile(os.path.join(candidate, name)):
                return candidate
    return None


def discover_jobs(jobs_dir: str):
    """Yield ``(relative_dir, job_name)`` for every ``*.JOB`` under ``jobs_dir``.

    ``relative_dir`` is the job's directory relative to ``jobs_dir`` (``"."``
    for jobs sitting directly in the root), which disambiguates duplicate job
    names across subfolders (e.g. the two ``Fairlane`` jobs).
    """
    jobs = []
    for dirpath, _dirs, files in os.walk(jobs_dir):
        for filename in files:
            if filename.lower().endswith(".job"):
                rel = os.path.relpath(dirpath, jobs_dir).replace(os.sep, "/")
                jobs.append((rel, os.path.splitext(filename)[0]))
    return sorted(jobs)


def job_key(rel_dir: str, name: str) -> str:
    return f"{rel_dir}/{name}"


def run_job(rel_dir: str, name: str, engine_dir: str, jobs_dir: str):
    """Run one job through the GUI worker subprocess; return ``(status, record)``.

    ``status`` is one of ``success`` / ``fail`` / ``crash`` / ``error`` (or the
    worker's own ``open-failed`` / ``engine-missing`` / ``no-output`` statuses);
    ``record`` is the parsed JSON result line (``None`` on a crash).
    """
    job_dir = os.path.join(jobs_dir, rel_dir) if rel_dir != "." else jobs_dir
    proc = subprocess.run(
        [sys.executable, _WORKER, job_dir, name, engine_dir],
        capture_output=True,
        text=True,
        timeout=JOB_TIMEOUT_SECONDS,
    )
    record = None
    for line in (proc.stdout or "").strip().splitlines():
        try:
            record = json.loads(line)
        except json.JSONDecodeError:
            continue
    if record is not None:
        return record.get("status", "error"), record
    # No JSON line means the worker died before reporting (Fortran abort, etc.).
    return "crash", {"job": name, "status": "crash", "exit": proc.returncode}


class E2ESweepMixin:
    """Reusable sweep test logic; mix into a concrete ``unittest.TestCase``.

    Concrete suites declare this mixin *first*, then ``unittest.TestCase``::

        class JobsE2ETest(E2ESweepMixin, unittest.TestCase):
            JOBS_DIR = ...
            ...

    They set the class attributes below and should NOT override the test methods
    or ``setUpClass`` -- the whole contract is driven from the manifest.  The
    mixin deliberately is *not* a ``unittest.TestCase`` so pytest/unittest do
    not collect it as a runnable test class in its own right.
    """

    #: Absolute path of the job folder to sweep (set by subclass).
    JOBS_DIR: str = ""

    #: Human label used in the summary line (set by subclass).
    SUITE_LABEL: str = "E2E"

    #: Minimum jobs the sweep must discover (sanity floor so a broken scan
    #: cannot silently "pass" by running nothing).
    MIN_DISCOVERED_JOBS: int = 0

    #: Minimum jobs that must succeed when the full (unfiltered) sweep runs.
    MIN_SUCCESSFUL_JOBS: int = 0

    #: Jobs that currently FAIL with a clean engine error code or a GUI-side
    #: save/open error, keyed by directory-relative path (``rel_dir/name``).
    KNOWN_FAILURES: dict = {}

    #: Jobs that currently CRASH the engine (Fortran runtime abort / access
    #: violation) and cannot even report a clean error code.
    KNOWN_CRASHES: dict = {}

    #: Jobs whose outcome is non-deterministic: they flip between success and a
    #: clean error code across runs.  A *crash* here is still a regression.
    FLAKY: dict = {}

    engine_dir: str | None = None
    jobs: list = []

    @classmethod
    def setUpClass(cls) -> None:
        cls.engine_dir = locate_engine_dir()
        if cls.engine_dir is None:
            raise unittest.SkipTest(
                "ModSIM engine library not found; build it with `make` "
                "(or set MODSIM to the build directory)."
            )
        cls.jobs = discover_jobs(cls.JOBS_DIR)
        os.environ["MODSIM"] = cls.engine_dir

    @property
    def known_issues(self) -> dict:
        return {**self.KNOWN_FAILURES, **self.KNOWN_CRASHES, **self.FLAKY}

    # ------------------------------------------------------------------
    # Fast structural checks
    # ------------------------------------------------------------------
    def test_jobs_discovered(self):
        self.assertGreaterEqual(
            len(self.jobs),
            self.MIN_DISCOVERED_JOBS,
            f"expected at least {self.MIN_DISCOVERED_JOBS} {self.SUITE_LABEL} jobs, "
            f"found {len(self.jobs)}",
        )

    def test_manifest_keys_match_discovered_jobs(self):
        """Every manifest entry must correspond to a real discovered job."""
        discovered = {job_key(rel, name) for rel, name in self.jobs}
        for key in self.known_issues:
            self.assertIn(key, discovered, f"manifest entry not discovered: {key}")

    # ------------------------------------------------------------------
    # The sweep
    # ------------------------------------------------------------------
    def test_sweep(self):
        """Run every job; assert the pass/fail contract defined above."""
        filter_terms = [
            t.strip()
            for t in os.environ.get("MODSIM_E2E_JOBS", "").split(",")
            if t.strip()
        ]

        results = {}
        crashes = []
        clean_failures = []
        successes = []

        for rel_dir, name in self.jobs:
            key = job_key(rel_dir, name)
            if filter_terms and not any(t.lower() in key.lower() for t in filter_terms):
                continue

            with self.subTest(job=key):
                status, record = run_job(rel_dir, name, self.engine_dir, self.JOBS_DIR)
                results[key] = (status, record)

        # Partition for the summary + assertions.
        for key, (status, _record) in results.items():
            if status == "success":
                successes.append(key)
            elif status == "crash":
                crashes.append(key)
            else:
                clean_failures.append(key)

        # 1. New crashes are always a regression.
        unexpected_crashes = set(crashes) - set(self.KNOWN_CRASHES)
        self.assertFalse(
            unexpected_crashes,
            "jobs crashed that are not in KNOWN_CRASHES: "
            + ", ".join(sorted(unexpected_crashes)),
        )

        # 2. New clean failures are always a regression.  FLAKY jobs may fail
        #    legitimately, so they are excluded here (and handled in #3).
        unexpected_failures = (
            set(clean_failures) - set(self.KNOWN_FAILURES) - set(self.FLAKY)
        )
        self.assertFalse(
            unexpected_failures,
            "jobs failed that are not in KNOWN_FAILURES: "
            + ", ".join(sorted(unexpected_failures)),
        )

        # 3. Deterministic known failures/crashes must remain non-success (a
        #    fix turns this red so the manifest is updated to the new reality).
        #    FLAKY jobs are exempt: either outcome is accepted.
        regressed_to_success = []
        for key in list(self.KNOWN_FAILURES) + list(self.KNOWN_CRASHES):
            if key not in results:
                continue
            status, _ = results[key]
            if status == "success":
                regressed_to_success.append(key)
        self.assertFalse(
            regressed_to_success,
            "jobs listed as known-issues now succeed; remove them from the "
            "manifest: " + ", ".join(sorted(regressed_to_success)),
        )

        # 4. The whole suite must keep a healthy success count (only meaningful
        #    when the full sweep ran; a filtered subset has its own per-job
        #    contract above).
        if not filter_terms:
            self.assertGreaterEqual(
                len(successes),
                self.MIN_SUCCESSFUL_JOBS,
                f"only {len(successes)}/{len(results)} jobs succeeded "
                f"(threshold {self.MIN_SUCCESSFUL_JOBS})",
            )

        # Print a human-readable summary to stderr for the developer.
        flaky_seen = sorted(set(self.FLAKY) & set(results))
        print(
            f"\n{self.SUITE_LABEL} E2E: {len(successes)} ok, "
            f"{len(clean_failures)} fail, {len(crashes)} crash, "
            f"{len(flaky_seen)} flaky of {len(results)} run.",
            file=sys.stderr,
        )
