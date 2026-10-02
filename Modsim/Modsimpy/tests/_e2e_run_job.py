"""Subprocess worker for the ModSIM E2E smoke suite.

This module is NOT a test.  It runs ONE job through the GUI (headless,
``QT_QPA_PLATFORM=offscreen``) end-to-end -- the same path a user triggers with
File > Open followed by Simulate > Run -- and reports the outcome as a single
JSON line on stdout.

The Fortran engine can abort the whole process on malformed legacy input
(``forrtl: severe ...``), which a normal in-process test cannot survive.  Each
job therefore runs in its own subprocess so one bad job cannot take down the
rest of the sweep.  The parent test collects the exit status + JSON.

Usage::

    python _e2e_run_job.py <job_dir> <job_name> <engine_dir>

Exit status: 0 = the GUI run completed and the engine reported success
(-1/-1) with parseable outputs; 2 = the run completed but the engine reported
a failure or produced no parseable outputs; 3 = an unexpected Python error.
A crash (Fortran abort / segfault) surfaces as any other non-zero status,
which the parent reports as CRASH.
"""

from __future__ import annotations

import json
import os
import sys
import tempfile
import uuid
from unittest import mock

# Force offscreen Qt before any Qt import so no display is required.
os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

# Allow running directly from the tests directory.
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from PySide6.QtWidgets import QApplication, QMessageBox  # noqa: E402

from modsim.engine.engine_bridge import EngineLoadError  # noqa: E402
from modsim.gui import main_window as mw  # noqa: E402
from modsim.gui.main_window import (  # noqa: E402
    SIMULATION_COMPLETE,
    SIMULATION_ERROR,
)
from modsim.gui.plotting.parser import parse_results  # noqa: E402

SUCCESS = 0
FAILURE = 2
ERROR = 3


def _make_job_dir() -> str:
    """Create a writable scratch directory without ``tempfile.mkdtemp``.

    ``mkdtemp`` creates the directory with mode 0o700, which some sandboxed
    CI runners mark non-writable; ``os.makedirs`` with a UUID name keeps the
    normal inherited permissions and works everywhere.
    """
    base = tempfile.gettempdir()
    path = os.path.join(base, f"modsim_e2e_{os.getpid()}_{uuid.uuid4().hex}")
    os.makedirs(path)
    return path


def main(argv: list[str]) -> int:
    if len(argv) != 4:
        print(json.dumps({"status": "error", "message": "usage: <job_dir> <job_name> <engine_dir>"}))
        return ERROR

    job_dir, job_name, engine_dir = argv[1], argv[2], argv[3]
    os.environ["MODSIM"] = engine_dir

    app = QApplication.instance() or QApplication([])

    window = mw.MainWindow()
    scratch = _make_job_dir()
    recorded: dict = {}

    # Record the engine's return codes while the GUI drives it, so the report
    # can distinguish "engine failed cleanly" from "no outputs produced".
    real_engine = mw.ModsimEngine

    class RecordingEngine(real_engine):  # type: ignore[misc, valid-type]
        def inordcalc(self, path):  # noqa: D102
            code = super().inordcalc(path)
            recorded["inord"] = code
            return code

        def simop(self, path, cum_out=0):  # noqa: D102
            code, cum = super().simop(path, cum_out)
            recorded["simop"] = code
            recorded["cum"] = cum
            return code, cum

    mw.ModsimEngine = RecordingEngine

    try:
        # Patch the modal dialogs so an offscreen run never blocks waiting for
        # a human: critical/information are swallowed, the tear-reset question
        # answers "No" (leave tear data alone), keeping every path non-blocking.
        with mock.patch.object(QMessageBox, "critical", return_value=QMessageBox.StandardButton.Ok), \
             mock.patch.object(QMessageBox, "information", return_value=QMessageBox.StandardButton.Ok), \
             mock.patch.object(QMessageBox, "question", return_value=QMessageBox.StandardButton.No):
            # File > Open (reads the real job into memory, opens the flowsheet).
            loaded = window.open_job(directory=job_dir, name=job_name)
            if loaded is None:
                return _report(window, scratch, job_name, recorded, "open-failed")

            # Redirect the engine's working directory to the scratch copy so
            # the shipped job files are never written to.
            window._job_path = scratch  # noqa: SLF001 - test hook
            # Simulate > Run.
            window.run_simulation()
    except EngineLoadError as exc:
        return _report(window, scratch, job_name, recorded, "engine-missing", str(exc))
    except Exception as exc:  # noqa: BLE001 - report, never crash the worker
        return _report(window, scratch, job_name, recorded, "error", f"{type(exc).__name__}: {exc}")

    state = window.simulation_state
    if state != SIMULATION_COMPLETE:
        return _report(window, scratch, job_name, recorded, "fail", state)

    # Validate the outputs: parse the engine result files and require actual
    # stream / size-distribution data, not just a -1 exit code.
    results = parse_results(scratch)
    opdisp = os.path.isfile(os.path.join(scratch, "OPDISP.DAT"))
    n_streams = len(results.streams)
    n_sd = len(results.size_distributions)
    if opdisp and (n_streams or n_sd):
        return _report(
            window, scratch, job_name, recorded, "success",
            n_streams=n_streams, n_sd=n_sd, opdisp=opdisp,
        )
    return _report(
        window, scratch, job_name, recorded, "no-output",
        n_streams=n_streams, n_sd=n_sd, opdisp=opdisp,
    )


def _report(window, scratch, job_name, recorded, status, message="", **extra) -> int:
    payload = {
        "job": job_name,
        "status": status,
        "state": window.simulation_state,
        "inord": recorded.get("inord"),
        "simop": recorded.get("simop"),
    }
    if message:
        payload["message"] = message
    payload.update(extra)
    print(json.dumps(payload))
    window.close()
    import shutil

    shutil.rmtree(scratch, ignore_errors=True)
    return SUCCESS if status == "success" else (ERROR if status in ("error", "open-failed") else FAILURE)


if __name__ == "__main__":
    sys.exit(main(sys.argv))
