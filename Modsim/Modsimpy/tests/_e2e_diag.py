"""Diagnostic driver: run every job and print full records for failures.

Uses the same machinery as the E2E suites (_e2e_sweep.run_job) but prints the
per-job JSON status + diagnostic for non-successes, so failures can be
analysed without the manifest-based pass/fail contract in the way.
"""

from __future__ import annotations

import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _e2e_sweep import discover_jobs, job_key, locate_engine_dir, run_job  # noqa: E402

_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))


def main() -> None:
    out = {}
    for jobs_dir_name in ("Jobs", "JobsRPK"):
        jobs_dir = os.path.join(_ROOT, "Modsim", jobs_dir_name)
        engine_dir = locate_engine_dir() or ""
        for rel_dir, name in discover_jobs(jobs_dir):
            key = f"{jobs_dir_name}::{job_key(rel_dir, name)}"
            status, record = run_job(rel_dir, name, engine_dir, jobs_dir)
            entry = {"status": status}
            if record:
                entry.update(record)
            out[key] = entry

    failures = {k: v for k, v in out.items() if v["status"] != "success"}
    print(f"TOTAL={len(out)} NON_SUCCESS={len(failures)}")
    for key, rec in sorted(failures.items()):
        print(json.dumps({"job": key, **rec}))
    with open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "_e2e_diag.json"), "w") as fh:
        json.dump(out, fh, indent=1)


if __name__ == "__main__":
    main()
