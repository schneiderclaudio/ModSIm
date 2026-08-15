"""Job directory path resolution.

The job directory is resolved using the ``MODSIM`` environment variable,
falling back to a configured default (the ``Jobs`` directory shipped with the
package), then to a temporary directory.
"""

from __future__ import annotations

import os
import tempfile
from typing import Optional

# Environment variable that may point at the jobs directory.
MODSIM_ENV = "MODSIM"

# Default jobs directory relative to this package:
# <pkg>/modsim/io/paths.py -> <repo>/Modsim/Jobs
_DEFAULT_JOBS_DIR = os.path.join(
    os.path.dirname(
        os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    ),
    "Jobs",
)


def default_jobs_dir() -> str:
    """Return the configured default jobs directory."""
    return _DEFAULT_JOBS_DIR


def resolve_job_path(name: Optional[str] = None) -> str:
    """Resolve the base job directory, or a specific job within it.

    Resolution order:
      1. ``MODSIM`` environment variable (if set and a directory).
      2. The configured default jobs directory (if it exists).
      3. The system temporary directory.

    When ``name`` is given the returned path is ``<base>/<name>``.
    """
    base = os.environ.get(MODSIM_ENV)
    if not base or not os.path.isdir(base):
        base = _DEFAULT_JOBS_DIR
    if not os.path.isdir(base):
        base = tempfile.gettempdir()
    if name:
        return os.path.join(base, name)
    return base
