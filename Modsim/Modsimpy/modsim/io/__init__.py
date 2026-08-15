"""I/O for ModSIM legacy job files.

Provides readers, writers and path resolution for the legacy text job-file
format used by the Fortran engine.
"""

from .paths import default_jobs_dir, resolve_job_path
from .readers import read_job_directory
from .writers import write_datt_dat, write_job_directory, write_tears_out

__all__ = [
    "read_job_directory",
    "write_job_directory",
    "write_tears_out",
    "write_datt_dat",
    "resolve_job_path",
    "default_jobs_dir",
]
