"""ctypes bridge to the 64-bit ModSIM Fortran engine.

This module loads the compiled simulation engine shared library
(``ModsimMain.dll`` on Windows, ``libmodsim.so`` on Linux,
``libmodsim.dylib`` on macOS) and exposes the four exported entry points
used by the GUI:

* ``INORDCALC(path)``            -- job initialisation / in-order calc setup
* ``SIMOP(path, &cumOut)``       -- run the full simulation
* ``LJUBAMD(path, nameOfJob)``   -- Ljubljana liberation model
* ``BetaAMD(path, nameOfJob, &parameters)`` -- Beta liberation model

Every engine call takes a job-directory path string as its first argument.
The engine declares these as fixed-length Fortran ``CHARACTER*255`` buffers
(see ``SIMULATE.FOR``, ``LJUBAMD.FOR`` and ``BETAAMD.FOR``), so the path is
passed as a 255-byte buffer padded with ASCII spaces and carrying a trailing
separator (e.g. ``C:\\jobs\\myjob\\`` or ``/jobs/myjob/``). The engine strips
trailing spaces with ``LEN_TRIM``; it does NOT strip NUL bytes, so a
null-terminated C string would corrupt the path. The ``nameOfJob`` argument
to ``LJUBAMD``/``BETAAMD`` is likewise a ``CHARACTER*255`` buffer and is
space-padded the same way.

Pointer-width note
------------------
The engine is built for 64-bit targets, so every by-reference parameter is a
64-bit pointer on x64. ``ctypes.POINTER(...)`` always yields a pointer sized
to the running architecture (8 bytes on x64) regardless of the pointed-to
type, so the by-reference arguments are declared with ``ctypes.POINTER``.

The *pointed-to* values match the Fortran declarations exactly:

* ``INTEGER*4 CumOut``      -> ``ctypes.c_int32`` (always 32-bit)
* ``REAL*4 Parameters(4)``  -> ``ctypes.c_float`` (always 32-bit)

We deliberately use ``c_int32``/``c_float`` rather than ``c_long`` because
``c_long`` is 32-bit on Windows but 64-bit on Linux, whereas the Fortran
``INTEGER*4``/``REAL*4`` types are 32-bit on every platform. The pointer
itself remains 64-bit on x64, satisfying the "64-bit pointer" requirement.

Return codes are declared with their exact Fortran widths: ``INTEGER*2``
(``c_int16``) for ``INORDCALC``/``SIMOP`` and ``INTEGER*4`` (``c_int32``)
for ``LJUBAMD``/``BETAAMD``.
"""

from __future__ import annotations

import ctypes
import ctypes.util
import os
import sys
from typing import Optional, Sequence

__all__ = [
    "EngineLoadError",
    "ModsimEngine",
    "resolve_library_path",
    "encode_path",
    "inordcalc",
    "simop",
    "ljubamd",
    "betaamd",
]

#: Environment variable that may point at the directory containing the engine
#: library (and/or the default job directory).
MODSIM_ENV_VAR = "MODSIM"


class EngineLoadError(RuntimeError):
    """Raised when the ModSIM engine shared library cannot be located/loaded."""


def _library_names() -> Sequence[str]:
    """Return the platform-appropriate engine library file names."""
    if sys.platform == "win32":
        return ("ModsimMain.dll",)
    if sys.platform == "darwin":
        return ("libmodsim.dylib",)
    # Linux and other POSIX platforms.
    return ("libmodsim.so",)


def _candidate_paths(explicit_path: Optional[str]) -> list[str]:
    """Build an ordered list of candidate library paths to try loading."""
    candidates: list[str] = []

    if explicit_path:
        candidates.append(os.path.abspath(explicit_path))

    # Directory supplied via the MODSIM environment variable.
    env_dir = os.environ.get(MODSIM_ENV_VAR)
    if env_dir:
        for name in _library_names():
            candidates.append(os.path.join(env_dir, name))

    # Bare file names, resolved through the platform library search path.
    candidates.extend(_library_names())

    return candidates


def resolve_library_path(explicit_path: Optional[str] = None) -> Optional[str]:
    """Resolve the engine shared library to an absolute path, if possible.

    Checks, in order:

    1. An explicitly configured path (``explicit_path``).
    2. The directory named by the ``MODSIM`` environment variable.
    3. The platform library search path (via ``ctypes.util.find_library``).

    Returns the resolved path, or ``None`` if the library could not be found.
    """
    if explicit_path and os.path.isfile(explicit_path):
        return os.path.abspath(explicit_path)

    env_dir = os.environ.get(MODSIM_ENV_VAR)
    if env_dir:
        for name in _library_names():
            candidate = os.path.join(env_dir, name)
            if os.path.isfile(candidate):
                return os.path.abspath(candidate)

    # Fall back to the platform search path.
    for name in _library_names():
        found = ctypes.util.find_library(name)
        if found:
            return found

    return None


def encode_path(path: str) -> bytes:
    """Encode a job-directory path for the engine.

    Returns a fixed-length 255-byte buffer padded with ASCII spaces and
    carrying a trailing separator.

    The Fortran engine declares the path parameter as ``CHARACTER*255``
    (see ``SIMULATE.FOR``) and strips trailing spaces with ``LEN_TRIM``.
    It does NOT strip NUL bytes, so a null-terminated C string would leave
    NUL bytes in the path and break file opens. The buffer must therefore be
    exactly 255 bytes, space-padded (never NUL-terminated).

    * The path is made absolute and given a trailing separator
      (``os.path.join(path, '')``).
    * It is encoded as UTF-8 (the platform-native encoding on Linux/macOS and
      a safe superset of ANSI on Windows).
    * The result is padded to exactly 255 bytes with ASCII spaces, truncating
      if the encoded path is longer than 255 bytes.
    """
    if not path:
        raise ValueError("engine path must not be empty")

    path = os.path.abspath(path)
    if not path.endswith(os.sep):
        path = os.path.join(path, "")

    encoded = path.encode("utf-8")
    return encoded[:255].ljust(255, b" ")


def _encode_name(name: str) -> bytes:
    """Encode a job name for the engine as a fixed-length ``CHARACTER*255``.

    ``LJUBAMD``/``BETAAMD`` declare ``CHARACTER*255 JobName`` (see
    ``LJUBAMD.FOR``/``BETAAMD.FOR``) and compare it with ``LEN_TRIM(JobName)``,
    so the name must be a 255-byte buffer padded with ASCII spaces (never
    NUL-terminated), matching the path encoding.
    """
    encoded = name.encode("utf-8")
    return encoded[:255].ljust(255, b" ")


class ModsimEngine:
    """Thin ctypes wrapper around the ModSIM Fortran engine.

    The library is loaded eagerly on construction. If it cannot be found or
    loaded, an :class:`EngineLoadError` is raised with an actionable message.
    """

    def __init__(self, library_path: Optional[str] = None) -> None:
        self._library_path: Optional[str] = None
        self._lib: Optional[ctypes.CDLL] = None
        self.load(library_path)

    # ------------------------------------------------------------------ #
    # Loading
    # ------------------------------------------------------------------ #
    def load(self, library_path: Optional[str] = None) -> None:
        """Locate and load the engine library, then declare the entry points."""
        resolved = resolve_library_path(library_path)
        if resolved is None:
            raise EngineLoadError(
                "Could not locate the ModSIM engine library. "
                f"Looked for: {', '.join(_library_names())}. "
                f"Set the {MODSIM_ENV_VAR} environment variable to the "
                "directory containing the engine library, or pass an explicit "
                "path to ModsimEngine()."
            )

        try:
            if sys.platform == "win32":
                # The Fortran exports use the STDCALL calling convention.
                lib = ctypes.WinDLL(resolved)
            else:
                lib = ctypes.CDLL(resolved)
        except OSError as exc:  # pragma: no cover - platform dependent
            raise EngineLoadError(
                f"Failed to load the ModSIM engine library at '{resolved}': {exc}"
            ) from exc

        self._library_path = resolved
        self._lib = lib
        self._declare_functions()

    def _declare_functions(self) -> None:
        """Declare argtypes/restype for the four exported engine functions."""
        lib = self._lib
        assert lib is not None

        # INORDCALC(path) -> INTEGER*2
        lib.INORDCALC.argtypes = [ctypes.c_char_p]
        lib.INORDCALC.restype = ctypes.c_int16

        # SIMOP(path, &cumOut) -> INTEGER*2
        lib.SIMOP.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_int32)]
        lib.SIMOP.restype = ctypes.c_int16

        # LJUBAMD(path, nameOfJob) -> INTEGER*4
        lib.LJUBAMD.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
        lib.LJUBAMD.restype = ctypes.c_int32

        # BETAAMD(path, nameOfJob, &parameters) -> INTEGER*4
        lib.BETAAMD.argtypes = [
            ctypes.c_char_p,
            ctypes.c_char_p,
            ctypes.POINTER(ctypes.c_float),
        ]
        lib.BETAAMD.restype = ctypes.c_int32

    # ------------------------------------------------------------------ #
    # Public API
    # ------------------------------------------------------------------ #
    @property
    def library_path(self) -> Optional[str]:
        """Absolute path of the loaded engine library, if any."""
        return self._library_path

    def inordcalc(self, path: str) -> int:
        """Run job initialisation / in-order calculation setup.

        Returns the engine result code (``INTEGER*2``).
        """
        lib = self._require_loaded()
        return int(lib.INORDCALC(encode_path(path)))

    def simop(self, path: str) -> tuple[int, int]:
        """Run the full simulation.

        Returns a ``(result_code, cumulative_output)`` tuple. ``cumOut`` is
        ``0`` = none, ``1`` = write cumulative output with column headings.
        """
        lib = self._require_loaded()
        cum_out = ctypes.c_int32(0)
        result = lib.SIMOP(encode_path(path), ctypes.byref(cum_out))
        return int(result), int(cum_out.value)

    def ljubamd(self, path: str, name: str) -> int:
        """Run the Ljubljana liberation model.

        Returns the engine result code (``INTEGER*4``).
        """
        lib = self._require_loaded()
        return int(lib.LJUBAMD(encode_path(path), _encode_name(name)))

    def betaamd(self, path: str, name: str, parameters: Sequence[float]) -> int:
        """Run the Beta liberation model.

        ``parameters`` is a sequence of four floats (``REAL*4 Parameters(4)``).
        Returns the engine result code (``INTEGER*4``).
        """
        lib = self._require_loaded()
        if len(parameters) != 4:
            raise ValueError("betaamd() requires exactly 4 parameters")
        params = (ctypes.c_float * 4)(*parameters)
        return int(
            lib.BETAAMD(
                encode_path(path), _encode_name(name), ctypes.byref(params)
            )
        )

    def _require_loaded(self) -> ctypes.CDLL:
        if self._lib is None:
            raise EngineLoadError(
                "The ModSIM engine library is not loaded. Call load() first."
            )
        return self._lib


# ---------------------------------------------------------------------- #
# Module-level convenience API backed by a lazily-created shared engine.
# ---------------------------------------------------------------------- #
_shared_engine: Optional[ModsimEngine] = None


def _get_shared_engine() -> ModsimEngine:
    global _shared_engine
    if _shared_engine is None:
        _shared_engine = ModsimEngine()
    return _shared_engine


def inordcalc(path: str) -> int:
    """Module-level convenience wrapper for :meth:`ModsimEngine.inordcalc`."""
    return _get_shared_engine().inordcalc(path)


def simop(path: str) -> tuple[int, int]:
    """Module-level convenience wrapper for :meth:`ModsimEngine.simop`."""
    return _get_shared_engine().simop(path)


def ljubamd(path: str, name: str) -> int:
    """Module-level convenience wrapper for :meth:`ModsimEngine.ljubamd`."""
    return _get_shared_engine().ljubamd(path, name)


def betaamd(path: str, name: str, parameters: Sequence[float]) -> int:
    """Module-level convenience wrapper for :meth:`ModsimEngine.betaamd`."""
    return _get_shared_engine().betaamd(path, name, parameters)
