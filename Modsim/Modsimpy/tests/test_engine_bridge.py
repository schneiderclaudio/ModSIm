"""Smoke tests for the ModSIM engine ctypes bridge.

These tests verify the bridge imports, that the four engine functions are
declared with the correct argtypes/restype, and that path-string handling
produces the right bytes (fixed-length 255-byte, space-padded, trailing
separator).

The tests do NOT require the actual Fortran DLL to be present: if the engine
library cannot be located, the signature-declaration checks are skipped
gracefully. Path-handling checks always run.
"""

import ctypes
import os
import sys
import unittest
from unittest import mock

from modsim.engine import (
    EngineLoadError,
    MODSIM_ENV_VAR,
    ModsimEngine,
    encode_path,
    resolve_library_path,
)

#: Whether the engine library is available on this machine.
ENGINE_AVAILABLE = resolve_library_path() is not None


class PathHandlingTest(unittest.TestCase):
    """Path-string handling must not require the DLL."""

    def test_encode_path_space_padded(self):
        raw = encode_path(r"C:\jobs\myjob")
        # The engine declares CHARACTER*255 and strips trailing spaces with
        # LEN_TRIM, so the buffer must be exactly 255 bytes, space-padded and
        # NOT NUL-terminated.
        self.assertEqual(len(raw), 255)
        self.assertFalse(raw.endswith(b"\x00"))
        # The payload (minus the trailing spaces) must be the absolute path
        # with a trailing separator.
        payload = raw.rstrip(b" ")
        self.assertTrue(payload.endswith(os.sep.encode("utf-8")))

    def test_encode_path_trailing_separator(self):
        # A path without a trailing separator gets one added.
        raw = encode_path(os.path.join("some", "dir"))
        self.assertTrue(raw.rstrip(b" ").endswith(os.sep.encode("utf-8")))

    def test_encode_path_does_not_double_separator(self):
        # A path that already ends with a separator is not doubled.
        base = os.path.join("some", "dir")
        with_sep = os.path.join(base, "")
        self.assertEqual(encode_path(base), encode_path(with_sep))

    def test_encode_path_empty_raises(self):
        with self.assertRaises(ValueError):
            encode_path("")

    def test_encode_path_absolute(self):
        raw = encode_path("relative_job")
        payload = raw.rstrip(b" ")
        self.assertTrue(os.path.isabs(payload))


class SignatureDeclarationTest(unittest.TestCase):
    """Verify the four engine functions are declared with correct types.

    Skipped gracefully when the engine library is not available.
    """

    @unittest.skipUnless(ENGINE_AVAILABLE, "ModSIM engine library not available")
    def test_functions_declared(self):
        engine = ModsimEngine()
        lib = engine._lib  # noqa: SLF001 - direct inspection for the test
        assert lib is not None

        # INORDCALC(path) -> INTEGER*2
        self.assertEqual(lib.INORDCALC.argtypes, [ctypes.c_char_p])
        self.assertEqual(lib.INORDCALC.restype, ctypes.c_int16)

        # SIMOP(path, &cumOut) -> INTEGER*2
        self.assertEqual(
            lib.SIMOP.argtypes,
            [ctypes.c_char_p, ctypes.POINTER(ctypes.c_int32)],
        )
        self.assertEqual(lib.SIMOP.restype, ctypes.c_int16)

        # LJUBAMD(path, nameOfJob) -> INTEGER*4
        self.assertEqual(lib.LJUBAMD.argtypes, [ctypes.c_char_p, ctypes.c_char_p])
        self.assertEqual(lib.LJUBAMD.restype, ctypes.c_int32)

        # BETAAMD(path, nameOfJob, &parameters) -> INTEGER*4
        self.assertEqual(
            lib.BETAAMD.argtypes,
            [
                ctypes.c_char_p,
                ctypes.c_char_p,
                ctypes.POINTER(ctypes.c_float),
            ],
        )
        self.assertEqual(lib.BETAAMD.restype, ctypes.c_int32)

    @unittest.skipUnless(ENGINE_AVAILABLE, "ModSIM engine library not available")
    def test_pointer_width_is_64bit_on_x64(self):
        engine = ModsimEngine()
        lib = engine._lib  # noqa: SLF001
        assert lib is not None
        # The by-reference parameters must be 64-bit pointers on x64.
        self.assertEqual(ctypes.sizeof(ctypes.c_void_p), 8)
        self.assertEqual(ctypes.sizeof(lib.SIMOP.argtypes[1]), 8)
        self.assertEqual(ctypes.sizeof(lib.BETAAMD.argtypes[2]), 8)


class LoadErrorTest(unittest.TestCase):
    """A missing library must raise a clear, actionable error."""

    def test_missing_explicit_path_raises(self):
        # A bad explicit path must raise even when the MODSIM environment
        # variable would otherwise resolve the library, so the test is
        # deterministic regardless of the developer's environment.
        with mock.patch.dict(os.environ, {MODSIM_ENV_VAR: ""}):
            with self.assertRaises(EngineLoadError):
                ModsimEngine(library_path=os.path.join("does", "not", "exist.dll"))


if __name__ == "__main__":
    unittest.main(verbosity=2)
