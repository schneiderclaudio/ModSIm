"""Self-tests for the ModSIM unit-conversion module.

Run with::

    C:\\Python314\\python.exe -m pytest tests/test_conversion.py

or directly::

    C:\\Python314\\python.exe tests/test_conversion.py
"""

import math
import os
import sys

# Allow running the file directly without installing the package.
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from modsim.units.conversion import (  # noqa: E402
    ConversionTable,
    UnknownFamilyError,
    UnknownUnitError,
    convert,
    get_family,
    list_families,
    list_units,
    unit_in_family,
)


def approx(actual, expected, tol=1e-9):
    return abs(actual - expected) <= tol * max(1.0, abs(expected))


def test_length():
    assert approx(convert(1.0, "m", "cm"), 100.0)
    assert approx(convert(1.0, "m", "mm"), 1000.0)
    assert approx(convert(1.0, "ft", "m"), 0.3048)
    assert approx(convert(1.0, "in", "cm"), 2.54)
    assert approx(convert(1.0, "m", "ft"), 1.0 / 0.3048)


def test_mass_flow():
    assert approx(convert(1.0, "kg/s", "t/h"), 3.6)
    assert approx(convert(3.6, "t/h", "kg/s"), 1.0)
    # 1 short ton/hour = 2000 lb/h = 907.18474 kg/h
    assert approx(convert(1.0, "stonphr", "kg/s"), 907.18474 / 3600.0)
    assert approx(convert(1.0, "lb/h", "kg/s"), 0.45359237 / 3600.0)


def test_volumetric_flow():
    assert approx(convert(1.0, "m3/s", "m3/h"), 3600.0)
    assert approx(convert(1.0, "L/s", "m3/h"), 3.6)
    assert approx(convert(1.0, "m3/h", "L/s"), 1.0 / 3.6)


def test_power():
    assert approx(convert(1.0, "kW", "W"), 1000.0)
    assert approx(convert(1.0, "MW", "kW"), 1000.0)
    assert approx(convert(1.0, "hp", "kW"), 0.7456998715822702)


def test_density():
    assert approx(convert(1.0, "t/m3", "kg/m3"), 1000.0)
    assert approx(convert(1.0, "g/cm3", "kg/m3"), 1000.0)
    assert approx(convert(1.0, "g/cm3", "t/m3"), 1.0)


def test_pressure():
    assert approx(convert(1.0, "MPa", "kPa"), 1000.0)
    assert approx(convert(1.0, "bar", "kPa"), 100.0)
    assert approx(convert(1.0, "psi", "kPa"), 6.894757293168361)


def test_energy():
    assert approx(convert(1.0, "kWh", "MJ"), 3.6)
    assert approx(convert(1.0, "MWh", "kWh"), 1000.0)


def test_temperature_affine():
    assert approx(convert(0.0, "C", "F"), 32.0)
    assert approx(convert(100.0, "C", "F"), 212.0)
    assert approx(convert(0.0, "C", "K"), 273.15)
    assert approx(convert(32.0, "F", "C"), 0.0)
    assert approx(convert(212.0, "F", "C"), 100.0)
    assert approx(convert(273.15, "K", "C"), 0.0)
    # Round-trip through the base unit.
    assert approx(convert(convert(98.6, "F", "C"), "C", "F"), 98.6)


def test_identity():
    for family in list_families():
        for unit in list_units(family):
            assert approx(convert(7.5, unit, unit), 7.5), (family, unit)


def test_list_units():
    assert list_units("length") == ["cm", "ft", "in", "m", "mm"]
    assert "stonphr" in list_units("mass_flow")
    assert "C" in list_units("temperature")


def test_list_families():
    families = list_families()
    for expected in (
        "length",
        "mass_flow",
        "volumetric_flow",
        "power",
        "density",
        "pressure",
        "energy",
        "temperature",
    ):
        assert expected in families


def test_get_family():
    family = get_family("length")
    assert family.base_unit == "m"
    assert family.contains("ft")
    assert not family.contains("kW")


def test_unit_in_family():
    assert unit_in_family("ft", "length")
    assert not unit_in_family("kW", "length")


def test_unknown_unit_raises():
    try:
        convert(1.0, "m", "bogus")
    except UnknownUnitError:
        pass
    else:
        raise AssertionError("Expected UnknownUnitError")


def test_unknown_family_raises():
    try:
        get_family("bogus")
    except UnknownFamilyError:
        pass
    else:
        raise AssertionError("Expected UnknownFamilyError")


def test_cross_family_rejected():
    # 'm' is length, 'kW' is power -> must raise.
    try:
        convert(1.0, "m", "kW")
    except UnknownUnitError:
        pass
    else:
        raise AssertionError("Expected UnknownUnitError for cross-family convert")


def test_custom_table():
    table = ConversionTable()
    assert "length" in table.list_families()
    assert approx(table.convert(2.0, "m", "cm"), 200.0)


def _run_all():
    tests = [
        (name, obj)
        for name, obj in sorted(globals().items())
        if name.startswith("test_") and callable(obj)
    ]
    failures = 0
    for name, fn in tests:
        try:
            fn()
            print(f"PASS  {name}")
        except Exception as exc:  # noqa: BLE001
            failures += 1
            print(f"FAIL  {name}: {exc!r}")
    print(f"\n{len(tests) - failures}/{len(tests)} tests passed")
    return failures


if __name__ == "__main__":
    sys.exit(1 if _run_all() else 0)
