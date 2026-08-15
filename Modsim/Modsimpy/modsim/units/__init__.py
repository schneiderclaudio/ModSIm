"""Unit-conversion tables for the ModSIM GUI.

Provides data-driven conversion families (length, mass flow, volumetric
flow, power, density, pressure, energy, temperature) and a shared conversion
API used by equipment dialogs and data-entry forms.
"""

from modsim.units.conversion import (
    ConversionError,
    ConversionTable,
    FAMILIES,
    UnknownFamilyError,
    UnknownUnitError,
    UnitFamily,
    convert,
    get_family,
    list_families,
    list_units,
    unit_in_family,
)

__all__ = [
    "ConversionError",
    "ConversionTable",
    "FAMILIES",
    "UnknownFamilyError",
    "UnknownUnitError",
    "UnitFamily",
    "convert",
    "get_family",
    "list_families",
    "list_units",
    "unit_in_family",
]
