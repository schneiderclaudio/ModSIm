"""Unit-conversion tables for the ModSIM GUI.

This module provides a data-driven set of unit-conversion "families"
(dimensions). Each family has a base unit and a table mapping every supported
unit to a conversion specification relative to that base.

Two kinds of conversion specification are supported:

* **Linear (scalar factor)** -- ``value_in_base = value * factor``. Used for
  most dimensions (length, mass flow, power, ...).
* **Affine (offset-aware callable)** -- used for temperature, where the
  relationship between scales is ``value_in_base = a * value + b`` (e.g.
  Celsius to Kelvin is ``K = C + 273.15``, not a plain multiplication).

The module is intentionally dependency-free (pure Python) so it can be reused
by every equipment dialog and data-entry form without pulling in extra
dependencies.

Typical usage::

    from modsim.units.conversion import convert, get_family, list_units

    convert(1.0, "m", "cm")          # 100.0
    convert(1.0, "kg/s", "t/h")      # 3.6
    convert(0.0, "C", "F")           # 32.0
    list_units("length")             # ["m", "cm", "mm", "ft", "in"]
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Dict, Iterable, List, Optional, Tuple, Union

__all__ = [
    "ConversionError",
    "UnknownUnitError",
    "UnknownFamilyError",
    "ConversionSpec",
    "UnitFamily",
    "ConversionTable",
    "convert",
    "get_family",
    "list_units",
    "unit_in_family",
    "FAMILIES",
]


class ConversionError(Exception):
    """Base error for all unit-conversion failures."""


class UnknownUnitError(ConversionError):
    """Raised when a unit is not recognised within a family."""


class UnknownFamilyError(ConversionError):
    """Raised when a family name is not recognised."""


# A conversion spec is either a plain scalar factor (linear) or a callable
# that maps a value in this unit to the family's base unit.
Factor = Union[float, int]
ToBase = Callable[[float], float]
ConversionSpec = Union[Factor, ToBase]


@dataclass(frozen=True)
class UnitFamily:
    """A dimension (e.g. length) with a base unit and unit conversion specs.

    ``units`` maps a unit name to a conversion specification. For linear
    units the spec is the factor to multiply by to obtain the base-unit
    value. For affine units (temperature) the spec is a callable that
    converts a value in that unit to the base unit.
    """

    name: str
    base_unit: str
    units: Dict[str, ConversionSpec]

    def __post_init__(self) -> None:
        if self.base_unit not in self.units:
            raise ValueError(
                f"Family {self.name!r}: base unit {self.base_unit!r} "
                "must be present in the units table"
            )

    def to_base(self, value: float, unit: str) -> float:
        """Convert ``value`` expressed in ``unit`` to the family base unit."""
        spec = self._spec(unit)
        if callable(spec):
            return spec(value)
        return value * spec

    def from_base(self, value: float, unit: str) -> float:
        """Convert ``value`` expressed in the base unit to ``unit``."""
        spec = self._spec(unit)
        if callable(spec):
            # Invert the affine mapping numerically. Affine specs are
            # monotonic, so a simple inverse is well defined.
            return _invert_affine(spec, value)
        return value / spec

    def _spec(self, unit: str) -> ConversionSpec:
        try:
            return self.units[unit]
        except KeyError:
            raise UnknownUnitError(
                f"Unit {unit!r} is not a member of family {self.name!r}. "
                f"Available units: {sorted(self.units)}"
            ) from None

    def contains(self, unit: str) -> bool:
        """Return True if ``unit`` belongs to this family."""
        return unit in self.units


def _invert_affine(func: ToBase, target: float) -> float:
    """Invert a monotonic affine callable ``func`` for a target base value.

    The inverse of ``y = a * x + b`` is ``x = (y - b) / a``. We recover
    ``a`` and ``b`` by sampling two points.
    """
    x0, x1 = 0.0, 1.0
    y0, y1 = func(x0), func(x1)
    a = y1 - y0
    if a == 0:
        raise ConversionError("Cannot invert a constant conversion function")
    b = y0 - a * x0
    return (target - b) / a


# ---------------------------------------------------------------------------
# Family definitions
# ---------------------------------------------------------------------------

# Temperature base is Kelvin. Each unit maps to K via an affine callable.
_TEMPERATURE = UnitFamily(
    name="temperature",
    base_unit="K",
    units={
        "K": lambda k: k,
        "C": lambda c: c + 273.15,
        "F": lambda f: (f - 32.0) * 5.0 / 9.0 + 273.15,
    },
)

FAMILIES: Dict[str, UnitFamily] = {
    family.name: family
    for family in (
        UnitFamily(
            name="length",
            base_unit="m",
            units={
                "m": 1.0,
                "cm": 0.01,
                "mm": 0.001,
                "ft": 0.3048,
                "in": 0.0254,
            },
        ),
        UnitFamily(
            name="mass_flow",
            base_unit="kg/s",
            units={
                "kg/s": 1.0,
                "t/h": 1000.0 / 3600.0,  # 1 t/h = 1000 kg / 3600 s
                "stonphr": 907.18474 / 3600.0,  # short ton (2000 lb) per hour
                "lb/h": 0.45359237 / 3600.0,
            },
        ),
        UnitFamily(
            name="volumetric_flow",
            base_unit="m3/h",
            units={
                "m3/h": 1.0,
                "m3/s": 3600.0,
                "L/s": 3.6,  # 1 L/s = 0.001 m3/s = 3.6 m3/h
                "ft3/min": 1.69901079552,  # 1 ft3/min = 0.028316846592 m3/s
            },
        ),
        UnitFamily(
            name="power",
            base_unit="kW",
            units={
                "kW": 1.0,
                "MW": 1000.0,
                "hp": 0.7456998715822702,  # mechanical horsepower
                "W": 0.001,
            },
        ),
        UnitFamily(
            name="density",
            base_unit="kg/m3",
            units={
                "kg/m3": 1.0,
                "t/m3": 1000.0,
                "g/cm3": 1000.0,
            },
        ),
        UnitFamily(
            name="pressure",
            base_unit="kPa",
            units={
                "kPa": 1.0,
                "MPa": 1000.0,
                "bar": 100.0,
                "psi": 6.894757293168361,
            },
        ),
        UnitFamily(
            name="energy",
            base_unit="kWh",
            units={
                "kWh": 1.0,
                "MJ": 1.0 / 3.6,  # 1 kWh = 3.6 MJ
                "MWh": 1000.0,
            },
        ),
        _TEMPERATURE,
    )
}


class ConversionTable:
    """A registry of unit families with convenience lookup helpers.

    This is the primary object used by dialogs to discover available units
    and perform conversions.
    """

    def __init__(self, families: Optional[Iterable[UnitFamily]] = None) -> None:
        self._families: Dict[str, UnitFamily] = {}
        for family in families if families is not None else FAMILIES.values():
            self._families[family.name] = family

    def get_family(self, name: str) -> UnitFamily:
        """Return the family with the given name.

        Raises :class:`UnknownFamilyError` if the family does not exist.
        """
        try:
            return self._families[name]
        except KeyError:
            raise UnknownFamilyError(
                f"Unknown unit family {name!r}. "
                f"Available families: {sorted(self._families)}"
            ) from None

    def list_units(self, family: str) -> List[str]:
        """Return the sorted unit names available in ``family``."""
        return sorted(self.get_family(family).units)

    def list_families(self) -> List[str]:
        """Return the sorted names of all registered families."""
        return sorted(self._families)

    def unit_in_family(self, unit: str, family: str) -> bool:
        """Return True if ``unit`` belongs to ``family``."""
        return self.get_family(family).contains(unit)

    def convert(self, value: float, from_unit: str, to_unit: str) -> float:
        """Convert ``value`` from ``from_unit`` to ``to_unit``.

        The two units must belong to the same family. The family is inferred
        from ``from_unit``.
        """
        family = self._family_for_unit(from_unit)
        if not family.contains(to_unit):
            raise UnknownUnitError(
                f"Unit {to_unit!r} is not a member of family "
                f"{family.name!r}. Available units: {sorted(family.units)}"
            )
        base_value = family.to_base(value, from_unit)
        return family.from_base(base_value, to_unit)

    def _family_for_unit(self, unit: str) -> UnitFamily:
        for family in self._families.values():
            if family.contains(unit):
                return family
        raise UnknownUnitError(
            f"Unit {unit!r} does not belong to any registered family. "
            f"Available families: {sorted(self._families)}"
        )


# A default, module-level registry shared across the application.
DEFAULT_TABLE = ConversionTable()


# ---------------------------------------------------------------------------
# Module-level convenience API
# ---------------------------------------------------------------------------

def convert(value: float, from_unit: str, to_unit: str) -> float:
    """Convert ``value`` from ``from_unit`` to ``to_unit``.

    Both units must belong to the same family. See :class:`ConversionTable`
    for details.
    """
    return DEFAULT_TABLE.convert(value, from_unit, to_unit)


def get_family(name: str) -> UnitFamily:
    """Return the unit family with the given name."""
    return DEFAULT_TABLE.get_family(name)


def list_units(family: str) -> List[str]:
    """Return the sorted unit names available in ``family``."""
    return DEFAULT_TABLE.list_units(family)


def list_families() -> List[str]:
    """Return the sorted names of all registered families."""
    return DEFAULT_TABLE.list_families()


def unit_in_family(unit: str, family: str) -> bool:
    """Return True if ``unit`` belongs to ``family``."""
    return DEFAULT_TABLE.unit_in_family(unit, family)
