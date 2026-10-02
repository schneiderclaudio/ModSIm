"""Data-driven schemas for ModSIM equipment parameter dialogs.

An :class:`EquipmentSchema` describes an equipment model purely as data: a
model code plus an ordered list of :class:`Field` definitions (key, label,
type, unit, bounds, default, choices).  The dialog generator
(:mod:`modsim.gui.dialogs.generator`) turns a schema into a ``QDialog`` form
without any hand-written form code, so adding a new equipment model is just a
matter of registering a new schema.

Field order is significant: it maps positionally onto the flat parameter list
stored in a ``.cur`` file (``CurUnit.params``).  Field ``i`` corresponds to
``params[i]``.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Dict, List, Optional

__all__ = [
    "FieldType",
    "Field",
    "EquipmentSchema",
    "SCHEMAS",
    "get_schema",
    "list_schemas",
    "register_schema",
]


class FieldType(Enum):
    """The data type of a schema field, which selects the input widget."""

    FLOAT = "float"
    INT = "int"
    STR = "str"
    BOOL = "bool"
    CHOICE = "choice"


@dataclass(frozen=True)
class Field:
    """A single parameter field in an equipment schema.

    ``key`` is the machine-readable name used to index values; ``label`` is
    the human-readable text shown next to the input.  ``unit`` is a unit name
    recognised by :mod:`modsim.units.conversion` (e.g. ``"m"``, ``"kW"``,
    ``"kPa"``); it is displayed as a suffix and used for unit handling.
    ``min``/``max`` are the schema bounds used for validation.  ``choices``
    supplies the options for :attr:`FieldType.CHOICE` fields.
    """

    key: str
    label: str
    type: FieldType = FieldType.FLOAT
    unit: Optional[str] = None
    min: Optional[float] = None
    max: Optional[float] = None
    default: Any = None
    choices: Optional[List[str]] = None
    help: Optional[str] = None

    def __post_init__(self) -> None:
        if self.type is FieldType.CHOICE and not self.choices:
            raise ValueError(
                f"Field {self.key!r} is a CHOICE field but has no choices"
            )


@dataclass(frozen=True)
class EquipmentSchema:
    """A complete description of one equipment model.

    ``code`` is the model code used in ``.cur`` files (e.g. ``"HFSU"``).
    ``fields`` are ordered and map positionally onto the unit's parameter
    list.
    """

    code: str
    name: str
    description: str = ""
    fields: List[Field] = field(default_factory=list)

    def field_keys(self) -> List[str]:
        """Return the ordered list of field keys."""
        return [f.key for f in self.fields]


# ---------------------------------------------------------------------------
# Field construction helpers (keeps schema definitions concise)
# ---------------------------------------------------------------------------

def _f(
    key: str,
    label: str,
    unit: Optional[str] = None,
    min: Optional[float] = None,
    max: Optional[float] = None,
    default: Any = None,
    help: Optional[str] = None,
) -> Field:
    return Field(
        key=key,
        label=label,
        type=FieldType.FLOAT,
        unit=unit,
        min=min,
        max=max,
        default=default,
        help=help,
    )


def _i(
    key: str,
    label: str,
    unit: Optional[str] = None,
    min: Optional[float] = None,
    max: Optional[float] = None,
    default: Any = None,
) -> Field:
    return Field(
        key=key,
        label=label,
        type=FieldType.INT,
        unit=unit,
        min=min,
        max=max,
        default=default,
    )


def _s(key: str, label: str, default: Any = None) -> Field:
    return Field(key=key, label=label, type=FieldType.STR, default=default)


def _b(key: str, label: str, default: bool = False) -> Field:
    return Field(key=key, label=label, type=FieldType.BOOL, default=default)


def _c(key: str, label: str, choices: List[str], default: Any = None) -> Field:
    return Field(
        key=key,
        label=label,
        type=FieldType.CHOICE,
        choices=choices,
        default=default,
    )


# ---------------------------------------------------------------------------
# Schema registry
# ---------------------------------------------------------------------------

SCHEMAS: Dict[str, EquipmentSchema] = {}


def register_schema(schema: EquipmentSchema) -> EquipmentSchema:
    """Register ``schema`` in the global registry keyed by its code."""
    SCHEMAS[schema.code] = schema
    return schema


def get_schema(code: str) -> EquipmentSchema:
    """Return the schema registered for ``code``.

    Raises :class:`KeyError` if no schema is registered for the code.
    """
    return SCHEMAS[code]


def list_schemas() -> List[str]:
    """Return the sorted codes of all registered schemas."""
    return sorted(SCHEMAS)


# ---------------------------------------------------------------------------
# Schema definitions
# ---------------------------------------------------------------------------

# --- Crushers --------------------------------------------------------------

register_schema(
    EquipmentSchema(
        code="JAW1",
        name="Jaw Crusher",
        description="Primary jaw crusher.",
        fields=[
            _f("closed_side_setting", "Closed-side setting", "m", 0.0, 1.0, 0.0254),
            _f("open_side_setting", "Open-side setting", "m", 0.0, 1.0, 0.12),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="GYRA",
        name="Gyratory Crusher",
        description="Primary gyratory crusher.",
        fields=[
            _f("closed_side_setting", "Closed-side setting", "m", 0.0, 1.0, 0.15),
            _f("open_side_setting", "Open-side setting", "m", 0.0, 1.0, 0.85),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 10880.0),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="CRSH",
        name="Cone Crusher",
        description="Secondary/tertiary cone crusher.",
        fields=[
            _f("closed_side_setting", "Closed-side setting", "m", 0.0, 1.0, 0.0254),
            _f("open_side_setting", "Open-side setting", "m", 0.0, 1.0, 0.2),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 12000.0),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 0.653),
            _f("reduction_ratio", "Reduction ratio", None, 1.0, 20.0, 1.6),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="CRS1",
        name="Roller Crusher",
        description="High-pressure roller crusher.",
        fields=[
            _f("closed_side_setting", "Closed-side setting", "m", 0.0, 1.0, 0.0254),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="CONE",
        name="Cone Crusher (Detailed)",
        description="Detailed cone crusher model.",
        fields=[
            _f("closed_side_setting", "Closed-side setting", "m", 0.0, 1.0, 0.002),
            _f("open_side_setting", "Open-side setting", "m", 0.0, 1.0, 1.0),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 9000.0),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 0.5),
            _f("product_size", "Product size", "m", 0.0, 5.0, 0.5),
            _f("eccentricity", "Eccentric throw", "m", 0.0, 1.0, 1.0),
            _f("speed", "Crusher speed", "rpm", 0.0, 1000.0, 1.0),
            _f("closed_side_ratio", "Closed-side ratio", None, 0.0, 10.0, 1.0),
            _f("open_side_ratio", "Open-side ratio", None, 0.0, 10.0, 2.0),
        ],
    )
)

# --- Conveyors / splitters -------------------------------------------------

register_schema(
    EquipmentSchema(
        code="CONV",
        name="Conveyor",
        description="Belt conveyor.",
        fields=[
            _f("belt_width", "Belt width", "m", 0.0, 10.0, 1.0),
            _f("belt_speed", "Belt speed", "m/s", 0.0, 20.0, 0.2),
            _f("length", "Conveyor length", "m", 0.0, 10000.0, 20.0),
            _f("lift", "Vertical lift", "m", 0.0, 1000.0, 2.0),
            _f("capacity", "Design capacity", "t/h", 0.0, 100000.0, 35.0),
            _f("power", "Drive power", "kW", 0.0, 100000.0, 1600.0),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="MIXR",
        name="Mixer",
        description="Stream mixer / junction (no parameters).",
        fields=[],
    )
)

register_schema(
    EquipmentSchema(
        code="SPLT",
        name="Splitter",
        description="Stream splitter.",
        fields=[
            _f("split_1", "Split fraction 1", None, 0.0, 1.0, 0.02),
            _f("split_2", "Split fraction 2", None, 0.0, 1.0, 0.5),
            _f("split_3", "Split fraction 3", None, 0.0, 1.0, 0.5),
        ],
    )
)

# --- Cyclones --------------------------------------------------------------

register_schema(
    EquipmentSchema(
        code="CYCA",
        name="Hydrocyclone",
        description="Classification hydrocyclone.",
        fields=[
            _f("diameter", "Cyclone diameter", "m", 0.01, 2.0, 0.2),
            _f("apex_diameter", "Apex (spigot) diameter", "m", 0.001, 1.0, 0.8),
            _f("vortex_finder", "Vortex finder diameter", "m", 0.0001, 1.0, 0.0004),
            _f("feed_pressure", "Feed pressure", "kPa", 0.0, 1000.0, 0.5),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 0.5),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="DMCY",
        name="Dense Medium Cyclone",
        description="Dense-medium separation cyclone.",
        fields=[
            _f("diameter", "Cyclone diameter", "m", 0.01, 2.0, 0.75),
            _f("apex_diameter", "Apex (spigot) diameter", "m", 0.001, 1.0, 0.15),
            _f("vortex_finder", "Vortex finder diameter", "m", 0.001, 1.0, 0.4),
        ],
    )
)

# --- Flotation -------------------------------------------------------------

register_schema(
    EquipmentSchema(
        code="FLTN",
        name="Flotation Cell",
        description="Mechanical flotation cell.",
        fields=[
            _f("cell_volume", "Cell volume", "m3", 0.0, 1000.0, 5.0),
            _f("air_rate", "Air rate", "m3/h", 0.0, 100000.0, 1.0),
            _f("froth_depth", "Froth depth", "m", 0.0, 5.0, 2.0),
            _f("pulp_level", "Pulp level", "m", 0.0, 10.0, 1.0),
            _f("residence_time", "Residence time", "min", 0.0, 1000.0, 15.0),
            _f("recovery", "Target recovery", None, 0.0, 1.0, 0.0),
            _f("k_rate", "Flotation rate constant", "1/min", 0.0, 100.0, 0.12),
            _f("frother_dosage", "Frother dosage", "g/t", 0.0, 1000.0, 0.8),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 0.025),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="FLTK",
        name="Flotation Column",
        description="Column flotation cell.",
        fields=[
            _f("column_diameter", "Column diameter", "m", 0.1, 10.0, 1.0),
            _f("column_height", "Column height", "m", 0.5, 30.0, 10.0),
            _f("air_rate", "Air rate", "m3/h", 0.0, 100000.0, 15.0),
            _f("wash_water", "Wash water rate", "m3/h", 0.0, 100000.0, 55.0),
            _f("froth_depth", "Froth depth", "m", 0.0, 5.0, 1.0),
            _f("bias", "Bias rate", "m3/h", 0.0, 100000.0, 90.0),
            _f("gas_holdup", "Gas holdup", None, 0.0, 1.0, 0.3),
            _f("pulp_level", "Pulp level", "m", 0.0, 10.0, 1.0),
            _f("k_rate", "Flotation rate constant", "1/min", 0.0, 100.0, 0.01),
            _f("recovery", "Target recovery", None, 0.0, 1.0, 0.5),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 0.5),
            _f("residence_time", "Residence time", "min", 0.0, 1000.0, 0.5),
            _f("frother_dosage", "Frother dosage", "g/t", 0.0, 1000.0, 0.5),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="KLIC",
        name="Column Flotation (Kynch)",
        description="Kynch column flotation model.",
        fields=[
            _f("column_diameter", "Column diameter", "m", 0.1, 10.0, 1.0),
            _f("column_height", "Column height", "m", 0.5, 30.0, 10.0),
            _f("air_rate", "Air rate", "m3/h", 0.0, 100000.0, 15.0),
            _f("wash_water", "Wash water rate", "m3/h", 0.0, 100000.0, 55.0),
            _f("froth_depth", "Froth depth", "m", 0.0, 5.0, 1.0),
            _f("bias", "Bias rate", "m3/h", 0.0, 100000.0, 90.0),
            _f("gas_holdup", "Gas holdup", None, 0.0, 1.0, 0.3),
            _f("pulp_level", "Pulp level", "m", 0.0, 10.0, 1.0),
            _f("k_rate", "Flotation rate constant", "1/min", 0.0, 100.0, 0.01),
        ],
    )
)

# --- Mills -----------------------------------------------------------------

register_schema(
    EquipmentSchema(
        code="SAGM",
        name="SAG Mill",
        description="Semi-autogenous grinding mill.",
        fields=[
            _f("diameter", "Mill diameter", "m", 0.5, 15.0, 1.0),
            _f("length", "Mill length", "m", 0.5, 15.0, 0.95),
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 0.0165),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 0.02),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 1.0),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 0.2),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 0.20),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 0.75),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 0.25),
            _f("ore_hardness", "Ore hardness index", None, 0.0, 100.0, 8.0),
            _f("grinding_media", "Grinding media density", "t/m3", 0.0, 20.0, 6.0),
            _f("mill_filling", "Mill filling fraction", None, 0.0, 1.0, 0.04),
            _f("liner_life", "Liner life", "h", 0.0, 100000.0, 2.0),
            _f("motor_efficiency", "Motor efficiency", None, 0.0, 1.0, 0.35),
            _f("bearing_pressure", "Bearing pressure", "kPa", 0.0, 100000.0, 10.0),
            _f("lubrication_flow", "Lubrication flow", "m3/h", 0.0, 1000.0, 50.0),
            _f("cooling_water", "Cooling water flow", "m3/h", 0.0, 1000.0, 72.0),
            _f("gear_ratio", "Gear ratio", None, 0.0, 100.0, 40.0),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="SAGT",
        name="SAG Mill (Test)",
        description="Semi-autogenous grinding mill (test variant).",
        fields=[
            _f("diameter", "Mill diameter", "m", 0.5, 15.0, 1.0),
            _f("length", "Mill length", "m", 0.5, 15.0, 0.95),
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 0.0165),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 0.02),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 1.0),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 0.254),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 0.20),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 0.75),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 0.25),
            _f("ore_hardness", "Ore hardness index", None, 0.0, 100.0, 1.89),
            _f("grinding_media", "Grinding media density", "t/m3", 0.0, 20.0, 1.0),
            _f("mill_filling", "Mill filling fraction", None, 0.0, 1.0, 0.64),
            _f("liner_life", "Liner life", "h", 0.0, 100000.0, 0.5),
            _f("motor_efficiency", "Motor efficiency", None, 0.0, 1.0, 0.30),
            _f("bearing_pressure", "Bearing pressure", "kPa", 0.0, 100000.0, 12.0),
            _f("lubrication_flow", "Lubrication flow", "m3/h", 0.0, 1000.0, 80.0),
            _f("cooling_water", "Cooling water flow", "m3/h", 0.0, 1000.0, 72.0),
            _f("gear_ratio", "Gear ratio", None, 0.0, 100.0, 6.0),
            _f("trunnion_diameter", "Trunnion diameter", "m", 0.0, 5.0, 0.96),
            _f("shell_thickness", "Shell thickness", "m", 0.0, 1.0, 0.017),
            _f("liner_thickness", "Liner thickness", "m", 0.0, 1.0, 0.0126),
            _f("discharge_grate", "Discharge grate opening", "m", 0.0, 1.0, 0.006),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="SAGL",
        name="SAG Mill (Large)",
        description="Large semi-autogenous grinding mill.",
        fields=[
            _f("diameter", "Mill diameter", "m", 0.5, 15.0, 1.0),
            _f("length", "Mill length", "m", 0.5, 15.0, 0.95),
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 0.0165),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 0.02),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 1.0),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 0.2),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 0.20),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 0.75),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 0.25),
            _f("ore_hardness", "Ore hardness index", None, 0.0, 100.0, 8.0),
            _f("grinding_media", "Grinding media density", "t/m3", 0.0, 20.0, 6.0),
            _f("mill_filling", "Mill filling fraction", None, 0.0, 1.0, 0.04),
            _f("liner_life", "Liner life", "h", 0.0, 100000.0, 2.0),
            _f("motor_efficiency", "Motor efficiency", None, 0.0, 1.0, 0.35),
            _f("bearing_pressure", "Bearing pressure", "kPa", 0.0, 100000.0, 10.0),
            _f("lubrication_flow", "Lubrication flow", "m3/h", 0.0, 1000.0, 50.0),
            _f("cooling_water", "Cooling water flow", "m3/h", 0.0, 1000.0, 72.0),
            _f("gear_ratio", "Gear ratio", None, 0.0, 100.0, 40.0),
            _f("trunnion_diameter", "Trunnion diameter", "m", 0.0, 5.0, 0.96),
            _f("shell_thickness", "Shell thickness", "m", 0.0, 1.0, 0.017),
            _f("liner_thickness", "Liner thickness", "m", 0.0, 1.0, 0.0126),
            _f("discharge_grate", "Discharge grate opening", "m", 0.0, 1.0, 0.006),
            _f("feed_chute", "Feed chute length", "m", 0.0, 10.0, 6.0),
            _f("discharge_cone", "Discharge cone angle", "deg", 0.0, 90.0, 6.0),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="FAGM",
        name="FAG Mill",
        description="Fully autogenous grinding mill.",
        fields=[
            _f("diameter", "Mill diameter", "m", 0.5, 15.0, 0.5),
            _f("length", "Mill length", "m", 0.5, 15.0, 3.723),
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 0.0),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 0.0),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 0.3994),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 0.72),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 0.10),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 2.513),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 0.2),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 1.0),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 2.0),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 1.0),
            _f("ore_hardness", "Ore hardness index", None, 0.0, 100.0, 10.0),
            _f("grinding_media", "Grinding media density", "t/m3", 0.0, 20.0, 7.8),
            _f("mill_filling", "Mill filling fraction", None, 0.0, 1.0, 0.75),
            _f("liner_life", "Liner life", "h", 0.0, 100000.0, 40.0),
            _f("motor_efficiency", "Motor efficiency", None, 0.0, 1.0, 0.10),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="GMIL",
        name="Grinding Mill",
        description="Generic grinding mill.",
        fields=[
            _f("diameter", "Mill diameter", "m", 0.5, 15.0, 1.0),
            _f("length", "Mill length", "m", 0.5, 15.0, 0.95),
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 0.0165),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 0.02),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 1.0),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 0.2),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 0.20),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 0.75),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 0.25),
            _f("ore_hardness", "Ore hardness index", None, 0.0, 100.0, 8.0),
            _f("grinding_media", "Grinding media density", "t/m3", 0.0, 20.0, 6.0),
            _f("mill_filling", "Mill filling fraction", None, 0.0, 1.0, 0.04),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="GMSU",
        name="Grinding Mill (SAG)",
        description="SAG grinding mill (detailed).",
        fields=[
            _f("diameter", "Mill diameter", "m", 0.5, 15.0, 1.0),
            _f("length", "Mill length", "m", 0.5, 15.0, 0.95),
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 0.0165),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 0.02),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 1.0),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 0.2),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 0.20),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 0.75),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 0.25),
            _f("ore_hardness", "Ore hardness index", None, 0.0, 100.0, 8.0),
            _f("grinding_media", "Grinding media density", "t/m3", 0.0, 20.0, 6.0),
            _f("mill_filling", "Mill filling fraction", None, 0.0, 1.0, 0.04),
            _f("liner_life", "Liner life", "h", 0.0, 100000.0, 2.0),
            _f("motor_efficiency", "Motor efficiency", None, 0.0, 1.0, 0.35),
            _f("bearing_pressure", "Bearing pressure", "kPa", 0.0, 100000.0, 10.0),
            _f("lubrication_flow", "Lubrication flow", "m3/h", 0.0, 1000.0, 50.0),
            _f("cooling_water", "Cooling water flow", "m3/h", 0.0, 1000.0, 72.0),
            _f("gear_ratio", "Gear ratio", None, 0.0, 100.0, 40.0),
            _f("trunnion_diameter", "Trunnion diameter", "m", 0.0, 5.0, 0.96),
            _f("shell_thickness", "Shell thickness", "m", 0.0, 1.0, 0.017),
            _f("liner_thickness", "Liner thickness", "m", 0.0, 1.0, 0.0126),
            _f("discharge_grate", "Discharge grate opening", "m", 0.0, 1.0, 0.006),
            _f("feed_chute", "Feed chute length", "m", 0.0, 10.0, 6.0),
            _f("discharge_cone", "Discharge cone angle", "deg", 0.0, 90.0, 6.0),
            _i("trunnion_bearing", "Trunnion bearing type", min=0, max=20, default=0),
            _i("lubrication_type", "Lubrication type", min=0, max=20, default=0),
            _i("cooling_type", "Cooling type", min=0, max=20, default=0),
            _i("drive_type", "Drive type", min=0, max=20, default=0),
            _i("gear_type", "Gear type", min=0, max=20, default=0),
            _i("shell_material", "Shell material", min=0, max=20, default=0),
            _i("liner_material", "Liner material", min=0, max=20, default=0),
            _i("ball_material", "Ball material", min=0, max=20, default=0),
            _i("feed_type", "Feed type", min=0, max=20, default=0),
            _i("discharge_type", "Discharge type", min=0, max=20, default=0),
            _i("control_mode", "Control mode", min=0, max=20, default=0),
            _i("alarm_level", "Alarm level", min=0, max=20, default=0),
            _i("shutdown_level", "Shutdown level", min=0, max=20, default=0),
        ],
    )
)

# --- Screens / high-frequency ----------------------------------------------

register_schema(
    EquipmentSchema(
        code="HFSU",
        name="High-Frequency Screen",
        description="High-frequency vibrating screen.",
        fields=[
            _f("screen_width", "Screen width", "m", 0.1, 10.0, 0.62),
            _f("screen_length", "Screen length", "m", 0.1, 20.0, 0.551),
            _f("deck_angle", "Deck angle", "deg", 0.0, 90.0, 1.0),
            _f("amplitude", "Vibration amplitude", "mm", 0.0, 50.0, 2.17),
            _f("frequency", "Vibration frequency", "Hz", 0.0, 100.0, 0.696),
            _f("cut_size", "Cut size", "mm", 0.0, 100.0, 0.0),
            _f("efficiency", "Screen efficiency", None, 0.0, 1.0, 0.79),
            _f("feed_rate", "Feed rate", "t/h", 0.0, 100000.0, 5.36),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 0.0625),
            _f("water_rate", "Wash water rate", "m3/h", 0.0, 100000.0, 42.0),
            _f("deck_area", "Deck area", "m2", 0.0, 1000.0, 74.0),
            _f("screen_open_area", "Open area fraction", None, 0.0, 1.0, 0.0),
            _f("mesh_size", "Mesh size", "mm", 0.0, 100.0, 0.0),
            _f("wire_diameter", "Wire diameter", "mm", 0.0, 50.0, 0.0),
            _f("stroke", "Stroke length", "mm", 0.0, 100.0, 0.0),
            _f("slope", "Screen slope", "deg", 0.0, 90.0, 0.0),
            _f("bearing_life", "Bearing life", "h", 0.0, 100000.0, 0.0),
            _f("motor_power", "Motor power", "kW", 0.0, 100000.0, 5000.0),
            _f("vibration_force", "Vibration force", "kN", 0.0, 100000.0, 0.0),
            _f("feed_chute", "Feed chute length", "m", 0.0, 10.0, 3.0),
            _f("discharge_weir", "Discharge weir height", "m", 0.0, 5.0, 0.0),
            _f("deck_number", "Number of decks", None, 1.0, 5.0, 1.0),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="HFMI",
        name="High-Frequency Mill",
        description="High-frequency mill.",
        fields=[
            _f("diameter", "Mill diameter", "m", 0.5, 15.0, 1.0),
            _f("length", "Mill length", "m", 0.5, 15.0, 0.95),
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 0.0165),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 0.02),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 1.0),
        ],
    )
)

register_schema(
    EquipmentSchema(
        code="HFML",
        name="High-Frequency Mill (Large)",
        description="Large high-frequency mill.",
        fields=[
            _f("diameter", "Mill diameter", "m", 0.5, 15.0, 1.0),
            _f("length", "Mill length", "m", 0.5, 15.0, 0.95),
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 0.0165),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 0.02),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 1.0),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 0.2),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 0.20),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 0.75),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 0.25),
        ],
    )
)

# --- Thickeners ------------------------------------------------------------

register_schema(
    EquipmentSchema(
        code="KYNC",
        name="Thickener (Kynch)",
        description="Kynch sedimentation thickener.",
        fields=[
            _f("diameter", "Thickener diameter", "m", 0.5, 200.0, 23.0),
            _f("depth", "Thickener depth", "m", 0.5, 50.0, 3.0),
            _f("underflow_density", "Underflow density", "t/m3", 0.0, 10.0, 1.0),
            _f("feed_density", "Feed density", "t/m3", 0.0, 10.0, 0.01),
            _f("settling_rate", "Settling rate", "m/h", 0.0, 100.0, 1.58),
            _f("flocculant_dosage", "Flocculant dosage", "g/t", 0.0, 1000.0, 0.0),
            _f("rake_speed", "Rake speed", "rpm", 0.0, 100.0, 0.0),
        ],
    )
)

# ---------------------------------------------------------------------------
# Legacy model catalog (from the VB6 GUI parameter forms)
# ---------------------------------------------------------------------------
# Models used by real ``.cur`` jobs that do not have a dedicated schema above.
# Each entry is ``(code, name, [parameter labels], target_count)``; ``label``
# order maps positionally onto the unit's parameter list (the order the VB6
# parameter form presents them, which is the order the engine reads them).
# When ``target_count`` exceeds the number of labels (observed ``NoPARAM`` for
# the code across the shipped jobs), the remaining fields are generic
# "Parameter N" fields so every engine-read value is exposed - and therefore
# preserved - by the dialog.
_LEGACY_MODELS = (
    ("SCRN", "Screen", ["Number of screens in parallel", "Surface water on screen oversize %", "Mesh size", "Length of screen", "Width of screen", "Transmission efficiency %"], 7),
    ("SCR1", "Wet Screen", ["Efficiency parameter", "Water split to underflow", "D50"], 3),
    ("SCR2", "Single-deck Screen (Karra)", ["Number of screens in parallel", "Screen length", "Screen width", "Bulk density of material", "Angle of inclination", "Water retained on oversize %", "Wire diameter", "Mesh size", "Surface water retained on oversize %"], 9),
    ("DSC1", "Double-deck Screen", ["Number of screens in parallel", "Width of screen", "Surface water on oversize % (deck 1)", "Transmission efficiency % (deck 1)", "Length of screen (deck 1)", "Mesh size (deck 1)", "Surface water on oversize % (deck 2)", "Transmission efficiency % (deck 2)", "Length of screen (deck 2)", "Mesh size (deck 2)"], 10),
    ("DSC2", "Double-deck Screen (Karra)", ["Number of screens in parallel", "Screen type (0=resilient, 1=woven wire, 2=punched steel)", "Bulk density of material", "Width of screen", "Angle of inclination", "Water retained on oversize % (deck 1)", "Length of screen (deck 1)", "Wire diameter (deck 1)", "Mesh size (deck 1)", "Water retained on oversize % (deck 2)", "Length of screen (deck 2)", "Wire diameter (deck 2)", "Mesh size (deck 2)"], 13),
    ("CSCR", "Screen (Empirical)", ["Width of screen", "Number of screens in parallel", "Angle of inclination", "Mesh size", "Surface water on screen oversize %", "Length of screen", "Open area %"], 9),
    ("DWSC", "Dewatering Screen", ["Mesh size", "Angle of vibration relative to screen", "Angle of inclination", "Vibration amplitude", "Vibration frequency (rpm)", "Width of screen", "Length of screen", "Ultimate moisture content %"], 8),
    ("PSCN", "Probability Screen", ["Number of screens in parallel", "Screen length", "Surface water on screen oversize %", "Screen aperture size", "Screen width", "Angle of inclination of screen", "Screen vibration throw angle", "Vibration frequency", "Amplitude of vibration"], 9),
    ("DRUM", "Dense-Medium Drum", ["Specific gravity of separation"], 1),
    ("WOCY", "Water-Only Cyclone", ["Specific gravity of separation"], 3),
    ("NOP_", "No Operation (Pass-Through)", [], 0),
    ("BLBX", "Black Box", [], 0),
    ("BLBS", "Black Box (Separator)", [], 1),
    ("CBOX", "Black Box (Control)", [], 1),
    ("CYCL", "Hydrocyclone (Plitt)", ["Slurry viscosity", "Feed head", "Vortex finder diameter", "Spigot diameter", "Inlet diameter", "Vortex-spigot distance", "Flow-split parameter", "Sharpness index parameter", "d50 parameter", "Factor for slurry density in separation zone", "Index for variation of d50 with density", "Number of cyclones in cluster", "Cyclone diameter"], 13),
    ("CYCB", "Classifier (Empirical)", [], 5),
    ("DOFI", "WHIMS (Dobby-Finch)", [], 7),
    ("ELUT", "Elutriator", ["Settling velocity at cut point", "Sharpness index", "Particle sphericity", "Short circuit to underflow"], 4),
    ("EMJC", "Crusher (Empirical)", [], 4),
    ("FAGT", "Autogenous Mill (Test)", [], 21),
    ("GMI1", "Ball Mill (3 Regions)", ["Specific rate at 1 mm", "Alpha", "Mu (mm)", "Lambda", "Phi at 5 mm", "Delta", "Gamma", "Beta", "Classification function", "Sharpness index", "D50 (mm)", "Hold-up in the mill", "Parameters for breakage function", "Parameters for selection function", "Hardgrove grindability index"], 15),
    ("JAW2", "Jaw Crusher", [], 2),
    ("JNES", "Magnetic Separator (Jones WHIMS)", [], 3),
    ("KLIM", "Flotation (Kinetic)", ["k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "Kinetic constant (1/min)", "G-class", "Ultimate recovery %"], 15),
    ("MILL", "Ball Mill (Selection/Breakage)", ["Residence time in the mill (mins)", "Lambda", "Delta", "Phi at 5 mm", "Gamma", "Beta", "Parameters for breakage function", "Selection function at 1 mm", "Alpha", "Parameters for selection function", "Mu (mm)"], 11),
    ("NAGE", "Mill (NAGE)", [], 10),
    ("RODL", "Rod Mill (Universal)", ["Exponential coefficient for decrease of velocity with size", "Residence time of water (mins)", "Lambda", "Delta", "Phi at 5 mm", "Gamma", "Beta", "Parameters for breakage function", "S1 (1/min)", "Alpha", "Parameters for selection function", "Mu (mm)"], 12),
    ("RODM", "Rod Mill (Plug Flow)", ["Exponential coefficient for decrease of velocity with size", "Residence time of water (mins)", "Lambda", "Delta", "Phi at 5 mm", "Gamma", "Beta", "Parameters for breakage function", "Selection function at 1 mm", "Alpha", "Parameters for selection function", "Mu (mm)"], 12),
    ("SHHD", "Short Head Crusher", [], 5),
    ("SJIG", "Jig (Single-stage)", [], 2),
    ("THIC", "Thickener", [], 1),
    ("WDM2", "Magnetic Concentrator (Wet Drum)", [], 5),
    ("WDMS", "Magnetic Concentrator (Drum)", ["Exponent for recovery curve", "Exponent for effect of size on bypass", "By-pass fraction"], 3),
    ("WICY", "Cyclone (Water Inlet)", ["Number of units in parallel", "Vortex finder diameter", "Feed pressure", "Truncated cone diameter", "Apex diameter"], 5),
)

for _code, _name, _labels, _target in _LEGACY_MODELS:
    _fields: List[Field] = [
        Field(key=f"p{i + 1}", label=label, type=FieldType.FLOAT, default=0.0)
        for i, label in enumerate(_labels)
    ]
    while len(_fields) < _target:
        _n = len(_fields) + 1
        _fields.append(
            Field(key=f"p{_n}", label=f"Parameter {_n}", type=FieldType.FLOAT, default=0.0)
        )
    register_schema(
        EquipmentSchema(
            code=_code,
            name=_name,
            description="Legacy ModSIM model parameter form.",
            fields=_fields,
        )
    )

# A convenience alias so callers can iterate over every registered schema.
ALL_SCHEMAS: List[EquipmentSchema] = list(SCHEMAS.values())
