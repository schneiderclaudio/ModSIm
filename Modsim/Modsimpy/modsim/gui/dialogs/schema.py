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
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 6.0),
            _f("product_size", "Product size", "m", 0.0, 5.0, 6.0),
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
            _f("split_1", "Split fraction 1", None, 0.0, 1.0, 2.0),
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
            _f("diameter", "Cyclone diameter", "m", 0.01, 2.0, 2.75),
            _f("apex_diameter", "Apex (spigot) diameter", "m", 0.001, 1.0, 0.15),
            _f("vortex_finder", "Vortex finder diameter", "m", 0.001, 1.0, 2.0),
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
            _f("pulp_level", "Pulp level", "m", 0.0, 10.0, 300.0),
            _f("residence_time", "Residence time", "min", 0.0, 1000.0, 15.0),
            _f("recovery", "Target recovery", None, 0.0, 1.0, 0.0),
            _f("k_rate", "Flotation rate constant", "1/min", 0.0, 100.0, 0.12),
            _f("frother_dosage", "Frother dosage", "g/t", 0.0, 1000.0, 0.8),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 2.5),
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
            _f("pulp_level", "Pulp level", "m", 0.0, 10.0, 90.0),
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
            _f("pulp_level", "Pulp level", "m", 0.0, 10.0, 90.0),
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
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 1.65),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 2.0),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 1.0),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 0.2),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 20.0),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 75.0),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 25.0),
            _f("ore_hardness", "Ore hardness index", None, 0.0, 100.0, 8.0),
            _f("grinding_media", "Grinding media density", "t/m3", 0.0, 20.0, 6.0),
            _f("mill_filling", "Mill filling fraction", None, 0.0, 1.0, 4.0),
            _f("liner_life", "Liner life", "h", 0.0, 100000.0, 2.0),
            _f("motor_efficiency", "Motor efficiency", None, 0.0, 1.0, 35.0),
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
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 1.65),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 2.0),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 1.0),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 0.254),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 20.0),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 75.0),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 25.0),
            _f("ore_hardness", "Ore hardness index", None, 0.0, 100.0, 1.89),
            _f("grinding_media", "Grinding media density", "t/m3", 0.0, 20.0, 1.0),
            _f("mill_filling", "Mill filling fraction", None, 0.0, 1.0, 0.64),
            _f("liner_life", "Liner life", "h", 0.0, 100000.0, 0.5),
            _f("motor_efficiency", "Motor efficiency", None, 0.0, 1.0, 30.0),
            _f("bearing_pressure", "Bearing pressure", "kPa", 0.0, 100000.0, 12.0),
            _f("lubrication_flow", "Lubrication flow", "m3/h", 0.0, 1000.0, 80.0),
            _f("cooling_water", "Cooling water flow", "m3/h", 0.0, 1000.0, 72.0),
            _f("gear_ratio", "Gear ratio", None, 0.0, 100.0, 6.0),
            _f("trunnion_diameter", "Trunnion diameter", "m", 0.0, 5.0, 96.0),
            _f("shell_thickness", "Shell thickness", "m", 0.0, 1.0, 1.17),
            _f("liner_thickness", "Liner thickness", "m", 0.0, 1.0, 1.26),
            _f("discharge_grate", "Discharge grate opening", "m", 0.0, 1.0, 6.0),
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
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 1.65),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 2.0),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 1.0),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 0.2),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 20.0),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 75.0),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 25.0),
            _f("ore_hardness", "Ore hardness index", None, 0.0, 100.0, 8.0),
            _f("grinding_media", "Grinding media density", "t/m3", 0.0, 20.0, 6.0),
            _f("mill_filling", "Mill filling fraction", None, 0.0, 1.0, 4.0),
            _f("liner_life", "Liner life", "h", 0.0, 100000.0, 2.0),
            _f("motor_efficiency", "Motor efficiency", None, 0.0, 1.0, 35.0),
            _f("bearing_pressure", "Bearing pressure", "kPa", 0.0, 100000.0, 10.0),
            _f("lubrication_flow", "Lubrication flow", "m3/h", 0.0, 1000.0, 50.0),
            _f("cooling_water", "Cooling water flow", "m3/h", 0.0, 1000.0, 72.0),
            _f("gear_ratio", "Gear ratio", None, 0.0, 100.0, 40.0),
            _f("trunnion_diameter", "Trunnion diameter", "m", 0.0, 5.0, 96.0),
            _f("shell_thickness", "Shell thickness", "m", 0.0, 1.0, 1.17),
            _f("liner_thickness", "Liner thickness", "m", 0.0, 1.0, 1.26),
            _f("discharge_grate", "Discharge grate opening", "m", 0.0, 1.0, 6.0),
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
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 0.748),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 0.0),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 0.3994),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 0.72),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 10.0),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 2.513),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 50.0),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 1.0),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 2.0),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 1.0),
            _f("ore_hardness", "Ore hardness index", None, 0.0, 100.0, 10.0),
            _f("grinding_media", "Grinding media density", "t/m3", 0.0, 20.0, 40.0),
            _f("mill_filling", "Mill filling fraction", None, 0.0, 1.0, 75.0),
            _f("liner_life", "Liner life", "h", 0.0, 100000.0, 40.0),
            _f("motor_efficiency", "Motor efficiency", None, 0.0, 1.0, 10.0),
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
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 1.65),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 2.0),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 1.0),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 0.2),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 20.0),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 75.0),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 25.0),
            _f("ore_hardness", "Ore hardness index", None, 0.0, 100.0, 8.0),
            _f("grinding_media", "Grinding media density", "t/m3", 0.0, 20.0, 6.0),
            _f("mill_filling", "Mill filling fraction", None, 0.0, 1.0, 4.0),
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
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 1.65),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 2.0),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 1.0),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 0.2),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 20.0),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 75.0),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 25.0),
            _f("ore_hardness", "Ore hardness index", None, 0.0, 100.0, 8.0),
            _f("grinding_media", "Grinding media density", "t/m3", 0.0, 20.0, 6.0),
            _f("mill_filling", "Mill filling fraction", None, 0.0, 1.0, 4.0),
            _f("liner_life", "Liner life", "h", 0.0, 100000.0, 2.0),
            _f("motor_efficiency", "Motor efficiency", None, 0.0, 1.0, 35.0),
            _f("bearing_pressure", "Bearing pressure", "kPa", 0.0, 100000.0, 10.0),
            _f("lubrication_flow", "Lubrication flow", "m3/h", 0.0, 1000.0, 50.0),
            _f("cooling_water", "Cooling water flow", "m3/h", 0.0, 1000.0, 72.0),
            _f("gear_ratio", "Gear ratio", None, 0.0, 100.0, 40.0),
            _f("trunnion_diameter", "Trunnion diameter", "m", 0.0, 5.0, 96.0),
            _f("shell_thickness", "Shell thickness", "m", 0.0, 1.0, 1.17),
            _f("liner_thickness", "Liner thickness", "m", 0.0, 1.0, 1.26),
            _f("discharge_grate", "Discharge grate opening", "m", 0.0, 1.0, 6.0),
            _f("feed_chute", "Feed chute length", "m", 0.0, 10.0, 6.0),
            _f("discharge_cone", "Discharge cone angle", "deg", 0.0, 90.0, 6.0),
            _f("trunnion_bearing", "Trunnion bearing type", None, None, None, None),
            _f("lubrication_type", "Lubrication type", None, None, None, None),
            _f("cooling_type", "Cooling type", None, None, None, None),
            _f("drive_type", "Drive type", None, None, None, None),
            _f("gear_type", "Gear type", None, None, None, None),
            _f("shell_material", "Shell material", None, None, None, None),
            _f("liner_material", "Liner material", None, None, None, None),
            _f("ball_material", "Ball material", None, None, None, None),
            _f("feed_type", "Feed type", None, None, None, None),
            _f("discharge_type", "Discharge type", None, None, None, None),
            _f("control_mode", "Control mode", None, None, None, None),
            _f("alarm_level", "Alarm level", None, None, None, None),
            _f("shutdown_level", "Shutdown level", None, None, None, None),
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
            _f("deck_angle", "Deck angle", "deg", 0.0, 90.0, -0.331),
            _f("amplitude", "Vibration amplitude", "mm", 0.0, 50.0, 2.17),
            _f("frequency", "Vibration frequency", "Hz", 0.0, 100.0, 0.696),
            _f("cut_size", "Cut size", "mm", 0.0, 100.0, 0.0),
            _f("efficiency", "Screen efficiency", None, 0.0, 1.0, 0.79),
            _f("feed_rate", "Feed rate", "t/h", 0.0, 100000.0, 5.36),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 6.25),
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
            _f("deck_number", "Number of decks", None, 1.0, 5.0, 0.0),
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
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 1.65),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 2.0),
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
            _f("ball_charge", "Ball charge fraction", None, 0.0, 0.6, 1.65),
            _f("ball_size", "Ball size", "mm", 0.0, 200.0, 3.3),
            _f("power", "Installed power", "kW", 0.0, 100000.0, 50.0),
            _f("speed", "Mill speed", "rpm", 0.0, 100.0, 1.0),
            _f("critical_speed", "Fraction of critical speed", None, 0.0, 1.0, 2.0),
            _f("feed_size", "Maximum feed size", "m", 0.0, 5.0, 1.0),
            _f("discharge_size", "Discharge size", "m", 0.0, 5.0, 0.2),
            _f("solids_fraction", "Feed solids fraction", None, 0.0, 1.0, 20.0),
            _f("circulating_load", "Circulating load", None, 0.0, 10.0, 75.0),
            _f("classifier_efficiency", "Classifier efficiency", None, 0.0, 1.0, 25.0),
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
            _f("depth", "Thickener depth", "m", 0.5, 50.0, 0.01),
            _f("underflow_density", "Underflow density", "t/m3", 0.0, 10.0, 1.0),
            _f("feed_density", "Feed density", "t/m3", 0.0, 10.0, 0.01),
            _f("settling_rate", "Settling rate", "m/h", 0.0, 100.0, 1.58),
            _f("flocculant_dosage", "Flocculant dosage", "g/t", 0.0, 1000.0, 0.0),
            _f("rake_speed", "Rake speed", "rpm", 0.0, 100.0, 0.0),
        ],
    )
)

# A convenience alias so callers can iterate over every registered schema.
ALL_SCHEMAS: List[EquipmentSchema] = list(SCHEMAS.values())
