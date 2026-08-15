"""Equipment parameter dialogs for the ModSIM GUI.

This package provides data-driven equipment parameter dialogs: schemas define
equipment models as data, and a generator turns a schema into a ``QDialog``
form with validation and job-file (``.cur``) read/write wiring.
"""

from .schema import (
    EquipmentSchema,
    Field,
    FieldType,
    SCHEMAS,
    get_schema,
    list_schemas,
    register_schema,
)
from .generator import (
    EquipmentDialog,
    edit_equipment,
    generate_dialog,
    load_values_from_job,
    validate_value,
    validate_values,
    write_values_to_job,
)

__all__ = [
    "EquipmentSchema",
    "Field",
    "FieldType",
    "SCHEMAS",
    "get_schema",
    "list_schemas",
    "register_schema",
    "EquipmentDialog",
    "edit_equipment",
    "generate_dialog",
    "load_values_from_job",
    "validate_value",
    "validate_values",
    "write_values_to_job",
]
