## ADDED Requirements

### Requirement: Equipment parameter dialogs
The system SHALL provide parameter-entry dialogs for equipment models that read and write the corresponding job-file data sections.

#### Scenario: Open an equipment dialog
- **WHEN** the user selects an equipment model on the flowsheet
- **THEN** a dialog opens showing the model's parameters

#### Scenario: Edit and persist parameters
- **WHEN** the user edits parameters and confirms
- **THEN** the values are written to the job-file data section for that model

### Requirement: Data-driven form generation
The system SHALL generate equipment dialogs from a schema (fields, types, units, bounds) where a model is defined by a schema rather than a hand-written form.

#### Scenario: New model from schema
- **WHEN** a new equipment model is defined by a schema
- **THEN** a dialog is generated from the schema without writing a new form

### Requirement: Parameter validation
The system SHALL validate parameter values against schema bounds before writing.

#### Scenario: Out-of-bounds value
- **WHEN** the user enters a parameter value outside the schema bounds
- **THEN** the system flags the value and does not write it until corrected
