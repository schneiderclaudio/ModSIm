## ADDED Requirements

### Requirement: Unit conversion tables
The system SHALL provide unit-conversion tables (e.g., m/cm/ft, kgps/stonphr) as a data-driven conversion module.

#### Scenario: Convert a value
- **WHEN** the user enters a value in one unit and selects a target unit
- **THEN** the system converts the value using the conversion table

### Requirement: Shared conversion across dialogs
The system SHALL make the conversion module available to all equipment dialogs and data-entry forms.

#### Scenario: Conversion in a dialog
- **WHEN** an equipment dialog displays a parameter with a unit
- **THEN** the dialog uses the shared conversion module for unit handling
