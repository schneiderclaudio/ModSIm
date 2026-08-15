## ADDED Requirements

### Requirement: Flowsheet canvas renders units and streams
The system SHALL render a flowsheet as a 2D diagram on a `QGraphicsScene`/`QGraphicsView`, showing units as nodes and streams as connectors between them.

#### Scenario: Render a loaded flowsheet
- **WHEN** a job with a flowsheet is loaded
- **THEN** the canvas displays the units and their connecting streams

### Requirement: Drag-and-drop units
The system SHALL allow the user to add units to the canvas by dragging them from a palette and to reposition units by dragging.

#### Scenario: Add a unit
- **WHEN** the user drags a unit type from the palette onto the canvas
- **THEN** a new unit appears at the drop location

#### Scenario: Move a unit
- **WHEN** the user drags an existing unit to a new position
- **THEN** the unit moves and its connected streams follow

### Requirement: Draw and route streams
The system SHALL allow the user to create streams (connectors) between units and route them across the canvas.

#### Scenario: Connect two units
- **WHEN** the user draws a stream from one unit's port to another unit's port
- **THEN** a stream connector is created between the two units

### Requirement: Label and number units and streams
The system SHALL display and allow editing of unit and stream labels and numbers.

#### Scenario: Number units automatically
- **WHEN** units are added to the canvas
- **THEN** each unit is assigned a number and label

#### Scenario: Edit a label
- **WHEN** the user edits a unit's label
- **THEN** the label updates on the canvas

### Requirement: Persist and load layout
The system SHALL persist the flowsheet layout (unit positions, stream connections, labels) and reload it.

#### Scenario: Save layout
- **WHEN** the user saves the job
- **THEN** the flowsheet layout is written to the job files (e.g., `.syd` for stream connections)

#### Scenario: Load layout
- **WHEN** the user opens a job with a saved flowsheet
- **THEN** the canvas restores unit positions, streams, and labels
