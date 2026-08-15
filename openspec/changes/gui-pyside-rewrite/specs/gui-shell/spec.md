## ADDED Requirements

### Requirement: Application shell launches
The system SHALL provide a PySide6 MDI application shell that launches the main window with a menu bar, toolbar, and status bar.

#### Scenario: Launch application
- **WHEN** the user starts the application (`python -m modsim`)
- **THEN** a main window opens with a menu bar, toolbar, and status bar

#### Scenario: MDI child windows
- **WHEN** the user opens a flowsheet
- **THEN** the flowsheet is displayed in an MDI sub-window within the main window

### Requirement: Job open and save
The system SHALL allow the user to open and save simulation jobs through the shell.

#### Scenario: Open a job
- **WHEN** the user selects Open from the File menu and chooses a job directory
- **THEN** the job's flowsheet and data are loaded into the application

#### Scenario: Save a job
- **WHEN** the user selects Save from the File menu
- **THEN** the current job's files are written to the job directory

### Requirement: Window management
The system SHALL provide standard MDI window management (cascade, tile, arrange) via the Window menu.

#### Scenario: Cascade windows
- **WHEN** the user selects Cascade from the Window menu
- **THEN** all open MDI child windows are arranged in a cascading layout

### Requirement: Status feedback
The system SHALL display status information (current job, simulation state) in the status bar.

#### Scenario: Simulation status shown
- **WHEN** a simulation runs or completes
- **THEN** the status bar reflects the current simulation state
