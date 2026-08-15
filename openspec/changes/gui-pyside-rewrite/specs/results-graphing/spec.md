## ADDED Requirements

### Requirement: Parse engine result files
The system SHALL parse the result files written by the engine (e.g., `REPORT2.OUT` and other `.OUT` files) from the job directory.

#### Scenario: Parse results after simulation
- **WHEN** a simulation completes and writes result files
- **THEN** the system parses the result files into structured data

### Requirement: Render size distributions
The system SHALL render size-distribution plots from the parsed results.

#### Scenario: Display size distribution
- **WHEN** the user views a stream's size distribution
- **THEN** the system renders a size-distribution plot

### Requirement: Render liberation spectra
The system SHALL render liberation-spectra plots from the parsed results.

#### Scenario: Display liberation spectrum
- **WHEN** the user views a liberation spectrum
- **THEN** the system renders a liberation-spectra plot

### Requirement: Render stream plots
The system SHALL render stream plots (e.g., grade/recovery) from the parsed results.

#### Scenario: Display stream plot
- **WHEN** the user views a stream plot
- **THEN** the system renders the plot

### Requirement: Use Qt Charts for plotting
The system SHALL render plots using Qt Charts (GPL v3), which is acceptable for this project. matplotlib may be used as a fallback for plot types Qt Charts does not support well.

#### Scenario: Plotting library
- **WHEN** plots are rendered
- **THEN** they are rendered with Qt Charts, or matplotlib where Qt Charts is insufficient
