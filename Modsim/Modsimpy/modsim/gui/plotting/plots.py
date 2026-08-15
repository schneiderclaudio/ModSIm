"""Plot rendering for ModSIM results using Qt Charts.

Each ``*_plot`` function takes parsed result data and returns a ``QWidget``
containing a Qt Charts ``QChartView``.  Qt Charts (GPL v3) is the primary
renderer; matplotlib is used only as a lazy fallback for plot types Qt Charts
does not support well (e.g. bar charts with many categories), and is imported
on demand so it remains an optional dependency.

The module is import-safe without a running QApplication: the Qt imports are
deferred into the functions that need them, so the parser and data model can
be used headlessly.
"""

from __future__ import annotations

from typing import List, Optional

from .parser import LiberationSpectrum, Results, SizeDistribution

# ---------------------------------------------------------------------------
# Qt Charts helpers
# ---------------------------------------------------------------------------


def _qt_charts():
    """Import and return the Qt Charts module (raises ImportError if absent)."""
    from PySide6 import QtCharts  # noqa: PLC0415 - deferred optional import

    return QtCharts


def _qt_widgets():
    from PySide6 import QtWidgets  # noqa: PLC0415

    return QtWidgets


def _qt_core():
    from PySide6 import QtCore  # noqa: PLC0415

    return QtCore


def _qt_gui():
    from PySide6 import QtGui  # noqa: PLC0415

    return QtGui


def _make_chart_view(chart, title: str = "") -> "object":
    """Wrap a QChart in a QChartView inside a QWidget container."""
    QtCharts = _qt_charts()
    QtWidgets = _qt_widgets()
    if title:
        chart.setTitle(title)
    chart.legend().setVisible(True)
    chart.legend().setAlignment(_qt_core().Qt.AlignBottom)
    view = QtCharts.QChartView(chart)
    view.setRenderHint(_qt_gui().QPainter.Antialiasing)
    container = QtWidgets.QWidget()
    layout = QtWidgets.QVBoxLayout(container)
    layout.setContentsMargins(0, 0, 0, 0)
    layout.addWidget(view)
    return container


def _line_series(name: str, points: List[tuple], color=None) -> "object":
    """Build a QLineSeries from ``(x, y)`` points."""
    QtCharts = _qt_charts()
    QtGui = _qt_gui()
    series = QtCharts.QLineSeries()
    series.setName(name)
    for x, y in points:
        series.append(float(x), float(y))
    if color is not None:
        pen = series.pen()
        pen.setColor(QtGui.QColor(color))
        series.setPen(pen)
    return series


# ---------------------------------------------------------------------------
# Plot builders
# ---------------------------------------------------------------------------


def size_distribution_plot(data: SizeDistribution) -> "object":
    """Render a size-distribution plot (size vs cumulative % passing).

    Uses a log-x axis, which is the conventional presentation for particle
    size distributions.  Returns a QWidget containing a Qt Charts chart.
    """
    QtCharts = _qt_charts()
    QtCore = _qt_core()
    QtGui = _qt_gui()

    chart = QtCharts.QChart()
    series = _line_series(
        f"Stream {data.stream}" + (f" - {data.label}" if data.label else ""),
        data.points(),
    )
    chart.addSeries(series)

    axis_x = QtCharts.QLogValueAxis()
    axis_x.setTitleText("Size (m)")
    axis_x.setLabelFormat("%.1e")
    axis_x.setBase(10.0)
    axis_x.setRange(_log_floor(min(data.sizes)), _log_ceil(max(data.sizes)))

    axis_y = QtCharts.QValueAxis()
    axis_y.setTitleText("Cumulative passing (%)")
    axis_y.setRange(0.0, 105.0)
    axis_y.setLabelFormat("%.0f")

    chart.addAxis(axis_x, QtCore.Qt.AlignBottom)
    chart.addAxis(axis_y, QtCore.Qt.AlignLeft)
    series.attachAxis(axis_x)
    series.attachAxis(axis_y)

    return _make_chart_view(chart, title="Size Distribution")


def liberation_spectra_plot(data: List[LiberationSpectrum]) -> "object":
    """Render liberation-spectra plot(s) for a stream.

    Plots each spectrum (unconditional / conditional) as a line of mass
    fraction vs liberation class.  Returns a QWidget containing a Qt Charts
    chart.
    """
    QtCharts = _qt_charts()
    QtCore = _qt_core()

    chart = QtCharts.QChart()
    colors = ["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"]
    for idx, spec in enumerate(data):
        series = _line_series(
            f"{spec.kind.title()} (stream {spec.stream})",
            spec.points(),
            color=colors[idx % len(colors)],
        )
        chart.addSeries(series)

    axis_x = QtCharts.QValueAxis()
    axis_x.setTitleText("Liberation class")
    axis_x.setLabelFormat("%d")
    axis_x.setRange(0, max((s.classes[-1] for s in data), default=1) + 1)

    axis_y = QtCharts.QValueAxis()
    axis_y.setTitleText("Mass fraction")
    axis_y.setLabelFormat("%.3f")
    axis_y.setRange(0.0, 1.1)

    chart.addAxis(axis_x, QtCore.Qt.AlignBottom)
    chart.addAxis(axis_y, QtCore.Qt.AlignLeft)
    for series in chart.series():
        series.attachAxis(axis_x)
        series.attachAxis(axis_y)

    return _make_chart_view(chart, title="Liberation Spectra")


def stream_plot(data: Results) -> "object":
    """Render a stream summary plot (e.g. solid flowrate per stream).

    Stream plots in ModSIM are typically categorical (one value per stream),
    which Qt Charts does not support well as a native series type.  We render
    them as a bar chart using matplotlib embedded via ``FigureCanvasQTAgg``,
    falling back to a Qt Charts line plot if matplotlib is unavailable.
    """
    streams = [s for s in data.streams if s.solid_flow is not None]
    if not streams:
        return _empty_plot("No stream flow data available")

    try:
        return _stream_plot_matplotlib(streams)
    except ImportError:
        return _stream_plot_qtcharts(streams)


def _stream_plot_matplotlib(streams) -> "object":
    """Render a stream bar chart with matplotlib (lazy import)."""
    import matplotlib  # noqa: PLC0415 - optional dependency
    matplotlib.use("QtAgg")
    from matplotlib.backends.backend_qtagg import FigureCanvasQTAgg  # noqa: PLC0415
    from matplotlib.figure import Figure  # noqa: PLC0415

    QtWidgets = _qt_widgets()

    labels = [f"S{s.stream}" for s in streams]
    values = [s.solid_flow or 0.0 for s in streams]

    fig = Figure(figsize=(6, 4), dpi=100)
    ax = fig.add_subplot(111)
    ax.bar(labels, values, color="#1f77b4")
    ax.set_xlabel("Stream")
    ax.set_ylabel("Solid flowrate (tonne/hr)")
    ax.set_title("Stream Solid Flowrates")
    ax.grid(axis="y", linestyle="--", alpha=0.4)
    fig.tight_layout()

    canvas = FigureCanvasQTAgg(fig)
    container = QtWidgets.QWidget()
    layout = QtWidgets.QVBoxLayout(container)
    layout.setContentsMargins(0, 0, 0, 0)
    layout.addWidget(canvas)
    return container


def _stream_plot_qtcharts(streams) -> "object":
    """Fallback stream plot using Qt Charts line series."""
    QtCharts = _qt_charts()
    QtCore = _qt_core()

    chart = QtCharts.QChart()
    series = QtCharts.QLineSeries()
    series.setName("Solid flowrate")
    for s in streams:
        series.append(float(s.stream), float(s.solid_flow or 0.0))
    chart.addSeries(series)

    axis_x = QtCharts.QValueAxis()
    axis_x.setTitleText("Stream")
    axis_x.setLabelFormat("%d")
    axis_x.setRange(0, max((s.stream for s in streams), default=1) + 1)

    axis_y = QtCharts.QValueAxis()
    axis_y.setTitleText("Solid flowrate (tonne/hr)")
    axis_y.setLabelFormat("%.1f")

    chart.addAxis(axis_x, QtCore.Qt.AlignBottom)
    chart.addAxis(axis_y, QtCore.Qt.AlignLeft)
    series.attachAxis(axis_x)
    series.attachAxis(axis_y)

    return _make_chart_view(chart, title="Stream Solid Flowrates")


def _empty_plot(message: str) -> "object":
    """Return a QWidget with a plain label (no data to plot)."""
    QtWidgets = _qt_widgets()
    container = QtWidgets.QWidget()
    layout = QtWidgets.QVBoxLayout(container)
    label = QtWidgets.QLabel(message)
    label.setAlignment(_qt_core().Qt.AlignCenter)
    layout.addWidget(label)
    return container


# ---------------------------------------------------------------------------
# Axis helpers
# ---------------------------------------------------------------------------


def _log_floor(value: float) -> float:
    """Return a power of 10 at or below ``value`` (never 0)."""
    import math  # noqa: PLC0415

    if value <= 0:
        return 1e-6
    return 10.0 ** math.floor(math.log10(value))


def _log_ceil(value: float) -> float:
    """Return a power of 10 at or above ``value`` (never 0)."""
    import math  # noqa: PLC0415

    if value <= 0:
        return 1.0
    return 10.0 ** math.ceil(math.log10(value))


# ---------------------------------------------------------------------------
# Viewer
# ---------------------------------------------------------------------------


class ResultsViewer:
    """A widget that shows parsed results with a list to select a plot.

    This is a thin wrapper around a ``QTabWidget``: one tab per plot type
    (size distributions, liberation spectra, stream summary).  It is kept as a
    plain class so it can be instantiated without a running event loop for
    testing; the returned ``widget`` is the actual ``QWidget``.
    """

    def __init__(self, results: Results):
        QtWidgets = _qt_widgets()
        self.results = results
        self.widget = QtWidgets.QTabWidget()
        self.widget.setWindowTitle("ModSIM Results")

        self._build_size_tab()
        self._build_liberation_tab()
        self._build_stream_tab()

    def _build_size_tab(self) -> None:
        QtWidgets = _qt_widgets()
        if not self.results.size_distributions:
            self.widget.addTab(_empty_plot("No size distribution data"), "Size Distributions")
            return
        tabs = QtWidgets.QTabWidget()
        for sd in self.results.size_distributions:
            tabs.addTab(
                size_distribution_plot(sd),
                f"Stream {sd.stream}",
            )
        self.widget.addTab(tabs, "Size Distributions")

    def _build_liberation_tab(self) -> None:
        QtWidgets = _qt_widgets()
        if not self.results.liberation_spectra:
            self.widget.addTab(_empty_plot("No liberation spectra data"), "Liberation")
            return
        tabs = QtWidgets.QTabWidget()
        for stream in sorted({ls.stream for ls in self.results.liberation_spectra}):
            specs = self.results.liberation_spectra_for(stream)
            tabs.addTab(liberation_spectra_plot(specs), f"Stream {stream}")
        self.widget.addTab(tabs, "Liberation")

    def _build_stream_tab(self) -> None:
        self.widget.addTab(stream_plot(self.results), "Stream Summary")
