"""Plotting for ModSIM engine results.

Provides a robust parser for engine result files (:mod:`modsim.gui.plotting.parser`)
and Qt Charts / matplotlib renderers (:mod:`modsim.gui.plotting.plots`).
"""

from .parser import (
    LiberationSpectrum,
    Results,
    SizeDistribution,
    StreamData,
    parse_results,
)
from .plots import (
    ResultsViewer,
    liberation_spectra_plot,
    size_distribution_plot,
    stream_plot,
)

__all__ = [
    "parse_results",
    "Results",
    "SizeDistribution",
    "LiberationSpectrum",
    "StreamData",
    "size_distribution_plot",
    "liberation_spectra_plot",
    "stream_plot",
    "ResultsViewer",
]
