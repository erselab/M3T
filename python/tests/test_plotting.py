"""Plotting: the logic, plus a render smoke test.

Pixel-comparing against terra's output would be neither achievable nor meaningful
(different rendering engines), so what's tested here is the behaviour that
actually decides what a reader sees: the log transform, the colour-range rules,
the file routing, and the degenerate cases R goes out of its way to still plot.
The render tests only assert that a real, non-blank PNG comes out.
"""

from __future__ import annotations

import numpy as np
import pytest
import xarray as xr

from m3t import geo
from m3t.plotting import (
    log_plot,
    not_log_plot,
    output_path,
    prep_plot_data,
    resolve_zlim,
)


@pytest.fixture
def template():
    return geo.make_grid((-75.0, 40.0, -74.6, 40.4), 0.1, "epsg:4326")


def _grid(template, values):
    da = xr.DataArray(
        np.asarray(values, dtype="float64"),
        coords=template.coords,
        dims=template.dims,
    )
    da.rio.write_crs("epsg:4326", inplace=True)
    return da


# --------------------------------------------------------------------------- #
# prep_plot_data
# --------------------------------------------------------------------------- #
def test_prep_plot_data_is_log10_with_zeros_dropped(template):
    da = _grid(template, np.full(template.shape, 1.0))
    da.values[0, 0] = 100.0
    da.values[0, 1] = 0.0  # log10(0) = -inf -> must become NaN, not -inf

    out = prep_plot_data(da)
    assert out.values[0, 0] == pytest.approx(2.0)
    assert out.values[1, 1] == pytest.approx(0.0)
    assert np.isnan(out.values[0, 1])
    assert np.isfinite(out.values[np.isfinite(out.values)]).all()


def test_prep_plot_data_drops_negatives_rather_than_producing_nan_math(template):
    da = _grid(template, np.full(template.shape, -5.0))
    out = prep_plot_data(da)
    assert np.isnan(out.values).all()


# --------------------------------------------------------------------------- #
# colour range
# --------------------------------------------------------------------------- #
def test_zlim_defaults_to_data_range_nudged_outwards(template):
    da = _grid(template, np.full(template.shape, 1.0))
    da.values[0, 0] = 4.0
    lo, hi = resolve_zlim(da, None, None)
    # nudged out so the extreme cells are not clipped out of the colour range
    assert lo < 1.0 and hi > 4.0
    assert hi == pytest.approx(4.0 * 1.00001)


def test_zlim_min_above_data_max_falls_back_to_data_min(template):
    """R's guard: a caller-supplied floor above everything in the data is discarded."""
    da = _grid(template, np.full(template.shape, 1.0))
    lo, hi = resolve_zlim(da, zlim_min=99.0, zlim_max=None)
    assert lo < hi
    assert lo == pytest.approx(1.0 * 0.99999)


def test_zlim_nudge_handles_negative_bounds(template):
    """log10 of small fluxes is negative; the nudge must widen, not narrow."""
    da = _grid(template, np.full(template.shape, -3.0))
    da.values[0, 0] = -1.0
    lo, hi = resolve_zlim(da, None, None)
    assert lo <= -3.0 and hi >= -1.0


# --------------------------------------------------------------------------- #
# file routing
# --------------------------------------------------------------------------- #
def test_summed_output_goes_to_its_own_folder(tmp_path):
    assert output_path(tmp_path, "landfills") == tmp_path / "landfills.png"
    assert (
        output_path(tmp_path, "Summed_final_inventory")
        == tmp_path / "Summed_Sectors" / "Summed_final_inventory.png"
    )


# --------------------------------------------------------------------------- #
# rendering
# --------------------------------------------------------------------------- #
def _png_is_real(path) -> bool:
    if not path.exists() or path.stat().st_size < 2000:
        return False
    return path.read_bytes()[:8] == b"\x89PNG\r\n\x1a\n"


def test_log_plot_renders(tmp_path, template):
    da = _grid(template, np.random.default_rng(0).uniform(0.1, 500, template.shape))
    p = log_plot(da, "Landfills", filename="landfills", plot_directory=tmp_path)
    assert _png_is_real(p)


def test_not_log_plot_renders(tmp_path, template):
    da = _grid(template, np.random.default_rng(1).uniform(0.1, 500, template.shape))
    p = not_log_plot(da, "Pipes", filename="pipes", plot_directory=tmp_path)
    assert _png_is_real(p)


@pytest.mark.parametrize("fn", [log_plot, not_log_plot])
def test_all_zero_raster_is_still_plotted(tmp_path, template, fn):
    """R deliberately renders an empty sector as a flat zero map rather than skipping."""
    da = _grid(template, np.zeros(template.shape))
    p = fn(da, "Empty sector", filename="empty", plot_directory=tmp_path)
    assert _png_is_real(p)


@pytest.mark.parametrize("fn", [log_plot, not_log_plot])
def test_all_nan_raster_is_still_plotted(tmp_path, template, fn):
    da = _grid(template, np.full(template.shape, np.nan))
    p = fn(da, "No data", filename="nodata", plot_directory=tmp_path)
    assert _png_is_real(p)


def test_zlim_clips_the_data_not_just_the_bar(tmp_path, template):
    """R clips values to the bounds -- that is what "saturated" in its titles means."""
    da = _grid(template, np.full(template.shape, 1e6))
    da.values[0, 0] = 1e-6
    p = log_plot(
        da, "Saturated", filename="sat", plot_directory=tmp_path, zlim_min=0.0
    )
    assert _png_is_real(p)
