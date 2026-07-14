"""Geo shim invariant tests.

These pin the terra-parity conventions in geo.py. Where a value should match
``terra`` exactly (grid math, global reductions), the expected numbers are the
ones terra produces; a companion R script in tests/golden/ regenerates the
oracle fixtures for the heavier warping/extract verbs (Phase 3).
"""

from __future__ import annotations

import numpy as np
import pytest

from m3t import geo


def test_make_grid_dimensions_and_extent():
    # terra::rast(ext(-75,-72,39,42), resolution=1) -> 3x3, extent preserved
    g = geo.make_grid((-75.0, 39.0, -72.0, 42.0), 1.0, "epsg:4326")
    assert g.shape == (3, 3)
    assert geo.res(g) == (1.0, 1.0)
    xmin, ymin, xmax, ymax = geo.ext(g)
    assert (xmin, ymin, xmax, ymax) == pytest.approx((-75.0, 39.0, -72.0, 42.0))
    assert np.isnan(g.values).all()  # vals=NA


def test_make_grid_preserves_resolution_refits_extent():
    # terra rast(SpatVector, resolution): resolution preserved, cell count
    # rounded, minimums anchored. width 3.5 at res 1 -> round(3.5)=4 cols,
    # xmax refit to -75 + 4 = -71 (extent expands past the input box here).
    g = geo.make_grid((-75.0, 39.0, -71.5, 42.0), 1.0, "epsg:4326")
    nrow, ncol = g.shape
    assert (nrow, ncol) == (3, 4)
    assert geo.res(g) == pytest.approx((1.0, 1.0))
    assert geo.ext(g) == pytest.approx((-75.0, 39.0, -71.0, 42.0))


def test_make_grid_rounds_cell_count_not_ceil():
    # width 3.2 -> round(3.2)=3 cols (grid need not cover the whole box)
    g = geo.make_grid((-75.0, 39.0, -71.8, 42.0), 1.0, "epsg:4326")
    assert g.shape[1] == 3
    assert geo.ext(g)[2] == pytest.approx(-72.0)


def test_make_grid_anchors_minimum():
    # non-grid-aligned min is kept as-is
    g = geo.make_grid((-74.3, 39.2, -71.1, 41.9), 1.0, "epsg:4326")
    xmin, ymin, xmax, ymax = geo.ext(g)
    assert (xmin, ymin) == pytest.approx((-74.3, 39.2))
    assert (xmax, ymax) == pytest.approx((-71.3, 42.2))


def test_cell_centers_are_half_pixel_offset():
    g = geo.make_grid((0.0, 0.0, 2.0, 2.0), 1.0, "epsg:32618")
    assert list(g["x"].values) == pytest.approx([0.5, 1.5])
    # north-up: y descends from top
    assert list(g["y"].values) == pytest.approx([1.5, 0.5])


def test_global_sum_and_na_rm():
    g = geo.make_grid((0.0, 0.0, 2.0, 2.0), 1.0, "epsg:32618")
    g.values[:] = [[1.0, 2.0], [3.0, np.nan]]
    assert geo.global_(g, "sum") == pytest.approx(6.0)
    assert geo.global_(g, "max") == pytest.approx(3.0)
    assert geo.global_(g, "notNA") == 3


def test_aggregate_sum_conserves_total():
    g = geo.make_grid((0.0, 0.0, 4.0, 4.0), 1.0, "epsg:32618")
    g.values[:] = 1.0
    agg = geo.aggregate(g, 2, fun="sum")
    assert agg.shape == (2, 2)
    assert geo.res(agg) == (2.0, 2.0)
    assert geo.global_(agg, "sum") == pytest.approx(geo.global_(g, "sum"))


def test_cell_area_projected_is_geodesic_not_planar():
    # terra::cellSize defaults to transform=TRUE -> geodesic area, so a 500 m
    # UTM cell is ~248658.8 m^2, not the planar 250000.
    g = geo.make_grid((0.0, 0.0, 1000.0, 1000.0), 500.0, "epsg:32618")
    area = geo.cell_area(g, unit="m")
    assert float(area.values[0, 0]) == pytest.approx(248658.81812, rel=1e-6)
    assert float(area.values[0, 0]) != pytest.approx(250000.0)


def test_cell_area_geographic_row_invariant():
    g = geo.make_grid((-75.0, 39.0, -72.0, 42.0), 1.0, "epsg:4326")
    area = geo.cell_area(g, unit="km")
    # cells in the same latitude row share an area; higher-latitude rows shrink
    assert area.values[0, 0] == pytest.approx(area.values[0, 1])
    assert area.values[0, 0] < area.values[2, 0]  # row 0 is northernmost


def test_rasterize_counts_geometry():
    gpd = pytest.importorskip("geopandas")
    from shapely.geometry import box

    template = geo.make_grid((0.0, 0.0, 4.0, 4.0), 1.0, "epsg:32618")
    poly = gpd.GeoDataFrame(geometry=[box(0.5, 0.5, 2.5, 2.5)], crs="epsg:32618")
    burned = geo.rasterize(poly, template, field=None, background=0.0)
    assert geo.global_(burned, "sum") > 0
    assert burned.rio.crs == template.rio.crs


# --------------------------------------------------------------------------- #
# Mass conservation
#
# The rasters are fluxes (nmol/m2/s), so the mass in a cell is flux * cell_area.
# When a fine grid is coarsened onto the run grid, the emissions from fine cells
# that only *partly* fall inside a coarse cell -- or inside the domain at all --
# must still be carried: total mass in == total mass out. That is the whole point
# of the coverage weighting, and it is a scientific requirement of this code, not a
# nicety. These tests exist so a change to the masking or weighting cannot quietly
# start dropping boundary emissions.
# --------------------------------------------------------------------------- #
def _mass(da) -> float:
    """Total mass on a flux raster: sum(flux * cell_area)."""
    return float(np.nansum(da.values * geo.cell_area(da, unit="m").values))


def test_project_partial_to_grid_conserves_mass():
    """Coarsening a fine flux grid onto the domain must preserve total mass."""
    import geopandas as gpd
    from shapely.geometry import box

    # a fine 100 m grid in an equal-area CRS, uniform flux
    crs = "epsg:5070"
    fine = geo.make_grid((0, 0, 10_000, 10_000), 100.0, crs, fill=1.0)

    # domain deliberately misaligned with the coarse grid, so its edge cuts cells
    domain = gpd.GeoDataFrame(geometry=[box(1_050, 1_050, 8_950, 8_950)], crs=crs)
    coarse = geo.make_grid((0, 0, 10_000, 10_000), 1_000.0, crs)

    out = geo.project_partial_to_grid(fine, domain, coarse)

    # the mass that should survive is the flux (1.0) over the domain's area
    expected = 1.0 * domain.geometry.area.sum()
    assert _mass(out) == pytest.approx(expected, rel=1e-2)


def test_partial_boundary_cells_are_kept_not_dropped():
    """A cell straddling the domain edge keeps a *fraction* of its flux, not zero.

    The failure this guards against is masking on cell centres: a boundary cell
    whose centre lies just outside the domain gets discarded whole, taking its
    emissions with it. On a coastal domain that silently lost ~15% of the total.
    """
    import geopandas as gpd
    from shapely.geometry import box

    crs = "epsg:5070"
    fine = geo.make_grid((0, 0, 4_000, 4_000), 100.0, crs, fill=1.0)
    coarse = geo.make_grid((0, 0, 4_000, 4_000), 1_000.0, crs)

    # domain edge at x=2_400 cuts the third column of coarse cells (2000-3000),
    # leaving their centres (2_500) *outside* the domain
    domain = gpd.GeoDataFrame(geometry=[box(0, 0, 2_400, 4_000)], crs=crs)
    out = geo.project_partial_to_grid(fine, domain, coarse)

    col = out.values[:, 2]  # the straddling column
    assert np.all(np.isfinite(col)), "boundary cells were dropped instead of weighted"
    # 400 m of the 1000 m cell is inside -> ~0.4 of the flux
    assert col.mean() == pytest.approx(0.4, abs=0.05)
