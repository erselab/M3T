"""Domain grid parity vs R (tests/golden/domain_oracle.json).

Regenerate with:
    conda run -n M3T Rscript python/tests/golden/capture_domain_oracle.R

Each case reproduces the R domain_template built by CH4_inventory_build.R for a
box/CONUS domain; build_domain must produce an identical grid (shape, extent,
resolution).
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from m3t import domain, geo

_ORACLE_PATH = Path(__file__).parent / "golden" / "domain_oracle.json"
pytestmark = pytest.mark.golden

# (name, xrange, yrange, res) matching capture_domain_oracle.R
_CASES = [
    ("box_int", (-75.0, -72.0), (39.0, 42.0), 1.0),
    ("box_noninteger", (-75.0, -71.5), (39.0, 42.0), 1.0),
    ("conus_1deg", (-130.0, -60.0), (20.0, 55.0), 1.0),
    ("box_halfdeg", (-83.0, -80.0), (40.0, 42.0), 0.5),
    ("box_width_3p2", (-75.0, -71.8), (39.0, 42.0), 1.0),
    ("box_offset_min", (-74.3, -71.1), (39.2, 41.9), 1.0),
]


@pytest.fixture(scope="module")
def oracle():
    if not _ORACLE_PATH.exists():
        pytest.skip(
            "domain_oracle.json missing; run "
            "`conda run -n M3T Rscript python/tests/golden/capture_domain_oracle.R`"
        )
    return json.loads(_ORACLE_PATH.read_text())


@pytest.mark.parametrize("name,xrange,yrange,res", _CASES)
def test_box_domain_grid_matches_r(oracle, name, xrange, yrange, res):
    o = oracle[name]
    bounds = (min(xrange), min(yrange), max(xrange), max(yrange))
    tmpl, _geom = domain.build_domain(bounds, res, "epsg:4326")
    assert tmpl.shape == (o["nrow"], o["ncol"])
    # terra ext order is (xmin, xmax, ymin, ymax)
    xmin, ymin, xmax, ymax = geo.ext(tmpl)
    assert (xmin, xmax, ymin, ymax) == pytest.approx(tuple(o["ext"]))
    assert geo.res(tmpl) == pytest.approx(tuple(o["res"]))


def test_conus_constant_matches_case():
    tmpl, geom = domain.build_domain("CONUS", 1.0, "epsg:4326")
    assert geom == domain.CONUS_BOUNDS
    assert tmpl.shape == (35, 70)


def test_state_selection_excludes_border_neighbours():
    """Clipping states to the domain must not admit zero-area border artifacts.

    A state that merely *borders* the domain intersects it in a line, which is not
    an empty geometry — so filtering on `is_empty` alone let MA and NY into a CT+RI
    run, dragging in their counties (89 instead of 13) and their SEDS/NEI rows.
    """
    import geopandas as gpd
    from shapely.geometry import box

    from m3t.domain import build_state_tigerlines

    # two unit squares sharing an edge at x=1, plus a third further out
    states = gpd.GeoDataFrame(
        {"STUSPS": ["AA", "BB", "CC"], "STATEFP": ["01", "02", "03"]},
        geometry=[box(0, 0, 1, 1), box(1, 0, 2, 1), box(5, 5, 6, 6)],
        crs="epsg:4326",
    )
    domain = states[states["STUSPS"] == "AA"]

    selected, names = build_state_tigerlines(states, domain, "epsg:4326")
    assert names == ["AA"], f"border neighbour leaked in: {names}"
    assert (selected.geometry.area > 0).all()
