"""Compare m3t.geo against terra outputs captured in tests/golden/geo_oracle.json.

Regenerate the oracle with:
    conda run -n M3T Rscript python/tests/golden/capture_geo_oracle.R

The oracle encodes terra's *actual* behaviour (extent-preserving grids,
geodesic cellSize, etc.); these tests fail if the Python shim drifts from it.
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pytest

from m3t import geo

_ORACLE_PATH = Path(__file__).parent / "golden" / "geo_oracle.json"
pytestmark = pytest.mark.golden


@pytest.fixture(scope="module")
def oracle():
    if not _ORACLE_PATH.exists():
        pytest.skip(
            "geo_oracle.json missing; run "
            "`conda run -n M3T Rscript python/tests/golden/capture_geo_oracle.R`"
        )
    return json.loads(_ORACLE_PATH.read_text())


def _ext_terra_order(da) -> tuple[float, float, float, float]:
    # terra ext() order is (xmin, xmax, ymin, ymax); geo.ext is (xmin,ymin,xmax,ymax)
    xmin, ymin, xmax, ymax = geo.ext(da)
    return (xmin, xmax, ymin, ymax)


def test_make_grid_3x3_matches_terra(oracle):
    o = oracle["make_grid_3x3"]
    g = geo.make_grid((-75.0, 39.0, -72.0, 42.0), 1.0, "epsg:4326")
    assert g.shape == (o["nrow"], o["ncol"])
    assert _ext_terra_order(g) == pytest.approx(tuple(o["ext"]))
    assert geo.res(g) == pytest.approx(tuple(o["res"]))


def test_make_grid_snap_matches_terra(oracle):
    o = oracle["make_grid_snap"]
    g = geo.make_grid((-75.0, 39.0, -71.5, 42.0), 1.0, "epsg:4326")
    assert g.shape == (o["nrow"], o["ncol"])
    assert _ext_terra_order(g) == pytest.approx(tuple(o["ext"]))
    assert geo.res(g) == pytest.approx(tuple(o["res"]))


def test_global_matches_terra(oracle):
    o = oracle["global"]
    g = geo.make_grid((0.0, 0.0, 2.0, 2.0), 1.0, "epsg:32618")
    g.values[:] = [[1.0, 2.0], [3.0, np.nan]]
    assert geo.global_(g, "sum") == pytest.approx(o["sum"])
    assert geo.global_(g, "max") == pytest.approx(o["max"])
    assert geo.global_(g, "notNA") == o["notNA"]


def test_aggregate_matches_terra(oracle):
    o = oracle["aggregate"]
    g = geo.make_grid((0.0, 0.0, 4.0, 4.0), 1.0, "epsg:32618")
    g.values[:] = 1.0
    agg = geo.aggregate(g, 2, fun="sum")
    assert agg.shape == (o["nrow"], o["ncol"])
    assert geo.res(agg) == pytest.approx(tuple(o["res"]))
    assert geo.global_(agg, "sum") == pytest.approx(o["total"])


def test_cell_area_matches_terra(oracle):
    # terra took values(cellSize(...))[1,1] -> the top-left cell
    g = geo.make_grid((0.0, 0.0, 1000.0, 1000.0), 500.0, "epsg:32618")
    area = geo.cell_area(g, unit="m")
    assert float(area.values[0, 0]) == pytest.approx(oracle["cell_area_m"], rel=1e-9)
