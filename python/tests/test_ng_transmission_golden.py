"""Golden test: Python NG transmission sector vs the real R Natural_Gas_Transmission.

Oracle from capture_ng_transmission_oracle.R (real internal function on a PA box).
Fixtures (committed): subpart W (2019, transmission+processing), facility
locations, and EIA pipelines intersecting the domain bbox.

Tolerance: dims exact; per-cell flux rel 1e-3 (the per-cell geodesic pipeline
length is the loosest op — clip + geodesic length vs terra); global sums rel 1e-4.
"""

from __future__ import annotations

import json
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
import pytest

from m3t import datasets, geo
from m3t.sectors.ng_transmission import build_ghgi_frames, compute_ng_transmission

pytestmark = pytest.mark.golden

_GOLD = Path(__file__).parent / "golden" / "ng_transmission"
_ORACLE = _GOLD / "ng_transmission_oracle.json"
_FILES = ["subpartW_ngtrans.csv", "facility_data_ngtrans.csv", "eia_pipes_pa_all.geojson"]


@pytest.fixture(scope="module")
def oracle():
    if not _ORACLE.exists() or not all((_GOLD / f).exists() for f in _FILES):
        pytest.skip(
            "NG transmission golden fixtures missing; run "
            "`conda run -n M3T Rscript python/tests/golden/capture_ng_transmission_oracle.R`"
        )
    return json.loads(_ORACLE.read_text())


@pytest.fixture(scope="module")
def results(oracle):
    p = oracle["params"]
    xmin, xmax, ymin, ymax = p["domain_ext"]
    bounds = (xmin, ymin, xmax, ymax)
    template = geo.make_grid(bounds, p["domain_res"], "epsg:4326")
    comp_frame, pipe_frame = build_ghgi_frames(p["GHGI_data_yr"])
    facility = pd.read_csv(_GOLD / "facility_data_ngtrans.csv", low_memory=False)
    subW = pd.read_csv(_GOLD / "subpartW_ngtrans.csv", low_memory=False)
    pipes = gpd.read_file(_GOLD / "eia_pipes_pa_all.geojson")
    return compute_ng_transmission(
        ghgi_transmission_compressors=comp_frame,
        ghgi_pipeline=pipe_frame,
        hifld=datasets.load("HIFLD_NG_data"),
        eia_pipes=pipes,
        ghgrp_facility_data=facility,
        subpartW=subW,
        ghgrp_combustion=datasets.load("GHGRP_combustion_emissions"),
        ghgi_data_yr=p["GHGI_data_yr"],
        domain_template=template,
        domain=bounds,
        domain_crs="epsg:4326",
    )


_MAP = {
    "NG_trans_pipes.nc": "NG_trans_pipes",
    "NG_trans_compressors.nc": "NG_trans_compressors",
    "NG_transmission_sector_total.nc": "NG_transmission_sector_total",
}


@pytest.mark.parametrize("fname,key", list(_MAP.items()))
def test_raster_matches_r(oracle, results, fname, key):
    r = oracle["rasters"][fname]
    da = results[key]
    assert da.shape == (r["nrow"], r["ncol"]), f"{key} dims"

    py = da.values.astype("float64").ravel()
    ref = np.array([np.nan if v is None else v for v in r["values"]], dtype="float64")

    assert float(np.nansum(py)) == pytest.approx(r["sum"], rel=1e-4), f"{key} global sum"
    assert np.allclose(py, ref, rtol=1e-3, atol=1e-9, equal_nan=True), f"{key} cell values"


def test_total_is_pipes_plus_compressors(results):
    assert np.allclose(
        results["NG_transmission_sector_total"].values,
        results["NG_trans_pipes"].values + results["NG_trans_compressors"].values,
    )
