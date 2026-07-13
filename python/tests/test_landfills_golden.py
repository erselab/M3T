"""Golden test: Python landfills sector vs the real R Municipal_solid_waste.

The oracle (tests/golden/landfills/landfills_oracle.json) was produced by calling
M3T:::Municipal_solid_waste directly on a small PA/NJ box; see
capture_landfills_oracle.R. The facility fixture is filtered to landfill
facilities (identical results, small enough to commit) — regenerate it from the
live EPA table if needed:

    python -c "import pandas as pd; from m3t import datasets; \
        fac=pd.read_csv('https://data.epa.gov/dmapservice/ghg.pub_dim_facility/CSV', low_memory=False); \
        ids=set(datasets.load('GHGRP_landfills').facility_id); \
        cols=['facility_id','year','facility_name','latitude','longitude','state','reporting_status','county_fips','zip']; \
        fac[fac.facility_id.isin(ids)][cols].to_csv('tests/golden/landfills/facility_data_landfills.csv', index=False)"

Tolerance: grid dims exact; per-cell flux within rel 1e-4 (abs floor for zeros);
global sums within rel 1e-6.
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from m3t import datasets, geo
from m3t.sectors.landfills import compute_landfills

pytestmark = pytest.mark.golden

_GOLD = Path(__file__).parent / "golden" / "landfills"
_ORACLE = _GOLD / "landfills_oracle.json"
_FACILITY = _GOLD / "facility_data_landfills.csv"


@pytest.fixture(scope="module")
def oracle():
    if not _ORACLE.exists() or not _FACILITY.exists():
        pytest.skip(
            "landfills golden fixtures missing; run "
            "`conda run -n M3T Rscript python/tests/golden/capture_landfills_oracle.R`"
        )
    return json.loads(_ORACLE.read_text())


@pytest.fixture(scope="module")
def results(oracle):
    p = oracle["params"]
    xmin, xmax, ymin, ymax = p["domain_ext"]
    bounds = (xmin, ymin, xmax, ymax)
    template = geo.make_grid(bounds, p["domain_res"], p["domain_crs"])
    facility = pd.read_csv(_FACILITY, low_memory=False)
    return compute_landfills(
        ghgrp_landfills=datasets.load("GHGRP_landfills"),
        ghgrp_combustion=datasets.load("GHGRP_combustion_emissions"),
        ghgrp_facility_data=facility,
        lmop=datasets.load("LMOP_data"),
        ghgi_landfill_total=p["GHGI_landfill_total"],
        ghgi_data_yr=p["GHGI_data_yr"],
        domain_template=template,
        domain=bounds,
        domain_crs=p["domain_crs"],
    )


# map oracle filenames -> result keys
_MAP = {
    "MSW_GHGRP_reported.nc": "MSW_GHGRP_reported",
    "MSW_GHGRP_generation_first.nc": "MSW_GHGRP_generation_first",
    "MSW_GHGRP_collection_first.nc": "MSW_GHGRP_collection_first",
    "MSW_LMOP.nc": "MSW_LMOP",
    "Landfill_sector_total_GHGRP_reported.nc": "Landfill_sector_total_GHGRP_reported",
    "Landfill_sector_total_GHGRP_generation_first.nc": "Landfill_sector_total_GHGRP_generation_first",
    "Landfill_sector_total_GHGRP_collection_first.nc": "Landfill_sector_total_GHGRP_collection_first",
}


@pytest.mark.parametrize("fname,key", list(_MAP.items()))
def test_landfill_raster_matches_r(oracle, results, fname, key):
    r = oracle["rasters"][fname]
    da = results[key]

    # grid dims exact
    assert da.shape == (r["nrow"], r["ncol"]), f"{key} dims"

    py = da.values.astype("float64").ravel()  # row-major, top-left first
    ref = np.array([np.nan if v is None else v for v in r["values"]], dtype="float64")

    # empty cells are 0 in both (R sets [is.na]<-0)
    assert np.allclose(py, ref, rtol=1e-4, atol=1e-9, equal_nan=True), f"{key} cell values"

    # global sum
    assert float(np.nansum(py)) == pytest.approx(r["sum"], rel=1e-6)
    # non-zero cell count
    assert int(np.count_nonzero(py)) == r["nonzero"], f"{key} nonzero count"


def test_sector_total_is_lmop_plus_ghgrp(results):
    lmop = results["MSW_LMOP"].values
    for method in ("reported", "generation_first", "collection_first"):
        total = results[f"Landfill_sector_total_GHGRP_{method}"].values
        ghgrp = results[f"MSW_GHGRP_{method}"].values
        assert np.allclose(total, lmop + ghgrp)
