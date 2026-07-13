"""Golden test: Python wastewater sector vs the real R Wastewater.

The oracle (tests/golden/wastewater/wastewater_oracle.json) was produced by calling
M3T:::Wastewater on an IA+NE state domain with every method variant enabled; see
capture_wastewater_oracle.R.

Domain note: the port plan's reference domain is CT/RI, but the GHGRP subpart-II
(industrial) stream has no reporters there at all, and the R errors selecting
columns from the resulting zero-row facility set. IA+NE are the top two states by
2019 subpart-II count and are adjacent, so they also exercise per-state septic.

Tolerance, municipal + industrial: grid dims exact; per-cell flux within rel 1e-4
(abs floor for zeros); global sums within rel 1e-6.

Tolerance, septic: looser, and deliberately so — see `test_septic_matches_r`. The
median per-cell error is 2.3e-8 (i.e. the interior is exact) and the totals agree
to <=4e-5, but ~1% of cells differ by more because terra and GDAL break ties
differently when deciding which cells a polygon boundary covers. The offenders are
confined to the domain edge and the IA/NE state line; there is no interior drift.
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from m3t import datasets, geo
from m3t.config import Config
from m3t.sectors.wastewater import (
    _KT_TO_MOL_S,
    compute_septic,
    compute_wastewater,
    septic_state_info,
)

pytestmark = pytest.mark.golden

_GOLD = Path(__file__).parent / "golden" / "wastewater"
_ORACLE = _GOLD / "wastewater_oracle.json"
_FACILITY = _GOLD / "facility_data_wastewater.csv"
_DMR = _GOLD / "in" / "DMR_data.csv"
_DOMAIN = _GOLD / "domain_ia_ne.geojson"
_NLCD = _GOLD / "nlcd_suburbia_2019_ia_ne.tif"
_STATES = ["IA", "NE"]

# Ported streams only. The septic rasters and the sector totals that depend on
# them are in the oracle but not yet reproducible in Python.
_PORTED = [
    "Wastewater_CWNS_GHGI_dom_central",
    "Wastewater_CWNS_Moore_dom_central",
    "Wastewater_DMR_GHGI_dom_central",
    "Wastewater_DMR_Moore_dom_central",
    "Wastewater_ind",
]


@pytest.fixture(scope="module")
def oracle():
    missing = [p.name for p in (_ORACLE, _FACILITY, _DMR, _DOMAIN) if not p.exists()]
    if missing:
        pytest.skip(
            f"wastewater golden fixtures missing ({', '.join(missing)}); run "
            "`M3T_DATA=/path/to/M3T_Processed conda run -n M3T Rscript "
            "python/tests/golden/capture_wastewater_oracle.R`"
        )
    return json.loads(_ORACLE.read_text())


@pytest.fixture(scope="module")
def results(oracle):
    import geopandas as gpd

    p = oracle["params"]
    xmin, xmax, ymin, ymax = p["domain_ext"]
    template = geo.make_grid((xmin, ymin, xmax, ymax), p["domain_res"], p["domain_crs"])
    domain = gpd.read_file(_DOMAIN).to_crs(p["domain_crs"])

    return compute_wastewater(
        ghgrp_wastewater=datasets.load("GHGRP_wastewater"),
        ghgrp_facility_data=pd.read_csv(_FACILITY, low_memory=False),
        ghgi_wastewater_data=Config().GHGI_wastewater_data,
        ghgi_data_yr=p["GHGI_data_yr"],
        domain_template=template,
        domain=domain,
        domain_crs=p["domain_crs"],
        cwns=datasets.load("CWNS_2022"),
        cwns_yr=2022,
        dmr=pd.read_csv(_DMR, low_memory=False),
        use_cwns=True,
        use_dmr=True,
        method_ghgi=True,
        method_moore=True,
    )


@pytest.mark.parametrize("key", _PORTED)
def test_wastewater_raster_matches_r(oracle, results, key):
    r = oracle["rasters"][f"{key}.nc"]
    da = results[key]

    assert da.shape == (r["nrow"], r["ncol"]), f"{key} dims"

    py = da.values.astype("float64").ravel()  # row-major, top-left first
    ref = np.array([np.nan if v is None else v for v in r["values"]], dtype="float64")

    assert np.allclose(py, ref, rtol=1e-4, atol=1e-9, equal_nan=True), f"{key} cell values"
    assert float(np.nansum(py)) == pytest.approx(r["sum"], rel=1e-6)
    assert int(np.count_nonzero(py)) == r["nonzero"], f"{key} nonzero count"


@pytest.fixture(scope="module")
def septic(oracle):
    import geopandas as gpd
    import rioxarray

    if not _NLCD.exists():
        pytest.skip(f"{_NLCD.name} missing (built by capture_wastewater_oracle.R)")

    p = oracle["params"]
    xmin, xmax, ymin, ymax = p["domain_ext"]
    template = geo.make_grid((xmin, ymin, xmax, ymax), p["domain_res"], p["domain_crs"])
    domain = gpd.read_file(_DOMAIN).to_crs(p["domain_crs"])
    suburbia = rioxarray.open_rasterio(_NLCD, masked=True).squeeze("band", drop=True)

    cfg = Config()
    yr = p["GHGI_data_yr"]
    ghgi = cfg.GHGI_wastewater_data
    row = ghgi[ghgi["year"] == yr]

    national_area = pd.read_csv(_GOLD / "in" / "Total_national_septic_area.csv")
    nearest = (national_area["year"] - p["inventory_year"]).abs().idxmin()

    areas = pd.read_csv(_GOLD / "in" / "wastewater_state_septic_area.csv")
    areas = areas[["state", str(yr)]].rename(
        columns={"state": "X", str(yr): "open_or_low_int_area"}
    )
    areas = areas[areas["X"].isin(_STATES)]

    info, _ = septic_state_info(
        state_septic_1990=datasets.load("Wastewater_1990_state_septic"),
        reported_state_info=cfg.Wastewater_reported_State_info,
        national_info=cfg.National_wastewater_info,
        state_population=datasets.load("Census_state_population_M3T"),
        state_lookup=domain,
        states=_STATES,
        inventory_year=p["inventory_year"],
        ghgi_data_yr=yr,
    )

    return compute_septic(
        suburbia=suburbia,
        septic_epa_emiss=float(row["Septic Emissions"].iloc[0]) * _KT_TO_MOL_S,
        total_national_area=float(
            national_area.loc[nearest, "Total_national_open_or_low_int_area"]
        ),
        state_info=info,
        state_total_areas=areas,
        state_tigerlines=domain,
        domain=domain,
        domain_template=template,
        domain_crs=p["domain_crs"],
        ghgi_ef=float(row["EF"].iloc[0]),
        national=True,
        by_state=True,
    )


@pytest.mark.parametrize(
    "key",
    ["Wastewater_dom_septic_national", "Wastewater_dom_septic_bystate"],
)
def test_septic_matches_r(oracle, septic, key):
    """Septic vs terra, with a bounded allowance for boundary-cell tie-breaks.

    Everything up to the final reprojection reproduces terra exactly (verified
    cell-for-cell on the pre-projection raster). What remains is that terra and
    GDAL disagree about which cells a polygon boundary covers, which shifts a
    handful of edge cells. We assert that the disagreement stays *bounded and
    localised* rather than pretending it is zero:

    * the median cell is exact (<=1e-6 relative),
    * >=97% of cells are within the normal 1e-4 tolerance,
    * no cell is off by more than 5%,
    * the sector total is within 1e-4.
    """
    r = oracle["rasters"][f"{key}.nc"]
    da = septic[key]
    assert da.shape == (r["nrow"], r["ncol"])

    py = da.values.astype("float64")
    ref = np.array([np.nan if v is None else v for v in r["values"]], dtype="float64").reshape(
        py.shape
    )

    comparable = np.isfinite(py) & np.isfinite(ref) & (np.abs(ref) > 0)
    rel = np.abs(py[comparable] - ref[comparable]) / np.abs(ref[comparable])

    assert np.median(rel) <= 1e-6, f"{key}: interior drift, median rel {np.median(rel):.2e}"
    assert (rel <= 1e-4).mean() >= 0.97, f"{key}: only {(rel <= 1e-4).mean():.1%} of cells within 1e-4"
    assert rel.max() <= 5e-2, f"{key}: worst cell off by {rel.max():.2%}"
    assert float(np.nansum(py)) == pytest.approx(r["sum"], rel=1e-4)

    # the NaN mask may differ on at most a couple of boundary cells
    assert int((np.isnan(py) != np.isnan(ref)).sum()) <= 2


def test_septic_totals_are_positive(septic):
    for key, da in septic.items():
        assert float(np.nansum(da.values)) > 0, key


@pytest.fixture(scope="module")
def totals(oracle, septic):
    """Re-run compute_wastewater with septic injected, giving the 8 sector totals."""
    import geopandas as gpd

    p = oracle["params"]
    xmin, xmax, ymin, ymax = p["domain_ext"]
    template = geo.make_grid((xmin, ymin, xmax, ymax), p["domain_res"], p["domain_crs"])
    domain = gpd.read_file(_DOMAIN).to_crs(p["domain_crs"])

    return compute_wastewater(
        ghgrp_wastewater=datasets.load("GHGRP_wastewater"),
        ghgrp_facility_data=pd.read_csv(_FACILITY, low_memory=False),
        ghgi_wastewater_data=Config().GHGI_wastewater_data,
        ghgi_data_yr=p["GHGI_data_yr"],
        domain_template=template,
        domain=domain,
        domain_crs=p["domain_crs"],
        cwns=datasets.load("CWNS_2022"),
        cwns_yr=2022,
        dmr=pd.read_csv(_DMR, low_memory=False),
        use_cwns=True,
        use_dmr=True,
        method_ghgi=True,
        method_moore=True,
        septic={
            "national": septic["Wastewater_dom_septic_national"],
            "state": septic["Wastewater_dom_septic_bystate"],
        },
    )


@pytest.mark.parametrize(
    "key",
    [
        f"Wastewater_sector_total_{s}_{m}_{k}"
        for s in ("CWNS", "DMR")
        for m in ("GHGI", "Moore")
        for k in ("national", "state")
    ],
)
def test_sector_total_matches_r(oracle, totals, key):
    """All 8 source x method x septic combinations, vs R.

    Same bounded tolerance as the septic rasters they contain (the municipal and
    industrial parts they sum with are exact).
    """
    r = oracle["rasters"][f"{key}.nc"]
    py = totals[key].values.astype("float64")
    ref = np.array([np.nan if v is None else v for v in r["values"]], dtype="float64").reshape(
        py.shape
    )

    comparable = np.isfinite(py) & np.isfinite(ref) & (np.abs(ref) > 0)
    rel = np.abs(py[comparable] - ref[comparable]) / np.abs(ref[comparable])

    assert np.median(rel) <= 1e-6, f"{key}: median rel {np.median(rel):.2e}"
    assert (rel <= 1e-4).mean() >= 0.97, f"{key}: {(rel <= 1e-4).mean():.1%} within 1e-4"
    assert rel.max() <= 5e-2, f"{key}: worst cell off by {rel.max():.2%}"
    assert float(np.nansum(py)) == pytest.approx(r["sum"], rel=1e-4)


def test_moore_exceeds_ghgi(results):
    """Moore et al. implies roughly 2x the GHGI-disaggregated total (see R docs)."""
    for source in ("CWNS", "DMR"):
        moore = float(np.nansum(results[f"Wastewater_{source}_Moore_dom_central"].values))
        ghgi = float(np.nansum(results[f"Wastewater_{source}_GHGI_dom_central"].values))
        assert moore > ghgi


def test_septic_absent_means_no_sector_totals(results):
    """Without septic rasters injected, no sector total is emitted (see module docstring)."""
    assert not [k for k in results if k.startswith("Wastewater_sector_total")]
