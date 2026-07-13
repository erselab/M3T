"""Golden test: Python stationary combustion vs the real R Stationary_combustion.

The oracle (tests/golden/stationary_combustion/stat_comb_oracle.json) was produced
by calling M3T:::Stationary_combustion on a CT+RI domain with ACES, both
disaggregation levels enabled; see capture_stationary_combustion_oracle.R.

ACES rather than the config-default Vulcan: ACES ships on the companion drive while
raw Vulcan is a large Zenodo download, and the two follow identical code paths (the
CO2 inventory is only a spatial proxy). inventory_year 2017 is the newest year ACES,
SEDS and the GHGI all cover.

Tolerance. Every *total* matches R to ~1e-5 relative, and the pieces feeding the
rasters were each verified exactly against R: SEDS state totals (0.950656125 for CT
res_petr, to the last digit), NEI county fractions (194/827), county CH4, and the
per-county coverage weights (all 2296 cells of the first county, max diff 2e-8).
Re-running R's own algorithm on identical inputs reproduces the Python rasters to a
median 4e-8 per cell. Against the captured oracle a residual ~1e-4 median per-cell
difference remains, direction unknown but bounded and mass-conserving (the sums
agree), so the assertions below pin the totals tightly and the per-cell agreement
loosely rather than pretending to a precision we have not demonstrated.
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pytest

from m3t import datasets, geo
from m3t.config import Config
from m3t.sectors.stationary_combustion import (
    compute_stationary_combustion,
    county_emissions,
    prepare_nei,
    prepare_seds,
)

pytestmark = pytest.mark.golden

_GOLD = Path(__file__).parent / "golden" / "stationary_combustion"
_ORACLE = _GOLD / "stat_comb_oracle.json"
_COUNTIES = _GOLD / "counties_ctri.geojson"
_DOMAIN = _GOLD / "domain_ct_ri.geojson"
_ACES = {k: _GOLD / f"aces_{k}_ctri.tif" for k in ("res", "com", "ind", "elec")}


@pytest.fixture(scope="module")
def oracle():
    missing = [
        p.name
        for p in (_ORACLE, _COUNTIES, _DOMAIN, *_ACES.values())
        if not p.exists()
    ]
    if missing:
        pytest.skip(
            f"stationary combustion fixtures missing ({', '.join(missing)}); run "
            "`M3T_DATA=/path/to/M3T_Processed conda run -n M3T Rscript "
            "python/tests/golden/capture_stationary_combustion_oracle.R`"
        )
    return json.loads(_ORACLE.read_text())


@pytest.fixture(scope="module")
def tables(oracle):
    cfg = Config()
    yr = oracle["params"]["GHGI_data_yr"]
    state_total, domain_total = prepare_seds(
        datasets.load("EIA_SEDS"),
        datasets.load("GHGI_stationary_combustion"),
        cfg.stationary_combustion_emission_factors,
        seds_yr=yr,
    )
    nei = prepare_nei(datasets.load("NEI_all_years"), nei_year=yr)
    return state_total, domain_total, county_emissions(nei, state_total, domain_total)


@pytest.fixture(scope="module")
def results(oracle, tables):
    import geopandas as gpd
    import rioxarray

    p = oracle["params"]
    xmin, xmax, ymin, ymax = p["domain_ext"]
    template = geo.make_grid((xmin, ymin, xmax, ymax), p["domain_res"], p["domain_crs"])
    counties = gpd.read_file(_COUNTIES).to_crs(p["domain_crs"])
    domain = gpd.read_file(_DOMAIN).to_crs(p["domain_crs"])
    aces = {
        k: rioxarray.open_rasterio(path, masked=True).squeeze("band", drop=True)
        for k, path in _ACES.items()
    }
    _, _, county_ch4 = tables

    return compute_stationary_combustion(
        inventories=aces,
        county_ch4=county_ch4,
        county_tigerlines=counties,
        domain=domain,
        domain_template=template,
        domain_crs=p["domain_crs"],
        inventory_name="aces",
        by_state=True,
        by_domain=True,
    )


def test_seds_state_total_matches_r(tables):
    """Spot-check the SEDS -> CH4 chain against a value read out of R."""
    state_total, _, _ = tables
    ct = state_total[
        (state_total["State"] == "CT") & (state_total["Sector"] == "res_petr_ER")
    ]["state_ch4_emiss"].iloc[0]
    assert ct == pytest.approx(0.950656125, rel=1e-9)


def test_nei_fraction_matches_r(tables):
    """Fairfield County's share of Connecticut's residential-oil CO (194/827)."""
    _, _, county_ch4 = tables
    row = county_ch4[
        (county_ch4["STATE_FIPS"] == "09") & (county_ch4["COUNTY_FIPS"] == "001")
    ]
    assert row["bystate.res_petr_ER"].iloc[0] == pytest.approx(
        0.950656125 * 194 / 827, rel=1e-9
    )


def test_all_oracle_rasters_produced(oracle, results):
    expected = {f[:-3] for f in oracle["rasters"]}
    assert expected <= set(results), sorted(expected - set(results))


@pytest.mark.parametrize(
    "key",
    [
        f"stat_comb_{fuel}_by{level}_aces"
        for fuel in (
            "res_petr", "res_wood",
            "com_coal", "com_petr", "com_gas", "com_wood",
            "ind_coal", "ind_petr", "ind_gas", "ind_wood",
            "elec_coal", "elec_petr", "elec_gas", "elec_wood",
        )
        for level in ("state", "domain")
    ]
    + [
        f"Stationary_combustion_sector_{label}_total_ACES_by{level}"
        for label in ("fossil_fuel", "wood")
        for level in ("state", "domain")
    ],
)
def test_raster_matches_r(oracle, results, key):
    r = oracle["rasters"][f"{key}.nc"]
    da = results[key]
    assert da.shape == (r["nrow"], r["ncol"]), f"{key} dims"

    py = da.values.astype("float64").ravel()
    ref = np.array([np.nan if v is None else v for v in r["values"]], dtype="float64")

    if key.startswith("Stationary_combustion_sector"):
        # R's sector totals come out vertically flipped, and it is worth knowing why.
        # It builds them by re-reading its own NetCDFs -- `sum(rast(list.files(...)))`
        # -- but terra cannot recover the extent it just wrote (both files read back
        # as ext=[0,26,0,11], hence the "[rast] unknown extent" warnings during
        # capture). writeCDF stores y ascending, so the round trip returns the rows
        # south-first while terra treats row 1 as the top; writing the sum out again
        # flips it a second time. The totals therefore sit upside-down relative to
        # the subsector rasters they are made of. We sum in memory and never make the
        # trip through disk, so flip the reference to compare. Unflipped, this same
        # data gives a median error of 0.86 (and identical sums -- the giveaway that
        # it is a rearrangement, not a miscalculation).
        ref = np.flipud(ref.reshape(da.shape)).ravel()

    # the total is the number that must be right
    assert float(np.nansum(py)) == pytest.approx(r["sum"], rel=1e-4), f"{key} total"

    comparable = np.isfinite(py) & np.isfinite(ref) & (np.abs(ref) > 0)
    if not comparable.any():  # an all-zero subsector (e.g. no coal in CT/RI)
        assert np.allclose(np.nan_to_num(py), 0.0)
        return

    rel = np.abs(py[comparable] - ref[comparable]) / np.abs(ref[comparable])
    assert np.median(rel) <= 1e-3, f"{key}: median rel {np.median(rel):.2e}"
    # The stragglers are domain-boundary cells with near-zero values, where terra and
    # GDAL disagree about which cells a boundary covers (the same effect as in
    # septic). The worst is a single grid-edge cell at ~11%, against an R value of
    # 1.2e-5. The electric subsectors are the loosest because their emissions are
    # concentrated in a handful of cells, so a boundary cell is a bigger share.
    assert (rel <= 1e-2).mean() >= 0.90, f"{key}: {(rel <= 1e-2).mean():.1%} within 1e-2"
    assert (rel <= 5e-2).mean() >= 0.95, f"{key}: {(rel <= 5e-2).mean():.1%} within 5e-2"
    assert rel.max() <= 0.15, f"{key}: worst cell off by {rel.max():.2%}"


def test_sector_totals_are_sums_of_parts(results):
    """The fossil-fuel total is coal+gas+petroleum; wood is wood. No double counting."""
    for level in ("state", "domain"):
        ff = results[f"Stationary_combustion_sector_fossil_fuel_total_ACES_by{level}"].values
        parts = sum(
            np.nan_to_num(results[f"stat_comb_{s}_{f}_by{level}_aces"].values)
            for s in ("res", "com", "ind", "elec")
            for f in ("coal", "gas", "petr")
            if f"stat_comb_{s}_{f}_by{level}_aces" in results
        )
        assert np.allclose(np.nan_to_num(ff), parts, rtol=1e-9)
