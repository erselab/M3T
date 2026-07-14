"""Golden test: Python NG distribution vs the real R Natural_Gas_Distribution.

The oracle (tests/golden/ng_distribution/ng_dist_oracle.json) was produced by
calling M3T:::Natural_Gas_Distribution on a CT+RI domain with ACES, both
disaggregation levels enabled; see capture_ng_distribution_oracle.R. ACES rather
than the default Vulcan for the same reason as stationary combustion: it ships on
the companion drive and the code path is identical.

Accuracy. The 12 `bydomain` rasters match R outright at the normal 1e-4 tolerance.
The 12 `bystate` rasters have a median per-cell error of ~1e-8 (the interior is
exact) and totals agreeing to ~1e-6, but a handful of cells on the domain boundary
differ -- the same terra-vs-GDAL disagreement over which cells a polygon edge
covers that shows up in septic and stationary combustion. CT and RI are small and
coastal, so boundary cells are an unusually large share of a 236-cell grid; the
worst is 13% on a cell whose value is 3e-4. `bydomain` is immune because it
dissolves the states into a single polygon.

by_LDC is not ported (it reads the output of a semi-manual script that ships
outside the package) and is off by default.
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from m3t import datasets, geo
from m3t.config import Config
from m3t.sectors.ng_distribution import (
    COM_TOTALS,
    RES_TOTALS,
    SUBSECTORS,
    compute_emissions,
    compute_ng_distribution,
    prepare_activity,
    resolve_ghgi_tables,
    stations_per_mile,
)

pytestmark = pytest.mark.golden

_GOLD = Path(__file__).parent / "golden" / "ng_distribution"
_ORACLE = _GOLD / "ng_dist_oracle.json"
_FACILITY = _GOLD / "facility_data_ngdist.csv"
_SUBW = _GOLD / "subpartW_ngdist.csv"
_DOMAIN = _GOLD / "domain_ct_ri.geojson"
_ACES = {k: _GOLD / f"aces_{k}_ctri.tif" for k in ("res", "com")}


@pytest.fixture(scope="module")
def oracle():
    missing = [
        p.name for p in (_ORACLE, _FACILITY, _SUBW, _DOMAIN, *_ACES.values()) if not p.exists()
    ]
    if missing:
        pytest.skip(
            f"NG distribution fixtures missing ({', '.join(missing)}); run "
            "`M3T_DATA=/path/to/M3T_Processed conda run -n M3T Rscript "
            "python/tests/golden/capture_ng_distribution_oracle.R`"
        )
    return json.loads(_ORACLE.read_text())


@pytest.fixture(scope="module")
def emissions(oracle):
    yr = oracle["params"]["GHGI_data_yr"]
    states = list(oracle["params"]["domain_states"])
    cfg = Config()

    above, below = stations_per_mile(
        pd.read_csv(_FACILITY, low_memory=False),
        pd.read_csv(_SUBW, low_memory=False),
        datasets.load("GHGRP_LDC"),
        datasets.load("Neighboring_states"),
        year=yr,
    )
    activity = prepare_activity(
        datasets.load("EIA_NG_data"),
        datasets.load("PHMSA_natural_gas_distribution"),
        year=yr,
        states=states,
        above_rate=above,
        below_rate=below,
    )
    return compute_emissions(
        activity,
        resolve_ghgi_tables(datasets.load("GHGI_NG_distribution"), yr),
        cfg.natural_gas_pipeline_emission_factors,
        res_post_meter_ef=cfg.natural_gas_res_post_meter_emission_factor,
        com_post_meter_ef=cfg.natural_gas_com_post_meter_emission_factor,
    )


@pytest.fixture(scope="module")
def results(oracle, emissions):
    import geopandas as gpd
    import rioxarray

    p = oracle["params"]
    xmin, xmax, ymin, ymax = p["domain_ext"]
    template = geo.make_grid((xmin, ymin, xmax, ymax), p["domain_res"], p["domain_crs"])
    domain = gpd.read_file(_DOMAIN).to_crs(p["domain_crs"])
    aces = {
        k: rioxarray.open_rasterio(path, masked=True).squeeze("band", drop=True)
        for k, path in _ACES.items()
    }
    return compute_ng_distribution(
        emissions=emissions,
        state_tigerlines=domain,
        inventories=aces,
        domain=domain,
        domain_template=template,
        domain_crs=p["domain_crs"],
        inventory_name="aces",
        by_state=True,
        by_domain=True,
        # R parity: the R's gridding is not mass-conserving (it treats a 1 km
        # conformal-projection pixel as 1 km2 and area-averages the flux, inflating
        # the total ~1.4%). The shipped default fixes that; this fixture pins the
        # R-compatible path so parity stays reproducible. Mass conservation is
        # checked separately, in test_mass_conservation.py.
        mass_conserving=False,
    )


def test_post_meter_is_residential_only(emissions):
    """The commercial post-meter EF is 0, so all post-meter loss is residential."""
    assert (emissions["post_meter_ER_total_com"] == 0).all()
    assert (emissions["post_meter_ER_total_res"] > 0).all()


def test_all_subsectors_present(emissions):
    assert set(emissions.columns) == set(RES_TOTALS) | set(COM_TOTALS)
    assert len(SUBSECTORS) == 6


def test_all_oracle_rasters_produced(oracle, results):
    expected = {f[:-3] for f in oracle["rasters"]}
    assert expected <= set(results), sorted(expected - set(results))


@pytest.mark.parametrize(
    "key",
    [
        f"NG_dist_{sub}_{sector}_by{level}_aces"
        for sub in SUBSECTORS
        for sector in ("res", "com")
        for level in ("state", "domain")
    ],
)
def test_raster_matches_r(oracle, results, key):
    r = oracle["rasters"][f"{key}.nc"]
    da = results[key]
    assert da.shape == (r["nrow"], r["ncol"]), f"{key} dims"

    py = da.values.astype("float64").ravel()
    ref = np.array([np.nan if v is None else v for v in r["values"]], dtype="float64")

    assert float(np.nansum(py)) == pytest.approx(r["sum"], rel=1e-4), f"{key} total"

    comparable = np.isfinite(py) & np.isfinite(ref) & (np.abs(ref) > 0)
    if not comparable.any():  # commercial post-meter is identically zero
        assert np.allclose(np.nan_to_num(py), 0.0)
        return

    rel = np.abs(py[comparable] - ref[comparable]) / np.abs(ref[comparable])
    # the interior is exact; the stragglers are coastal domain-boundary cells
    assert np.median(rel) <= 1e-6, f"{key}: median rel {np.median(rel):.2e}"
    assert (rel <= 1e-4).mean() >= 0.85, f"{key}: {(rel <= 1e-4).mean():.1%} within 1e-4"
    assert (rel <= 1e-2).mean() >= 0.95, f"{key}: {(rel <= 1e-2).mean():.1%} within 1e-2"
    assert rel.max() <= 0.15, f"{key}: worst cell off by {rel.max():.2%}"


def test_bydomain_is_exact(oracle, results):
    """Dissolving the states removes the internal border, so bydomain has no
    boundary ambiguity at all and must match R at the normal tolerance."""
    for key, da in results.items():
        if "bydomain" not in key:
            continue
        r = oracle["rasters"][f"{key}.nc"]
        py = da.values.astype("float64").ravel()
        ref = np.array([np.nan if v is None else v for v in r["values"]], dtype="float64")
        assert np.allclose(py, ref, rtol=1e-4, atol=1e-12, equal_nan=True), key
