"""Golden test: Python remaining-GEPA vs the real R Prepare_GEPA.

The oracle (tests/golden/gepa/gepa_oracle.json) was produced by calling
M3T:::Prepare_GEPA on a CT+RI domain at *two* resolutions, because the function
branches on which grid is coarser and the branches share no code:

* **0.1 deg** — same as GEPA, so it refines with a nearest disagg and reprojects
  nearest. This is the default case, and it reproduces R essentially bit-exactly
  (median relative error ~3e-13).
* **0.25 deg** — coarser than GEPA, so it area-averages, weighting cells by how
  much of them lies inside the domain. This is the only place in all of M3T that
  asks for terra's *exact* extract weights rather than the sub-sampled
  approximation. Matches to a median ~1e-5, with a few boundary cells at ~1e-3.
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pytest

from m3t import geo
from m3t.sectors.remaining_gepa import (
    LANDFILL_VAR,
    NON_THERMO_VARS,
    THERMO_VARS,
    compute_gepa,
)

pytestmark = pytest.mark.golden

_GOLD = Path(__file__).parent / "golden" / "gepa"
_ORACLE = _GOLD / "gepa_oracle.json"
_DOMAIN = _GOLD / "domain_ct_ri.geojson"
_GEPA = _GOLD / "in" / "gepa_ctri.nc"

_OUTPUTS = ["GEPA_ind_landfill", "GEPA_non_thermo", "GEPA_thermo"]


@pytest.fixture(scope="module")
def oracle():
    missing = [p.name for p in (_ORACLE, _DOMAIN, _GEPA) if not p.exists()]
    if missing:
        pytest.skip(
            f"GEPA fixtures missing ({', '.join(missing)}); run "
            "`M3T_GEPA=/path/to/gepa.nc conda run -n M3T Rscript "
            "python/tests/golden/capture_gepa_oracle.R`"
        )
    return json.loads(_ORACLE.read_text())


@pytest.fixture(scope="module")
def gepa():
    import xarray as xr

    return xr.open_dataset(_GEPA).rio.write_crs("epsg:4326")


@pytest.fixture(scope="module")
def results(oracle, gepa):
    import geopandas as gpd

    p = oracle["params"]
    domain = gpd.read_file(_DOMAIN).to_crs(p["domain_crs"])
    out = {}
    for dres in p["resolutions"]:
        template = geo.make_grid(tuple(domain.total_bounds), dres, p["domain_crs"])
        out[dres] = compute_gepa(
            gepa, domain=domain, domain_template=template, domain_crs=p["domain_crs"]
        )
    return out


def test_sector_groups_do_not_overlap():
    """No GEPA layer may land in two groups -- that would double-count it."""
    groups = [{LANDFILL_VAR}, set(NON_THERMO_VARS), set(THERMO_VARS)]
    for i, a in enumerate(groups):
        for b in groups[i + 1 :]:
            assert not (a & b)


def test_m3t_owned_sectors_are_excluded(gepa):
    """M3T computes these itself; taking GEPA's copy too would double-count.

    Stationary combustion is the trap: GEPA has both a Mobile and a Stationary
    combustion layer, and only Mobile belongs here.
    """
    taken = {LANDFILL_VAR, *NON_THERMO_VARS, *THERMO_VARS}
    assert "emi_ch4_1A_Combustion_Stationary" in gepa  # present in the file...
    assert "emi_ch4_1A_Combustion_Stationary" not in taken  # ...but deliberately unused
    for name in gepa.data_vars:
        if "Landfills_Municipal" in name or "Distribution" in name:
            assert name not in taken


@pytest.mark.parametrize("dres", [0.1, 0.25])
@pytest.mark.parametrize("key", _OUTPUTS)
def test_raster_matches_r(oracle, results, key, dres):
    r = oracle["rasters"][f"{key}@{dres}"]
    da = results[dres][key]
    assert da.shape == (r["nrow"], r["ncol"]), f"{key}@{dres} dims"

    py = da.values.astype("float64").ravel()
    ref = np.array([np.nan if v is None else v for v in r["values"]], dtype="float64")

    # the NaN mask must agree exactly: terra::mask keeps every *touched* cell, and
    # getting that wrong silently drops coastal cells (and 15% of the emissions)
    assert int((np.isnan(py) != np.isnan(ref)).sum()) == 0, f"{key}@{dres} NaN mask"

    assert float(np.nansum(py)) == pytest.approx(r["sum"], rel=1e-5), f"{key}@{dres} total"

    comparable = np.isfinite(py) & np.isfinite(ref) & (np.abs(ref) > 0)
    rel = np.abs(py[comparable] - ref[comparable]) / np.abs(ref[comparable])
    if dres == 0.1:
        # nearest-neighbour throughout: this should be exact
        assert rel.max() <= 1e-6, f"{key}@{dres} max rel {rel.max():.2e}"
    else:
        # area-average: a few domain-boundary cells differ
        assert np.median(rel) <= 1e-4, f"{key}@{dres} median rel {np.median(rel):.2e}"
        assert rel.max() <= 5e-3, f"{key}@{dres} max rel {rel.max():.2e}"


def test_units_converted_from_molec_cm2(results):
    """GEPA is molec/cm2/s; the outputs are nmol/m2/s (factor ~1.66e-9)."""
    total = float(np.nansum(results[0.1]["GEPA_non_thermo"].values))
    assert 100 < total < 400  # raw molec/cm2/s would be ~1e11
