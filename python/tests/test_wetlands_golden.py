"""Golden test: Python wetlands vs the real R SOCCR_Wetlands + Wetcharts prep.

The oracle (tests/golden/wetlands/wetlands_oracle.json) was produced on a CT+RI
domain by capture_wetlands_oracle.R, which runs the Wetcharts preparation block of
CH4_inventory_build (it lives inline in the orchestrator, not in a function) and
then M3T:::SOCCR_Wetlands with SOCCR1 and SOCCR2 both enabled.

**R's sector-total block does not run at all** in this environment. It builds the
totals by re-reading the Wetcharts NetCDF it just wrote, and terra cannot recover
the extent it wrote, so the addition dies with "[+] extents do not match". Same
root cause as the flipped stationary-combustion totals: writeCDF output is not
readable back by this GDAL/terra build. The four component rasters are written
before that point and are the reference here; the sector totals are plain sums of
them, which `test_sector_totals_are_sums` checks structurally.

Accuracy. Wetcharts matches R to a median 3.5e-8. The NWI/SOCCR rasters match to a
median ~5e-6. The stragglers are domain-boundary cells (terra and GDAL disagree
about which cells a polygon edge covers) -- and CT/RI is nearly all coastline, so
those are ~25% of a 286-cell grid, which is also why the totals sit ~2% low.
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pytest

from m3t import geo
from m3t.config import Config
from m3t.sectors.wetlands import (
    FRESHWATER_TYPES,
    SOCCR_TYPES,
    combine_nwi_states,
    compute_soccr,
    compute_wetcharts,
)

pytestmark = pytest.mark.golden

_GOLD = Path(__file__).parent / "golden" / "wetlands"
_ORACLE = _GOLD / "wetlands_oracle.json"
_DOMAIN = _GOLD / "domain_ct_ri.geojson"
_NWI = _GOLD / "in" / "processed_NWI_data"
_WATERSHEDS = _GOLD / "in" / "Watersheds.gpkg"
_WETCHARTS = _GOLD / "in" / "wetcharts_ctri.tif"


@pytest.fixture(scope="module")
def oracle():
    missing = [
        p.name for p in (_ORACLE, _DOMAIN, _WATERSHEDS, _WETCHARTS) if not p.exists()
    ]
    if missing or not _NWI.exists():
        pytest.skip(
            "wetlands fixtures missing; run `M3T_DATA=/path/to/M3T_Processed "
            "conda run -n M3T Rscript python/tests/golden/capture_wetlands_oracle.R`"
        )
    return json.loads(_ORACLE.read_text())


@pytest.fixture(scope="module")
def results(oracle):
    import geopandas as gpd
    import rioxarray

    p = oracle["params"]
    xmin, xmax, ymin, ymax = p["domain_ext"]
    template = geo.make_grid((xmin, ymin, xmax, ymax), p["domain_res"], p["domain_crs"])
    domain = gpd.read_file(_DOMAIN).to_crs(p["domain_crs"])
    cfg = Config()

    per_state: dict[str, list] = {}
    for state in p["domain_states"]:
        da = rioxarray.open_rasterio(
            _NWI / f"{state}_combined_NWI_wetland_landcover.tif", masked=True
        )
        for i, cls in enumerate(da.attrs.get("long_name", [])):
            per_state.setdefault(cls, []).append(da.isel(band=i))
    nwi = {cls: combine_nwi_states(v) for cls, v in per_state.items()}

    soccr = compute_soccr(
        nwi,
        wetland_efs=cfg.Wetland_EFs,
        watersheds=gpd.read_file(_WATERSHEDS),
        domain=domain,
        domain_template=template,
        domain_crs=p["domain_crs"],
        use_soccr1=True,
        use_soccr2=True,
    )

    wet = rioxarray.open_rasterio(_WETCHARTS, masked=True)
    wet = wet.assign_coords(band_name=("band", list(wet.attrs.get("long_name", []))))
    wetcharts, _ = compute_wetcharts(
        wet,
        inventory_year=p["inventory_year"],
        model_subsets=cfg.Wetcharts_model_subset,
        domain=domain,
        domain_template=template,
        domain_crs=p["domain_crs"],
    )
    return {**soccr, **wetcharts}


def test_nwi_classes_complete(results):
    assert set(SOCCR_TYPES) | set(FRESHWATER_TYPES) == {
        "M2", "E2", "PFO", "PNF", "R1", "R2", "R3", "R4", "L1", "L2",
    }
    assert {"Freshwater", "SOCCR1", "SOCCR2"} <= set(results)


def test_wetland_efs_are_converted_from_g_per_year(results):
    """The config EFs are g CH4/m2/yr; the rasters must be nmol/m2/s.

    SOCCR_Wetlands.R converts them on its first line. Missing that conversion is a
    silent factor of ~1.975 -- it does not change the spatial pattern at all, only
    the magnitude, so nothing but a magnitude check catches it.
    """
    # freshwater over CT/RI lands around ~47 nmol/m2/s summed; without the
    # conversion it would be ~24.
    assert 35 < float(np.nansum(results["Freshwater"].values)) < 60


@pytest.mark.parametrize(
    "key", ["Freshwater", "SOCCR1", "SOCCR2", "Wetcharts_NLCD_Downscaled_subset_1"]
)
def test_raster_matches_r(oracle, results, key):
    r = oracle["rasters"][f"{key}.nc"]
    da = results[key]
    assert da.shape == (r["nrow"], r["ncol"]), f"{key} dims"

    py = da.values.astype("float64").ravel()
    ref = np.array([np.nan if v is None else v for v in r["values"]], dtype="float64")

    comparable = np.isfinite(py) & np.isfinite(ref) & (np.abs(ref) > 0)
    rel = np.abs(py[comparable] - ref[comparable]) / np.abs(ref[comparable])

    # the interior is exact; the stragglers are coastal domain-boundary cells
    assert np.median(rel) <= 1e-4, f"{key}: median rel {np.median(rel):.2e}"
    assert (rel <= 1e-4).mean() >= 0.70, f"{key}: {(rel <= 1e-4).mean():.1%} within 1e-4"
    assert (rel <= 5e-2).mean() >= 0.95, f"{key}: {(rel <= 5e-2).mean():.1%} within 5e-2"
    assert rel.max() <= 0.15, f"{key}: worst cell off by {rel.max():.2%}"
    assert float(np.nansum(py)) == pytest.approx(r["sum"], rel=3e-2), f"{key} total"


def test_sector_totals_are_sums(results):
    """Freshwater is added to every variant; the estimates stay separate.

    R cannot produce these itself here (see the module docstring), so this is a
    structural check rather than a comparison.
    """
    fresh = np.nan_to_num(results["Freshwater"].values)
    for variant in ("SOCCR1", "SOCCR2", "Wetcharts_NLCD_Downscaled_subset_1"):
        total = np.nan_to_num(results[variant].values) + fresh
        assert np.all(total >= fresh - 1e-12)
        assert float(total.sum()) > float(fresh.sum())
