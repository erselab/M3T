"""The finer GEPA categories partition the same layers and roll up to the aggregates.

These are an additive, second output (`out/remaining_gepa/`) for downstream uses
that want to swap an alternative inventory in for one source group. The contract
that keeps them trustworthy: every layer lands in exactly one fine category, the
fine categories cover exactly the same 20 layers as the three aggregates, and the
fine fluxes sum to the aggregate fluxes.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

from m3t import geo
from m3t.sectors.remaining_gepa import (
    GEPA_FINE_CATEGORIES,
    LANDFILL_VAR,
    NON_THERMO_VARS,
    THERMO_VARS,
    compute_gepa,
    compute_gepa_fine,
)

_GOLD = Path(__file__).parent / "golden" / "gepa"
_GEPA = _GOLD / "in" / "gepa_ctri.nc"
_DOMAIN = _GOLD / "domain_ct_ri.geojson"

# which aggregate each fine category rolls into
_FINE_TO_AGG = {
    "GEPA_oil_gas_upstream": "GEPA_thermo",
    "GEPA_coal": "GEPA_thermo",
    "GEPA_other": "GEPA_thermo",
    "GEPA_livestock": "GEPA_non_thermo",
    "GEPA_crop_ag": "GEPA_non_thermo",
    "GEPA_industrial_landfill": "GEPA_ind_landfill",
}


# --------------------------------------------------------------------------- #
# structure (no data needed)
# --------------------------------------------------------------------------- #
def test_fine_categories_partition_the_layers():
    """Every layer in exactly one fine category; together they cover all 20."""
    seen: list[str] = []
    for vs in GEPA_FINE_CATEGORIES.values():
        seen += vs
    assert len(seen) == len(set(seen)), "a layer appears in two fine categories"
    assert set(seen) == {LANDFILL_VAR, *NON_THERMO_VARS, *THERMO_VARS}


def test_fine_categories_respect_the_thermo_split():
    """Fine categories nest inside their aggregate (no biogenic/fossil mixing)."""
    agg_layers = {
        "GEPA_thermo": set(THERMO_VARS),
        "GEPA_non_thermo": set(NON_THERMO_VARS),
        "GEPA_ind_landfill": {LANDFILL_VAR},
    }
    for fine, agg in _FINE_TO_AGG.items():
        assert set(GEPA_FINE_CATEGORIES[fine]) <= agg_layers[agg], fine
    # and the fine categories of an aggregate exactly reconstruct it
    for agg, layers in agg_layers.items():
        union: set[str] = set()
        for fine, a in _FINE_TO_AGG.items():
            if a == agg:
                union |= set(GEPA_FINE_CATEGORIES[fine])
        assert union == layers, agg


def test_composting_is_grouped_with_crop_ag():
    assert "emi_ch4_5B1_Composting" in GEPA_FINE_CATEGORIES["GEPA_crop_ag"]


# --------------------------------------------------------------------------- #
# numeric roll-up (needs the committed CT+RI fixture)
# --------------------------------------------------------------------------- #
@pytest.fixture(scope="module")
def rasters():
    if not (_GEPA.exists() and _DOMAIN.exists()):
        pytest.skip("GEPA CT+RI fixture missing")
    import geopandas as gpd
    import xarray as xr

    gepa = xr.open_dataset(_GEPA).rio.write_crs("epsg:4326")
    domain = gpd.read_file(_DOMAIN).to_crs("epsg:4326")
    template = geo.make_grid(tuple(domain.total_bounds), 0.1, "epsg:4326")
    kw = dict(domain=domain, domain_template=template, domain_crs="epsg:4326")
    return compute_gepa(gepa, **kw), compute_gepa_fine(gepa, **kw)


def test_fine_fluxes_sum_to_aggregates(rasters):
    aggregates, fine = rasters
    for agg in ("GEPA_thermo", "GEPA_non_thermo", "GEPA_ind_landfill"):
        parts = [fine[f].fillna(0.0) for f, a in _FINE_TO_AGG.items() if a == agg]
        summed = sum(parts).values
        ref = aggregates[agg].fillna(0.0).values
        assert np.allclose(summed, ref, rtol=1e-6, atol=1e-12), f"{agg} roll-up"


def test_all_six_fine_categories_present(rasters):
    _, fine = rasters
    assert set(fine) == set(GEPA_FINE_CATEGORIES)
    for da in fine.values():
        assert da.shape == next(iter(fine.values())).shape
