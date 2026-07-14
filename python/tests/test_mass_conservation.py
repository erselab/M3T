"""Mass conservation: what goes into the grid must come out of it.

The rasters are fluxes (nmol/m²/s), so the mass in a cell is ``flux * cell_area``.
For the Vulcan/ACES-disaggregated sectors the input is known exactly — the county
CH₄ totals — so the gridded raster's total mass must equal it. Emissions from fine
pixels that only partly fall inside a coarse cell, or inside the region, have to be
carried across, not dropped.

This is a scientific requirement of M3T, not a rounding detail, so it gets its own
test file rather than living inside a tolerance in a golden test.

The R does *not* conserve mass here, and the gap is ~1.4%:

* it treats each 1 km Vulcan/ACES pixel as exactly 1 km² (mol/s "per pixel" becomes
  mol/km²/s with a ``*1000``). Those grids are Lambert Conformal Conic — conformal,
  not equal-area — so a "1 km" pixel is really ~1.009 km² at CT/RI's latitude;
* it then area-*averages* the flux onto the run grid and weights cells by their
  coverage of the domain's *bounding box*, neither of which conserves a total.

``Config.Mass_conserving_regrid`` (default True) replaces that with a conservative
regrid. Set it False to reproduce R's numbers; the golden tests do exactly that.
"""

from __future__ import annotations

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

_GOLD = Path(__file__).parent / "golden" / "stationary_combustion"
_COUNTIES = _GOLD / "counties_ctri.geojson"
_DOMAIN = _GOLD / "domain_ct_ri.geojson"
_ACES = {k: _GOLD / f"aces_{k}_ctri.tif" for k in ("res", "com", "ind", "elec")}

_SUBSECTORS = ["res_petr", "res_wood", "com_gas", "ind_gas", "elec_gas"]


def _raster_mass(da, template) -> float:
    """Total mass on a flux raster (nmol/m²/s -> mol/s)."""
    area = geo.cell_area(template, unit="m").values
    return float(np.nansum(da.values * area)) * 1e-9


@pytest.fixture(scope="module")
def setup():
    import geopandas as gpd
    import rioxarray

    for p in (_COUNTIES, _DOMAIN, *_ACES.values()):
        if not p.exists():
            pytest.skip("stationary combustion fixtures missing")

    domain = gpd.read_file(_DOMAIN).to_crs("epsg:4326")
    counties = gpd.read_file(_COUNTIES).to_crs("epsg:4326")
    template = geo.make_grid(tuple(domain.total_bounds), 0.1, "epsg:4326")
    aces = {
        k: rioxarray.open_rasterio(p, masked=True).squeeze("band", drop=True)
        for k, p in _ACES.items()
    }

    cfg = Config()
    state_total, domain_total = prepare_seds(
        datasets.load("EIA_SEDS"),
        datasets.load("GHGI_stationary_combustion"),
        cfg.stationary_combustion_emission_factors,
        seds_yr=2017,
    )
    county_ch4 = county_emissions(
        prepare_nei(datasets.load("NEI_all_years"), nei_year=2017), state_total, domain_total
    )

    def run(mass_conserving: bool):
        return compute_stationary_combustion(
            inventories=aces,
            county_ch4=county_ch4,
            county_tigerlines=counties,
            domain=domain,
            domain_template=template,
            domain_crs="epsg:4326",
            inventory_name="aces",
            by_state=True,
            by_domain=False,
            mass_conserving=mass_conserving,
        )

    # the mass that goes in: the CT/RI county totals (mol/s)
    ctri = county_ch4[county_ch4["STATE_FIPS"].isin(["09", "44"])]
    mass_in = {s: float(ctri[f"bystate.{s}_ER"].sum()) for s in _SUBSECTORS}
    return run, template, mass_in, domain


@pytest.mark.parametrize("subsector", _SUBSECTORS)
def test_gridded_mass_equals_county_totals(setup, subsector):
    """The raster must carry exactly the emissions the counties handed it."""
    run, template, mass_in, _ = setup
    results = run(True)
    da = results[f"stat_comb_{subsector}_bystate_aces"]
    assert _raster_mass(da, template) == pytest.approx(mass_in[subsector], rel=2e-3)


def test_r_parity_path_does_not_conserve_mass(setup):
    """Pin the R's ~1.4% inflation, so the difference stays visible and deliberate.

    If this ever starts passing at the conserving tolerance, the R-parity path has
    silently changed and the golden tests are no longer testing what they claim.
    """
    run, template, mass_in, _ = setup
    results = run(False)
    ratios = [
        _raster_mass(results[f"stat_comb_{s}_bystate_aces"], template) / mass_in[s]
        for s in _SUBSECTORS
    ]
    assert np.mean(ratios) == pytest.approx(1.014, abs=0.01)


def test_emissions_stay_inside_the_region(setup):
    """No emissions may land in cells the region does not touch at all."""
    run, template, mass_in, domain = setup
    results = run(True)
    weights = geo.coverage_fraction(template, domain)
    untouched = np.isnan(weights.values)
    area = geo.cell_area(template, unit="m").values

    for subsector in _SUBSECTORS:
        da = results[f"stat_comb_{subsector}_bystate_aces"].values
        leaked = float(np.nansum(da[untouched] * area[untouched])) * 1e-9
        assert leaked / mass_in[subsector] < 5e-3, f"{subsector} leaked outside the region"
