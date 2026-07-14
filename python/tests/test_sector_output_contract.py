"""Audit: the top-level filenames sectors write must match what `combine` reads.

Sectors hand work to `combine` through the filesystem: each writes its sector
totals to `out/`, and `combine` discovers the available variations *by filename*.
That contract is invisible to the type checker and to every per-sector golden
test, so a single misspelled name makes a whole sector silently vanish from the
combined inventory. It has already happened twice:

* `ng_distribution` wrote its variations only into a subdirectory, so combine
  found no NG-distribution variations at all;
* stationary combustion built its name with `inventory_name.upper()`, yielding
  `..._VULCAN_bystate.nc` where R (and combine) expect `..._Vulcan_bystate.nc` --
  and `Use_Vulcan` is the *default*.

`R_TOP_LEVEL` below is the authoritative list, read straight out of the R sources
(every `writeCDF(..., file.path(output_directory, ...))` call).
"""

from __future__ import annotations

import pytest
import xarray as xr

from m3t import geo
from m3t.combine import discover_sectors
from m3t.disaggregation import INVENTORY_LABEL
from m3t.sectors.ng_distribution import sector_total_name as ngd_name
from m3t.sectors.stationary_combustion import sector_total_name as sc_name

_INVENTORIES = ("aces", "vulcan")

# --- everything R writes to output_directory (the combine input surface) ----- #
R_TOP_LEVEL = sorted(
    [
        # Prepare_GEPA.R
        "GEPA_ind_landfill.nc",
        "GEPA_non_thermo.nc",
        "GEPA_thermo.nc",
        # Natural_Gas_Transmission.R
        "NG_transmission_sector_total.nc",
        # Municipal_solid_waste.R
        "Landfill_sector_total_GHGRP_reported.nc",
        "Landfill_sector_total_GHGRP_generation_first.nc",
        "Landfill_sector_total_GHGRP_collection_first.nc",
        # SOCCR_Wetlands.R  (Wetcharts is one file per model subset)
        "Wetland_sector_total_SOCCR1.nc",
        "Wetland_sector_total_SOCCR2.nc",
        "Wetland_sector_total_Wetcharts_NLCD_subset_1.nc",
    ]
    # Wastewater.R
    + [
        f"Wastewater_sector_total_{s}_{m}_{k}.nc"
        for s in ("CWNS", "DMR")
        for m in ("GHGI", "Moore")
        for k in ("state", "national")
    ]
    # Natural_Gas_Distribution.R
    + [
        f"NG_distribution_sector_total_{i}_by{lvl}.nc"
        for i in ("ACES", "Vulcan")
        for lvl in ("LDC", "state", "domain")
    ]
    # Stationary_combustion.R  (two files per variation: fossil fuel AND wood)
    + [
        f"Stationary_combustion_sector_{kind}_total_{i}_by{lvl}.nc"
        for kind in ("fossil_fuel", "wood")
        for i in ("ACES", "Vulcan")
        for lvl in ("state", "domain")
    ]
)


def test_combine_claims_every_file_r_writes(tmp_path):
    """No R output may be silently ignored by combine's discovery."""
    out = tmp_path / "out"
    out.mkdir()
    template = geo.make_grid((-75.0, 40.0, -74.7, 40.2), 0.1, "epsg:4326")
    for name in R_TOP_LEVEL:
        da = xr.full_like(template.astype("float64"), 1.0)
        da.rio.write_crs("epsg:4326", inplace=True)
        geo.write_cdf(da, out / name, varname="methane_emissions")

    set_files, sectors = discover_sectors(out)
    claimed = {p.name for p in set_files}
    for s in sectors:
        for v in s.variations:
            claimed |= {p.name for p in v.files}

    unclaimed = sorted(set(R_TOP_LEVEL) - claimed)
    assert not unclaimed, f"combine ignores files R writes: {unclaimed}"


def test_stationary_combustion_names_match_r():
    """The name the *sector itself* builds must be exactly R's -- `Vulcan`, not `VULCAN`."""
    for inv in _INVENTORIES:
        for level in ("state", "domain"):
            for label in ("fossil_fuel", "wood"):
                name = f"{sc_name(label, inv, level)}.nc"
                assert name in R_TOP_LEVEL, f"{name} is not a filename R writes"


def test_ng_distribution_names_match_r():
    for inv in _INVENTORIES:
        for level in ("state", "domain", "LDC"):
            name = f"{ngd_name(inv, level)}.nc"
            assert name in R_TOP_LEVEL, f"{name} is not a filename R writes"


def test_inventory_label_is_not_just_upper():
    """Guard the exact trap that bit us: `.upper()` gives VULCAN, R wants Vulcan."""
    assert INVENTORY_LABEL["vulcan"] == "Vulcan" != "vulcan".upper()
    assert INVENTORY_LABEL["aces"] == "ACES"


@pytest.mark.parametrize("inv", _INVENTORIES)
def test_both_inventories_are_discoverable(tmp_path, inv):
    """A full variation set for one inventory must be seen by combine."""
    out = tmp_path / "out"
    out.mkdir()
    template = geo.make_grid((-75.0, 40.0, -74.7, 40.2), 0.1, "epsg:4326")
    label = INVENTORY_LABEL[inv]
    names = [
        f"Stationary_combustion_sector_fossil_fuel_total_{label}_bystate.nc",
        f"Stationary_combustion_sector_wood_total_{label}_bystate.nc",
        f"NG_distribution_sector_total_{label}_bystate.nc",
    ]
    for name in names:
        da = xr.full_like(template.astype("float64"), 1.0)
        da.rio.write_crs("epsg:4326", inplace=True)
        geo.write_cdf(da, out / name, varname="methane_emissions")

    _, sectors = discover_sectors(out)
    by_name = {s.name: s for s in sectors}
    assert "Stationary_Combustion_options" in by_name, f"{label} stat comb not discovered"
    assert "Natural_Gas_Distribution_options" in by_name, f"{label} NG dist not discovered"
    stat = by_name["Stationary_Combustion_options"]
    assert [v.label for v in stat.variations] == [f"{label}_bystate"]
    # the variation must carry BOTH files
    assert len(stat.variations[0].files) == 2
