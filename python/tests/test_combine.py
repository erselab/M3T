"""Combine_across_sectors: correctness, plus an explicit pin on where R is wrong.

Driven by the same synthetic constant-valued sector rasters the R oracle uses
(tests/golden/capture_combine_oracle.R), so every expected value is arithmetic we
can state in closed form.

`combine.py` is a *corrected* port. R's version:
  1. gives each individual combination only ONE of stationary combustion's two
     files (fossil OR wood) and writes a key CSV that doesn't match the rasters;
  2. halves stationary combustion in the summary (min/mean/max across the
     individual fossil/wood layers instead of across variation totals);
  3. cannot run Separate_thermo + summary at all.
See combine.py's module docstring. The R-parity tests below pin R's numbers so the
divergence stays deliberate and visible.
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest
import xarray as xr

from m3t import geo
from m3t.combine import combine_across_sectors, discover_sectors
from m3t.config import Config
from m3t.context import RunContext

_ORACLE = Path(__file__).parent / "golden" / "combine" / "combine_oracle.json"

# The synthetic sector values (must match capture_combine_oracle.R).
_INPUTS = {
    "GEPA_ind_landfill.nc": 1,
    "GEPA_non_thermo.nc": 2,
    "GEPA_thermo.nc": 4,
    "NG_transmission_sector_total.nc": 8,
    "Landfill_sector_total_GHGRP_reported.nc": 10,
    "Landfill_sector_total_GHGRP_generation_first.nc": 20,
    "Wetland_sector_total_SOCCR1.nc": 100,
    "Wetland_sector_total_Wetcharts_NLCD_subset_1.nc": 200,
    "Wastewater_sector_total_DMR_Moore_national.nc": 1000,
    "Wastewater_sector_total_CWNS_GHGI_state.nc": 2000,
    "NG_distribution_sector_total_Vulcan_bystate.nc": 10000,
    "NG_distribution_sector_total_ACES_bystate.nc": 20000,
    "Stationary_combustion_sector_fossil_fuel_total_Vulcan_bystate.nc": 100000,
    "Stationary_combustion_sector_wood_total_Vulcan_bystate.nc": 200000,
    "Stationary_combustion_sector_fossil_fuel_total_ACES_bystate.nc": 300000,
    "Stationary_combustion_sector_wood_total_ACES_bystate.nc": 400000,
}

SET_TOTAL = 1 + 2 + 4 + 8  # 15


@pytest.fixture
def ctx(tmp_path) -> RunContext:
    template = geo.make_grid((-75.0, 40.0, -74.7, 40.2), 0.1, "epsg:4326")
    out = tmp_path / "out"
    out.mkdir(parents=True)
    for name, value in _INPUTS.items():
        da = xr.full_like(template.astype("float64"), float(value))
        da.rio.write_crs("epsg:4326", inplace=True)
        geo.write_cdf(da, out / name, varname="methane_emissions")

    cfg = Config()
    cfg.Separate_thermo = True
    cfg.Create_summary_combinations = True
    cfg.Create_individual_combinations = True
    return RunContext(
        config=cfg,
        run_directory=tmp_path,
        input_directory=tmp_path / "in",
        output_directory=out,
        plot_directory=None,
        inventory_year=2019,
        domain_template=template,
        domain=(-75.0, 40.0, -74.7, 40.2),
        domain_crs="epsg:4326",
    )


def _const(path: Path, layer: int | None = None) -> float:
    ds = xr.open_dataset(path, decode_coords="all")
    da = ds[next(iter(ds.data_vars))]
    if layer is not None:
        da = da.isel(stat=layer)
    vals = np.asarray(da.values, dtype="float64").ravel()
    return float(vals[np.isfinite(vals)][0])


# --------------------------------------------------------------------------- #
# discovery
# --------------------------------------------------------------------------- #
def test_discovers_sectors_and_variations(ctx):
    set_files, sectors = discover_sectors(ctx.output_directory)
    assert sorted(p.name for p in set_files) == sorted(
        n for n in _INPUTS if n.startswith(("GEPA_", "NG_transmission"))
    )
    got = {s.name: [v.label for v in s.variations] for s in sectors}
    assert got["Landfill_options"] == ["GHGRP_reported", "GHGRP_generation_first"]
    assert got["Natural_Gas_Distribution_options"] == ["ACES_bystate", "Vulcan_bystate"]
    assert got["Stationary_Combustion_options"] == ["ACES_bystate", "Vulcan_bystate"]
    # only variations whose files exist are offered
    assert "GHGRP_collection_first" not in got["Landfill_options"]


def test_stationary_combustion_variation_keeps_both_files(ctx):
    """The bug R has: a variation must carry fossil fuel AND wood."""
    _, sectors = discover_sectors(ctx.output_directory)
    stat = next(s for s in sectors if s.name == "Stationary_Combustion_options")
    for v in stat.variations:
        kinds = {("fossil_fuel" if "fossil_fuel" in p.name else "wood") for p in v.files}
        assert kinds == {"fossil_fuel", "wood"}, f"{v.label} lost a file"


# --------------------------------------------------------------------------- #
# summary combinations
# --------------------------------------------------------------------------- #
def test_summary_min_mean_max(ctx):
    combine_across_sectors(ctx)
    p = ctx.output_directory / "Combined_files" / "summary_combinations" / (
        "Summary_combination_inventories.nc"
    )
    # per-sector variation totals (stat comb SUMS fossil+wood):
    #   landfill  10 / 20        wetland 100 / 200
    #   wastewater 2000 / 1000   ng dist 20000 / 10000
    #   stat comb  700000 (ACES: 300k+400k) / 300000 (Vulcan: 100k+200k)
    expect_min = SET_TOTAL + 10 + 100 + 1000 + 10000 + 300000
    expect_max = SET_TOTAL + 20 + 200 + 2000 + 20000 + 700000
    expect_mean = SET_TOTAL + 15 + 150 + 1500 + 15000 + 500000
    assert _const(p, 0) == pytest.approx(expect_min)
    assert _const(p, 1) == pytest.approx(expect_mean)
    assert _const(p, 2) == pytest.approx(expect_max)


def test_thermo_split_summary(ctx):
    combine_across_sectors(ctx)
    sdir = ctx.output_directory / "Combined_files" / "summary_combinations"
    thermo = sdir / "Summary_combination_thermogenic_inventories.nc"
    non = sdir / "Summary_combination_non_thermogenic_inventories.nc"

    # thermogenic: GEPA_thermo(4) + NG transmission(8) + NG dist + stat comb FOSSIL
    t_mean = 4 + 8 + (20000 + 10000) / 2 + (300000 + 100000) / 2
    # non-thermogenic: GEPA ind landfill(1) + GEPA non-thermo(2) + landfills
    #                  + wetlands + wastewater + stat comb WOOD
    n_mean = 1 + 2 + 15 + 150 + 1500 + (400000 + 200000) / 2
    assert _const(thermo, 1) == pytest.approx(t_mean)
    assert _const(non, 1) == pytest.approx(n_mean)

    # the two halves must reconstruct the whole
    total = ctx.output_directory / "Combined_files" / "summary_combinations" / (
        "Summary_combination_inventories.nc"
    )
    assert _const(thermo, 1) + _const(non, 1) == pytest.approx(_const(total, 1))


# --------------------------------------------------------------------------- #
# individual combinations
# --------------------------------------------------------------------------- #
def test_individual_combinations_and_key_agree(ctx):
    combine_across_sectors(ctx)
    cdir = ctx.output_directory / "Combined_files"
    key = pd.read_csv(cdir / "Combined_inventory_key.csv")

    # 2 landfill x 2 ngdist x 2 wastewater x 2 wetland x 2 statcomb
    assert len(key) == 32
    files = sorted(cdir.glob("Combined_inventory_combination_*.nc"))
    assert len(files) == 32

    value_of = {
        "GHGRP_reported": 10, "GHGRP_generation_first": 20,
        "SOCCR1": 100, "Wetcharts_NLCD_subset_1": 200,
        "DMR_Moore_national": 1000, "CWNS_GHGI_state": 2000,
    }
    ngdist = {"ACES_bystate": 20000, "Vulcan_bystate": 10000}
    statcomb = {"ACES_bystate": 700000, "Vulcan_bystate": 300000}  # fossil + wood

    for _, row in key.iterrows():
        num = str(int(row["Inventory_Number"])).zfill(2)
        expect = (
            SET_TOTAL
            + value_of[row["Landfill_options"]]
            + ngdist[row["Natural_Gas_Distribution_options"]]
            + value_of[row["Wastewater_options"]]
            + value_of[row["Wetland_options"]]
            + statcomb[row["Stationary_Combustion_options"]]
        )
        got = _const(cdir / f"Combined_inventory_combination_{num}.nc")
        assert got == pytest.approx(expect), f"combination {num} does not match its key row"


def test_thermo_individual_halves_sum_to_whole(ctx):
    combine_across_sectors(ctx)
    cdir = ctx.output_directory / "Combined_files"
    for i in range(1, 33):
        num = str(i).zfill(2)
        whole = _const(cdir / f"Combined_inventory_combination_{num}.nc")
        t = _const(cdir / "thermogenic" / f"Thermogenic_combined_inventory_combination_{num}.nc")
        n = _const(
            cdir / "non_thermogenic"
            / f"Non_thermogenic_combined_inventory_combination_{num}.nc"
        )
        assert t + n == pytest.approx(whole), f"thermo split does not reconstruct combo {num}"


def test_config_flags_respected(tmp_path, ctx):
    ctx.config.Create_individual_combinations = False
    ctx.config.Separate_thermo = False
    combine_across_sectors(ctx)
    cdir = ctx.output_directory / "Combined_files"
    assert (cdir / "summary_combinations" / "Summary_combination_inventories.nc").exists()
    assert not list(cdir.glob("Combined_inventory_combination_*.nc"))
    assert not (cdir / "thermogenic").exists()


# --------------------------------------------------------------------------- #
# explicit divergence from R
# --------------------------------------------------------------------------- #
@pytest.mark.golden
def test_r_summary_halves_stationary_combustion(ctx):
    """Pin R's summary bug: it averages the fossil/wood layers instead of summing.

    R's mean stat-comb contribution is mean(100k,200k,300k,400k)=250k; the correct
    value is mean(300k, 700k)=500k. If this ever stops holding, R changed.
    """
    if not _ORACLE.exists():
        pytest.skip("combine oracle missing; run capture_combine_oracle.R")
    o = json.loads(_ORACLE.read_text())
    r_mean = o["summary"]["Summary_combination_inventories.nc"]["values"][1]

    combine_across_sectors(ctx)
    ours = _const(
        ctx.output_directory / "Combined_files" / "summary_combinations"
        / "Summary_combination_inventories.nc",
        1,
    )
    assert r_mean == pytest.approx(266680)          # R
    assert ours == pytest.approx(516680)            # corrected
    assert ours - r_mean == pytest.approx(250000)   # exactly the halved stat-comb


@pytest.mark.golden
def test_r_individual_combinations_drop_a_stationary_combustion_file(ctx):
    """Pin R's individual-combination corruption.

    R combo 1's key says stationary combustion = ACES_bystate (fossil 300k + wood
    400k = 700k), but its raster only contains fossil_ACES (300k).
    """
    if not _ORACLE.exists():
        pytest.skip("combine oracle missing; run capture_combine_oracle.R")
    o = json.loads(_ORACLE.read_text())
    r_c1 = o["individual"]["Combined_inventory_combination_01.nc"]["values"]
    r_c1 = r_c1[0] if isinstance(r_c1, list) else r_c1

    combine_across_sectors(ctx)
    ours = _const(
        ctx.output_directory / "Combined_files" / "Combined_inventory_combination_01.nc"
    )
    base = SET_TOTAL + 10 + 20000 + 2000 + 100   # everything but stationary combustion
    assert r_c1 == pytest.approx(base + 300000)  # R: fossil only
    assert ours == pytest.approx(base + 700000)  # corrected: fossil + wood
