"""End-to-end orchestrator tests.

Verify that ch4_inventory_build runs the full control flow offline: correct grid,
all enabled sectors emit output, the total is the sum, and directory/settings
artifacts are written.

The domain is the IA+NE state pair rather than a bare bounding box, because that
is what exercises the state-Tigerlines plumbing: wastewater's septic branch (on by
default) needs `state_tigerlines` / `state_name_list` on the RunContext, which only
a `tigerlines=` run produces. The still-stubbed sectors emit zeros as before.
"""

from __future__ import annotations

from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
import pytest
import rioxarray
import xarray as xr

from m3t import geo
from m3t.config import Config
from m3t.inventory import ch4_inventory_build
from m3t.sectors import base
from m3t.validation import ConfigError

_GOLD = Path(__file__).parent / "golden"
_FAC_LF = _GOLD / "landfills" / "facility_data_landfills.csv"
_FAC_NG = _GOLD / "ng_transmission" / "facility_data_ngtrans.csv"
_SUBW = _GOLD / "ng_transmission" / "subpartW_ngtrans.csv"
_PIPES = _GOLD / "ng_transmission" / "eia_pipes_pa_all.geojson"
_WW = _GOLD / "wastewater"
_FAC_WW = _WW / "facility_data_wastewater.csv"
_DMR = _WW / "in" / "DMR_data.csv"
_TIGER = _WW / "domain_ia_ne.geojson"
_NLCD = _WW / "nlcd_suburbia_2019_ia_ne.tif"

_FIXTURES = (_FAC_LF, _FAC_NG, _SUBW, _PIPES, _FAC_WW, _DMR, _TIGER, _NLCD)

_STATES = ["IA", "NE"]

# A plain bbox still works for runs that don't need state geometries (i.e. with
# wastewater's septic branch disabled).
_PA_BOX = (-80.5, 39.7, -74.7, 42.3)


@pytest.fixture
def base_config():
    """Default config minus stationary combustion.

    That sector needs county Tigerlines and a CONUS-wide gridded CO2 inventory
    (Vulcan/ACES, hundreds of MB) that we cannot commit as fixtures, so the
    orchestrator tests leave it stubbed; it has its own golden test.
    """
    cfg = Config()
    cfg.Process_stationary_combustion = False
    return cfg


@pytest.fixture(scope="module")
def tigerlines():
    if not _TIGER.exists():
        pytest.skip("orchestrator fixtures missing")
    return gpd.read_file(_TIGER)


@pytest.fixture(scope="module")
def shared_inputs():
    """Inject the committed fixtures for the ported sectors so runs stay offline."""
    for f in _FIXTURES:
        if not f.exists():
            pytest.skip("orchestrator fixtures missing")
    fac = pd.concat(
        [
            pd.read_csv(_FAC_LF, low_memory=False),
            pd.read_csv(_FAC_NG, low_memory=False),
            pd.read_csv(_FAC_WW, low_memory=False),
        ],
        ignore_index=True,
    ).drop_duplicates(subset=["facility_id", "year"])

    national = pd.read_csv(_WW / "in" / "Total_national_septic_area.csv")
    areas = pd.read_csv(_WW / "in" / "wastewater_state_septic_area.csv")
    areas = areas[["state", "2019"]].rename(
        columns={"state": "X", "2019": "open_or_low_int_area"}
    )

    return {
        "ghgrp_facility_data": fac,
        "ghgrp_subpartW_emissions": pd.read_csv(_SUBW, low_memory=False),
        "eia_transmission_pipes": gpd.read_file(_PIPES),
        "dmr_data": pd.read_csv(_DMR, low_memory=False),
        "nlcd_suburbia": rioxarray.open_rasterio(_NLCD, masked=True).squeeze("band", drop=True),
        "nlcd_year": 2019,
        "septic_total_national_area": float(
            national.loc[(national["year"] - 2019).abs().idxmin(),
                         "Total_national_open_or_low_int_area"]
        ),
        "septic_state_areas": areas[areas["X"].isin(_STATES)],
    }


def test_runs_end_to_end_on_states(tmp_path, shared_inputs, tigerlines, base_config):
    ctx = ch4_inventory_build(
        run_directory=tmp_path,
        inventory_year=2019,
        domain=_STATES,
        domain_res=0.2,
        domain_crs="epsg:4326",
        tigerlines=tigerlines,
        config=base_config,
        shared=shared_inputs,
    )
    # the orchestrator derived the state geometries + ordered name list
    assert ctx.shared["state_name_list"] == _STATES
    assert len(ctx.shared["state_tigerlines"]) == 2

    # grid parity with the state extent at 0.2 deg
    assert geo.res(ctx.domain_template) == pytest.approx((0.2, 0.2))

    # wastewater ran for real (not the stub): it wrote its variant rasters
    assert (ctx.output_directory / "Wastewater" / "Wastewater_ind.nc").exists()
    assert (ctx.output_directory / "Wastewater" / "Wastewater_dom_septic_national.nc").exists()
    assert (ctx.output_directory / "Wastewater" / "Municipal_watewater_treatment.csv").exists()
    assert float(np.nansum(xr.open_dataset(ctx.output_directory / "wastewater.nc")
                           ["methane_emissions"].values)) > 0

    # every enabled sector ran (all but stationary combustion, see base_config)
    enabled_keys = [s.key for s in base.SECTORS if ctx.config.get(s.process_flag)]
    assert ctx.shared["sectors_run"] == enabled_keys
    assert len(enabled_keys) == 6
    assert "stationary_combustion" not in enabled_keys

    # each sector wrote a NetCDF; the total exists
    for key in enabled_keys:
        assert (ctx.output_directory / f"{key}.nc").exists()
    total_path = ctx.output_directory / "M3T_total.nc"
    assert total_path.exists()

    # directory + run-settings artifacts
    assert (ctx.input_directory / "Run_settings.txt").exists()
    assert (ctx.input_directory / "GHGRP").is_dir()


def test_total_is_sum_of_sectors(tmp_path, shared_inputs, tigerlines, base_config):
    ctx = ch4_inventory_build(
        run_directory=tmp_path,
        inventory_year=2019,
        domain=_STATES,
        domain_res=0.5,
        tigerlines=tigerlines,
        config=base_config,
        shared=shared_inputs,
    )
    ds = xr.open_dataset(ctx.output_directory / "M3T_total.nc")
    total = ds[next(iter(ds.data_vars))]
    assert total.shape == ctx.domain_template.shape
    assert total.attrs.get("m3t_n_sectors_combined") == 6

    # the total equals the cell-wise sum of the per-sector rasters
    manual = np.zeros(total.shape)
    for key in ctx.shared["sectors_run"]:
        sda = xr.open_dataset(ctx.output_directory / f"{key}.nc")
        manual += np.nan_to_num(sda[next(iter(sda.data_vars))].values)
    assert np.allclose(np.nan_to_num(total.values), manual, rtol=1e-6)


def test_disabled_sector_is_skipped(tmp_path, shared_inputs, base_config):
    cfg = base_config
    cfg.Process_wastewater = False
    ctx = ch4_inventory_build(
        run_directory=tmp_path,
        inventory_year=2019,
        domain=_PA_BOX,
        domain_res=0.5,
        config=cfg,
        shared=shared_inputs,
    )
    assert "wastewater" not in ctx.shared["sectors_run"]
    assert not (ctx.output_directory / "wastewater.nc").exists()
    assert (ctx.output_directory / "landfills.nc").exists()


def test_invalid_config_raises_before_running(tmp_path):
    cfg = Config()
    cfg.Wastewater_use_CWNS = False
    cfg.Wastewater_use_DMR = False
    with pytest.raises(ConfigError):
        ch4_inventory_build(
            run_directory=tmp_path,
            inventory_year=2019,
            domain=(-75.0, 39.0, -72.0, 42.0),
            domain_res=1.0,
            config=cfg,
        )
    # nothing should have been dispatched
    assert not (tmp_path / "out" / "landfills.nc").exists()


def test_does_not_mutate_passed_config(tmp_path, shared_inputs, tigerlines, base_config):
    cfg = base_config
    ctx = ch4_inventory_build(
        run_directory=tmp_path,
        inventory_year=2019,
        domain=_STATES,
        domain_res=0.5,
        config=cfg,
        tigerlines=tigerlines,
        shared=shared_inputs,
    )
    # the run operates on a copy: the context config is a distinct object, and
    # mutating it does not touch the caller's config.
    assert ctx.config is not cfg
    ctx.config.Process_landfills = False
    assert cfg.Process_landfills is True
