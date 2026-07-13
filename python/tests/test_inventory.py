"""End-to-end orchestrator tests (Phase 2, stubbed sectors).

Verify that ch4_inventory_build runs the full control flow offline on a small
box/CONUS domain: correct grid, all enabled sectors emit output, the total is
the (zero) sum, and directory/settings artifacts are written.
"""

from __future__ import annotations

from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
import pytest
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

# PA box where the committed fixtures (landfills, compressors, pipelines) all
# have data, so the ported sectors do real work through the orchestrator.
_PA_BOX = (-80.5, 39.7, -74.7, 42.3)


@pytest.fixture(scope="module")
def shared_inputs():
    """Inject the committed fixtures for the ported sectors so runs stay offline."""
    for f in (_FAC_LF, _FAC_NG, _SUBW, _PIPES):
        if not f.exists():
            pytest.skip("orchestrator fixtures missing")
    fac = pd.concat(
        [pd.read_csv(_FAC_LF, low_memory=False), pd.read_csv(_FAC_NG, low_memory=False)],
        ignore_index=True,
    ).drop_duplicates(subset=["facility_id", "year"])
    return {
        "ghgrp_facility_data": fac,
        "ghgrp_subpartW_emissions": pd.read_csv(_SUBW, low_memory=False),
        "eia_transmission_pipes": gpd.read_file(_PIPES),
    }


def test_runs_end_to_end_on_box(tmp_path, shared_inputs):
    ctx = ch4_inventory_build(
        run_directory=tmp_path,
        inventory_year=2019,
        domain=_PA_BOX,
        domain_res=0.2,
        domain_crs="epsg:4326",
        shared=shared_inputs,
    )
    # grid parity with the domain we asked for (PA box at 0.2 deg)
    assert ctx.domain_template.shape == (13, 29)
    assert geo.res(ctx.domain_template) == pytest.approx((0.2, 0.2))

    # every enabled sector ran (all 7 on by default)
    enabled_keys = [s.key for s in base.SECTORS if ctx.config.get(s.process_flag)]
    assert ctx.shared["sectors_run"] == enabled_keys
    assert len(enabled_keys) == 7

    # each sector wrote a NetCDF; the total exists
    for key in enabled_keys:
        assert (ctx.output_directory / f"{key}.nc").exists()
    total_path = ctx.output_directory / "M3T_total.nc"
    assert total_path.exists()

    # directory + run-settings artifacts
    assert (ctx.input_directory / "Run_settings.txt").exists()
    assert (ctx.input_directory / "GHGRP").is_dir()


def test_total_is_sum_of_sectors(tmp_path, shared_inputs):
    ctx = ch4_inventory_build(
        run_directory=tmp_path,
        inventory_year=2019,
        domain=_PA_BOX,
        domain_res=0.5,
        shared=shared_inputs,
    )
    ds = xr.open_dataset(ctx.output_directory / "M3T_total.nc")
    total = ds[next(iter(ds.data_vars))]
    assert total.shape == ctx.domain_template.shape
    assert total.attrs.get("m3t_n_sectors_combined") == 7

    # the total equals the cell-wise sum of the per-sector rasters
    manual = np.zeros(total.shape)
    for key in ctx.shared["sectors_run"]:
        sda = xr.open_dataset(ctx.output_directory / f"{key}.nc")
        manual += np.nan_to_num(sda[next(iter(sda.data_vars))].values)
    assert np.allclose(np.nan_to_num(total.values), manual, rtol=1e-6)


def test_disabled_sector_is_skipped(tmp_path, shared_inputs):
    cfg = Config()
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


def test_does_not_mutate_passed_config(tmp_path, shared_inputs):
    cfg = Config()
    ctx = ch4_inventory_build(
        run_directory=tmp_path,
        inventory_year=2019,
        domain=_PA_BOX,
        domain_res=0.5,
        config=cfg,
        shared=shared_inputs,
    )
    # the run operates on a copy: the context config is a distinct object, and
    # mutating it does not touch the caller's config.
    assert ctx.config is not cfg
    ctx.config.Process_landfills = False
    assert cfg.Process_landfills is True
