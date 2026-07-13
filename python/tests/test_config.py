"""Config parity tests — values must match R/M3T_config.R exactly."""

from __future__ import annotations

import math

import pytest

from m3t.config import Config, M3T_get_config, M3T_set_config, source_option_names

_CH4 = 16.043


def test_defaults_scalar_parity():
    c = Config()
    assert c.Terra_datatype == "FLT8S"
    assert c.Base_timeout == 60 * 20
    assert c.Use_ACES is False and c.Use_Vulcan is True
    assert c.Source_Vulcan == "download"
    assert c.GHGI_landfill_total == "GHGI"
    assert c.Process_landfills is True
    assert c.NG_distribution_by_state is True
    assert c.Wastewater_use_DMR is True and c.Wastewater_use_CWNS is False


def test_res_post_meter_emission_factor():
    c = Config()
    expected = 0.5 / 100 * 7850 / 401 / (_CH4 * 60 * 60 * 24 * 365)
    assert math.isclose(c.natural_gas_res_post_meter_emission_factor, expected, rel_tol=1e-15)
    assert c.natural_gas_com_post_meter_emission_factor == 0.0


def test_pipeline_efs_converted():
    c = Config()
    df = c.natural_gas_pipeline_emission_factors
    assert list(df.index) == ["Bare_Steel", "Cast_Iron", "Coated_steel", "Plastic"]
    assert df.loc["Bare_Steel", "Leaks_per_mile"] == 0.51
    assert math.isclose(
        df.loc["Cast_Iron", "Avg_emissions_mol_per_s"], 1.72 / (_CH4 * 60), rel_tol=1e-15
    )


def test_stationary_combustion_efs_include_elec_petr():
    c = Config()
    df = c.stationary_combustion_emission_factors
    # 14 fuel/sector columns in R
    assert df.shape == (1, 14)
    assert df.loc[0, "elec_coal"] == 1
    assert df.loc[0, "elec_petr"] == 3  # was easy to drop when porting
    assert math.isclose(df.loc[0, "elec_gas"], 5.4 / (1.0550559 * 0.9), rel_tol=1e-15)


def test_wetland_efs_rows():
    c = Config()
    assert list(c.Wetland_EFs.index) == ["SOCCR1", "SOCCR2"]
    assert c.Wetland_EFs.loc["SOCCR2", "PNF"] == 33.28


def test_wastewater_tables_shapes():
    c = Config()
    assert c.GHGI_wastewater_data.shape == (13, 4)
    assert list(c.National_wastewater_info["Year"]) == [1990, 2011, 2013, 2015, 2017, 2019, 2021, 2023]


def test_get_set_helpers_and_singleton_isolation():
    assert M3T_get_config("Process_landfills") is True
    M3T_set_config(Process_landfills=False)
    assert M3T_get_config("Process_landfills") is False
    # restore so test order doesn't leak
    M3T_set_config(Process_landfills=True)


def test_copy_is_deep():
    c = Config()
    c2 = c.copy()
    c2.natural_gas_pipeline_emission_factors.iloc[0, 0] = -999
    assert c.natural_gas_pipeline_emission_factors.iloc[0, 0] == 0.51


def test_unknown_option_raises():
    c = Config()
    with pytest.raises(KeyError):
        c.get("does_not_exist")
    with pytest.raises(KeyError):
        c.set(nope=1)


def test_source_option_names():
    names = source_option_names()
    assert "Source_Vulcan" in names
    assert all(n.startswith("Source_") for n in names)
    # every listed source option actually exists on Config
    c = Config()
    for n in names:
        assert hasattr(c, n)
