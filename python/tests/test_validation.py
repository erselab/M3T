"""Config validation tests (port of the R error-checking block)."""

from __future__ import annotations

import pytest

from m3t.config import Config
from m3t.validation import ConfigError, check_config, validate_config


def test_default_config_is_valid():
    assert validate_config(Config()) == []


def test_landfill_total_must_be_number_or_ghgi():
    c = Config()
    c.GHGI_landfill_total = "nonsense"
    problems = validate_config(c)
    assert any("GHGI_landfill_total" in p for p in problems)
    c.GHGI_landfill_total = 1234.0
    assert not any("GHGI_landfill_total" in p for p in validate_config(c))


def test_landfill_requires_a_ghgrp_method():
    c = Config()
    c.landfill_ghgrp_reported = False
    c.landfill_ghgrp_generation_first = False
    c.landfill_ghgrp_collection_first = False
    assert any("landfill_ghgrp" in p for p in validate_config(c))


def test_downscaling_proxy_required():
    c = Config()
    c.Use_ACES = False
    c.Use_Vulcan = False
    # still processing stat combustion / NG dist -> error
    assert any("Use_ACES" in p for p in validate_config(c))
    # disabling those sectors clears it
    c.Process_stationary_combustion = False
    c.Process_natural_gas_distribution = False
    assert not any("Use_ACES" in p for p in validate_config(c))


def test_wastewater_option_families():
    c = Config()
    c.Wastewater_use_CWNS = False
    c.Wastewater_use_DMR = False
    assert any("Wastewater_use_CWNS" in p for p in validate_config(c))


def test_empty_source_not_flagged_matches_r_leniency():
    # R only flags non-character sources; empty strings pass (Source_byLDC_file
    # defaults to "").  Faithful port must be equally lenient.
    c = Config()
    c.Source_GHGI = ""
    assert not any("Source_GHGI" in p for p in validate_config(c))


def test_bad_source_type_flagged():
    c = Config()
    c.Source_LMOP = 123  # not a string
    assert any("Source_LMOP" in p for p in validate_config(c))


def test_combine_requires_a_combination():
    c = Config()
    c.Create_summary_combinations = False
    c.Create_individual_combinations = False
    assert any("Combine_sectors" in p for p in validate_config(c))


def test_check_config_raises_and_accumulates():
    c = Config()
    c.Source_LMOP = 123  # non-string source
    c.Wastewater_use_CWNS = False
    c.Wastewater_use_DMR = False
    with pytest.raises(ConfigError) as exc:
        check_config(c)
    msg = str(exc.value)
    assert "Source_LMOP" in msg and "Wastewater_use_CWNS" in msg
