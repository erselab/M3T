"""Config validation.

Port of the error-checking block in ``R/CH4_inventory_build.R`` (lines ~360-447).
The R code accumulates every problem into one message and stops once, rather than
failing on the first. :func:`validate_config` returns the list of problems;
:func:`check_config` raises :class:`ConfigError` with the combined message.
"""

from __future__ import annotations

from pathlib import Path

from .config import Config, source_option_names


class ConfigError(ValueError):
    """Raised when the config has an unusable combination of options."""


def validate_config(config: Config, input_directory: str | Path | None = None) -> list[str]:
    """Return a list of config problems (empty if the config is runnable).

    ``input_directory`` is only needed for the two checks that look for a
    prepared ``byLDC_merged`` shapefile / raw NLCD file; when ``None`` those
    filesystem checks are skipped (they only fire for non-default configs).
    """
    problems: list[str] = []
    c = config

    # Each Source_* option must be a string ("M3T"/"download"/path). This mirrors
    # R exactly, including its leniency: R's check (`value == 0 | !is.character`)
    # flags only *non-character* values, so an empty string (the default for
    # Source_byLDC_file) passes here and is caught later only if actually needed.
    bad_sources = [name for name in source_option_names(c) if not isinstance(c.get(name), str)]
    if bad_sources:
        problems.append(
            'Must set all data sources to "M3T", "download", or a file path. '
            + ", ".join(bad_sources)
            + " are set incorrectly (not text)."
        )

    # Landfills
    if c.Process_landfills and not (
        isinstance(c.GHGI_landfill_total, (int, float)) or c.GHGI_landfill_total == "GHGI"
    ):
        problems.append(
            "Must set Process_landfills to False or set GHGI_landfill_total to a "
            'number or "GHGI".'
        )
    if c.Process_landfills and not (
        c.landfill_ghgrp_reported
        or c.landfill_ghgrp_generation_first
        or c.landfill_ghgrp_collection_first
    ):
        problems.append(
            "Must set Process_landfills to False or enable at least one of "
            "landfill_ghgrp_reported / landfill_ghgrp_generation_first / "
            "landfill_ghgrp_collection_first."
        )

    # Downscaling proxy required for stationary combustion / NG distribution
    if (not c.Use_ACES and not c.Use_Vulcan) and (
        c.Process_stationary_combustion or c.Process_natural_gas_distribution
    ):
        problems.append(
            "Must disable both Process_stationary_combustion and "
            "Process_natural_gas_distribution or enable Use_ACES and/or Use_Vulcan "
            "to disaggregate them."
        )

    # Stationary combustion aggregation level
    if c.Process_stationary_combustion and not (
        c.stationary_combustion_by_state or c.stationary_combustion_by_domain
    ):
        problems.append(
            "Must disable Process_stationary_combustion or enable "
            "stationary_combustion_by_state and/or stationary_combustion_by_domain."
        )

    # NG distribution aggregation level
    if c.Process_natural_gas_distribution and not (
        c.NG_distribution_by_LDC or c.NG_distribution_by_state or c.NG_distribution_by_domain
    ):
        problems.append(
            "Must disable Process_natural_gas_distribution or enable "
            "NG_distribution_by_LDC / NG_distribution_by_state / NG_distribution_by_domain."
        )

    # NG distribution by-LDC needs a prepared merged shapefile
    if input_directory is not None and c.Process_natural_gas_distribution and c.NG_distribution_by_LDC:
        shp = Path(input_directory) / "byLDC_merged" / "byLDC_merged.shp"
        if not shp.exists():
            problems.append(
                "NG_distribution_by_LDC must be False, or the byLDC prep step must "
                "be run and its output present before running."
            )

    # Wastewater
    if c.Process_wastewater and not (c.Wastewater_use_CWNS or c.Wastewater_use_DMR):
        problems.append(
            "Must disable Process_wastewater or enable Wastewater_use_CWNS and/or "
            "Wastewater_use_DMR (the only input-data options)."
        )
    if c.Process_wastewater and not (
        c.Wastewater_Municipal_Method_Moore or c.Wastewater_Municipal_Method_GHGI
    ):
        problems.append(
            "Must disable Process_wastewater or enable "
            "Wastewater_Municipal_Method_Moore and/or Wastewater_Municipal_Method_GHGI."
        )
    if c.Process_wastewater and not (
        c.Wastewater_national_septic or c.Wastewater_state_septic
    ):
        problems.append(
            "Must disable Process_wastewater or enable Wastewater_national_septic "
            "and/or Wastewater_state_septic."
        )

    # Wetlands
    if c.Process_wetlands_and_inland_waters and not (
        c.Use_SOCCR1 or c.Use_SOCCR2 or c.Use_Wetcharts
    ):
        problems.append(
            "Must disable Process_wetlands_and_inland_waters or enable Use_SOCCR1 / "
            "Use_SOCCR2 / Use_Wetcharts."
        )
    if (
        c.Process_wetlands_and_inland_waters
        and c.Use_Wetcharts
        and c.Source_wetland_NLCD != "M3T"
        and not Path(c.Source_wetland_NLCD).exists()
    ):
        problems.append(
            "For Wetcharts with a non-M3T Source_wetland_NLCD, the path must exist "
            '(or set Source_wetland_NLCD to "M3T").'
        )

    # Combine
    if c.Combine_sectors and not (
        c.Create_summary_combinations or c.Create_individual_combinations
    ):
        problems.append(
            "Must disable Combine_sectors or enable Create_summary_combinations or "
            "Create_individual_combinations."
        )

    return problems


def check_config(config: Config, input_directory: str | Path | None = None) -> None:
    """Raise :class:`ConfigError` if the config has any problems."""
    problems = validate_config(config, input_directory)
    if problems:
        raise ConfigError(
            "The following errors were found based on the config data:\n\n"
            + "\n\n".join(problems)
        )
