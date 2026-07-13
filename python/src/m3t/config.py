"""Configuration for M3T.

Port of ``R/M3T_config.R``.

In the R package, options live in a private environment ``M3T_config`` that is
mutated globally (and partially reset on exit inside ``CH4_inventory_build``).
Here we instead use an explicit :class:`Config` dataclass that is passed through
the call chain. This removes the hidden global state while preserving every
option name and default value so runs remain comparable to the R package.

For parity/convenience a module-level default instance and ``get_config`` /
``set_config`` helpers are provided that mirror ``M3T_get_config`` /
``M3T_set_config``. Prefer passing an explicit ``Config`` in library code; the
module-level singleton exists mainly for interactive use and tests.

Design notes on option value conventions (kept identical to R):

* ``Source_*`` options are either the string ``"M3T"`` (use preprocessed data
  shipped with the package / companion Zenodo), ``"download"`` (fetch from the
  original source), or a filesystem path to a local copy.
* A handful of options accept *either* a number *or* a keyword string (e.g.
  ``GHGI_landfill_total`` is a float or the literal ``"GHGI"``). These are typed
  as ``float | str`` and validated leniently, matching R.
* Tabular options (emission factors, septic fractions, ...) are stored as
  ``pandas.DataFrame`` built by the ``_default_*`` helpers below so the literal
  values live in exactly one place.
"""

from __future__ import annotations

from copy import deepcopy
from dataclasses import dataclass, field, fields
from typing import Any

import pandas as pd

# Methane molar mass (g/mol), used in several emission-factor conversions.
_CH4_MOLAR_MASS = 16.043


# --------------------------------------------------------------------------- #
# Default tabular options (built once, copied into each Config instance).
# --------------------------------------------------------------------------- #
def _default_ng_pipeline_emission_factors() -> pd.DataFrame:
    """Weller et al. 2020 (doi:10.1021/acs.est.0c00437), g/min -> mol/s."""
    df = pd.DataFrame(
        {
            "Leaks_per_mile": [0.51, 1.00, 0.61, 0.43],
            "Avg_emissions_mol_per_s": [v / (_CH4_MOLAR_MASS * 60) for v in (2.24, 1.72, 2.00, 2.03)],
        },
        index=["Bare_Steel", "Cast_Iron", "Coated_steel", "Plastic"],
    )
    return df


def _default_stationary_combustion_emission_factors() -> pd.DataFrame:
    """IPCC EFs; elec-gas from Hajny et al. One-row frame keyed by fuel/sector."""
    return pd.DataFrame(
        {
            "com_coal": [10.0],
            "ind_coal": [10.0],
            "elec_coal": [1.0],
            "res_petr": [10.0],
            "com_petr": [10.0],
            "ind_petr": [3.0],
            "elec_petr": [3.0],
            "com_gas": [5.0],
            "ind_gas": [1.0],
            # g/mmbtu -> g/GJ and low->high heating value (0.9)
            "elec_gas": [5.4 / (1.0550559 * 0.9)],
            "res_wood": [300.0],
            "com_wood": [300.0],
            "ind_wood": [30.0],
            "elec_wood": [30.0],
        }
    )


def _default_ghgi_wastewater_data() -> pd.DataFrame:
    """GHGI emission data and EFs; EF from Leverenz et al. 2010."""
    return pd.DataFrame(
        {
            "EF": [10.7] * 13,
            "Septic Emissions": [204, 200, 204, 240, 236, 236, 240, 236, 236, 232, 227, 223, 215],
            "Nonseptic Emissions": [108, 104, 108, 128, 124, 124, 116, 104, 100, 250, 246, 273, 270],
            "year": list(range(2010, 2023)),
        }
    )


def _default_wastewater_reported_state_info() -> pd.DataFrame:
    """State septic fractions from the American Housing Survey (as of 2025)."""
    rows = [
        ("CA", 2015, 0.0645), ("CA", 2021, 0.0408), ("CA", 2023, 0.0560),
        ("FL", 2017, 0.1509), ("FL", 2019, 0.1265), ("FL", 2023, 0.1611),
        ("MA", 2023, 0.2403),
        ("NY", 2015, 0.2083), ("NY", 2019, 0.1626), ("NY", 2021, 0.1592), ("NY", 2023, 0.2029),
        ("OH", 2015, 0.1963),
        ("TX", 2015, 0.1441), ("TX", 2023, 0.1276),
    ]
    return pd.DataFrame(rows, columns=["State", "Year", "Septic_Fraction"])


def _default_national_wastewater_info() -> pd.DataFrame:
    return pd.DataFrame(
        {
            "Year": [1990, 2011, 2013, 2015, 2017, 2019, 2021, 2023],
            "Septic_Fraction": [0.241, 0.1949, 0.1856, 0.1986, 0.1791, 0.1635, 0.1522, 0.1858],
        }
    )


def _default_wetland_efs() -> pd.DataFrame:
    """State of the Carbon Cycle Report EFs, g CH4 / m^2 / yr. Rows SOCCR1/SOCCR2."""
    return pd.DataFrame(
        {
            "E2_Atlantic": [10.3, 20.43], "M2_Atlantic": [10.3, 20.43],
            "E2_Gulf": [10.3, 27.47], "M2_Gulf": [10.3, 27.47],
            "E2_Pacific": [10.3, 21.87], "M2_Pacific": [10.3, 21.87],
            "E2_Hudson": [10.3, 21.87], "M2_Hudson": [10.3, 21.87],
            "PFO": [36.0, 24.74], "PNF": [36.0, 33.28],
            "L1": [5.0, 5.0], "L2": [5.0, 5.0],
            "R1": [7.88, 7.88], "R2": [7.88, 7.88], "R3": [7.88, 7.88], "R4": [7.88, 7.88],
        },
        index=["SOCCR1", "SOCCR2"],
    )


def _default_wetcharts_model_subset() -> list[list[int]]:
    return [
        [1913, 1914, 1923, 1924, 1933, 1934, 2913, 2914, 2923,
         2924, 2933, 2934, 3913, 3914, 3923, 3924, 3933, 3934]
    ]


@dataclass
class Config:
    """All M3T options with the same names and defaults as ``R/M3T_config.R``.

    Grouped by comment blocks matching the R source. Use :meth:`get` / :meth:`set`
    for name-based access mirroring ``M3T_get_config`` / ``M3T_set_config``.
    """

    # --- terra / download options ---------------------------------------- #
    Terra_datatype: str = "FLT8S"
    Terra_progress: int = 0
    Base_timeout: int = 60 * 20  # seconds

    # --- across-sector method variations --------------------------------- #
    Use_ACES: bool = False
    Use_Vulcan: bool = True

    # --- across-sector data sources -------------------------------------- #
    Source_Tigerlines_data: str = "M3T"
    Source_GHGRP_facility_data: str = "M3T"
    Source_GHGRP_combustion: str = "M3T"
    Source_GHGI: str = "M3T"
    Source_GHGRP_NG: str = "M3T"
    Source_Cartographic_Boundaries_data: str = "M3T"
    Source_ACES: str = "M3T"
    Source_Vulcan: str = "download"

    # --- landfills ------------------------------------------------------- #
    Process_landfills: bool = True
    landfill_ghgrp_reported: bool = False
    landfill_ghgrp_generation_first: bool = True
    landfill_ghgrp_collection_first: bool = False
    Source_GHGRP_landfills: str = "M3T"
    Source_LMOP: str = "M3T"
    GHGI_landfill_total: float | str = "GHGI"

    # --- natural gas distribution ---------------------------------------- #
    Process_natural_gas_distribution: bool = True
    NG_distribution_by_LDC: bool = False
    NG_distribution_by_state: bool = True
    NG_distribution_by_domain: bool = False
    Source_EIA_NG_file: str = "M3T"
    Source_PHMSA_file: str = "M3T"
    Source_GHGRP_LDC: str = "M3T"
    Source_byLDC_file: str = ""
    natural_gas_pipeline_emission_factors: pd.DataFrame = field(
        default_factory=_default_ng_pipeline_emission_factors
    )
    # Fischer et al. 2018 (doi:10.1021/acs.est.8b03217): 0.5% of residential
    # consumption; converts cubic feet -> grams then g/yr -> mol/s.
    natural_gas_res_post_meter_emission_factor: float = (
        0.5 / 100 * 7850 / 401 / (_CH4_MOLAR_MASS * 60 * 60 * 24 * 365)
    )
    natural_gas_com_post_meter_emission_factor: float = 0.0
    GHGI_MnR: float | str = "GHGI"
    GHGI_maintenance: float | str = "GHGI"
    GHGI_meters: float | str = "GHGI"
    GHGI_services: float | str = "GHGI"

    # --- natural gas transmission ---------------------------------------- #
    Process_natural_gas_transmission: bool = True
    Source_HIFLD_compressor_file: str = "M3T"
    Source_EIA_transmission_file: str = "M3T"
    GHGI_Pipeline: float | str = "GHGI"
    GHGI_transmission_compressors: float | str = "GHGI"

    # --- stationary combustion ------------------------------------------- #
    Process_stationary_combustion: bool = True
    stationary_combustion_by_state: bool = True
    stationary_combustion_by_domain: bool = False
    Source_EIA_SEDS_data: str = "M3T"
    Source_NEI_data: str = "M3T"
    stationary_combustion_emission_factors: pd.DataFrame = field(
        default_factory=_default_stationary_combustion_emission_factors
    )

    # --- wastewater ------------------------------------------------------ #
    Process_wastewater: bool = True
    Wastewater_use_CWNS: bool = False
    Wastewater_use_DMR: bool = True
    Wastewater_Municipal_Method_Moore: bool = True
    Wastewater_Municipal_Method_GHGI: bool = False
    Wastewater_national_septic: bool = True
    Wastewater_state_septic: bool = False
    Source_wastewater_NLCD: str = "M3T"
    Source_CWNS: str = "M3T"
    Source_DMR: str = "M3T"
    Source_State_population_data: str = "M3T"
    Source_GHGRP_wastewater: str = "M3T"
    GHGI_wastewater_data: pd.DataFrame = field(default_factory=_default_ghgi_wastewater_data)
    Total_national_open_or_low_int_area: float | str = "M3T"
    Wastewater_reported_State_info: pd.DataFrame = field(
        default_factory=_default_wastewater_reported_state_info
    )
    National_wastewater_info: pd.DataFrame = field(
        default_factory=_default_national_wastewater_info
    )

    # --- wetlands & inland waters ---------------------------------------- #
    Process_wetlands_and_inland_waters: bool = True
    Use_SOCCR1: bool = False
    Use_SOCCR2: bool = False
    Use_Wetcharts: bool = True
    Wetcharts_model_subset: list[list[int]] = field(default_factory=_default_wetcharts_model_subset)
    Source_wetland_NLCD: str = "M3T"
    Source_Watershed_file: str = "M3T"
    Source_wetcharts: str = "M3T"
    Source_NWI: str = "M3T"
    Wetland_EFs: pd.DataFrame = field(default_factory=_default_wetland_efs)

    # --- remaining sectors (gridded EPA) --------------------------------- #
    Process_remaining_sectors_from_gridded_EPA: bool = True
    Source_GEPA: str = "download"

    # --- combined inventory ---------------------------------------------- #
    Combine_sectors: bool = True
    Separate_thermo: bool = False
    Create_summary_combinations: bool = True
    Create_individual_combinations: bool = False

    # ------------------------------------------------------------------ #
    # name-based accessors (parity with M3T_get_config / M3T_set_config)
    # ------------------------------------------------------------------ #
    def get(self, option: str | None = None) -> Any:
        """Return one option by name, or a dict of all options if ``option`` is None."""
        if option is None:
            return {f.name: getattr(self, f.name) for f in fields(self)}
        if not hasattr(self, option):
            raise KeyError(f"Unknown M3T config option: {option!r}")
        return getattr(self, option)

    def set(self, **kwargs: Any) -> None:
        """Set one or more options by name in place."""
        for name, value in kwargs.items():
            if not hasattr(self, name):
                raise KeyError(f"Unknown M3T config option: {name!r}")
            setattr(self, name, value)

    def copy(self) -> "Config":
        """Deep copy (so DataFrame options are not shared between runs)."""
        return deepcopy(self)


# --------------------------------------------------------------------------- #
# Module-level singleton + R-style helpers.
# --------------------------------------------------------------------------- #
M3T_config = Config()


def M3T_get_config(option: str | None = None) -> Any:
    """Mirror of R ``M3T_get_config`` operating on the module singleton."""
    return M3T_config.get(option)


def M3T_set_config(**kwargs: Any) -> None:
    """Mirror of R ``M3T_set_config`` operating on the module singleton."""
    M3T_config.set(**kwargs)


# Names of all ``Source_*`` options — used by the orchestrator's error checks.
def source_option_names(cfg: Config | None = None) -> list[str]:
    cfg = cfg or M3T_config
    return [f.name for f in fields(cfg) if f.name.startswith("Source_")]
