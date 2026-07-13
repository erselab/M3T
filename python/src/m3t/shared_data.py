"""Prepare data shared across sectors. Port of the shared-data prep blocks in
``R/CH4_inventory_build.R`` (GHGRP facility data + GHGI year/total resolution).

Populated lazily into ``RunContext.shared`` before sector dispatch, so a sector
reads e.g. ``ctx.shared["ghgrp_facility_data"]`` rather than re-loading it.

Only the pieces the currently-ported sectors need are wired; more are added as
sectors land in Phase 3. Anything sourced from the Zenodo companion (which isn't
wired yet) raises a clear error pointing at the ``"download"`` / file-path /
inject alternatives.
"""

from __future__ import annotations

from pathlib import Path

import pandas as pd

from . import datasets
from .context import RunContext
from .download import trycatch_downloader

_FACILITY_URL = "https://data.epa.gov/dmapservice/ghg.pub_dim_facility/CSV"
_SUBPARTW_URL = "https://data.epa.gov/dmapservice/ghg.ef_w_emissions_source_ghg/csv"
_EIA_PIPES_URL = (
    "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/"
    "Natural_Gas_Interstate_and_Intrastate_Pipelines_1/FeatureServer/0/query"
)


def _zenodo_todo(name: str, hint_path) -> NotImplementedError:
    return NotImplementedError(
        f"Source '{name}'='M3T' expects the Zenodo companion file {hint_path}, which is "
        "not wired yet. Use 'download' or a local path, or inject it into ctx.shared."
    )


def load_ghgrp_facility_data(source: str, input_directory: Path, *, timeout: float = 60 * 20):
    """Load the GHGRP ``pub_dim_facility`` table per its ``Source_`` setting.

    * ``"download"`` — fetch the live EPA table.
    * a file path — read that CSV.
    * ``"M3T"`` — expects ``<input>/GHGRP/facility_data.csv`` from the Zenodo
      companion; raises if absent (companion not wired yet).
    """
    if source == "download":
        dest = Path(input_directory) / "GHGRP" / "facility_data.csv"
        trycatch_downloader(_FACILITY_URL, output_location=dest, method="save", timeout=timeout)
        df = pd.read_csv(dest, low_memory=False)
    elif source == "M3T":
        companion = Path(input_directory) / "GHGRP" / "facility_data.csv"
        if not companion.exists():
            raise NotImplementedError(
                "Source_GHGRP_facility_data='M3T' expects the Zenodo companion file "
                f"{companion}, which is not wired yet. Set it to 'download' or a local "
                "CSV path, or inject ctx.shared['ghgrp_facility_data']."
            )
        df = pd.read_csv(companion, low_memory=False)
    else:
        df = pd.read_csv(source, low_memory=False)

    # match R: zero-pad county fips / zip (not used downstream, but faithful)
    if "county_fips" in df.columns:
        df["county_fips"] = df["county_fips"].apply(
            lambda v: f"{int(v):05d}" if pd.notna(v) else v
        )
    if "zip" in df.columns:
        df["zip"] = df["zip"].apply(lambda v: f"{int(v):05d}" if pd.notna(v) else v)
    return df


def load_ghgrp_subpartW(source: str, input_directory: Path, *, timeout: float = 60 * 20):
    """Load GHGRP subpart W (oil & gas) emissions per its ``Source_GHGRP_NG`` setting."""
    if source == "download":
        dest = Path(input_directory) / "GHGRP" / "Oil_and_gas_W.csv"
        trycatch_downloader(_SUBPARTW_URL, output_location=dest, method="save", timeout=timeout)
        return pd.read_csv(dest, low_memory=False)
    if source == "M3T":
        raise _zenodo_todo("Source_GHGRP_NG", Path(input_directory) / "GHGRP" / "Oil_and_gas_W.csv")
    return pd.read_csv(source, low_memory=False)


def load_eia_transmission_pipes(source: str, input_directory: Path, *, timeout: float = 60 * 20):
    """Load EIA transmission pipeline lines per ``Source_EIA_transmission_file``.

    ``"download"`` pages the full ArcGIS FeatureServer (2000-record cap); a path
    reads a local vector file; ``"M3T"`` expects the Zenodo companion geojson.
    """
    import geopandas as gpd

    if source == "download":
        return _download_eia_pipes(timeout=timeout)
    if source == "M3T":
        raise _zenodo_todo(
            "Source_EIA_transmission_file",
            Path(input_directory) / "EIA" / "EIA_transmission_pipeline_map.geojson",
        )
    return gpd.read_file(source)


def _download_eia_pipes(*, timeout: float = 60 * 20):
    """Page all EIA pipeline features from the ArcGIS FeatureServer."""
    import json

    import geopandas as gpd
    import requests

    feats: list = []
    offset = 0
    while True:
        params = {
            "outFields": "*", "where": "1=1", "outSR": "4326", "f": "geojson",
            "resultOffset": offset, "resultRecordCount": 2000,
        }
        resp = requests.get(_EIA_PIPES_URL, params=params, timeout=timeout)
        resp.raise_for_status()
        fc = resp.json()
        batch = fc.get("features", [])
        feats.extend(batch)
        if len(batch) < 2000:
            break
        offset += 2000
    return gpd.GeoDataFrame.from_features(json.loads(json.dumps(feats)), crs="epsg:4326")


_DMR_YEARS = range(2010, 2025)


def _nearest(target: int, options) -> int:
    return min(options, key=lambda y: abs(target - y))


def load_dmr(source: str, input_directory: Path, inventory_year: int) -> pd.DataFrame:
    """Discharge Monitoring Report municipal flow, for the year nearest the run year.

    ``"M3T"`` reads ``<input>/DMR_data.csv`` from the Zenodo companion (a
    multi-year table filtered here); a path reads a user-exported ECHO CSV, whose
    header sits below a "Data Source" preamble and whose column names use dots.
    """
    if source == "M3T":
        path = Path(input_directory) / "DMR_data.csv"
        if not path.exists():
            raise _zenodo_todo("Source_DMR", path)
        dmr = pd.read_csv(path, low_memory=False)
        return dmr[dmr["year"] == _nearest(inventory_year, _DMR_YEARS)]

    with open(source) as fh:
        head = [next(fh) for _ in range(10)]
    skip = next((i for i, line in enumerate(head) if "Data Source" in line), 0)
    dmr = pd.read_csv(source, skiprows=skip, low_memory=False)
    dmr.columns = [c.replace(".", "_") for c in dmr.columns]
    return dmr[dmr["Facility_Latitude"].notna() & dmr["Facility_Longitude"].notna()]


def load_wastewater_nlcd(source: str, input_directory: Path, inventory_year: int):
    """NLCD 'developed open space + low intensity' fractional cover, nearest year.

    Returns ``(suburbia, nlcd_year)``. ``"M3T"`` reads the companion
    ``combined_wastewater_NLCD.tif``, whose bands are years; we select the band
    nearest the run year (the R prints a note when it has to substitute).
    """
    import rioxarray

    if source != "M3T":
        raise NotImplementedError(
            "Source_wastewater_NLCD: only the packaged 'M3T' companion raster is "
            "ported; the per-state NLCD_fractions_by_state path is not"
        )
    path = Path(input_directory) / "combined_wastewater_NLCD.tif"
    if not path.exists():
        raise _zenodo_todo("Source_wastewater_NLCD", path)

    da = rioxarray.open_rasterio(path, masked=True)
    years = [int(y) for y in (da.attrs.get("long_name") or ())]
    if years:
        year = _nearest(inventory_year, years)
        da = da.isel(band=years.index(year))
    else:  # single-band raster (e.g. the clipped test fixture)
        year = int(da.attrs.get("year", inventory_year))
        da = da.squeeze("band", drop=True) if "band" in da.dims else da
    return da, year


def load_septic_areas(input_directory: Path, inventory_year: int, ghgi_data_yr: int, states):
    """National and per-state area (km²) of the septic-bearing NLCD classes."""
    in_dir = Path(input_directory)
    nat_path = in_dir / "Total_national_septic_area.csv"
    state_path = in_dir / "wastewater_state_septic_area.csv"
    for p in (nat_path, state_path):
        if not p.exists():
            raise _zenodo_todo("Total_national_open_or_low_int_area", p)

    national = pd.read_csv(nat_path)
    row = national.iloc[(national["year"] - inventory_year).abs().idxmin()]
    total_national_area = float(row["Total_national_open_or_low_int_area"])

    state = pd.read_csv(state_path)
    year_cols = [c for c in state.columns if c != "state"]
    col = str(_nearest(ghgi_data_yr, [int(c) for c in year_cols]))
    state = state[["state", col]].rename(
        columns={"state": "X", col: "open_or_low_int_area"}
    )
    return total_national_area, state[state["X"].isin(list(states))]


def load_state_population(source: str, input_directory: Path) -> pd.DataFrame:
    """Census state population estimates (``POPESTIMATE<year>`` columns)."""
    if source == "M3T":
        return datasets.load("Census_state_population_M3T")
    if source == "download":
        raise NotImplementedError(
            "Source_State_population_data='download' is not ported; use 'M3T' or a CSV path"
        )
    return pd.read_csv(source)


def _needs_ghgrp_facility(cfg) -> bool:
    return bool(
        cfg.Process_landfills
        or cfg.Process_natural_gas_distribution
        or cfg.Process_natural_gas_transmission
        or cfg.Process_wastewater
    )


def resolve_ghgi_year(inventory_year: int) -> int:
    """GHGI_data_yr = inventory_year, clamped to the latest available GHGI year."""
    tbl = datasets.load("GHGI_landfill_total_M3T")
    ghgi_file_yr = int(pd.to_numeric(tbl["Year"]).max())
    return ghgi_file_yr if inventory_year > ghgi_file_yr else int(inventory_year)


def prepare_shared_data(ctx: RunContext) -> None:
    """Populate ``ctx.shared`` with cross-sector inputs the enabled sectors need.

    Respects anything already present in ``ctx.shared`` (so tests / callers can
    inject data and stay offline).
    """
    cfg = ctx.config

    if _needs_ghgrp_facility(cfg) and "ghgrp_facility_data" not in ctx.shared:
        ctx.shared["ghgrp_facility_data"] = load_ghgrp_facility_data(
            cfg.Source_GHGRP_facility_data, ctx.input_directory, timeout=cfg.Base_timeout
        )

    # GHGRP subpart W (NG distribution + transmission)
    if (
        (cfg.Process_natural_gas_distribution or cfg.Process_natural_gas_transmission)
        and "ghgrp_subpartW_emissions" not in ctx.shared
    ):
        ctx.shared["ghgrp_subpartW_emissions"] = load_ghgrp_subpartW(
            cfg.Source_GHGRP_NG, ctx.input_directory, timeout=cfg.Base_timeout
        )

    # EIA transmission pipeline lines (NG transmission)
    if cfg.Process_natural_gas_transmission and "eia_transmission_pipes" not in ctx.shared:
        ctx.shared["eia_transmission_pipes"] = load_eia_transmission_pipes(
            cfg.Source_EIA_transmission_file, ctx.input_directory, timeout=cfg.Base_timeout
        )

    if "ghgi_data_yr" not in ctx.shared:
        ctx.shared["ghgi_data_yr"] = resolve_ghgi_year(ctx.inventory_year)

    # --- wastewater companion inputs ------------------------------------- #
    if cfg.Process_wastewater:
        if cfg.Wastewater_use_DMR and "dmr_data" not in ctx.shared:
            ctx.shared["dmr_data"] = load_dmr(
                cfg.Source_DMR, ctx.input_directory, ctx.inventory_year
            )

        needs_septic = cfg.Wastewater_national_septic or cfg.Wastewater_state_septic
        if needs_septic and "nlcd_suburbia" not in ctx.shared:
            if "state_tigerlines" not in ctx.shared:
                raise ValueError(
                    "septic emissions need the state Tigerlines: pass `tigerlines=` to "
                    "ch4_inventory_build (or inject ctx.shared['state_tigerlines'])"
                )
            suburbia, nlcd_year = load_wastewater_nlcd(
                cfg.Source_wastewater_NLCD, ctx.input_directory, ctx.inventory_year
            )
            ctx.shared["nlcd_suburbia"] = suburbia
            ctx.shared["nlcd_year"] = nlcd_year
            if nlcd_year != ctx.inventory_year:
                print(
                    f"National Land Cover Data used for septic does not include "
                    f"{ctx.inventory_year}, using {nlcd_year} as the nearest available"
                )

            total_area, state_areas = load_septic_areas(
                ctx.input_directory,
                ctx.inventory_year,
                ctx.shared["ghgi_data_yr"],
                ctx.shared["state_name_list"],
            )
            ctx.shared["septic_total_national_area"] = total_area
            ctx.shared["septic_state_areas"] = state_areas

        if "state_population" not in ctx.shared:
            ctx.shared["state_population"] = load_state_population(
                cfg.Source_State_population_data, ctx.input_directory
            )

    # Landfill national total (resolve the "GHGI" keyword to a number).
    if cfg.Process_landfills and "ghgi_landfill_total" not in ctx.shared:
        total = cfg.GHGI_landfill_total
        if total == "GHGI":
            tbl = datasets.load("GHGI_landfill_total_M3T")
            yr = ctx.shared["ghgi_data_yr"]
            total = float(tbl.loc[pd.to_numeric(tbl["Year"]) == yr, "Emissions"].iloc[0])
        ctx.shared["ghgi_landfill_total"] = float(total)
