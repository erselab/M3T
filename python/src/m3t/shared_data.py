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

    # Landfill national total (resolve the "GHGI" keyword to a number).
    if cfg.Process_landfills and "ghgi_landfill_total" not in ctx.shared:
        total = cfg.GHGI_landfill_total
        if total == "GHGI":
            tbl = datasets.load("GHGI_landfill_total_M3T")
            yr = ctx.shared["ghgi_data_yr"]
            total = float(tbl.loc[pd.to_numeric(tbl["Year"]) == yr, "Emissions"].iloc[0])
        ctx.shared["ghgi_landfill_total"] = float(total)
