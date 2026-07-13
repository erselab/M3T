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

import numpy as np
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


# The ACES NetCDFs keep their georeferencing in a `crs` variable (proj4 +
# geotransform attributes) rather than on the data variable, and GDAL does not pick
# it up: opened as-is they have extent 0..ncol and no CRS. These are the values from
# the files' own attributes; tests/golden/capture_stationary_combustion_oracle.R
# applies exactly the same ones, so R and Python agree on the grid.
_ACES_PROJ = (
    "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 "
    "+ellps=WGS84 +units=m +no_defs"
)
_ACES_ORIGIN = (-2300000.0, 1300000.0)  # top-left x, y
_ACES_RES = 1000.0
_ACES_SECTORS = {
    "res": "Residential",
    "com": "Commercial",
    "ind": "Industrial",
    "elec": "Elec",
}
_ACES_YEARS = range(2012, 2018)


def georeference_aces(da):
    """Attach ACES's own CRS/extent to a raster GDAL failed to georeference."""
    from rasterio.transform import from_origin

    if "band" in da.dims:
        da = da.squeeze("band", drop=True)
    da = da.rio.write_crs(_ACES_PROJ)
    da = da.rio.write_transform(
        from_origin(_ACES_ORIGIN[0], _ACES_ORIGIN[1], _ACES_RES, _ACES_RES)
    )
    nrow, ncol = da.shape
    xs = _ACES_ORIGIN[0] + (np.arange(ncol) + 0.5) * _ACES_RES
    ys = _ACES_ORIGIN[1] - (np.arange(nrow) + 0.5) * _ACES_RES
    return da.assign_coords({da.dims[1]: xs, da.dims[0]: ys}).rename(
        {da.dims[0]: "y", da.dims[1]: "x"}
    )


def load_aces(source: str, input_directory: Path, inventory_year: int):
    """The four ACES sectoral CO2 rasters, keyed res/com/ind/elec."""
    import rioxarray

    base = Path(source) if source != "M3T" else Path(input_directory) / "ACES V2.0"
    if not base.exists():
        raise _zenodo_todo("Source_ACES", base)

    year = _nearest(inventory_year, _ACES_YEARS)
    out = {}
    for key, name in _ACES_SECTORS.items():
        path = base / f"ACES_annual_{name}_{year}.nc"
        if not path.exists():
            raise FileNotFoundError(f"ACES file missing: {path}")
        da = rioxarray.open_rasterio(f'netcdf:"{path}":flux_co2', masked=True)
        out[key] = georeference_aces(da)
    return out


_VULCAN_SECTORS = {"res": "res", "com": "com", "ind": "ind", "elec": "elc"}
_VULCAN_YEARS = range(2010, 2023)


def load_vulcan(source: str, input_directory: Path, inventory_year: int):
    """The four Vulcan v4.0 sectoral CO2 rasters, keyed res/com/ind/elec.

    Expects the *extracted* GeoTIFFs (the Zenodo zips unpack to one per year); see
    :func:`m3t.download.download_vulcan`. Unlike ACES these are properly
    georeferenced, so they need no fixing up.
    """
    import rioxarray

    base = Path(source) if source != "M3T" else Path(input_directory) / "Vulcan_v4.0"
    if source == "download":
        from .download import download_vulcan

        base = Path(input_directory) / "Vulcan_v4.0"
        if len(list(base.glob("*.tif"))) < 4:
            download_vulcan(base)
    if not base.exists():
        raise _zenodo_todo("Source_Vulcan", base)

    year = _nearest(inventory_year, _VULCAN_YEARS)
    out = {}
    for key, code in _VULCAN_SECTORS.items():
        path = base / f"v4.{code}.co2.usa.1km.lcc.mn.{year}.tif"
        if not path.exists():
            raise FileNotFoundError(
                f"Vulcan file missing: {path}. The zips must be extracted to .tif "
                "(Source_Vulcan='download' does this for you)."
            )
        da = rioxarray.open_rasterio(path, masked=True)
        out[key] = da.squeeze("band", drop=True) if "band" in da.dims else da
    return out, year


def load_county_tigerlines(
    source: str, input_directory: Path, inventory_year: int, state_fips
):
    """County polygons for the run's states, from the companion year-layered gpkg.

    ``state_fips`` comes from the already-derived state Tigerlines (counties carry
    ``STATEFP`` but no ``STUSPS``, so the states table is what links them).
    """
    import fiona
    import geopandas as gpd

    base = Path(source) if source != "M3T" else (
        Path(input_directory) / "combined_county_tigerlines.gpkg"
    )
    if not base.exists():
        raise _zenodo_todo("Source_Tigerlines_data", base)

    layers = [int(y) for y in fiona.listlayers(base)]
    layer = str(_nearest(inventory_year, layers))
    counties = gpd.read_file(base, layer=layer)
    return counties[counties["STATEFP"].isin(set(state_fips))]


def resolve_nei_year(nei: pd.DataFrame, inventory_year: int) -> int:
    """NEI runs every 3 years; take the nearest available inventory."""
    years = sorted(int(y) for y in nei["INVENTORY YEAR"].unique())
    return _nearest(inventory_year, years)


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

    # --- gridded CO2 inventory: stationary combustion + NG distribution ---- #
    needs_inventory = cfg.Process_stationary_combustion or cfg.Process_natural_gas_distribution
    if needs_inventory:
        if "state_tigerlines" not in ctx.shared:
            raise ValueError(
                "stationary combustion / NG distribution disaggregate by state: pass "
                "`tigerlines=` to ch4_inventory_build (or inject "
                "ctx.shared['state_tigerlines'])"
            )

    # only stationary combustion resolves emissions down to counties
    if cfg.Process_stationary_combustion and "county_tigerlines" not in ctx.shared:
        ctx.shared["county_tigerlines"] = load_county_tigerlines(
            cfg.Source_Tigerlines_data,
            ctx.input_directory,
            ctx.inventory_year,
            ctx.shared["state_tigerlines"]["STATEFP"],
        )

    if needs_inventory:

        if cfg.Use_ACES and "aces_inventories" not in ctx.shared:
            ctx.shared["aces_inventories"] = load_aces(
                cfg.Source_ACES, ctx.input_directory, ctx.inventory_year
            )
        if cfg.Use_Vulcan and "vulcan_inventories" not in ctx.shared:
            inventories, vulcan_year = load_vulcan(
                cfg.Source_Vulcan, ctx.input_directory, ctx.inventory_year
            )
            ctx.shared["vulcan_inventories"] = inventories
            if vulcan_year != ctx.inventory_year:
                print(
                    f"Vulcan does not include {ctx.inventory_year}, "
                    f"using {vulcan_year} as the nearest data available"
                )

    if cfg.Process_stationary_combustion and "nei_year" not in ctx.shared:
        nei_year = resolve_nei_year(datasets.load("NEI_all_years"), ctx.inventory_year)
        ctx.shared["nei_year"] = nei_year
        if nei_year != ctx.inventory_year:
            print(
                f"NEI is every 3 years and does not have an inventory for "
                f"{ctx.inventory_year}. Using {nei_year} as the nearest available data."
            )

    # --- wetlands companion inputs ---------------------------------------- #
    if cfg.Process_wetlands_and_inland_waters:
        if "state_tigerlines" not in ctx.shared:
            raise ValueError(
                "wetlands needs the per-state NWI rasters, selected from the state "
                "list: pass `tigerlines=` to ch4_inventory_build"
            )
        if cfg.Use_Wetcharts and "wetcharts" not in ctx.shared:
            ctx.shared["wetcharts"] = load_wetcharts(cfg.Source_wetcharts, ctx.input_directory)
        if "nwi" not in ctx.shared:
            ctx.shared["nwi"] = load_nwi(
                cfg.Source_NWI, ctx.input_directory, ctx.shared["state_name_list"]
            )
        if cfg.Use_SOCCR2 and "watersheds" not in ctx.shared:
            ctx.shared["watersheds"] = load_watersheds(
                cfg.Source_Watershed_file, ctx.input_directory
            )

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


def load_wetcharts(source: str, input_directory: Path):
    """The pre-downscaled Wetcharts ensemble raster (bands ``<year>_model_<id>``)."""
    import rioxarray

    base = (
        Path(input_directory) / "combined_NLCD_downscaled_wetcharts.tif"
        if source == "M3T"
        else Path(source)
    )
    if not base.exists():
        raise _zenodo_todo("Source_wetcharts", base)
    da = rioxarray.open_rasterio(base, masked=True)
    return da.assign_coords(band_name=("band", list(da.attrs.get("long_name", []))))


def load_nwi(source: str, input_directory: Path, states):
    """Per-state NWI wetland-class rasters, combined across states with ``max``.

    Returns ``{class: DataArray}``. The state rasters overlap at their borders, so
    ``max`` merges them without double-counting the seam (see combine_nwi_states).
    """
    import rioxarray

    from .sectors.wetlands import combine_nwi_states

    base = (
        Path(input_directory) / "processed_NWI_data" if source == "M3T" else Path(source)
    )
    if not base.exists():
        raise _zenodo_todo("Source_NWI", base)

    per_class: dict = {}
    for state in states:
        path = base / f"{state}_combined_NWI_wetland_landcover.tif"
        if not path.exists():
            raise FileNotFoundError(f"NWI raster missing for {state}: {path}")
        da = rioxarray.open_rasterio(path, masked=True)
        for i, cls in enumerate(da.attrs.get("long_name", [])):
            per_class.setdefault(cls, []).append(da.isel(band=i))
    return {cls: combine_nwi_states(v) for cls, v in per_class.items()}


def load_watersheds(source: str, input_directory: Path):
    """Watershed polygons (their ``NAW1_EN`` names give the SOCCR2 ocean basin)."""
    import geopandas as gpd

    base = Path(input_directory) / "Watersheds.gpkg" if source == "M3T" else Path(source)
    if not base.exists():
        raise _zenodo_todo("Source_Watershed_file", base)
    return gpd.read_file(base)
