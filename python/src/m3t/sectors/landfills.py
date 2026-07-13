"""Municipal solid waste (landfills) sector. Port of ``R/Municipal_solid_waste.R``.

Emissions come from three data sources:

* **GHGRP** facility-level reported methane (subpart HH landfill + subpart C
  combustion), available three ways — ``reported`` (HH as reported), and two
  forced variants using the HH-6 first-order-decay (``generation_first``) or HH-8
  collection-efficiency (``collection_first``) equation results.
* **GHGI** national landfill total — the residual ``GHGI - GHGRP`` is spread
  evenly over ...
* **LMOP** facilities not already in the GHGRP.

Each source is converted to mol/s, rasterized (summing facilities per cell) onto
the target grid, and turned into a flux (nmol/m²/s) by dividing by geodesic cell
area. Sector totals add the LMOP flux to each enabled GHGRP variant.

The heavy lifting is in :func:`compute_landfills`, which takes plain inputs and
returns rasters, so it can be golden-tested without the orchestrator.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

import pandas as pd
import xarray as xr

from .. import geo
from ..context import RunContext
from ._spatial import clip_points_to_domain as _clip_to_domain
from ._spatial import project_points as _project_points

# Unit conversions to mol/s (CH4 molar mass 16.043 g/mol; seconds per year).
_SEC_PER_YR = 365 * 24 * 60 * 60
_CH4 = 16.043
_MT_TO_MOL_S = 1e6 / (_CH4 * _SEC_PER_YR)   # metric tonnes CH4/yr -> mol/s
_GG_TO_MOL_S = 1e9 / (_CH4 * _SEC_PER_YR)   # gigagrams CH4/yr -> mol/s

_GHGRP_METHODS = {
    "reported": "reported_method",
    "generation_first": "generation_first_method",
    "collection_first": "collection_first_method",
}


def _flux_from_points(points: pd.DataFrame, value_col: str, template: xr.DataArray) -> xr.DataArray:
    """Rasterize summed point emissions (mol/s) and convert to flux (nmol/m²/s).

    Port of the R ``landfill_rasterize`` helper: sum per cell, ``*1e9/cellSize``,
    then set empty cells to 0.
    """
    rast = geo.rasterize_points_sum(
        points["x"].to_numpy(), points["y"].to_numpy(), points[value_col].to_numpy(), template
    )
    area = geo.cell_area(template, unit="m")
    flux = rast * 1e9 / area
    flux = flux.fillna(0.0)
    flux.name = "methane_emissions"
    return flux


def compute_landfills(
    *,
    ghgrp_landfills: pd.DataFrame,
    ghgrp_combustion: pd.DataFrame,
    ghgrp_facility_data: pd.DataFrame,
    lmop: pd.DataFrame,
    ghgi_landfill_total: float,
    ghgi_data_yr: int,
    domain_template: xr.DataArray,
    domain: Any,
    domain_crs: str = "epsg:4326",
    methods: tuple[str, ...] = ("reported", "generation_first", "collection_first"),
) -> dict[str, xr.DataArray]:
    """Compute landfill flux rasters. Returns ``{name: DataArray}`` including each
    enabled GHGRP variant (``MSW_GHGRP_<method>``), ``MSW_LMOP``, and the sector
    totals (``Landfill_sector_total_GHGRP_<method>``).
    """
    ghgrp_year = int(ghgi_data_yr)

    # --- combine landfill (HH) + combustion (C) GHGRP emissions ------------- #
    landfills = ghgrp_landfills.rename(columns={"ghg_quantity": "HH_emissions"}).copy()
    combustion = ghgrp_combustion.rename(columns={"ghg_quantity": "C_emissions"}).copy()

    emis = landfills.merge(
        combustion, on=["facility_id", "year", "facility_name", "ghg_name"], how="left"
    )
    for col in ("HH_emissions", "C_emissions", "generation_first_HH6", "collection_first_HH8"):
        emis[col] = pd.to_numeric(emis[col], errors="coerce")
    # reported total per facility (MT CH4/yr): HH + C, treating missing as 0
    emis["ghg_quantity"] = emis[["HH_emissions", "C_emissions"]].sum(axis=1, skipna=True)

    # national GHGRP total for the year (MT -> Gg)
    ghgrp_national = emis.loc[emis["year"] == ghgrp_year, "ghg_quantity"].sum() / 1000.0
    non_ghgrp_total = ghgi_landfill_total - ghgrp_national  # Gg CH4/yr

    # --- attach facility locations / reporting status ---------------------- #
    all_data = ghgrp_facility_data.merge(emis, on=["facility_id", "year"], suffixes=("", "_emis"))
    ghgrp = all_data[all_data["year"] == ghgrp_year]

    # facilities that stopped reporting without a valid reason and are landfills
    # we don't already have current-year data for
    stopped = ghgrp_facility_data.loc[
        (ghgrp_facility_data["reporting_status"] == "STOPPED_REPORTING_UNKNOWN_REASON")
        & (ghgrp_facility_data["year"] <= ghgrp_year),
        "facility_id",
    ].unique()
    nonreporting = [f for f in stopped if f in set(emis["facility_id"])]
    nonreporting = [f for f in nonreporting if f not in set(ghgrp["facility_id"])]

    # for each such facility, take the row whose year is closest to ghgrp_year
    nr_data = all_data[all_data["facility_id"].isin(nonreporting)].copy()
    if len(nr_data):
        nr_data["_dist"] = (nr_data["year"] - ghgrp_year).abs()
        nr_data = (
            nr_data.sort_values(["facility_id", "_dist"])
            .groupby("facility_id", as_index=False)
            .first()
            .drop(columns="_dist")
        )

    updated = pd.concat([nr_data, ghgrp], ignore_index=True)

    for col in ("latitude", "longitude", "generation_first_HH6", "collection_first_HH8",
                "HH_emissions", "C_emissions"):
        updated[col] = pd.to_numeric(updated[col], errors="coerce")

    # --- MT -> mol/s and per-method sums ----------------------------------- #
    for col in ("HH_emissions", "generation_first_HH6", "collection_first_HH8", "C_emissions"):
        updated[col] = updated[col] * _MT_TO_MOL_S

    updated["reported_method"] = updated[["HH_emissions", "C_emissions"]].sum(axis=1, skipna=True)
    updated["generation_first_method"] = updated[["generation_first_HH6", "C_emissions"]].sum(
        axis=1, skipna=True
    )
    updated["collection_first_method"] = updated[["collection_first_HH8", "C_emissions"]].sum(
        axis=1, skipna=True
    )
    # facilities without a gas-capture system (NaN HH-6/HH-8) fall back to reported
    no_gen = updated["generation_first_HH6"].isna()
    no_col = updated["collection_first_HH8"].isna()
    updated.loc[no_gen, "generation_first_method"] = updated.loc[no_gen, "reported_method"]
    updated.loc[no_col, "collection_first_method"] = updated.loc[no_col, "reported_method"]

    # --- project GHGRP points to the domain CRS ---------------------------- #
    ghgrp_pts = _project_points(
        updated, "longitude", "latitude", domain_crs,
        keep=["facility_id", "reported_method", "generation_first_method", "collection_first_method"],
    )
    ghgrp_pts = _clip_to_domain(ghgrp_pts, domain, domain_crs)

    out: dict[str, xr.DataArray] = {}
    ghgrp_flux: dict[str, xr.DataArray] = {}
    for method in methods:
        col = _GHGRP_METHODS[method]
        flux = _flux_from_points(ghgrp_pts, col, domain_template)
        ghgrp_flux[method] = flux
        out[f"MSW_GHGRP_{method}"] = flux

    # --- LMOP: distribute (GHGI - GHGRP) residual over non-GHGRP LMOP sites - #
    lmop_non = lmop[~lmop["GHGRP ID"].isin(ghgrp["facility_id"])].copy()
    if len(nr_data):
        lmop_non = lmop_non[~lmop_non["GHGRP ID"].isin(nr_data["facility_id"])]
    # exclude landfills opened after the GHGRP year (keep unknown open dates)
    opened = pd.to_numeric(lmop_non["Year Landfill Opened"], errors="coerce")
    lmop_non = lmop_non[(opened <= ghgrp_year) | opened.isna()]

    # national per-facility average (uses the pre-crop national count)
    n_lmop = len(lmop_non)
    avg_non_ghgrp = non_ghgrp_total / n_lmop if n_lmop else 0.0
    emiss_mol_s = avg_non_ghgrp * _GG_TO_MOL_S

    lmop_pts = _project_points(lmop_non, "Longitude", "Latitude", domain_crs, keep=["GHGRP ID"])
    lmop_pts = _clip_to_domain(lmop_pts, domain, domain_crs)
    lmop_pts["emiss"] = emiss_mol_s
    lmop_flux = _flux_from_points(lmop_pts, "emiss", domain_template)
    out["MSW_LMOP"] = lmop_flux

    # --- sector totals ----------------------------------------------------- #
    for method in methods:
        total = lmop_flux + ghgrp_flux[method]
        total.name = "methane_emissions"
        out[f"Landfill_sector_total_GHGRP_{method}"] = total

    return out


@dataclass
class LandfillsSector:
    """RunContext wrapper: read shared inputs, compute, write NetCDF outputs."""

    key: str = "landfills"
    name: str = "Municipal solid waste (landfills)"
    process_flag: str = "Process_landfills"

    def run(self, ctx: RunContext) -> None:
        from .. import datasets

        cfg = ctx.config
        methods = tuple(
            m
            for m, flag in (
                ("reported", cfg.landfill_ghgrp_reported),
                ("generation_first", cfg.landfill_ghgrp_generation_first),
                ("collection_first", cfg.landfill_ghgrp_collection_first),
            )
            if flag
        )

        results = compute_landfills(
            ghgrp_landfills=datasets.load("GHGRP_landfills"),
            ghgrp_combustion=datasets.load("GHGRP_combustion_emissions"),
            ghgrp_facility_data=ctx.shared["ghgrp_facility_data"],
            lmop=datasets.load("LMOP_data"),
            ghgi_landfill_total=ctx.shared["ghgi_landfill_total"],
            ghgi_data_yr=ctx.shared["ghgi_data_yr"],
            domain_template=ctx.domain_template,
            domain=ctx.domain,
            domain_crs=ctx.domain_crs,
            methods=methods,
        )

        # main + LMOP rasters under out/Landfills; sector totals at out/ top level
        for method in methods:
            ctx.write_output(results[f"MSW_GHGRP_{method}"], f"MSW_GHGRP_{method}.nc", subdir="Landfills")
        ctx.write_output(results["MSW_LMOP"], "MSW_LMOP.nc", subdir="Landfills")
        for method in methods:
            ctx.write_output(
                results[f"Landfill_sector_total_GHGRP_{method}"],
                f"Landfill_sector_total_GHGRP_{method}.nc",
            )
        # the combine step reads one raster per sector; expose the reported (or
        # first enabled) sector total under the stub's <key>.nc name
        primary = f"Landfill_sector_total_GHGRP_{methods[0]}"
        ctx.write_output(results[primary], f"{self.key}.nc")

        ctx.shared.setdefault("sector_results", {})[self.key] = results


def register() -> None:
    from . import base

    base.register(LandfillsSector())
