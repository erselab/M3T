"""Natural gas transmission sector. Port of ``R/Natural_Gas_Transmission.R``.

Two components:

* **Pipelines** — a GHGI-derived emission factor (mol CH4 / m pipeline / s) is
  multiplied by the per-cell length of EIA transmission pipelines.
* **Compressors** — a GHGI national average emission rate (mol / station / s) is
  assigned to every HIFLD compressor, then overwritten with GHGRP subpart W (+
  combustion) values where a facility matches (HIFLD carries a manually-aligned
  ``GHGRP ID``); unmatched GHGRP compressors are added as extra points.

Both are rasterized to mol/s per cell and converted to flux (nmol/m²/s) via
geodesic cell area. :func:`compute_ng_transmission` takes plain inputs so it can
be golden-tested against the R function.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

import pandas as pd
import xarray as xr

from .. import geo
from ..context import RunContext
from ..download import make_consistent
from ._spatial import clip_points_to_domain, project_points

_SEC_PER_YR = 365 * 24 * 60 * 60
_CH4 = 16.043
_MT_TO_MOL_S = 1e6 / (_CH4 * _SEC_PER_YR)

_SEG_TRANSMISSION = "Onshore natural gas transmission compression [98.230(a)(4)]"
_SEG_PROCESSING = "Onshore natural gas processing [98.230(a)(3)]"
# HIFLD compressor points inside this lon/lat box (Canada) are dropped up front.
_CANADA_BOX = (-125.0, 49.0001, -95.0, 60.0)


def _flux(rast: xr.DataArray, template: xr.DataArray) -> xr.DataArray:
    """mol/s per cell -> flux nmol/m²/s; empty cells 0."""
    flux = rast * 1e9 / geo.cell_area(template, unit="m")
    flux = flux.fillna(0.0)
    flux.name = "methane_emissions"
    return flux


def _prepare_subpartW(subpartW: pd.DataFrame, combustion: pd.DataFrame, ghgrp_year: int) -> pd.DataFrame:
    """Aggregate subpart W to facility totals, add combustion, drop processing combustion."""
    w = subpartW[subpartW["reporting_year"] == ghgrp_year]
    agg = (
        w.groupby(
            ["facility_id", "reporting_year", "facility_name", "industry_segment"], as_index=False
        )["total_reported_ch4_emissions"]
        .sum()
        .rename(columns={"total_reported_ch4_emissions": "ghg_quantity"})
    )

    trans = agg[agg["industry_segment"] == _SEG_TRANSMISSION].copy()
    processing = agg[agg["industry_segment"] == _SEG_PROCESSING]

    # match the combustion schema via make_consistent, then keep combustion's cols
    trans["ghg_gas_name"] = "methane"
    trans = make_consistent(trans)
    trans = trans[list(combustion.columns)]

    trans = trans.rename(columns={"ghg_quantity": "W_emissions"})
    comb = combustion.rename(columns={"ghg_quantity": "C_emissions"})
    merged = trans.merge(comb, on=["facility_id", "year", "facility_name", "ghg_name"], how="left")

    merged["W_emissions"] = pd.to_numeric(merged["W_emissions"], errors="coerce")
    merged["C_emissions"] = pd.to_numeric(merged["C_emissions"], errors="coerce")
    merged["ghg_quantity"] = merged[["W_emissions", "C_emissions"]].sum(axis=1, skipna=True)

    # for processing facilities the combustion isn't part of the transmission total
    is_proc = merged["facility_id"].isin(processing["facility_id"])
    merged.loc[is_proc, "ghg_quantity"] = merged.loc[is_proc, "W_emissions"]

    return merged[merged["ghg_quantity"] > 0]


def _compressor_avg_emissions(ghgi_comp: pd.DataFrame) -> float:
    """Port of the GHGI transmission-compressor EF resolution (mol/station/s).

    ``ghgi_comp`` has columns Emissions/Total_stations in the original 11-row
    component order. Generator emissions (engine=row8, turbine=row9, 1-indexed)
    are scaled by the transmission fraction of engines/turbines, storage rows
    (6,7) are dropped, and the total is divided by the flaring-row station count.
    """
    e = ghgi_comp["Emissions"].to_numpy(dtype="float64").copy()
    # R 1-indexed rows -> 0-indexed
    engine_frac = e[3] / (e[3] + e[5])
    turbine_frac = e[4] / (e[4] + e[6])
    e[7] = engine_frac * e[7]
    e[8] = turbine_frac * e[8]
    keep = [0, 1, 2, 3, 4, 7, 8, 9, 10]  # R c(1:5, 8:11)
    e_keep = e[keep]
    stations = ghgi_comp["Total_stations"].to_numpy(dtype="float64")[keep]
    return float(e_keep.sum() / stations[2])  # R [3,3] -> flaring-row station count


def _pipeline_ef(ghgi_pipe: pd.DataFrame) -> float:
    """Pipeline EF (mol/m/s) = sum(emissions) / leaks-row pipeline length."""
    e = ghgi_pipe["Emissions"].to_numpy(dtype="float64")
    stations = ghgi_pipe["Total_stations"].to_numpy(dtype="float64")
    return float(e.sum() / stations[0])


def compute_ng_transmission(
    *,
    ghgi_transmission_compressors: pd.DataFrame,
    ghgi_pipeline: pd.DataFrame,
    hifld: pd.DataFrame,
    eia_pipes,
    ghgrp_facility_data: pd.DataFrame,
    subpartW: pd.DataFrame,
    ghgrp_combustion: pd.DataFrame,
    ghgi_data_yr: int,
    domain_template: xr.DataArray,
    domain: Any,
    domain_crs: str = "epsg:4326",
) -> dict[str, xr.DataArray]:
    """Compute the pipeline + compressor flux rasters and their sector total."""
    ghgrp_year = int(ghgi_data_yr)

    compressor_avg = _compressor_avg_emissions(ghgi_transmission_compressors)
    pipeline_ef = _pipeline_ef(ghgi_pipeline)

    # --- pipelines --------------------------------------------------------- #
    pipes = eia_pipes
    if pipes.crs is not None and str(pipes.crs) != domain_crs:
        pipes = pipes.to_crs(domain_crs)
    length_m = geo.rasterize_line_length(pipes, domain_template, unit="m")
    pipes_flux = _flux(length_m * pipeline_ef, domain_template)
    pipes_flux = geo.mask(pipes_flux, _domain_geom(domain, domain_crs), crs_=domain_crs)
    pipes_flux = pipes_flux.fillna(0.0)

    # --- compressors ------------------------------------------------------- #
    # HIFLD points, drop Canada, keep GHGRP ID for matching
    hifld = hifld.copy()
    cx0, cy0, cx1, cy1 = _CANADA_BOX
    in_canada = (
        hifld["LONGITUDE"].between(cx0, cx1) & hifld["LATITUDE"].between(cy0, cy1)
    )
    hifld = hifld[~in_canada]
    hifld_pts = project_points(
        hifld, "LONGITUDE", "LATITUDE", domain_crs, keep=["GHGRP ID", "NAME"]
    )
    hifld_pts = clip_points_to_domain(hifld_pts, domain, domain_crs)
    hifld_pts = hifld_pts.reset_index(drop=True)
    hifld_pts["emiss"] = compressor_avg  # default national average

    # GHGRP subpart W compressors in the domain, MT/yr -> mol/s
    subW = _prepare_subpartW(subpartW, ghgrp_combustion, ghgrp_year)
    ghgrp_comp = ghgrp_facility_data.merge(subW, on=["facility_id", "year"])
    ghgrp_comp["ghg_quantity"] = pd.to_numeric(ghgrp_comp["ghg_quantity"], errors="coerce")
    ghgrp_pts = project_points(
        ghgrp_comp, "longitude", "latitude", domain_crs, keep=["facility_id", "ghg_quantity"]
    )
    ghgrp_pts = clip_points_to_domain(ghgrp_pts, domain, domain_crs)
    ghgrp_pts["emiss"] = ghgrp_pts["ghg_quantity"] * _MT_TO_MOL_S

    if len(ghgrp_pts):
        # match GHGRP facility_id -> HIFLD "GHGRP ID" (first match), overwrite emiss
        hifld_ids = hifld_pts["GHGRP ID"].tolist()
        id_to_row = {}
        for i, fid in enumerate(hifld_ids):
            id_to_row.setdefault(fid, i)  # first occurrence, like R match()
        matched_rows, extra = [], []
        for _, row in ghgrp_pts.iterrows():
            r = id_to_row.get(row["facility_id"])
            if r is not None:
                hifld_pts.loc[r, "emiss"] = row["emiss"]
                matched_rows.append(r)
            else:
                extra.append({"x": row["x"], "y": row["y"], "emiss": row["emiss"]})
        if extra:
            hifld_pts = pd.concat([hifld_pts, pd.DataFrame(extra)], ignore_index=True)

    comp_rast = geo.rasterize_points_sum(
        hifld_pts["x"].to_numpy(), hifld_pts["y"].to_numpy(), hifld_pts["emiss"].to_numpy(),
        domain_template,
    )
    compressor_flux = _flux(comp_rast, domain_template)
    compressor_flux = geo.mask(compressor_flux, _domain_geom(domain, domain_crs), crs_=domain_crs)
    compressor_flux = compressor_flux.fillna(0.0)

    total = pipes_flux + compressor_flux
    total.name = "methane_emissions"

    return {
        "NG_trans_pipes": pipes_flux,
        "NG_trans_compressors": compressor_flux,
        "NG_transmission_sector_total": total,
    }


def _domain_geom(domain: Any, domain_crs: str):
    """Return a list of shapely geometries for masking (from bbox or GeoDataFrame)."""
    if isinstance(domain, tuple) and len(domain) == 4:
        from shapely.geometry import box

        return [box(domain[0], domain[1], domain[2], domain[3])]
    dom = domain if str(getattr(domain, "crs", domain_crs)) == domain_crs else domain.to_crs(domain_crs)
    return list(dom.geometry)


def build_ghgi_frames(ghgi_data_yr: int):
    """Resolve the two GHGI EF frames from the packaged GHGI_NG_transmission dataset."""
    from .. import datasets

    ng = datasets.load("GHGI_NG_transmission")
    yr = str(ghgi_data_yr)
    comp = pd.DataFrame(
        {
            "Emissions": ng["GHGI_transmission_compressors_Emissions"][yr].to_numpy(),
            "Total_stations": ng["GHGI_transmission_compressors_Activity"][yr].to_numpy(),
        }
    )
    pipe = pd.DataFrame(
        {
            "Emissions": ng["GHGI_Pipeline_Emissions"][yr].to_numpy(),
            "Total_stations": ng["GHGI_Pipeline_Activity"][yr].to_numpy(),
        }
    )
    return comp, pipe


@dataclass
class NGTransmissionSector:
    key: str = "natural_gas_transmission"
    name: str = "Natural gas transmission"
    process_flag: str = "Process_natural_gas_transmission"

    def run(self, ctx: RunContext) -> None:
        from .. import datasets

        comp_frame, pipe_frame = build_ghgi_frames(ctx.shared["ghgi_data_yr"])
        results = compute_ng_transmission(
            ghgi_transmission_compressors=comp_frame,
            ghgi_pipeline=pipe_frame,
            hifld=datasets.load("HIFLD_NG_data"),
            eia_pipes=ctx.shared["eia_transmission_pipes"],
            ghgrp_facility_data=ctx.shared["ghgrp_facility_data"],
            subpartW=ctx.shared["ghgrp_subpartW_emissions"],
            ghgrp_combustion=datasets.load("GHGRP_combustion_emissions"),
            ghgi_data_yr=ctx.shared["ghgi_data_yr"],
            domain_template=ctx.domain_template,
            domain=ctx.domain,
            domain_crs=ctx.domain_crs,
        )
        ctx.write_output(results["NG_trans_pipes"], "NG_trans_pipes.nc", subdir="NG_transmission")
        ctx.write_output(
            results["NG_trans_compressors"], "NG_trans_compressors.nc", subdir="NG_transmission"
        )
        ctx.write_output(results["NG_transmission_sector_total"], "NG_transmission_sector_total.nc")
        ctx.write_output(results["NG_transmission_sector_total"], f"{self.key}.nc")
        ctx.shared.setdefault("sector_results", {})[self.key] = results


def register() -> None:
    from . import base

    base.register(NGTransmissionSector())
