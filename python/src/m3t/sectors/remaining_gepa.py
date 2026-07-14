"""Remaining sectors, from the gridded EPA inventory. Port of ``R/Prepare_GEPA.R``.

M3T builds its own estimates for landfills, wastewater, natural gas and stationary
combustion. Everything *else* in the national inventory — cows, rice, coal mines,
oil systems, mobile combustion — is taken as-is from EPA's gridded methane
inventory (GEPA v2) rather than rebuilt, and this sector just reprojects the
relevant layers onto the run's grid.

Three outputs, grouping GEPA's sector layers by what they are:

* ``GEPA_ind_landfill`` — industrial landfills (the *municipal* ones are M3T's own
  landfill sector, so only the industrial layer is taken here).
* ``GEPA_non_thermo`` — biogenic: enteric fermentation, manure, rice, field
  burning, composting.
* ``GEPA_thermo`` — fossil: mobile combustion, coal (surface/underground/
  abandoned), petroleum systems, abandoned oil & gas, gas exploration/processing/
  production, petrochemicals, ferroalloy.

Note what is deliberately *not* included: ``emi_ch4_1A_Combustion_Stationary``,
the gas distribution and municipal-landfill layers, and so on — M3T computes those
itself, and taking GEPA's version too would double-count them.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import xarray as xr

from .. import geo
from ..context import RunContext

# molec/cm2/s -> nmol/m2/s
_MOLEC_CM2_TO_NMOL_M2 = (1e9 * 100**2) / 6.022141e23

LANDFILL_VAR = "emi_ch4_5A1_Landfills_Industrial"

NON_THERMO_VARS = [
    "emi_ch4_5B1_Composting",
    "emi_ch4_3A_Enteric_Fermentation",
    "emi_ch4_3B_Manure_Management",
    "emi_ch4_3C_Rice_Cultivation",
    "emi_ch4_3F_Field_Burning",
]

THERMO_VARS = [
    "emi_ch4_1A_Combustion_Mobile",
    "emi_ch4_1B1a_Abandoned_Coal",
    "emi_ch4_1B1a_Surface_Coal",
    "emi_ch4_1B1a_Underground_Coal",
    "emi_ch4_1B2a_Petroleum_Systems_Exploration",
    "emi_ch4_1B2a_Petroleum_Systems_Production",
    "emi_ch4_1B2a_Petroleum_Systems_Refining",
    "emi_ch4_1B2a_Petroleum_Systems_Transport",
    "emi_ch4_1B2ab_Abandoned_Oil_Gas",
    "emi_ch4_1B2b_Natural_Gas_Exploration",
    "emi_ch4_1B2b_Natural_Gas_Processing",
    "emi_ch4_1B2b_Natural_Gas_Production",
    "emi_ch4_2B8_Industry_Petrochemical",
    "emi_ch4_2C2_Industry_Ferroalloy",
]


def _to_domain(
    da: xr.DataArray, domain, domain_template: xr.DataArray, domain_crs: str
) -> xr.DataArray:
    """GEPA grid -> domain grid.

    The R branches on which grid is coarser. GEPA is 0.1 degrees, so a run at 0.1
    degrees (or finer) takes the first branch: refine with a nearest disagg, then
    reproject nearest and mask. Nearest throughout, deliberately — it stops the
    reprojection interpolating across GEPA's cell edges, which would smear a
    coarse inventory across the domain. A *coarser* run takes the second branch and
    area-averages, weighting cells by how much of them is inside the domain.
    """

    src_crs = da.rio.crs
    dom = geo.as_polygons(domain, domain_crs)
    dom_src = dom.to_crs(src_crs)

    domain_res = geo.res(geo.project_to_crs(domain_template, src_crs))
    src_res = geo.res(da)

    if domain_res[0] - src_res[0] <= 1e-5:
        # domain at least as fine as GEPA: refine, then nearest onto the grid
        factor = max(int(round(src_res[0] / domain_res[0])), 1)
        out = geo.disagg(da, factor)
        out = geo.project_to_grid(out, domain_template, resampling="nearest")
        # terra::mask() keeps every cell the polygon *touches*, not just those whose
        # centre it covers -- verified against R here (touches=False drops 26 coastal
        # cells and 15% of the emissions; touches=True reproduces the mask exactly).
        return geo.mask_geometries(out, dom, touches=True, updatevalue=np.nan)

    # domain coarser than GEPA: area-average, honouring partial coverage. NB the R
    # asks for *exact* weights here (extract(weights=TRUE, exact=TRUE)), unlike the
    # approximate 10x10 sub-sampling it uses everywhere else.
    out = da.fillna(0.0)
    out = geo.crop_snap_out(out, tuple(dom_src.total_bounds))
    out = geo.mask_geometries(out, dom_src, touches=True, updatevalue=0.0)

    weights = geo.coverage_fraction(out, dom_src, exact=True)
    out = out.where(weights.isnull(), out * weights)

    pad = geo.res(geo.project_to_crs(domain_template, src_crs))
    out = geo.extend(out, (pad[0] * 5, pad[1] * 5), fill=0.0)
    out = geo.project_to_grid(out, domain_template, resampling="average")

    # NA out cells that are both outside the domain and exactly zero (a cell just
    # outside can still carry real signal that bled across in the reprojection).
    # "Outside" means untouched, per terra's mask semantics -- see above.
    outside = geo.mask_geometries(out, dom, touches=True, updatevalue=np.nan)
    return out.where(~(outside.isnull() & (out == 0)), np.nan)


def compute_gepa(
    gepa: xr.Dataset,
    *,
    domain,
    domain_template: xr.DataArray,
    domain_crs: str,
) -> dict[str, xr.DataArray]:
    """Group, convert and reproject the GEPA layers. Returns three flux rasters."""
    missing = [v for v in (LANDFILL_VAR, *NON_THERMO_VARS, *THERMO_VARS) if v not in gepa]
    if missing:
        raise ValueError(f"GEPA file is missing expected sectors: {missing}")

    def _layer(name: str) -> xr.DataArray:
        da = gepa[name]
        if "time" in da.dims:
            da = da.isel(time=0, drop=True)
        # GEPA names its axes lat/lon; the geo layer works in y/x.
        renames = {k: v for k, v in (("lat", "y"), ("lon", "x")) if k in da.dims}
        if renames:
            da = da.rename(renames)
        # GEPA stores latitude *ascending* (CF convention). Every raster in geo.py is
        # north-up, so flip it: left as-is, the transform is upside down and every
        # crop and mask lands in the wrong place -- silently, since the shapes still
        # look right.
        if da.sizes.get("y", 0) > 1 and float(da["y"][0]) < float(da["y"][-1]):
            da = da.isel(y=slice(None, None, -1))
        return da

    groups = {
        "GEPA_ind_landfill": _layer(LANDFILL_VAR),
        "GEPA_non_thermo": sum(_layer(v) for v in NON_THERMO_VARS),
        "GEPA_thermo": sum(_layer(v) for v in THERMO_VARS),
    }

    out: dict[str, xr.DataArray] = {}
    for name, da in groups.items():
        da = da * _MOLEC_CM2_TO_NMOL_M2
        da = da.rio.write_crs(gepa.rio.crs or "epsg:4326")
        flux = _to_domain(da, domain, domain_template, domain_crs)
        flux.name = "methane_emissions"
        out[name] = flux
    return out


@dataclass
class RemainingGEPASector:
    """RunContext wrapper: read shared inputs, compute, write NetCDF outputs."""

    key: str = "remaining_gepa"
    name: str = "Remaining sectors (gridded EPA)"
    process_flag: str = "Process_remaining_sectors_from_gridded_EPA"

    def run(self, ctx: RunContext) -> None:
        results = compute_gepa(
            ctx.shared["gepa"],
            domain=ctx.domain,
            domain_template=ctx.domain_template,
            domain_crs=ctx.domain_crs,
        )
        for name, da in results.items():
            ctx.write_output(da, f"{name}.nc")

        combined = sum(da.fillna(0.0) for da in results.values())
        combined.name = "methane_emissions"
        ctx.write_output(combined, f"{self.key}.nc")

        ctx.shared.setdefault("sector_results", {})[self.key] = results


def register() -> None:
    from . import base

    base.register(RemainingGEPASector())
