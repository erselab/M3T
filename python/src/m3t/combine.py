"""Combine per-sector rasters into a total. Scaffold port of ``R/Combine_across_sectors.R``.

The full R routine builds several summary/individual combinations, separates the
thermogenic sectors, and renders visuals. This Phase-2 scaffold implements the
core operation the pipeline needs end-to-end: read each sector's gridded output,
align to the target grid, sum, and write the total. The richer combination logic
lands alongside the real sectors in Phase 3.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import xarray as xr

from . import geo
from .context import RunContext

def combine_across_sectors(ctx: RunContext) -> Path:
    """Sum each sector's canonical raster onto the target grid; write ``M3T_total.nc``.

    Reads exactly one file per sector that ran — ``<key>.nc`` — from the run
    manifest (``ctx.shared["sectors_run"]``), so a sector's extra variant/detail
    outputs (e.g. the landfill method variants) are not double-counted. Missing
    cells are treated as zero.
    """
    total = ctx.blank_grid(fill=0.0)
    keys = ctx.shared.get("sectors_run", [])
    n = 0
    for key in keys:
        f = ctx.output_directory / f"{key}.nc"
        if not f.exists():
            continue
        da = _read_nc(f)
        if da.shape != total.shape:
            da = da.rio.reproject_match(total)
        total = total + da.fillna(0.0)
        n += 1

    total.name = "methane_emissions"
    total.attrs["m3t_n_sectors_combined"] = n
    out = ctx.output_directory / "M3T_total.nc"
    geo.write_cdf(total, out, varname="methane_emissions")
    return out


def _read_nc(path: Path) -> xr.DataArray:
    """Read a single-variable NetCDF written by a sector back into a DataArray."""
    ds = xr.open_dataset(path, decode_coords="all")
    # the sector wrote one data variable; take it
    varname = next(iter(ds.data_vars))
    da = ds[varname]
    if da.rio.crs is None:
        da = da.rio.write_crs("epsg:4326")
    return da.astype("float64").where(np.isfinite(da), np.nan)
