"""Spread aggregate emissions down to pixels using a gridded CO2 inventory.

Port of ``R/Inventory_based_disaggregation.R``. Used by stationary combustion and
(later) natural gas distribution: both know CH4 emissions per county but not
*where* inside the county they happen, so they borrow the spatial pattern of a
sectoral CO2 inventory (Vulcan or ACES).

For one county and one subsector::

    pixel_ch4 = county_ch4 * (cover-weighted pixel CO2 / county-total CO2)

Two details carry the fidelity:

* **Cover weights.** A pixel straddling a county line belongs to it only in part,
  so each pixel's CO2 is weighted by the fraction of that pixel inside the county
  (``terra::extract(weights=TRUE)``). Counties are processed independently, so a
  border pixel legitimately collects a share from each county it touches.
* **Counties with no CO2.** If the inventory shows zero CO2 across a county (it
  happens), the CH4 cannot be distributed proportionally, so it is spread evenly
  over the county's pixels — still weighted by coverage, so a pixel half inside
  gets half a share.
"""

from __future__ import annotations

import numpy as np
import xarray as xr

from . import geo


def county_cover_weights(inventory: xr.DataArray, counties) -> list[xr.DataArray]:
    """Per-county pixel coverage fractions on the inventory grid.

    One weight raster per row of ``counties`` (already in the inventory's CRS),
    in row order. Port of the R's ``split() |> lapply(extract(weights=TRUE))``.
    """
    return [
        geo.coverage_fraction(inventory, counties.iloc[[i]])
        for i in range(len(counties))
    ]


def inventory_based_disaggregation(
    inventory: xr.DataArray,
    totals: list[str],
    counties,
    covers: list[xr.DataArray],
    *,
    progress: str | None = None,
) -> dict[str, xr.DataArray]:
    """Disaggregate county CH4 totals onto the inventory grid.

    Parameters
    ----------
    inventory:
        Gridded sectoral CO2 (one ACES/Vulcan sector), used only for its spatial
        pattern.
    totals:
        Subsector column names in ``counties`` to disaggregate (e.g. the four
        commercial fuels). One output raster per name.
    counties:
        GeoDataFrame of county polygons carrying a CH4 column per ``totals`` entry
        (mol/s), in the inventory's CRS.
    covers:
        Per-county coverage-fraction rasters from :func:`county_cover_weights`.

    Returns ``{total: DataArray}`` — CH4 summed over all counties, on the
    inventory grid, in the same units as the county column (mol/s).
    """
    co2 = inventory.fillna(0.0)
    out = {t: geo.grid_like(inventory, fill=0.0, name=t) for t in totals}

    for i, cover in enumerate(covers):
        # weight this county's CO2 by how much of each pixel it actually owns;
        # pixels the county doesn't touch come back NaN -> contribute nothing
        w = cover.fillna(0.0)
        weighted = co2 * w
        co2_total = float(weighted.sum())

        if co2_total == 0.0:
            # no CO2 anywhere in this county: fall back to an even spread over the
            # county's (partial) pixels
            weighted = w
            co2_total = float(w.sum())
            if co2_total == 0.0:  # county covers no pixel at all
                continue

        frac = weighted / co2_total

        row = counties.iloc[i]
        for total in totals:
            value = row.get(total, np.nan)
            if value is None or (isinstance(value, float) and np.isnan(value)):
                continue
            out[total] = out[total] + frac * float(value)

        if progress:
            print(
                f"\rFinished mapping {progress} entry {i + 1} of {len(covers)}",
                end="",
                flush=True,
            )
    if progress:
        print()

    for total in totals:
        out[total].rio.write_crs(inventory.rio.crs, inplace=True)
    return out
