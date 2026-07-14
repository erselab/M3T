#!/usr/bin/env python
"""Build the committed ACES CO2 fixtures (GeoTIFF) from the companion NetCDFs.

Why this exists, and why the capture scripts read its output rather than the .nc:

``terra::rast(path, subds="flux_co2")`` reads the ACES NetCDFs as **all zeros** in
this environment. It reports the right shape, so nothing looks wrong -- but every
value is 0. That silently disables the CO2 proxy: `inventory_based_disaggregation`
sees a county with no CO2 and falls back to spreading the county's methane evenly.
Both R and Python then took that fallback, so the golden tests agreed while never
exercising the CO2-weighted path they exist to check.

(The same NetCDFs are also unreadable through rasterio here -- GDAL's netCDF driver
is an optional plugin and is missing from this conda build. xarray reads them fine,
which is what m3t.shared_data.load_aces now does.)

So: Python reads the NetCDF, attaches ACES's own georeferencing, crops to the test
domain, and writes a GeoTIFF. R and Python then both read that GeoTIFF, and the
oracle actually tests the disaggregation.

    M3T_DATA=/path/to/M3T_Processed python tests/golden/make_aces_fixtures.py
"""

from __future__ import annotations

import os
from pathlib import Path

import geopandas as gpd

from m3t import geo
from m3t.shared_data import load_aces

DATA = Path(os.environ.get("M3T_DATA", "/Volumes/Expansion/M3T_Processed"))
HERE = Path(__file__).parent
YEAR = 2017
MARGIN_M = 60_000  # matches the crop the capture scripts used

# which sectors each golden domain needs
TARGETS = {
    HERE / "stationary_combustion": ("res", "com", "ind", "elec"),
    HERE / "ng_distribution": ("res", "com"),
}


def main() -> None:
    aces = load_aces(str(DATA / "ACES V2.0"), Path("."), YEAR)

    for out_dir, sectors in TARGETS.items():
        domain = gpd.read_file(out_dir / "domain_ct_ri.geojson")
        bounds = domain.to_crs(aces["res"].rio.crs).total_bounds
        window = (
            bounds[0] - MARGIN_M, bounds[1] - MARGIN_M,
            bounds[2] + MARGIN_M, bounds[3] + MARGIN_M,
        )
        for sector in sectors:
            cropped = geo.crop_snap_out(aces[sector], window)
            path = out_dir / f"aces_{sector}_ctri.tif"
            cropped.rio.to_raster(path, dtype="float64")
            total = float(cropped.sum())
            print(f"{path.relative_to(HERE)}  {cropped.shape}  sum={total:,.0f}")
            if total == 0:
                raise SystemExit(f"{path} is all zeros -- the CO2 proxy would be inert")


if __name__ == "__main__":
    main()
