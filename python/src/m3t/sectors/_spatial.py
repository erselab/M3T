"""Shared point-geometry helpers for sectors (project to CRS, clip to domain)."""

from __future__ import annotations

from typing import Any

import pandas as pd


def project_points(
    df: pd.DataFrame, lon_col: str, lat_col: str, dst_crs: str, *, keep: list[str]
) -> pd.DataFrame:
    """Return a DataFrame with ``x``/``y`` in ``dst_crs`` from lon/lat (EPSG:4326).

    Rows with missing coordinates are dropped (matching terra dropping NA geoms).
    """
    import geopandas as gpd

    valid = df[df[lon_col].notna() & df[lat_col].notna()].copy()
    gdf = gpd.GeoDataFrame(
        valid[keep].copy(),
        geometry=gpd.points_from_xy(valid[lon_col], valid[lat_col]),
        crs="epsg:4326",
    )
    if str(dst_crs) != "epsg:4326":
        gdf = gdf.to_crs(dst_crs)
    gdf["x"] = gdf.geometry.x
    gdf["y"] = gdf.geometry.y
    return pd.DataFrame(gdf.drop(columns="geometry"))


def clip_points_to_domain(points: pd.DataFrame, domain: Any, domain_crs: str) -> pd.DataFrame:
    """Keep points inside the domain (a bbox tuple or a polygon GeoDataFrame)."""
    if isinstance(domain, tuple) and len(domain) == 4:
        xmin, ymin, xmax, ymax = domain
        m = (
            (points["x"] >= xmin)
            & (points["x"] <= xmax)
            & (points["y"] >= ymin)
            & (points["y"] <= ymax)
        )
        return points[m]
    import geopandas as gpd

    gdf = gpd.GeoDataFrame(
        points.copy(), geometry=gpd.points_from_xy(points["x"], points["y"]), crs=domain_crs
    )
    dom = domain if str(getattr(domain, "crs", domain_crs)) == domain_crs else domain.to_crs(domain_crs)
    joined = gpd.sjoin(gdf, dom[["geometry"]], predicate="within", how="inner")
    return pd.DataFrame(joined.drop(columns=["geometry", "index_right"]))
