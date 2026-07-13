"""Build the target domain grid.

Port of the domain-construction block in ``R/CH4_inventory_build.R`` (the
``isa(domain, ...)`` dispatch around lines 604-674) plus
``R/Define_custom_domain.R``.

The R code accepts several ``domain`` forms and, whatever the form, ends at:

    domain_template <- terra::rast(domain, resolution=domain_res, crs=domain_crs, vals=NA)

i.e. a raster template covering the domain's extent. For box/CONUS/file/vector
domains that is exactly :func:`m3t.geo.make_grid` on the domain's bounding box
(extent preserved, resolution adjusted — see the terra parity notes in
``geo.make_grid``).

Domain forms handled here:

* **bounding box** — a 2x2 array/DataFrame of corner coords ``[[xmin,ymin],
  [xmax,ymax]]`` (R's ``data.frame`` branch), or a 4-tuple ``(xmin,ymin,xmax,
  ymax)``.
* **"CONUS"** — the fixed box ``(-130, 20, -60, 55)``.
* **raster file** — read, take its CRS/res (if res not given) and extent.
* **vector file / GeoDataFrame** — take its (optionally dissolved) extent.
* **state/urban selection** — a GeoDataFrame of Census Tigerlines must be
  supplied via ``tigerlines=`` (loaded by the orchestrator in Phase 2); we
  select by ``STUSPS`` / ``NAME`` / ``STATEFP`` / ``UACE`` / urban-area name.

The interactive ``"custom"`` draw path is deferred (it needs a GUI); callers
should pass an explicit box or vector instead.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Sequence

import numpy as np
import xarray as xr

from . import geo

# Fixed CONUS bounding box used by the R "CONUS" branch (includes offshore).
CONUS_BOUNDS = (-130.0, 20.0, -60.0, 55.0)

BBox = tuple[float, float, float, float]  # (xmin, ymin, xmax, ymax)


def _normalize_res(domain_res: float | Sequence[float] | None) -> tuple[float, float] | None:
    """Mirror R: scalar -> (r, r); length-2 kept; None passed through."""
    if domain_res is None:
        return None
    if isinstance(domain_res, (int, float)):
        return (float(domain_res), float(domain_res))
    r = tuple(float(v) for v in domain_res)
    if len(r) == 1:
        return (r[0], r[0])
    if len(r) == 2:
        return (r[0], r[1])
    raise ValueError("domain_res must be length 1 or 2")


def _bounds_from_corners(corners: Any) -> BBox:
    """From a 2x2 array/DataFrame of corner coords (cols x, y) -> bbox.

    Matches R's ``data.frame`` branch which uses ``min``/``max`` of each column,
    so row order does not matter.
    """
    arr = np.asarray(corners, dtype="float64")
    if arr.shape != (2, 2):
        raise ValueError("corner array must be 2x2 (rows: two corners; cols: x, y)")
    xmin, xmax = float(arr[:, 0].min()), float(arr[:, 0].max())
    ymin, ymax = float(arr[:, 1].min()), float(arr[:, 1].max())
    return (xmin, ymin, xmax, ymax)


def _select_tigerlines(tigerlines, domain: Sequence[str]):
    """Select state/urban geometries from a Tigerlines GeoDataFrame.

    Mirrors the R selection order: STUSPS -> NAME -> urban name -> STATEFP ->
    UACE. All entries in ``domain`` must be the same type; selection is by
    membership so multiple states (e.g. ``["CT","RI"]``) work.
    """
    domain = list(domain)
    first = domain[0]
    is_numeric_like = str(first).isdigit()

    def _try(col: str):
        if col in tigerlines.columns:
            sub = tigerlines[tigerlines[col].isin(domain)]
            return sub if len(sub) else None
        return None

    if not is_numeric_like:
        for col in ("STUSPS", "NAME"):
            sub = _try(col)
            if sub is not None:
                return sub
        # urban-area name column varies by vintage; try common names
        for col in ("NAME10", "NAMELSAD", "UANAME"):
            sub = _try(col)
            if sub is not None:
                return sub
    else:
        for col in ("STATEFP", "UACE", "UACE10", "GEOID"):
            sub = _try(col)
            if sub is not None:
                return sub
    raise ValueError(f"domain {domain!r} did not match any Tigerlines selection column")


def build_domain(
    domain: Any,
    domain_res: float | Sequence[float] | None,
    domain_crs: str = "epsg:4326",
    *,
    tigerlines=None,
) -> tuple[xr.DataArray, Any]:
    """Build ``(domain_template, domain_geometry)``.

    ``domain_template`` is an all-NaN raster (``geo.make_grid``) covering the
    domain extent at ``domain_res`` in ``domain_crs`` — the parity target for the
    R ``domain_template``. ``domain_geometry`` is the bbox tuple or the selected
    GeoDataFrame (used later for masking).

    Parameters
    ----------
    domain:
        A bbox 4-tuple, a 2x2 corner array/DataFrame, ``"CONUS"``, a path to a
        raster/vector file, a GeoDataFrame, or a state/urban selector (str or
        list of str) — the last requires ``tigerlines``.
    domain_res:
        Scalar or ``(xres, yres)``. May be ``None`` only when ``domain`` is a
        raster file (then its native resolution is used), matching R.
    domain_crs:
        Target CRS. Ignored/overridden when ``domain`` is a raster file (its CRS
        wins, as in R).
    """
    res = _normalize_res(domain_res)

    # --- explicit bbox tuple -------------------------------------------- #
    if isinstance(domain, tuple) and len(domain) == 4 and all(
        isinstance(v, (int, float)) for v in domain
    ):
        if res is None:
            raise ValueError("domain_res is required for a bounding-box domain")
        bounds = (float(domain[0]), float(domain[1]), float(domain[2]), float(domain[3]))
        return geo.make_grid(bounds, res, domain_crs), bounds

    # --- 2x2 corner array / DataFrame ----------------------------------- #
    if hasattr(domain, "shape") or (
        isinstance(domain, (list, tuple)) and len(domain) == 2 and hasattr(domain[0], "__len__")
    ):
        try:
            bounds = _bounds_from_corners(domain)
        except ValueError:
            bounds = None
        if bounds is not None:
            if res is None:
                raise ValueError("domain_res is required for a corner-box domain")
            return geo.make_grid(bounds, res, domain_crs), bounds

    # --- string forms --------------------------------------------------- #
    if isinstance(domain, str):
        if domain == "CONUS":
            if res is None:
                raise ValueError("domain_res is required for CONUS")
            return geo.make_grid(CONUS_BOUNDS, res, domain_crs), CONUS_BOUNDS
        if domain.lower() == "custom":
            raise NotImplementedError(
                "interactive 'custom' domain drawing is not ported; pass an "
                "explicit bounding box or a vector file/GeoDataFrame instead"
            )
        path = Path(domain)
        if path.exists():
            return _build_from_file(path, res, domain_crs)
        # otherwise treat as a state/urban selector
        return _build_from_tigerlines([domain], res, domain_crs, tigerlines)

    # --- list/tuple of selector strings --------------------------------- #
    if isinstance(domain, (list, tuple)) and all(isinstance(d, str) for d in domain):
        return _build_from_tigerlines(list(domain), res, domain_crs, tigerlines)

    # --- GeoDataFrame --------------------------------------------------- #
    if _is_geodataframe(domain):
        return _build_from_vector(domain, res, domain_crs)

    raise TypeError(f"Unsupported domain type: {type(domain)!r}")


def _is_geodataframe(obj: Any) -> bool:
    try:
        import geopandas as gpd

        return isinstance(obj, gpd.GeoDataFrame)
    except Exception:
        return False


def _build_from_file(path: Path, res, domain_crs: str):
    """Domain from a raster or vector file. Mirrors R's file branch."""
    try:
        da = geo.read_raster(path)
        # raster: CRS and (if unset) resolution come from the file
        file_crs = str(da.rio.crs)
        file_res = geo.res(da) if res is None else res
        bounds = geo.ext(da)
        return geo.make_grid(bounds, file_res, file_crs), bounds
    except Exception:
        import geopandas as gpd

        gdf = gpd.read_file(path)
        return _build_from_vector(gdf, res, domain_crs)


def _build_from_vector(gdf, res, domain_crs: str):
    if res is None:
        raise ValueError("domain_res is required for a vector domain")
    if gdf.crs is not None and str(gdf.crs) != domain_crs:
        gdf = gdf.to_crs(domain_crs)
    xmin, ymin, xmax, ymax = gdf.total_bounds
    bounds = (float(xmin), float(ymin), float(xmax), float(ymax))
    return geo.make_grid(bounds, res, domain_crs), gdf


# Territories the R drops before any sector sees the state list.
_EXCLUDED_STATES = ("AK", "AS", "PR", "HI", "MP", "GU", "VI")


def build_state_tigerlines(tigerlines, domain_geom, domain_crs: str):
    """Return ``(state_tigerlines, state_name_list)`` for a run.

    Port of the block in ``CH4_inventory_build.R`` that finalises
    ``State_Tigerlines``: drop the non-CONUS territories, clip the states to the
    domain when the domain does not already contain them (a bbox domain cuts
    states in half; a state-selector domain is already exact, so the clip is a
    no-op), then sort by ``STUSPS`` — the order every downstream sector assumes
    when it lines a state table up against the geometries.
    """
    import geopandas as gpd

    states = tigerlines
    if "STUSPS" in states.columns:
        states = states[~states["STUSPS"].isin(_EXCLUDED_STATES)]
    if states.crs is not None and str(states.crs) != domain_crs:
        states = states.to_crs(domain_crs)

    if _is_geodataframe(domain_geom):
        dom = domain_geom if str(domain_geom.crs) == domain_crs else domain_geom.to_crs(domain_crs)
        within = states.geometry.within(dom.geometry.union_all())
        if not bool(within.all()):
            states = gpd.clip(states, dom)
    else:  # bbox tuple
        xmin, ymin, xmax, ymax = domain_geom
        states = gpd.clip(states, gpd.GeoSeries.from_wkt(
            [f"POLYGON(({xmin} {ymin},{xmax} {ymin},{xmax} {ymax},{xmin} {ymax},{xmin} {ymin}))"],
            crs=domain_crs,
        ).union_all())

    # Clipping leaves the *neighbours* behind as zero-area artifacts: a state that
    # merely shares a border with the domain intersects it in a line, which is not
    # empty. Dropping only empties would have let MA and NY into a CT+RI run, and
    # with them their counties (89 instead of 13) and their SEDS/NEI rows. Require
    # real area.
    states = states[states.geometry.area > 0].sort_values("STUSPS")
    return states, list(states["STUSPS"])


def _build_from_tigerlines(selectors, res, domain_crs, tigerlines):
    if tigerlines is None:
        raise ValueError(
            f"domain {selectors!r} is a state/urban selector but no `tigerlines` "
            "GeoDataFrame was supplied (loaded by the orchestrator in Phase 2)"
        )
    sub = _select_tigerlines(tigerlines, selectors)
    return _build_from_vector(sub, res, domain_crs)
