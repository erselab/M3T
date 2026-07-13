"""Geospatial shim: a thin, terra-parity layer over rioxarray / rasterio / geopandas.

This is the single most important module in the port. The R package makes 500+
calls into ``terra``; nearly all of them are one of ~15 verbs. Centralising them
here lets us pin the conventions that differ between ``terra`` and the
rioxarray/rasterio stack **once**, so every sector inherits identical behaviour:

Conventions pinned here (validated against ``terra`` in ``tests/test_geo.py``):

* **Cell registration.** Rasters are north-up: the transform origin is the
  top-left corner, ``yres`` is stored negative internally but exposed as a
  positive number by :func:`res` (matching ``terra::res``).
* **Grid from extent + resolution** (:func:`make_grid`). Mirrors
  ``terra::rast(x, resolution=r)``: ncol/nrow are ``round(width/xres)`` /
  ``round(height/yres)``, then ``xmax``/``ymin`` are recomputed from
  ``xmin``/``ymax`` so the resolution is preserved exactly. ``xmin`` and ``ymax``
  are the fixed anchors.
* **NoData.** Represented as ``numpy.nan`` in float rasters (M3T uses
  ``FLT8S``/float64 throughout — see ``Config.Terra_datatype``). ``na_rm`` in
  reductions skips NaN, matching ``na.rm=TRUE``.
* **Reprojection resampling** defaults to bilinear for continuous data and must
  be set explicitly to ``"sum"``/``"average"`` where the R code conserves mass
  (aggregation). Callers pass ``resampling=`` — we never silently pick a
  mass-changing method.

Rasters are represented as :class:`xarray.DataArray` with a CRS attached via
rioxarray (``da.rio``). Vectors are :class:`geopandas.GeoDataFrame`.
"""

from __future__ import annotations

from pathlib import Path
from typing import Callable, Literal

import numpy as np
import rioxarray  # noqa: F401  (registers the .rio accessor)
import xarray as xr
from rasterio.enums import Resampling
from rasterio.transform import from_origin

try:  # geopandas is only needed for the vector verbs
    import geopandas as gpd
except Exception:  # pragma: no cover
    gpd = None  # type: ignore

Number = float | int
BBox = tuple[float, float, float, float]  # (xmin, ymin, xmax, ymax)

_RESAMPLING = {
    "nearest": Resampling.nearest,
    "bilinear": Resampling.bilinear,
    "cubic": Resampling.cubic,
    "average": Resampling.average,
    "sum": Resampling.sum,
    "mode": Resampling.mode,
    "max": Resampling.max,
    "min": Resampling.min,
}


# --------------------------------------------------------------------------- #
# Grid / raster construction
# --------------------------------------------------------------------------- #
def make_grid(
    bounds: BBox,
    resolution: Number | tuple[Number, Number],
    crs: str,
    *,
    fill: float = np.nan,
    dtype: str = "float64",
    name: str = "layer",
) -> xr.DataArray:
    """Create an empty raster template. Parity with ``terra::rast(ext, resolution, crs, vals)``.

    Parameters
    ----------
    bounds:
        ``(xmin, ymin, xmax, ymax)`` in ``crs`` units.
    resolution:
        Scalar (equal x/y) or ``(xres, yres)``.
    crs:
        Any pyproj-understood CRS string (e.g. ``"epsg:4326"`` or a PROJ string).
    fill:
        Initial cell value (default NaN, matching ``vals=NA``).

    Notes
    -----
    Matches ``terra::rast(x, resolution=r)`` **when x is a SpatVector or
    SpatRaster** — the rule M3T uses everywhere it builds a template from a
    resolution (``domain_template``, flux rasters, ...). The **resolution is
    preserved exactly** and the extent is re-fitted: ``xmin``/``ymin`` are the
    anchors, ``ncol = round(width/xres)``, ``nrow = round(height/yres)`` (R
    round-half-to-even, matched by Python's ``round``), then ``xmax``/``ymax``
    are recomputed as ``xmin + ncol*xres`` / ``ymin + nrow*yres``.

    Consequences (all verified against terra in ``tests/test_geo_oracle.py`` /
    ``test_domain_oracle.py``):

    * A non-integer width **rounds** the cell count, so the grid need not fully
      cover the input box: width 3.2 at res 1 -> 3 cols (not 4).
    * A non-grid-aligned minimum is kept as-is: xmin -74.3 stays -74.3.

    (This differs from ``rast(SpatExtent, resolution)``, which instead keeps the
    extent and changes the resolution; M3T does not rely on that variant.)
    """
    xmin, ymin, xmax, ymax = bounds
    xmin, ymin, xmax, ymax = float(xmin), float(ymin), float(xmax), float(ymax)
    if isinstance(resolution, (int, float)):
        xres = yres = float(resolution)
    else:
        xres, yres = float(resolution[0]), float(resolution[1])

    ncol = max(1, int(round((xmax - xmin) / xres)))
    nrow = max(1, int(round((ymax - ymin) / yres)))
    # anchor the minimums; resolution preserved, extent re-fitted
    xmax = xmin + ncol * xres
    ymax = ymin + nrow * yres

    x = xmin + (np.arange(ncol) + 0.5) * xres
    y = ymax - (np.arange(nrow) + 0.5) * yres  # descending (north-up)

    data = np.full((nrow, ncol), fill, dtype=dtype)
    da = xr.DataArray(data, coords={"y": y, "x": x}, dims=("y", "x"), name=name)
    da.rio.write_crs(crs, inplace=True)
    da.rio.write_transform(from_origin(xmin, ymax, xres, yres), inplace=True)
    return da


def grid_like(template: xr.DataArray, *, fill: float = np.nan, name: str = "layer") -> xr.DataArray:
    """Empty raster matching ``template``'s grid/CRS. Parity with ``terra::rast(x)``."""
    out = xr.full_like(template.astype("float64"), fill)
    out.name = name
    out.rio.write_crs(template.rio.crs, inplace=True)
    return out


def read_raster(path: str | Path, *, masked: bool = True) -> xr.DataArray:
    """Read a raster into a DataArray. Parity with ``terra::rast(path)``.

    ``masked=True`` converts the file's nodata to NaN (float), matching how M3T
    treats missing cells.
    """
    da = rioxarray.open_rasterio(path, masked=masked)
    # rioxarray returns (band, y, x); squeeze single-band like terra's 1-layer rast
    if "band" in da.dims and da.sizes["band"] == 1:
        da = da.squeeze("band", drop=True)
    return da


def write_raster(da: xr.DataArray, path: str | Path, **kwargs) -> None:
    """Write a GeoTIFF. Parity with ``terra::writeRaster``."""
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    da.rio.to_raster(path, **kwargs)


def write_cdf(da: xr.DataArray, path: str | Path, *, varname: str = "layer", **kwargs) -> None:
    """Write a NetCDF. Parity with ``terra::writeCDF`` / ``writeCDF_no_newline``."""
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    da.rename(varname).to_netcdf(path, **kwargs)


# --------------------------------------------------------------------------- #
# Grid metadata accessors (parity with terra::res / ext / crs / values)
# --------------------------------------------------------------------------- #
def res(da: xr.DataArray) -> tuple[float, float]:
    """Return ``(xres, yres)`` as positive numbers, like ``terra::res``."""
    t = da.rio.transform()
    return (abs(t.a), abs(t.e))


def ext(da: xr.DataArray) -> BBox:
    """Return ``(xmin, ymin, xmax, ymax)``, like ``terra::ext``."""
    b = da.rio.bounds()
    return (b[0], b[1], b[2], b[3])


def crs(da: xr.DataArray):
    """Return the CRS, like ``terra::crs``."""
    return da.rio.crs


# --------------------------------------------------------------------------- #
# Reductions (parity with terra::global)
# --------------------------------------------------------------------------- #
_GLOBAL_FUNS: dict[str, Callable] = {
    "sum": np.nansum,
    "mean": np.nanmean,
    "min": np.nanmin,
    "max": np.nanmax,
    "notNA": lambda a: np.count_nonzero(~np.isnan(a)),
}


def global_(da: xr.DataArray, fun: str | Callable = "sum", *, na_rm: bool = True) -> float:
    """Whole-raster reduction. Parity with ``terra::global(x, fun, na.rm=)``.

    ``fun`` may be a name in {"sum","mean","min","max","notNA"} or a callable
    taking a 1-D numpy array.
    """
    a = np.asarray(da.values, dtype="float64").ravel()
    if not na_rm:
        a_use = a
        f = {"sum": np.sum, "mean": np.mean, "min": np.min, "max": np.max}.get(fun, None)
        if isinstance(fun, str) and f is not None:
            return float(f(a_use))
    if isinstance(fun, str):
        try:
            return float(_GLOBAL_FUNS[fun](a))
        except KeyError as exc:
            raise ValueError(f"Unknown global fun {fun!r}") from exc
    return float(fun(a[~np.isnan(a)] if na_rm else a))


# --------------------------------------------------------------------------- #
# Warping (parity with terra::project / crop / mask / extend / aggregate)
# --------------------------------------------------------------------------- #
_AVERAGE_SUPERSAMPLE = 5


def project_to_grid(
    da: xr.DataArray,
    template: xr.DataArray,
    *,
    resampling: Literal["nearest", "bilinear", "cubic", "average", "sum", "mode", "max", "min"]
    = "bilinear",
) -> xr.DataArray:
    """Reproject/resample ``da`` onto ``template``'s grid+CRS. Parity with ``terra::project(x, y)``.

    ``resampling`` is required-by-convention to be chosen deliberately: use
    ``"sum"`` when conserving a mass total across a resolution change, ``"average"``
    for densities/fluxes, ``"bilinear"`` for smooth continuous fields.

    ``"average"`` does **not** map to GDAL's ``average``. GDAL takes an unweighted
    mean of the source pixels whose *centres* fall in the target cell, whereas
    ``terra``'s average is effectively area-weighted. Coarsening 1 km -> ~8.5 km
    that difference is large: a median per-cell error of 1.3e-3 and 60% of cells
    past our 1e-4 tolerance (the totals still agree, so it hides in mass-balance
    checks). Supersampling the source first makes each source pixel contribute in
    proportion to its overlap, which recovers the area weighting: at 5x the result
    matches terra to 1.5e-8, and 5x vs 10x are identical, so it is converged
    rather than tuned. Costs one extra in-memory resample of the source.
    """
    if resampling == "average":
        k = _AVERAGE_SUPERSAMPLE
        xres, yres = res(da)
        da = da.rio.reproject(
            da.rio.crs,
            resolution=(xres / k, yres / k),
            resampling=Resampling.nearest,
        )
    return da.rio.reproject_match(template, resampling=_RESAMPLING[resampling])


def project_to_crs(
    da: xr.DataArray,
    dst_crs: str,
    *,
    resampling: str = "bilinear",
) -> xr.DataArray:
    """Reproject to a CRS (grid chosen by rioxarray). Parity with ``terra::project(x, crs)``."""
    return da.rio.reproject(dst_crs, resampling=_RESAMPLING[resampling])


def crop(da: xr.DataArray, bounds: BBox) -> xr.DataArray:
    """Crop to a bounding box. Parity with ``terra::crop``."""
    return da.rio.clip_box(*bounds)


def mask(da: xr.DataArray, geometries, *, crs_=None, invert: bool = False) -> xr.DataArray:
    """Mask cells outside ``geometries`` to NaN. Parity with ``terra::mask``."""
    crs_ = crs_ if crs_ is not None else da.rio.crs
    return da.rio.clip(geometries, crs=crs_, drop=False, invert=invert)


def aggregate(da: xr.DataArray, factor: int, *, fun: str = "sum") -> xr.DataArray:
    """Aggregate by an integer factor. Parity with ``terra::aggregate(x, fact, fun)``.

    Uses xarray ``coarsen``; ``boundary="trim"`` matches terra dropping partial
    edge blocks.
    """
    reducer = {"sum": "sum", "mean": "mean", "min": "min", "max": "max"}[fun]
    coarse = da.coarsen(x=factor, y=factor, boundary="trim")
    out = getattr(coarse, reducer)()
    # coarsen keeps the parent's cached transform; recompute it from the new
    # (block-centre) coordinates so res()/ext() reflect the aggregated grid.
    xres, yres = res(da)
    new_x = out["x"].values
    new_y = out["y"].values
    xmin = float(new_x[0]) - factor * xres / 2.0
    ymax = float(new_y[0]) + factor * yres / 2.0
    out.rio.write_crs(da.rio.crs, inplace=True)
    out.rio.write_transform(from_origin(xmin, ymax, factor * xres, factor * yres), inplace=True)
    return out


# --------------------------------------------------------------------------- #
# Raster <-> vector (parity with terra::rasterize / extract / cellSize)
# --------------------------------------------------------------------------- #
def cell_area(template: xr.DataArray, *, unit: Literal["m", "km"] = "m") -> xr.DataArray:
    """Per-cell **geodesic** area. Parity with ``terra::cellSize(x, unit=)``.

    ``terra::cellSize`` defaults to ``transform=TRUE``: even for a projected CRS
    it computes each cell's area on the WGS84 ellipsoid by transforming the
    cell's four corners to lon/lat and taking the geodesic polygon area (verified
    against terra: a 500 m UTM-18N cell is 248658.818 m², not the planar
    250000 m²). We reproduce that exactly with ``pyproj``.

    Optimisation: for a geographic CRS every cell in a latitude row has the same
    area, so we compute one cell per row and broadcast. For a projected CRS the
    area varies per cell and we compute all of them (a numpy-vectorised geodesic
    quadrilateral; still one Python call, not a per-cell loop).
    """
    from pyproj import CRS as _CRS
    from pyproj import Geod, Transformer

    if template.rio.crs is None:
        raise ValueError("template has no CRS")

    src = _CRS.from_user_input(template.rio.crs)
    # Match terra by computing geodesic areas on the CRS's own ellipsoid
    # (falling back to WGS84 when it is underspecified).
    ell = src.ellipsoid
    if ell is not None and ell.inverse_flattening:
        geod = Geod(a=ell.semi_major_metre, rf=ell.inverse_flattening)
    else:
        geod = Geod(ellps="WGS84")

    nrow, ncol = template.shape
    xres, yres = res(template)
    xmin, ymin, xmax, ymax = ext(template)
    scale = 1.0 if unit == "m" else 1e-6

    to_ll = Transformer.from_crs(src, _CRS.from_epsg(4326), always_xy=True)

    def _quad_area(x0: float, y0: float) -> float:
        # corners of a cell with lower-left (x0, y0), counter-clockwise
        xs = [x0, x0 + xres, x0 + xres, x0]
        ys = [y0, y0, y0 + yres, y0 + yres]
        lons, lats = to_ll.transform(xs, ys)
        a, _ = geod.polygon_area_perimeter(lons, lats)
        return abs(a) * scale

    area = np.empty((nrow, ncol), dtype="float64")
    if src.is_geographic:
        # area depends only on the row (latitude band)
        for r in range(nrow):
            y0 = ymax - (r + 1) * yres  # lower edge of row r (rows go north->south)
            area[r, :] = _quad_area(xmin, y0)
    else:
        for r in range(nrow):
            y0 = ymax - (r + 1) * yres
            for c in range(ncol):
                x0 = xmin + c * xres
                area[r, c] = _quad_area(x0, y0)

    out = xr.DataArray(area, coords=template.coords, dims=template.dims, name="area")
    out.rio.write_crs(template.rio.crs, inplace=True)
    return out


def rasterize_points_sum(
    xs, ys, values, template: xr.DataArray, *, background: float = np.nan
) -> xr.DataArray:
    """Sum point ``values`` into the cells of ``template``. Parity with
    ``terra::rasterize(points, template, field, fun=sum)``.

    ``xs``/``ys`` are point coordinates already in the template's CRS. Points
    outside the grid are dropped (this is the implicit crop terra does). Cells
    with no points get ``background`` (default NaN, matching terra before the
    caller's ``[is.na]<-0``).

    Cell assignment matches terra: ``col = floor((x - xmin)/xres)``,
    ``row = floor((ymax - y)/yres)``.
    """
    xs = np.asarray(xs, dtype="float64")
    ys = np.asarray(ys, dtype="float64")
    values = np.asarray(values, dtype="float64")

    nrow, ncol = template.shape
    xmin, ymin, xmax, ymax = ext(template)
    xres, yres = res(template)

    col = np.floor((xs - xmin) / xres).astype("int64")
    row = np.floor((ymax - ys) / yres).astype("int64")
    inside = (col >= 0) & (col < ncol) & (row >= 0) & (row < nrow) & np.isfinite(values)

    acc = np.zeros((nrow, ncol), dtype="float64")
    counts = np.zeros((nrow, ncol), dtype="int64")
    np.add.at(acc, (row[inside], col[inside]), values[inside])
    np.add.at(counts, (row[inside], col[inside]), 1)

    out_vals = np.where(counts > 0, acc, background)
    out = xr.DataArray(out_vals, coords=template.coords, dims=template.dims, name="layer")
    out.rio.write_crs(template.rio.crs, inplace=True)
    return out


def crop_snap_out(da: xr.DataArray, bounds: BBox) -> xr.DataArray:
    """Crop to ``bounds``, expanding outward to whole cells.

    Parity with ``terra::crop(x, y, snap="out")``: any cell the bounds touch even
    partially is kept, so the result's extent is >= ``bounds`` and stays aligned
    to the source grid. (Plain :func:`crop` / ``rio.clip_box`` snaps differently,
    which shifts the grid — the septic path depends on staying aligned, because a
    finer sub-grid is later built from this extent and aggregated back.)
    """
    xmin, ymin, xmax, ymax = bounds
    Xmin, Ymin, Xmax, Ymax = ext(da)
    xres, yres = res(da)
    nrow, ncol = da.shape

    c0 = int(np.floor((xmin - Xmin) / xres))
    c1 = int(np.ceil((xmax - Xmin) / xres))
    r0 = int(np.floor((Ymax - ymax) / yres))
    r1 = int(np.ceil((Ymax - ymin) / yres))

    c0, c1 = max(c0, 0), min(c1, ncol)
    r0, r1 = max(r0, 0), min(r1, nrow)
    if c0 >= c1 or r0 >= r1:
        raise ValueError("crop bounds do not overlap the raster")

    out = da.isel(y=slice(r0, r1), x=slice(c0, c1))
    out.rio.write_crs(da.rio.crs, inplace=True)
    out.rio.write_transform(
        from_origin(Xmin + c0 * xres, Ymax - r0 * yres, xres, yres), inplace=True
    )
    return out


def mask_geometries(
    da: xr.DataArray,
    geometries,
    *,
    touches: bool = False,
    updatevalue: float = np.nan,
) -> xr.DataArray:
    """Set cells outside ``geometries`` to ``updatevalue``.

    Parity with ``terra::mask(x, y, touches=, updatevalue=)``. ``touches=True``
    keeps every cell the polygon touches (rasterio ``all_touched``); the default
    keeps cells whose centre falls inside.
    """
    from rasterio.features import geometry_mask

    geoms = getattr(geometries, "geometry", geometries)
    inside = geometry_mask(
        list(geoms),
        out_shape=da.shape,
        transform=da.rio.transform(),
        all_touched=touches,
        invert=True,  # True == inside the polygons
    )
    out = da.where(xr.DataArray(inside, coords=da.coords, dims=da.dims), updatevalue)
    out.rio.write_crs(da.rio.crs, inplace=True)
    return out


def extend(da: xr.DataArray, pad: tuple[float, float], *, fill: float = np.nan) -> xr.DataArray:
    """Grow the extent by ``pad`` (x, y map units) on every side, filling with ``fill``.

    Parity with ``terra::extend(x, ext(x) + pad, fill=)``. The requested extent
    rarely lands on a cell boundary, and terra snaps it to the *nearest* whole
    cell rather than rounding outward — verified against terra, which pads 42
    cells (not 43) for a 42.29-cell request. Rounding out instead shifts the grid
    by one cell and silently corrupts everything downstream of the reprojection.
    """
    xres, yres = res(da)
    xmin, ymin, xmax, ymax = ext(da)
    nx = int(round(pad[0] / xres))
    ny = int(round(pad[1] / yres))
    if nx <= 0 and ny <= 0:
        return da

    vals = np.pad(
        da.values.astype("float64"),
        ((ny, ny), (nx, nx)),
        mode="constant",
        constant_values=fill,
    )
    new_xmin, new_ymax = xmin - nx * xres, ymax + ny * yres
    xs = new_xmin + (np.arange(vals.shape[1]) + 0.5) * xres
    ys = new_ymax - (np.arange(vals.shape[0]) + 0.5) * yres

    out = xr.DataArray(vals, coords={"y": ys, "x": xs}, dims=("y", "x"), name=da.name)
    out.rio.write_crs(da.rio.crs, inplace=True)
    out.rio.write_transform(from_origin(new_xmin, new_ymax, xres, yres), inplace=True)
    return out


_COVERAGE_SUBCELLS = 10


def coverage_fraction(template: xr.DataArray, geometries) -> xr.DataArray:
    """Fraction of each cell covered by ``geometries`` (0..1).

    Parity with the weights from ``terra::extract(x, y, weights=TRUE)``, which the
    R uses to down-weight cells straddling the domain boundary before
    area-averaging onto a coarser grid.

    Two terra behaviours are reproduced here, both verified against terra directly:

    1. ``terra`` distinguishes ``weights=TRUE`` (approximate) from ``exact=TRUE``
       (exact area). ``weights`` splits each cell into a 10x10 grid of sub-cells
       and counts how many sub-cell *centres* fall inside the polygon, so its
       output is quantised to hundredths (terra returns exactly 100 distinct
       values). We sub-sample the same way rather than computing the exact
       intersection area: the exact area is a *better* number, but it is not the
       number the R pipeline uses, and parity beats accuracy here.

    2. **Weights are per polygon, and the last one wins.** ``extract`` returns one
       row per (polygon, cell), so a cell straddling two polygons comes back twice
       with partial weights that sum to ~1. The R then does
       ``x[cover$cell] <- x[cover$cell] * cover$weight`` — with duplicate indices,
       R's assignment keeps only the *last* write, so a cell on an interior state
       border is scaled by just one state's partial weight (e.g. 0.18x) instead of
       1x. That loses emissions on interior borders and is arguably an R bug, but
       reproducing it is the whole point of a golden port. Do not "fix" it here.

    Cells no polygon covers come back as NaN (not 0), because the R leaves such
    cells *unmultiplied* rather than zeroing them; callers must preserve that.
    """
    from rasterio.features import rasterize as _rio_rasterize

    n = _COVERAGE_SUBCELLS
    geoms = getattr(geometries, "geometry", geometries)
    if getattr(geometries, "crs", None) is not None and template.rio.crs is not None:
        if geometries.crs != template.rio.crs:
            geoms = geometries.to_crs(template.rio.crs).geometry

    nrow, ncol = template.shape
    xmin, _, _, ymax = ext(template)
    xres, yres = res(template)
    fine_transform = from_origin(xmin, ymax, xres / n, yres / n)

    weight = np.full((nrow, ncol), np.nan, dtype="float64")
    for geom in geoms:  # in order: later polygons overwrite earlier ones
        fine = _rio_rasterize(
            shapes=[(geom, 1)],
            out_shape=(nrow * n, ncol * n),
            transform=fine_transform,
            fill=0,
            all_touched=False,  # sub-cell *centre* must be inside, as terra does
            dtype="uint8",
        )
        frac = fine.reshape(nrow, n, ncol, n).sum(axis=(1, 3)) / float(n * n)
        covered = frac > 0  # terra only returns cells with a non-zero weight
        weight[covered] = frac[covered]

    out = xr.DataArray(weight, coords=template.coords, dims=template.dims, name="weight")
    out.rio.write_crs(template.rio.crs, inplace=True)
    return out


def _grid_cell_polygons(template: xr.DataArray):
    """GeoDataFrame of one box polygon per grid cell, with ``row``/``col`` labels."""
    import geopandas as gpd
    from shapely.geometry import box

    nrow, ncol = template.shape
    xmin, ymin, xmax, ymax = ext(template)
    xres, yres = res(template)
    polys, rows, cols = [], [], []
    for r in range(nrow):
        cell_ymax = ymax - r * yres
        cell_ymin = cell_ymax - yres
        for c in range(ncol):
            cell_xmin = xmin + c * xres
            polys.append(box(cell_xmin, cell_ymin, cell_xmin + xres, cell_ymax))
            rows.append(r)
            cols.append(c)
    return gpd.GeoDataFrame({"row": rows, "col": cols}, geometry=polys, crs=template.rio.crs)


def rasterize_line_length(
    lines_gdf, template: xr.DataArray, *, unit: Literal["m", "km"] = "m"
) -> xr.DataArray:
    """Per-cell line length. Parity with ``terra::rasterizeGeom(x, fun="length", unit=)``.

    For a geographic CRS the length is **geodesic** (metres on the ellipsoid, via
    ``pyproj``), matching terra's default; for a projected CRS it is planar. Lines
    are clipped to each cell (planar clip in the template CRS) and their lengths
    summed per cell. Empty cells are 0.
    """
    import geopandas as gpd
    from pyproj import CRS as _CRS
    from pyproj import Geod

    if lines_gdf.crs is not None and template.rio.crs is not None and lines_gdf.crs != template.rio.crs:
        lines_gdf = lines_gdf.to_crs(template.rio.crs)

    cells = _grid_cell_polygons(template)
    pieces = gpd.overlay(
        lines_gdf[["geometry"]], cells, how="intersection", keep_geom_type=True
    )

    scale = 1.0 if unit == "m" else 1e-3
    src = _CRS.from_user_input(template.rio.crs)
    if src.is_geographic:
        ell = src.ellipsoid
        geod = (
            Geod(a=ell.semi_major_metre, rf=ell.inverse_flattening)
            if ell is not None and ell.inverse_flattening
            else Geod(ellps="WGS84")
        )
        lengths = pieces.geometry.apply(lambda g: abs(geod.geometry_length(g)) * scale)
    else:
        lengths = pieces.geometry.length * scale

    arr = np.zeros(template.shape, dtype="float64")
    agg = lengths.groupby([pieces["row"], pieces["col"]]).sum()
    for (r, c), v in agg.items():
        arr[int(r), int(c)] = v

    out = xr.DataArray(arr, coords=template.coords, dims=template.dims, name="length")
    out.rio.write_crs(template.rio.crs, inplace=True)
    return out


def rasterize(
    gdf,
    template: xr.DataArray,
    *,
    field: str | None = None,
    background: float = np.nan,
    touches: bool = False,
):
    """Burn vector geometries onto ``template``'s grid. Parity with ``terra::rasterize``.

    ``field`` selects the attribute to burn (``None`` -> 1 where covered).
    ``touches`` maps to ``terra::rasterize(touches=)`` / rasterio ``all_touched``:
    burn every cell the geometry touches, not just those whose centre it covers.
    """
    from rasterio.features import rasterize as _rio_rasterize

    if gdf.crs is not None and template.rio.crs is not None and gdf.crs != template.rio.crs:
        gdf = gdf.to_crs(template.rio.crs)
    if field is None:
        shapes = ((geom, 1) for geom in gdf.geometry)
    else:
        shapes = ((geom, val) for geom, val in zip(gdf.geometry, gdf[field]))
    arr = _rio_rasterize(
        shapes=shapes,
        out_shape=template.shape,
        transform=template.rio.transform(),
        fill=background,
        all_touched=touches,
        dtype="float64",
    )
    out = xr.DataArray(arr, coords=template.coords, dims=template.dims, name=field or "layer")
    out.rio.write_crs(template.rio.crs, inplace=True)
    return out
