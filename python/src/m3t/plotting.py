"""Sector map visuals. Port of ``R/Plotting_individual_sectors.R``.

Two entry points, mirroring the R: :func:`log_plot` (log10 colour scale — right
for point sources, whose emissions span orders of magnitude) and
:func:`not_log_plot` (linear). Both render the same map: methane flux on a
viridis ramp, cells with no data in black, county outlines in grey and state
outlines in white on top.

Colour: terra's default raster palette *is* viridis, so matplotlib's ``viridis``
reproduces the R exactly — and it is also the right choice on its own merits
(flux is a magnitude, so it wants a single-hue perceptually-uniform sequential
ramp, not a rainbow).

Faithful to R, including the awkward parts:

* the data is **clipped** to ``zlim_min``/``zlim_max``, not merely the colour bar —
  values outside the range are pulled to the bound (that is what "saturated" in
  the R's titles means);
* the colour range is nudged outwards by a factor of 1.00001 so the extreme cells
  are not rendered as out-of-range;
* an output whose name contains ``Summed`` goes to a ``Summed_Sectors``
  subfolder;
* a raster that is entirely NA/zero is still plotted (as a flat zero map) rather
  than skipped;
* ``not_log_plot`` sets zero cells to NA so they take the "no data" colour.

One R bug is **not** reproduced — see :func:`not_log_plot`.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import xarray as xr

from . import geo

# Cells with no data. R uses colNA="black"; on a viridis ramp (whose low end is a
# very dark purple) black reads as "outside the domain" rather than "near zero".
NA_COLOUR = "black"
CMAP = "viridis"

COUNTY_COLOUR = "dimgrey"
STATE_COLOUR = "white"

_DPI = 100
_FIG_HEIGHT = 7.0  # inches; width is derived from the domain's shape
_MAX_ASPECT = 4.0  # don't let a very wide/tall domain produce a silly figure


def _map_aspect(da: xr.DataArray) -> float:
    """Vertical exaggeration for :meth:`Axes.set_aspect`.

    On a lon/lat grid a degree of longitude is shorter than a degree of latitude
    by ``cos(latitude)``, so plotting raw degrees squashes the map east-west.
    Correcting by ``1/cos(mean_lat)`` is what terra does, and it keeps the coastline
    the shape people expect. A projected CRS is already in linear units, so 1.
    """
    if da.rio.crs is not None and not da.rio.crs.is_geographic:
        return 1.0
    _, ymin, _, ymax = geo.ext(da)
    mean_lat = np.deg2rad((ymin + ymax) / 2.0)
    return float(1.0 / max(np.cos(mean_lat), 0.1))


def _figsize(da: xr.DataArray, aspect: float) -> tuple[float, float]:
    """Shape the figure like the domain, so the map fills it instead of letterboxing."""
    xmin, ymin, xmax, ymax = geo.ext(da)
    width_units = (xmax - xmin) or 1.0
    height_units = ((ymax - ymin) or 1.0) * aspect
    ratio = float(np.clip(width_units / height_units, 1 / _MAX_ASPECT, _MAX_ASPECT))
    # leave room for the colour bar and labels
    return (_FIG_HEIGHT * ratio + 2.5, _FIG_HEIGHT)


def prep_plot_data(da: xr.DataArray) -> xr.DataArray:
    """log10 the raster, turning the -inf produced by zeros into NaN. R: ``prep_plot_data``."""
    with np.errstate(divide="ignore", invalid="ignore"):
        out = xr.apply_ufunc(np.log10, da.where(da > 0))
    return out.where(np.isfinite(out))


def resolve_zlim(
    da: xr.DataArray, zlim_min: float | None, zlim_max: float | None
) -> tuple[float, float]:
    """The colour range R uses: fall back to the data range, then nudge outwards.

    If a supplied ``zlim_min`` is above the data's max (which happens when a sector
    is quieter than the default floor the caller assumed), R discards it and falls
    back to the data minimum rather than rendering an empty scale.
    """
    values = np.asarray(da.values, dtype="float64")
    finite = values[np.isfinite(values)]
    data_min = float(finite.min()) if finite.size else 0.0
    data_max = float(finite.max()) if finite.size else 0.0

    lo = data_min if zlim_min is None else float(zlim_min)
    hi = data_max if zlim_max is None else float(zlim_max)

    if lo > hi:
        lo = data_min

    hi = hi * 1.00001 if hi > 0 else hi * 0.99999
    lo = lo * 0.99999 if lo > 0 else lo * 1.00001
    return lo, hi


def output_path(plot_directory: str | Path, filename: str) -> Path:
    """Where the PNG lands. Anything named ``*Summed*`` goes to ``Summed_Sectors/``."""
    plot_directory = Path(plot_directory)
    if "Summed" in filename:
        return plot_directory / "Summed_Sectors" / f"{filename}.png"
    return plot_directory / f"{filename}.png"


def _is_degenerate(da: xr.DataArray) -> bool:
    """True when every cell is NA or zero — R still plots this, as a flat zero map."""
    v = np.asarray(da.values, dtype="float64")
    return bool(np.all(np.isnan(v) | (v == 0)))


def _render(
    da: xr.DataArray,
    *,
    path: Path,
    title: str,
    bar_label: str,
    vmin: float | None,
    vmax: float | None,
    counties=None,
    states=None,
) -> Path:
    import matplotlib

    matplotlib.use("Agg")  # headless; never pop a window mid-run
    import matplotlib.pyplot as plt
    from matplotlib.lines import Line2D

    cmap = matplotlib.colormaps[CMAP].with_extremes(bad=NA_COLOUR)

    xmin, ymin, xmax, ymax = geo.ext(da)
    path.parent.mkdir(parents=True, exist_ok=True)

    aspect = _map_aspect(da)
    fig, ax = plt.subplots(figsize=_figsize(da, aspect), dpi=_DPI)
    im = ax.imshow(
        np.asarray(da.values, dtype="float64"),
        extent=(xmin, xmax, ymin, ymax),
        origin="upper",  # the grid is north-up
        cmap=cmap,
        vmin=vmin,
        vmax=vmax,
        interpolation="nearest",
        aspect=aspect,
    )

    # boundaries: counties beneath, states on top
    if counties is not None and len(counties):
        counties.boundary.plot(ax=ax, color=COUNTY_COLOUR, linewidth=0.8)
    if states is not None and len(states):
        states.boundary.plot(ax=ax, color=STATE_COLOUR, linewidth=2.0)

    ax.set_xlim(xmin, xmax)
    ax.set_ylim(ymin, ymax)
    ax.set_xlabel("Longitude")
    ax.set_ylabel("Latitude")
    ax.set_title(title)

    bar = fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
    bar.set_label(bar_label)

    handles = []
    if states is not None and len(states):
        handles.append(Line2D([], [], color=STATE_COLOUR, lw=3, label="State"))
    if counties is not None and len(counties):
        handles.append(Line2D([], [], color=COUNTY_COLOUR, lw=3, label="County"))
    if handles:
        legend = ax.legend(handles=handles, loc="upper left", framealpha=1.0)
        legend.get_frame().set_facecolor("black")
        for text in legend.get_texts():
            text.set_color("white")

    fig.tight_layout()
    fig.savefig(path)
    plt.close(fig)
    return path


def render_run_plots(ctx) -> list[Path]:
    """Render a map per sector plus the whole-inventory total. Called when ``verbose``.

    R scatters ``log_plot`` calls through every sector, each with a hand-written
    title and (within a sector) a shared colour range. We render from the rasters
    the run actually wrote, which keeps plotting out of the science code and means
    a newly-added output is plotted for free.

    Flux spans orders of magnitude and is dominated by point sources, so the log
    scale is the default here, as it is for all but one of R's calls.
    """
    if ctx.plot_directory is None:
        return []

    counties = ctx.shared.get("county_tigerlines")
    states = ctx.shared.get("state_tigerlines")
    written: list[Path] = []

    for key in ctx.shared.get("sectors_run", []):
        path = ctx.output_directory / f"{key}.nc"
        if not path.exists():
            continue
        da = _open(path, ctx.domain_template)
        written.append(
            log_plot(
                da,
                title=key.replace("_", " ").capitalize(),
                filename=key,
                plot_directory=ctx.plot_directory,
                counties=counties,
                states=states,
            )
        )

    total = ctx.output_directory / "M3T_total.nc"
    if total.exists():
        da = _open(total, ctx.domain_template)
        written.append(
            log_plot(
                da,
                title="Summed inventory — all sectors",
                # "Summed" routes this to the Summed_Sectors subfolder, as in R
                filename="Summed_final_inventory",
                plot_directory=ctx.plot_directory,
                counties=counties,
                states=states,
            )
        )
    return written


def _open(path: Path, template: xr.DataArray) -> xr.DataArray:
    ds = xr.open_dataset(path, decode_coords="all")
    da = ds[next(iter(ds.data_vars))].astype("float64")
    if da.rio.crs is None:
        da = da.rio.write_crs(template.rio.crs)
    return da


def log_plot(
    da: xr.DataArray,
    title: str,
    *,
    filename: str,
    plot_directory: str | Path,
    counties=None,
    states=None,
    zlim_min: float | None = None,
    zlim_max: float | None = None,
) -> Path:
    """Map the raster on a log10 colour scale. R: ``log_plot``."""
    path = output_path(plot_directory, filename)

    if _is_degenerate(da):
        flat = xr.zeros_like(da.astype("float64"))
        return _render(
            flat, path=path, title=title, bar_label="nmol/m2/s",
            vmin=None, vmax=None, counties=counties, states=states,
        )

    logged = prep_plot_data(da)
    clipped = logged.clip(min=zlim_min, max=zlim_max)
    vmin, vmax = resolve_zlim(clipped, zlim_min, zlim_max)
    return _render(
        clipped, path=path, title=title, bar_label="log10(nmol/m2/s)",
        vmin=vmin, vmax=vmax, counties=counties, states=states,
    )


def not_log_plot(
    da: xr.DataArray,
    title: str,
    *,
    filename: str,
    plot_directory: str | Path,
    counties=None,
    states=None,
    zlim_min: float | None = None,
    zlim_max: float | None = None,
) -> Path:
    """Map the raster on a linear colour scale. R: ``not_log_plot``.

    **Diverges from R.** In R's "raster has exactly one distinct value" branch,
    ``not_log_plot`` runs the data through ``prep_plot_data`` — i.e. takes its
    log10 — while still labelling the colour bar ``nmol/m2/s``. The plot would show
    log-scaled numbers under a linear label. That is plainly unintended (the whole
    point of this function is the linear scale), so we stay linear throughout. It
    only ever bit the single-valued case, so no realistic map changes.
    """
    path = output_path(plot_directory, filename)

    if _is_degenerate(da):
        flat = xr.zeros_like(da.astype("float64"))
        return _render(
            flat, path=path, title=title, bar_label="nmol/m2/s",
            vmin=None, vmax=None, counties=counties, states=states,
        )

    clipped = da.astype("float64").clip(min=zlim_min, max=zlim_max)
    vmin, vmax = resolve_zlim(clipped, zlim_min, zlim_max)
    # zeros take the "no data" colour, as in R
    clipped = clipped.where(clipped != 0)
    return _render(
        clipped, path=path, title=title, bar_label="nmol/m2/s",
        vmin=vmin, vmax=vmax, counties=counties, states=states,
    )
