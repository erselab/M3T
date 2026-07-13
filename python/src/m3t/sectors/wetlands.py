"""Wetlands & inland waters sector. Ports ``R/SOCCR_Wetlands.R`` plus the Wetcharts
preparation block of ``CH4_inventory_build.R``.

Unlike the other sectors there is no activity data and no CO2 proxy: wetland
methane is a *flux per unit area of wetland*, so everything here is land cover
times an emission factor. Two independent estimates, which the sector totals keep
separate rather than blending:

* **Wetcharts** — an ensemble of process models (WetCHARTs v1.3.3), already
  downscaled to 0.1 degrees against NLCD land cover and shipped in the companion
  raster. We select the run year, average over a user-chosen subset of the 18
  ensemble members, and put it on the domain grid.

* **NWI + SOCCR** — the National Wetland Inventory gives the fraction of each
  1 km cell covered by each wetland class (``E2``/``M2`` estuarine, ``PFO``/``PNF``
  palustrine, ``L1``/``L2`` lakes, ``R1``-``R4`` rivers). Each class carries an
  emission factor:

  - ``SOCCR1`` — one national EF per class.
  - ``SOCCR2`` — the coastal classes (``E2``, ``M2``) vary by ocean basin, so the
    EF comes from the watershed the cell drains to (Atlantic / Gulf / Pacific /
    Hudson).
  - **Freshwater** (lakes and rivers) is *always* computed and added to every
    total, since it has no SOCCR variant.

The NWI rasters are per state and overlap at state lines (each extends a little
past its border), so they are combined with ``max``, not ``sum`` — taking the sum
would double-count the overlap.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd
import xarray as xr

from .. import geo
from ..context import RunContext

# NWI classes, split by which EF scheme applies.
SOCCR_TYPES = ["M2", "E2", "PFO", "PNF"]
FRESHWATER_TYPES = ["R1", "R2", "R3", "R4", "L1", "L2"]

# SOCCR2 makes the coastal EFs depend on the receiving ocean basin.
_BASINS = {
    "Atlantic Ocean": "Atlantic",
    "Gulf of Mexico": "Gulf",
    "Pacific Ocean": "Pacific",
    "Hudson Bay": "Hudson",
}

# The wetland EFs in the config are g CH4/m2/yr; the flux rasters are nmol/m2/s.
# SOCCR_Wetlands.R does this conversion on its very first line, before anything
# else touches the table -- easy to miss, and it is a factor of ~2.
_G_PER_YR_TO_NMOL_PER_S = 1e9 / (16.043 * 365.25 * 24 * 60 * 60)


def _to_domain(da: xr.DataArray, domain_template: xr.DataArray) -> xr.DataArray:
    """Fine NWI grid -> domain grid.

    Deliberately *not* :func:`geo.project_partial_to_grid`. That helper weights each
    cell by how much of it lies inside the domain, but here the R masks to the
    domain's *bounding box* (a rectangle), reprojects, and only applies the
    coverage weights once at the very end, on the coarse domain grid. Weighting on
    the fine grid as well would apply the fraction twice -- and it is not a subtle
    error: on a coastal domain like CT/RI it halves the answer.
    """
    import geopandas as gpd

    src_crs = da.rio.crs
    box_bounds = geo.ext(geo.project_to_crs(domain_template, src_crs))
    box = gpd.GeoDataFrame(
        geometry=gpd.GeoSeries.from_wkt(
            [
                "POLYGON(({0} {1},{2} {1},{2} {3},{0} {3},{0} {1}))".format(
                    box_bounds[0], box_bounds[1], box_bounds[2], box_bounds[3]
                )
            ],
            crs=src_crs,
        ),
        crs=src_crs,
    )
    pad = geo.res(geo.project_to_crs(domain_template, src_crs))

    # NOTE: no fillna(0) here, unlike every other sector. The NWI rasters are NaN
    # wherever there is no data (ocean, outside the state), and the R leaves those
    # NaN: `mask(updatevalue=0)` only zeros cells outside the *box*. The area-average
    # then skips them, so a coarse cell that is half ocean averages over its land
    # half. Zero-filling first would average the ocean in as real zeros and halve
    # the answer -- which is exactly what it did.
    out = geo.crop_snap_out(da, tuple(box.total_bounds))
    out = geo.mask_geometries(out, box, touches=True, updatevalue=0.0)
    out = geo.extend(out, (pad[0] * 5, pad[1] * 5), fill=0.0)
    return geo.project_to_grid(out, domain_template, resampling="average")


def compute_wetcharts(
    wetcharts: xr.DataArray,
    *,
    inventory_year: int,
    model_subsets: list[list[int]],
    domain,
    domain_template: xr.DataArray,
    domain_crs: str,
) -> tuple[dict[str, xr.DataArray], int]:
    """Wetcharts ensemble -> one raster per model subset, on the domain grid.

    The companion raster's bands are named ``<year>_model_<id>``. We take the year
    nearest the run year, put the bands on the domain grid, then average over each
    requested subset of ensemble members. Already in nmol/m²/s.
    """
    import geopandas as gpd

    names = [str(n) for n in wetcharts.coords["band_name"].values]
    years = sorted({int(n.split("_")[0]) for n in names})
    year = min(years, key=lambda y: abs(y - inventory_year))

    keep = [i for i, n in enumerate(names) if int(n.split("_")[0]) == year]
    sel = wetcharts.isel(band=keep)
    models = [int(names[i].split("_")[-1]) for i in keep]

    dom = domain if str(getattr(domain, "crs", "")) == domain_crs else domain.to_crs(domain_crs)
    if not isinstance(dom, gpd.GeoDataFrame):
        raise TypeError("wetlands needs a polygon domain")

    # The R branches on whether the companion raster is finer or coarser than the
    # run grid: finer -> area-average onto it; coarser -> refine with a nearest
    # disagg first, so the reprojection cannot interpolate across the coarse cell
    # edges. The companion is 0.1 deg and a 0.1 deg run makes these *nominally
    # equal*, but float noise tips R into the "finer" branch, so match that: treat
    # equal as finer and average.
    dom_res = geo.res(domain_template)
    src_res = geo.res(sel)
    coarser = src_res[0] > dom_res[0] * (1 + 1e-9)

    layers = {}
    for i, model in enumerate(models):
        band = sel.isel(band=i)
        band.rio.write_crs(sel.rio.crs, inplace=True)
        if coarser:
            factor = int(round(src_res[0] / dom_res[0]))
            da = geo.project_to_grid(
                geo.disagg(band, factor), domain_template, resampling="nearest"
            )
        else:
            da = geo.project_to_grid(band, domain_template, resampling="average")
        layers[model] = da

    # mask to the domain, then down-weight cells only partly inside it
    weights = geo.coverage_fraction(domain_template, dom)
    out: dict[str, xr.DataArray] = {}
    for n, subset in enumerate(model_subsets, start=1):
        members = [layers[m] for m in subset if m in layers]
        if not members:
            raise ValueError(f"Wetcharts subset {n} matched no models in {year}")
        mean = sum(m.fillna(0.0) for m in members) / len(members)
        mean = geo.mask_geometries(mean, dom, updatevalue=np.nan)
        mean = mean.where(weights.isnull(), mean * weights)
        mean.name = "methane_emissions"
        out[f"Wetcharts_NLCD_Downscaled_subset_{n}"] = mean
    return out, year


def _watershed_efs(watersheds, wetland_efs: pd.DataFrame, domain_crs: str):
    """Watershed polygons carrying the SOCCR2 coastal EF for their ocean basin."""
    ws = watersheds[["NAW1_EN", "geometry"]].dissolve(by="NAW1_EN").reset_index()
    ws = ws[ws["NAW1_EN"].isin(_BASINS)]
    for cls in ("E2", "M2"):
        ws[cls] = [
            float(wetland_efs.loc["SOCCR2", f"{cls}_{_BASINS[name]}"])
            for name in ws["NAW1_EN"]
        ]
    for cls in ("PFO", "PNF"):
        ws[cls] = float(wetland_efs.loc["SOCCR2", cls])
    return ws.to_crs(domain_crs)


def compute_soccr(
    nwi: dict[str, xr.DataArray],
    *,
    wetland_efs: pd.DataFrame,
    watersheds,
    domain,
    domain_template: xr.DataArray,
    domain_crs: str,
    use_soccr1: bool = False,
    use_soccr2: bool = False,
) -> dict[str, xr.DataArray]:
    """NWI wetland-class fractions x emission factors -> flux rasters.

    ``nwi`` maps a class name to its (already state-combined) fractional-cover
    raster. Returns ``Freshwater`` always, plus ``SOCCR1`` / ``SOCCR2`` if enabled.
    """
    dom = domain if str(getattr(domain, "crs", "")) == domain_crs else domain.to_crs(domain_crs)

    wetland_efs = wetland_efs * _G_PER_YR_TO_NMOL_PER_S  # g/m2/yr -> nmol/m2/s
    soccr1_efs = wetland_efs.loc["SOCCR1"]
    out: dict[str, xr.DataArray] = {}

    # every class needs to be on the domain grid first
    on_domain = {cls: _to_domain(da, domain_template) for cls, da in nwi.items()}

    freshwater = sum(
        on_domain[c].fillna(0.0) * float(soccr1_efs[c])
        for c in FRESHWATER_TYPES
        if c in on_domain
    )
    freshwater.name = "methane_emissions"
    out["Freshwater"] = freshwater

    soccr_present = [c for c in SOCCR_TYPES if c in on_domain]

    if use_soccr1:
        # SOCCR1 is national: E2/M2 share the single "Atlantic" column
        def ef1(cls: str) -> float:
            key = f"{cls}_Atlantic" if cls in ("E2", "M2") else cls
            return float(soccr1_efs[key])

        s1 = sum(on_domain[c].fillna(0.0) * ef1(c) for c in soccr_present)
        s1.name = "methane_emissions"
        out["SOCCR1"] = s1

    if use_soccr2:
        ws = _watershed_efs(watersheds, wetland_efs, domain_crs)
        s2 = None
        for cls in soccr_present:
            # rasterize the watershed EF onto the domain grid; the R burns it at 5x
            # and averages back so cells straddling a basin divide get a blend
            fine = geo.make_grid(
                geo.ext(domain_template),
                tuple(r / 5 for r in geo.res(domain_template)),
                domain_crs,
            )
            ef = geo.rasterize(ws, fine, field=cls, touches=True)
            ef = geo.aggregate(ef, 5, fun="mean")
            layer = on_domain[cls].fillna(0.0) * ef.values
            s2 = layer if s2 is None else s2 + layer
        s2.name = "methane_emissions"
        out["SOCCR2"] = s2

    # Mask to the domain polygon and apply the partial-coverage weights *once*,
    # here on the coarse grid -- exactly where the R does it (its per-class
    # reprojection above deliberately carries no weighting).
    weights = geo.coverage_fraction(domain_template, dom)
    for key, da in out.items():
        masked = geo.mask_geometries(da, dom, updatevalue=np.nan)
        masked = masked.where(weights.isnull(), masked * weights)
        masked.name = "methane_emissions"
        out[key] = masked

    return out


def combine_nwi_states(rasters: list[xr.DataArray]) -> xr.DataArray:
    """Combine per-state NWI rasters with ``max``.

    They overlap: each state's raster extends a little past its border, so summing
    would double-count the seam. ``max`` merges them without doing that.
    """
    stacked = xr.concat(rasters, dim="state")
    out = stacked.max(dim="state", skipna=True)
    out.rio.write_crs(rasters[0].rio.crs, inplace=True)
    return out


@dataclass
class WetlandsSector:
    """RunContext wrapper: read shared inputs, compute, write NetCDF outputs."""

    key: str = "wetlands"
    name: str = "Wetlands & inland waters"
    process_flag: str = "Process_wetlands_and_inland_waters"

    def run(self, ctx: RunContext) -> None:
        cfg = ctx.config

        results: dict[str, xr.DataArray] = {}
        wetcharts: dict[str, xr.DataArray] = {}
        if cfg.Use_Wetcharts:
            wetcharts, year = compute_wetcharts(
                ctx.shared["wetcharts"],
                inventory_year=ctx.inventory_year,
                model_subsets=cfg.Wetcharts_model_subset,
                domain=ctx.domain,
                domain_template=ctx.domain_template,
                domain_crs=ctx.domain_crs,
            )
            if year != ctx.inventory_year:
                print(
                    f"Prepared wetcharts does not include {ctx.inventory_year}, "
                    f"using {year} as the nearest data available"
                )
            results |= wetcharts

        soccr = compute_soccr(
            ctx.shared["nwi"],
            wetland_efs=cfg.Wetland_EFs,
            watersheds=ctx.shared.get("watersheds"),
            domain=ctx.domain,
            domain_template=ctx.domain_template,
            domain_crs=ctx.domain_crs,
            use_soccr1=cfg.Use_SOCCR1,
            use_soccr2=cfg.Use_SOCCR2,
        )
        results |= soccr

        for name, da in results.items():
            ctx.write_output(da, f"{name}.nc", subdir="Wetlands")

        # sector totals: freshwater is in every one; the estimates stay separate
        freshwater = soccr["Freshwater"]
        totals: dict[str, xr.DataArray] = {}
        for name, da in wetcharts.items():
            n = name.rsplit("_", 1)[-1]
            totals[f"Wetland_sector_total_Wetcharts_NLCD_subset_{n}"] = da.fillna(
                0.0
            ) + freshwater.fillna(0.0)
        for variant in ("SOCCR1", "SOCCR2"):
            if variant in soccr:
                totals[f"Wetland_sector_total_{variant}"] = soccr[variant].fillna(
                    0.0
                ) + freshwater.fillna(0.0)

        for name, da in totals.items():
            da.name = "methane_emissions"
            ctx.write_output(da, f"{name}.nc")

        primary = next(iter(totals.values())) if totals else freshwater
        ctx.write_output(primary, f"{self.key}.nc")

        ctx.shared.setdefault("sector_results", {})[self.key] = results | totals


def register() -> None:
    from . import base

    base.register(WetlandsSector())
