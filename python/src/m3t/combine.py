"""Combine sector outputs into whole-inventory estimates.

Port of ``R/Combine_across_sectors.R``.

Several sectors emit more than one *variation* of their total (landfills report
three GHGRP methods, wastewater eight source/method/septic combinations, and so
on). This module discovers which variations actually got written, then produces:

* **Summary combinations** — one 3-layer raster holding the cell-wise ``min`` /
  ``mean`` / ``max`` of the whole inventory across every variation. Because
  sectors are independent and additive, the min (max) over all combinations is
  just the sum of each sector's own min (max).
* **Individual combinations** — one raster per element of the cartesian product
  of the sectors' variations, plus ``Combined_inventory_key.csv`` recording which
  variation each one used.
* **Thermogenic / non-thermogenic** splits of both, when ``Separate_thermo``.

A few sectors have exactly one output and are always included ("set" sectors:
the three GEPA rasters and NG transmission).

Divergence from R
-----------------
This is a **corrected** port; ``R/Combine_across_sectors.R`` is buggy in three
ways, all confirmed against the R function on synthetic inputs (see
``tests/golden/capture_combine_oracle.R`` and ``tests/test_combine.py``):

1. **Individual combinations are corrupt in R.** Stationary combustion is the one
   sector whose variation spans *two* files (fossil fuel + wood). R meant to join
   them, but the guard reads ``isa(stat_comb_options_filenames[1], "matrix")`` —
   the ``[1]`` grabs the first *element* (a string), so the test is always FALSE
   and the join never happens. The 2xN matrix then flattens column-major inside
   ``expand.grid``, so the filename grid has 2x as many rows as the key. R then
   iterates over the *key's* row count, using only the first half. Net effect:
   every combination gets **only one of the two stationary-combustion files**, and
   **the key CSV does not describe the raster it names**.
2. **The summary understates stationary combustion.** It takes min/mean/max across
   the individual fossil/wood layers rather than across variation *totals*, which
   halves the mean.
3. **``Separate_thermo`` + summary cannot run at all.** ``writeCDF`` -> ``rast``
   drops the extent and CRS, so that block's accumulator (built from
   ``domain_template``, which *has* an extent) cannot be combined with the
   read-back sector layers: terra raises "[rast] extents do not match".

Here a variation always contributes **all** of its files, the key always matches
the rasters, and the thermogenic split works. ``tests/test_combine.py`` pins R's
numbers alongside ours so the divergence stays explicit.
"""

from __future__ import annotations

import itertools
from dataclasses import dataclass
from pathlib import Path

import pandas as pd
import xarray as xr

from . import geo
from .context import RunContext

# Sectors with exactly one output; always added to every combination.
SET_FILES = (
    "GEPA_ind_landfill.nc",
    "GEPA_non_thermo.nc",
    "GEPA_thermo.nc",
    "NG_transmission_sector_total.nc",
)

# Which set files are thermogenic (the rest are not).
THERMO_SET = {"GEPA_thermo.nc", "NG_transmission_sector_total.nc"}


@dataclass(frozen=True)
class Variation:
    """One choice a sector offers: a label plus the file(s) that make it up."""

    label: str
    files: tuple[Path, ...]


@dataclass(frozen=True)
class SectorOptions:
    """A sector and the variations of it that were actually written."""

    name: str  # column name used in the key CSV
    variations: tuple[Variation, ...]


def _product_labels(*factors: tuple[str, ...]) -> list[str]:
    """Cartesian product with the FIRST factor varying fastest (R's expand.grid)."""
    out = []
    for combo in itertools.product(*reversed(factors)):
        out.append("_".join(reversed(combo)))
    # itertools varies the LAST factor fastest; reversing the factors and the
    # tuple restores R's ordering (first factor fastest).
    return out


def discover_sectors(output_directory: Path) -> tuple[list[Path], list[SectorOptions]]:
    """Find the set files and the per-sector variations present in ``output_directory``.

    Sector order matches R's (alphabetical by its internal option-variable name),
    so combination numbering lines up with R's key.
    """
    present = {p.name for p in output_directory.glob("*.nc")}

    def f(name: str) -> Path:
        return output_directory / name

    set_files = [f(n) for n in SET_FILES if n in present]

    sectors: list[SectorOptions] = []

    # --- Landfills: one file per variation ------------------------------- #
    landfill = [
        Variation(lbl, (f(fn),))
        for lbl in ("GHGRP_reported", "GHGRP_generation_first", "GHGRP_collection_first")
        if (fn := f"Landfill_sector_total_{lbl}.nc") in present
    ]
    if landfill:
        sectors.append(SectorOptions("Landfill_options", tuple(landfill)))

    # --- NG distribution -------------------------------------------------- #
    ng = [
        Variation(lbl, (f(fn),))
        for lbl in _product_labels(("ACES", "Vulcan"), ("byLDC", "bystate", "bydomain"))
        if (fn := f"NG_distribution_sector_total_{lbl}.nc") in present
    ]
    if ng:
        sectors.append(SectorOptions("Natural_Gas_Distribution_options", tuple(ng)))

    # --- Wastewater -------------------------------------------------------- #
    ww = [
        Variation(lbl, (f(fn),))
        for lbl in _product_labels(("CWNS", "DMR"), ("GHGI", "Moore"), ("state", "national"))
        if (fn := f"Wastewater_sector_total_{lbl}.nc") in present
    ]
    if ww:
        sectors.append(SectorOptions("Wastewater_options", tuple(ww)))

    # --- Wetlands: SOCCR variants, plus one variation per Wetcharts subset -- #
    wet = [
        Variation(lbl, (f(fn),))
        for lbl in ("SOCCR1", "SOCCR2")
        if (fn := f"Wetland_sector_total_{lbl}.nc") in present
    ]
    wet += [
        Variation(n[len("Wetland_sector_total_") : -len(".nc")], (f(n),))
        for n in sorted(present)
        if n.startswith("Wetland_sector_total_Wetcharts_NLCD")
    ]
    if wet:
        sectors.append(SectorOptions("Wetland_options", tuple(wet)))

    # --- Stationary combustion: each variation is fossil fuel AND wood ------ #
    stat = []
    for lbl in _product_labels(("ACES", "Vulcan"), ("bystate", "bydomain")):
        files = tuple(
            f(fn)
            for kind in ("fossil_fuel", "wood")
            if (fn := f"Stationary_combustion_sector_{kind}_total_{lbl}.nc") in present
        )
        if files:
            stat.append(Variation(lbl, files))
    if stat:
        sectors.append(SectorOptions("Stationary_Combustion_options", tuple(stat)))

    return set_files, sectors


def _is_thermo(path: Path) -> bool:
    """Thermogenic sources: NG distribution/transmission, fossil-fuel combustion, GEPA thermo."""
    n = path.name
    if n in THERMO_SET:
        return True
    return n.startswith("NG_distribution_sector_total") or "fossil_fuel" in n


def _read(path: Path, template: xr.DataArray) -> xr.DataArray:
    """Read a sector raster onto the target grid, NaN -> 0."""
    ds = xr.open_dataset(path, decode_coords="all")
    da = ds[next(iter(ds.data_vars))].astype("float64")
    if da.rio.crs is None:
        da = da.rio.write_crs(template.rio.crs)
    if da.shape != template.shape:
        da = da.rio.reproject_match(template)
    return da.fillna(0.0)


def _sum_files(files, template: xr.DataArray) -> xr.DataArray:
    total = xr.zeros_like(template.astype("float64"))
    for p in files:
        total = total + _read(p, template)
    return total


def _write(da: xr.DataArray, path: Path) -> Path:
    da = da.rename("methane_emissions")
    da.attrs["units"] = "nmol/m2/s"
    geo.write_cdf(da, path, varname="methane_emissions")
    return path


def _write_summary(
    stats: dict[str, xr.DataArray], path: Path, longname: str
) -> Path:
    """Write a 3-layer min/mean/max raster (R writes one 3-layer NetCDF)."""
    da = xr.concat(
        [stats["min"], stats["mean"], stats["max"]],
        dim=pd.Index(["min", "mean", "max"], name="stat"),
    )
    da.attrs["long_name"] = longname
    return _write(da, path)


def _summary_stats(
    set_total: xr.DataArray,
    sectors: list[SectorOptions],
    template: xr.DataArray,
    *,
    keep,
) -> dict[str, xr.DataArray]:
    """min/mean/max of the whole inventory, counting only files passing ``keep``.

    Sectors are additive and independent, so the extremes over all combinations
    are the sum of each sector's own extremes over its variations.
    """
    acc = {k: set_total.copy() for k in ("min", "mean", "max")}
    for sector in sectors:
        totals = [
            _sum_files([p for p in v.files if keep(p)], template) for v in sector.variations
        ]
        totals = [t for t in totals if t is not None]
        if not totals:
            continue
        stacked = xr.concat(totals, dim="variation")
        acc["min"] = acc["min"] + stacked.min(dim="variation")
        acc["mean"] = acc["mean"] + stacked.mean(dim="variation")
        acc["max"] = acc["max"] + stacked.max(dim="variation")
    return acc


def combine_across_sectors(ctx: RunContext) -> Path:
    """Build the combined inventories. Returns the path of ``M3T_total.nc``.

    Honours ``Create_summary_combinations``, ``Create_individual_combinations``
    and ``Separate_thermo`` from the config.
    """
    cfg = ctx.config
    out = ctx.output_directory
    template = ctx.domain_template

    combined_dir = out / "Combined_files"
    summary_dir = combined_dir / "summary_combinations"

    set_files, sectors = discover_sectors(out)

    all_thermo = [p for p in set_files if _is_thermo(p)]
    all_non_thermo = [p for p in set_files if not _is_thermo(p)]
    set_total = _sum_files(set_files, template)

    groups: list[tuple[str, object, list[Path], Path, str]] = [
        ("", lambda p: True, set_files, summary_dir / "Summary_combination_inventories.nc",
         "sum of the min, mean, max for each sector across all variations"),
    ]
    if cfg.Separate_thermo:
        groups += [
            ("Thermogenic_", _is_thermo, all_thermo,
             summary_dir / "Summary_combination_thermogenic_inventories.nc",
             "sum of the min, mean, max for each thermogenic sector across all variations"),
            ("Non_thermogenic_", lambda p: not _is_thermo(p), all_non_thermo,
             summary_dir / "Summary_combination_non_thermogenic_inventories.nc",
             "sum of the min, mean, max for each non-thermogenic sector across all variations"),
        ]

    # --- summary combinations --------------------------------------------- #
    if cfg.Create_summary_combinations:
        summary_dir.mkdir(parents=True, exist_ok=True)
        for _prefix, keep, gset, path, longname in groups:
            stats = _summary_stats(
                _sum_files(gset, template), sectors, template, keep=keep
            )
            _write_summary(stats, path, longname)

    # --- individual combinations ------------------------------------------ #
    if cfg.Create_individual_combinations and sectors:
        combined_dir.mkdir(parents=True, exist_ok=True)
        combos = list(_iter_combinations(sectors))
        width = len(str(len(combos)))

        subdirs = {}
        if cfg.Separate_thermo:
            for prefix, sub in (("Thermogenic_", "thermogenic"),
                                ("Non_thermogenic_", "non_thermogenic")):
                subdirs[prefix] = combined_dir / sub
                subdirs[prefix].mkdir(parents=True, exist_ok=True)

        rows = []
        for i, choice in enumerate(combos, start=1):
            files = [p for v in choice for p in v.files]
            num = str(i).zfill(width)

            total = set_total + _sum_files(files, template)
            _write(total, combined_dir / f"Combined_inventory_combination_{num}.nc")

            if cfg.Separate_thermo:
                for prefix, keep, gset in (
                    ("Thermogenic_", _is_thermo, all_thermo),
                    ("Non_thermogenic_", lambda p: not _is_thermo(p), all_non_thermo),
                ):
                    part = _sum_files(gset, template) + _sum_files(
                        [p for p in files if keep(p)], template
                    )
                    _write(
                        part,
                        subdirs[prefix]
                        / f"{prefix}combined_inventory_combination_{num}.nc",
                    )

            row = {"Inventory_Number": i}
            row |= {s.name: v.label for s, v in zip(sectors, choice)}
            for j, p in enumerate(set_files, start=1):
                row[f"Nonvarying_sector_{j}"] = p.stem
            rows.append(row)

        pd.DataFrame(rows).to_csv(
            combined_dir / "Combined_inventory_key.csv", index=False
        )

    # --- the headline total: one canonical raster per sector --------------- #
    total = template.astype("float64").copy()
    total.values[:] = 0.0
    n = 0
    for key in ctx.shared.get("sectors_run", []):
        p = out / f"{key}.nc"
        if p.exists():
            total = total + _read(p, template)
            n += 1
    total.attrs["m3t_n_sectors_combined"] = n
    return _write(total, out / "M3T_total.nc")


def _iter_combinations(sectors: list[SectorOptions]):
    """Cartesian product of the sectors' variations, first sector varying fastest.

    ``itertools.product`` varies its *last* argument fastest, so feed the sectors
    reversed and un-reverse each tuple.
    """
    for combo in itertools.product(*[s.variations for s in reversed(sectors)]):
        yield tuple(reversed(combo))
