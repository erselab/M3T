"""Shared run state passed to every sector.

The R sector functions each take a long positional argument list (domain,
domain_template, directories, config values, shared datasets, ...). Rather than
replicate those unwieldy signatures, the Python port bundles the shared state
into a single :class:`RunContext` that is threaded through the orchestrator and
every sector. Sectors read what they need from it and write their gridded output
back to ``output_directory`` as NetCDF — the same file-based contract the R code
uses (``Combine_across_sectors`` reads those files back).
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import xarray as xr

from . import geo
from .config import Config


@dataclass
class RunContext:
    """Everything a sector needs to run.

    Attributes
    ----------
    config:
        The (already-validated) :class:`~m3t.config.Config` for this run.
    run_directory, input_directory, output_directory, plot_directory:
        Resolved run folders. ``plot_directory`` is ``None`` unless ``verbose``.
    inventory_year:
        Requested inventory year.
    domain_template:
        All-NaN raster defining the target grid/CRS (from
        :func:`m3t.domain.build_domain`).
    domain:
        The domain geometry — a bbox tuple or a GeoDataFrame — used for masking.
    domain_crs:
        Target CRS string.
    verbose:
        Whether to write per-sector visuals.
    shared:
        Free-form dict for shared datasets loaded once by the orchestrator
        (GHGRP facility data, GHGI tables, Tigerlines, Vulcan/ACES rasters, ...),
        populated incrementally as sectors are ported in Phase 3.
    """

    config: Config
    run_directory: Path
    input_directory: Path
    output_directory: Path
    plot_directory: Path | None
    inventory_year: int
    domain_template: xr.DataArray
    domain: Any
    domain_crs: str
    verbose: bool = False
    shared: dict[str, Any] = field(default_factory=dict)

    # ------------------------------------------------------------------ #
    def blank_grid(self, fill: float = 0.0, name: str = "methane_emissions") -> xr.DataArray:
        """A copy of the target grid filled with ``fill`` (default 0)."""
        return geo.grid_like(self.domain_template, fill=fill, name=name)

    def write_output(
        self,
        da: xr.DataArray,
        filename: str,
        *,
        subdir: str | None = None,
        varname: str = "methane_emissions",
    ) -> Path:
        """Write a sector raster to ``output_directory`` (NetCDF), returning its path."""
        out_dir = self.output_directory if subdir is None else self.output_directory / subdir
        out_dir.mkdir(parents=True, exist_ok=True)
        path = out_dir / filename
        geo.write_cdf(da, path, varname=varname)
        return path
