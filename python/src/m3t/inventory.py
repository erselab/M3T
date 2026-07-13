"""``ch4_inventory_build`` — the orchestrator. Port of ``R/CH4_inventory_build.R``.

Phase 2 scaffold: this wires the full control flow — resolve directories, write a
run-settings record, validate the config, build the target-grid domain, dispatch
the enabled sectors, and combine — with the sectors running as zero-filled stubs
(see ``m3t.sectors.base``). It runs end-to-end offline for box/CONUS domains,
producing a correct grid and a summed total.

Deferred to later phases (kept as explicit TODOs below):

* Data acquisition — Zenodo companion, Vulcan/ACES downloads, Census Tigerlines,
  shared GHGRP/GHGI tables (Phase 2 data-wiring / Phase 3 per sector).
* State/urban-name domains — need the Tigerlines GeoDataFrame (pass ``tigerlines``
  to enable today).
* The ``res_check`` resolution floor (bump to ~1 km when finer than the Vulcan
  proxy) — needs the Vulcan CRS; only matters below ~0.01°.
"""

from __future__ import annotations

import datetime as _dt
from pathlib import Path
from typing import Any

from . import domain as _domain
from .combine import combine_across_sectors
from .config import Config, M3T_config
from .context import RunContext
from .sectors import base as _sectors
from .shared_data import prepare_shared_data
from .validation import check_config


def _resolve_directories(run_directory: str | Path, verbose: bool):
    run = Path(run_directory).expanduser().resolve()
    input_dir = run / "in"
    output_dir = run / "out"
    input_dir.mkdir(parents=True, exist_ok=True)
    output_dir.mkdir(parents=True, exist_ok=True)
    plot_dir: Path | None = None
    if verbose:
        plot_dir = run / "plots"
        (plot_dir / "Summed_Sectors").mkdir(parents=True, exist_ok=True)
    # subfolders the R code always creates under input/
    for sub in ("GHGRP", "EIA", "NEI"):
        (input_dir / sub).mkdir(exist_ok=True)
    return run, input_dir, output_dir, plot_dir


def _write_run_settings(
    input_dir: Path, *, run_directory, inventory_year, domain, domain_res, domain_crs, verbose, config: Config
) -> None:
    """Port of the ``sink()`` run-settings dump: record run args + config."""
    lines = [
        f"Run Date: {_dt.date.today().isoformat()}",
        "M3T package version: (python port)",
        "",
        "Run settings:",
        f"  run_directory = {run_directory}",
        f"  inventory_year = {inventory_year}",
        f"  domain = {domain!r}",
        f"  domain_res = {domain_res}",
        f"  domain_crs = {domain_crs}",
        f"  verbose = {verbose}",
        "",
        "Config settings:",
    ]
    for name, value in config.get().items():
        lines.append(f"  {name} = {value!r}")
    (input_dir / "Run_settings.txt").write_text("\n".join(lines) + "\n")


def ch4_inventory_build(
    run_directory: str | Path,
    inventory_year: int,
    domain: Any,
    domain_res: float | tuple[float, float] | None = None,
    domain_crs: str = "epsg:4326",
    *,
    verbose: bool = False,
    config: Config | None = None,
    tigerlines: Any = None,
    combine: bool | None = None,
    shared: dict[str, Any] | None = None,
) -> RunContext:
    """Build a gridded, sectoral methane inventory.

    Parameters mirror the R function's core arguments. ``config`` defaults to a
    **copy** of the module singleton :data:`m3t.config.M3T_config` (so a run never
    mutates global state — this is the explicit-config design chosen over R's
    global-env-with-on.exit-reset).

    ``shared`` pre-seeds :attr:`RunContext.shared` (e.g.
    ``{"ghgrp_facility_data": df}``) so a run can stay offline or reuse
    already-loaded inputs; :func:`~m3t.shared_data.prepare_shared_data` fills in
    the rest.

    Returns the :class:`~m3t.context.RunContext` (with ``output_directory`` and
    ``shared`` populated) so callers/tests can inspect what ran. Sector rasters
    are written under ``<run_directory>/out``; the combined total, if enabled, to
    ``out/M3T_total.nc``.
    """
    cfg = (config or M3T_config).copy()

    run, input_dir, output_dir, plot_dir = _resolve_directories(run_directory, verbose)

    # Fail fast on unusable config combinations (accumulated, like R).
    check_config(cfg, input_directory=input_dir)

    _write_run_settings(
        input_dir,
        run_directory=run,
        inventory_year=inventory_year,
        domain=domain,
        domain_res=domain_res,
        domain_crs=domain_crs,
        verbose=verbose,
        config=cfg,
    )

    # Build the target grid + domain geometry.
    domain_template, domain_geom = _domain.build_domain(
        domain, domain_res, domain_crs, tigerlines=tigerlines
    )

    ctx = RunContext(
        config=cfg,
        run_directory=run,
        input_directory=input_dir,
        output_directory=output_dir,
        plot_directory=plot_dir,
        inventory_year=int(inventory_year),
        domain_template=domain_template,
        domain=domain_geom,
        domain_crs=domain_crs,
        verbose=verbose,
        shared=dict(shared) if shared else {},
    )

    # Load cross-sector inputs the enabled sectors need (respects injected shared).
    prepare_shared_data(ctx)

    # Dispatch enabled sectors in R order.
    ran: list[str] = []
    for sector in _sectors.SECTORS:
        if _sectors.enabled(sector, ctx):
            print(f"Running sector: {sector.name}")
            sector.run(ctx)
            ran.append(sector.key)
    ctx.shared["sectors_run"] = ran

    do_combine = cfg.Combine_sectors if combine is None else combine
    if do_combine:
        total_path = combine_across_sectors(ctx)
        ctx.shared["combined_total"] = total_path
        print(f"Combined total written to {total_path}")

    return ctx
