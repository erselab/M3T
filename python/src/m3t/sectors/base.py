"""Sector interface + registry.

Each sector is a small object exposing:

* ``key``          — stable identifier / output subfolder name.
* ``name``         — human-readable name for logging.
* ``process_flag`` — the ``Config`` attribute that enables it.
* ``run(ctx)``     — do the work, writing gridded NetCDF output to
  ``ctx.output_directory``.

The orchestrator iterates :data:`SECTORS` in the same order as the R
``CH4_inventory_build`` dispatch and runs those whose ``process_flag`` is set.

During Phase 2 the concrete sectors are :class:`StubSector` instances that emit a
zero-filled raster on the target grid, so the whole pipeline runs end-to-end and
produces valid (if empty) output. Phase 3 replaces each stub with the real port.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Protocol, runtime_checkable

from ..context import RunContext


@runtime_checkable
class Sector(Protocol):
    key: str
    name: str
    process_flag: str

    def run(self, ctx: RunContext) -> None: ...


@dataclass
class StubSector:
    """A placeholder sector that writes a zero-filled grid.

    Lets the orchestrator and combine step be developed and tested before the
    real science lands. Each stub writes ``<key>.nc`` (all zeros) to the output
    directory and records itself in ``ctx.shared['stub_sectors_run']``.
    """

    key: str
    name: str
    process_flag: str

    def run(self, ctx: RunContext) -> None:
        da = ctx.blank_grid(fill=0.0)
        da.attrs["m3t_stub"] = 1
        da.attrs["m3t_sector"] = self.key
        ctx.write_output(da, f"{self.key}.nc")
        ctx.shared.setdefault("stub_sectors_run", []).append(self.key)


def enabled(sector: Sector, ctx: RunContext) -> bool:
    return bool(ctx.config.get(sector.process_flag))


# Real sector runners get swapped in here in Phase 3; wrapped so the registry
# below can stay declarative.
def _stub(key: str, name: str, flag: str) -> StubSector:
    return StubSector(key=key, name=name, process_flag=flag)


# Dispatch order mirrors R/CH4_inventory_build.R (landfills -> ... -> GEPA).
SECTORS: list[Sector] = [
    _stub("landfills", "Municipal solid waste (landfills)", "Process_landfills"),
    _stub("natural_gas_distribution", "Natural gas distribution", "Process_natural_gas_distribution"),
    _stub("natural_gas_transmission", "Natural gas transmission", "Process_natural_gas_transmission"),
    _stub("stationary_combustion", "Stationary combustion", "Process_stationary_combustion"),
    _stub("wastewater", "Wastewater", "Process_wastewater"),
    _stub("wetlands", "Wetlands & inland waters", "Process_wetlands_and_inland_waters"),
    _stub("remaining_gepa", "Remaining sectors (gridded EPA)", "Process_remaining_sectors_from_gridded_EPA"),
]


def register(sector: Sector) -> None:
    """Replace (by key) or append a sector — used in Phase 3 to swap in real ports."""
    for i, existing in enumerate(SECTORS):
        if existing.key == sector.key:
            SECTORS[i] = sector
            return
    SECTORS.append(sector)


# Convenience for tests / callers wanting the registry as a dict.
def sectors_by_key() -> dict[str, Sector]:
    return {s.key: s for s in SECTORS}


__all__ = ["Sector", "StubSector", "SECTORS", "enabled", "register", "sectors_by_key"]
