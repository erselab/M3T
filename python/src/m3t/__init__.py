"""M3T — Modular Methane Mapping Tool (Python port).

Gridded, sectoral methane emission inventories for U.S. urban areas.
Python port of the M3T R package; see ``PYTHON_PORT_PLAN.md`` at the repo root
for status, terra-parity notes, and remaining work.

Public surface:

* :func:`m3t.ch4_inventory_build` — the orchestrator. Builds the target grid,
  runs the enabled sectors, and combines them.
* :class:`m3t.config.Config` — all 79 options (copied per run, never global
  state); ``M3T_get_config`` / ``M3T_set_config`` are R-parity helpers.
* :class:`m3t.context.RunContext` — the run state threaded to every sector.
* :mod:`m3t.geo` — the terra-parity geospatial layer used by every sector.
* :mod:`m3t.datasets` — the 19 packaged reference datasets.
* :mod:`m3t.download` — retrying downloader + Zenodo/Vulcan fetchers.

All seven sectors are ported (:mod:`m3t.sectors`), each golden-tested against
the corresponding R function.
"""

from __future__ import annotations

from . import config, datasets, domain, download, geo, validation
from .config import Config, M3T_get_config, M3T_set_config
from .context import RunContext
from .inventory import ch4_inventory_build

__all__ = [
    "config",
    "datasets",
    "domain",
    "download",
    "geo",
    "validation",
    "Config",
    "M3T_get_config",
    "M3T_set_config",
    "RunContext",
    "ch4_inventory_build",
]

__version__ = "0.0.1.dev0"
