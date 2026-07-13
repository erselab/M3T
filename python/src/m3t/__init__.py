"""M3T — Modular Methane Mapping Tool (Python port).

Gridded, sectoral methane emission inventories for U.S. urban areas.
Python port of the M3T R package; see ``PYTHON_PORT_PLAN.md`` at the repo root.

Public surface (Phase 0):

* :class:`m3t.config.Config` and the ``M3T_get_config`` / ``M3T_set_config``
  helpers.
* :mod:`m3t.geo` — the terra-parity geospatial layer used by every sector.
* :mod:`m3t.download` — retrying downloader + Zenodo/Vulcan fetchers.

``ch4_inventory_build`` (the orchestrator) and the sector modules land in
Phases 2–3.
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
