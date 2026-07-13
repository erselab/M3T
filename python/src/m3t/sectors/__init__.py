# M3T sector modules (ports of R/<Sector>.R).
#
# `base.SECTORS` starts as zero-filled StubSector placeholders; ported sectors
# replace their stub via `base.register(...)` at import time below.
from . import (
    base,
    landfills,
    ng_distribution,
    ng_transmission,
    stationary_combustion,
    wastewater,
    wetlands,
)

# Register real (ported) sectors over their stubs.
landfills.register()
ng_transmission.register()
wastewater.register()
stationary_combustion.register()
ng_distribution.register()
wetlands.register()

__all__ = [
    "base",
    "landfills",
    "ng_distribution",
    "ng_transmission",
    "stationary_combustion",
    "wastewater",
    "wetlands",
]
