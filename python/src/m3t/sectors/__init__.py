# M3T sector modules (ports of R/<Sector>.R).
#
# `base.SECTORS` starts as zero-filled StubSector placeholders; ported sectors
# replace their stub via `base.register(...)` at import time below.
from . import base, landfills, ng_transmission, stationary_combustion, wastewater

# Register real (ported) sectors over their stubs.
landfills.register()
ng_transmission.register()
wastewater.register()

# stationary_combustion's science is complete and golden-tested against R, but it
# is not registered yet: ch4_inventory_build cannot supply its inputs. It needs the
# county Tigerlines and a gridded CO2 inventory on the RunContext, and the default
# config picks Vulcan (Use_Vulcan), which is a large Zenodo download that
# shared_data does not fetch. ACES works today if injected -- see
# tests/test_stationary_combustion_golden.py. Once those are wired through, call
# stationary_combustion.register() here and drop the stub.

__all__ = [
    "base",
    "landfills",
    "ng_transmission",
    "stationary_combustion",
    "wastewater",
]
