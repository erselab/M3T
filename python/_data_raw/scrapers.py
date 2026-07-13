"""Provenance / refresh tooling: regenerate packaged datasets from their sources.

These are Python ports of the R ``data-raw/*.R`` scripts. They are **not** part
of a normal M3T run — the package ships pre-built data (see ``build_data.py``).
Use them only to deliberately refresh a dataset from its upstream source.

Currently ported: the reproducible EPA GHGRP ``dmapservice`` CSV family, which
all share the ``make_consistent`` idiom. Other ``data-raw`` scripts are cataloged
in ``README.md`` with their reproducibility status (several read local files off
the original author's machine and cannot be reproduced here).

Each function returns a DataFrame equivalent to the corresponding ``.rda`` and,
if ``out_dir`` is given, writes ``<name>.parquet`` there so it can be diffed
against the shipped data.

Run a refresh, e.g.::

    python -m _data_raw.scrapers ghgrp_wastewater
"""

from __future__ import annotations

import sys
from pathlib import Path

import pandas as pd

# reuse the package's downloader + GHGRP normaliser so refresh matches runtime
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))
from m3t.download import make_consistent  # noqa: E402

_DMAP = "https://data.epa.gov/dmapservice"


def _read_epa_csv(table: str) -> pd.DataFrame:
    """Download and parse an EPA dmapservice table as CSV (as R read.csv did)."""
    return pd.read_csv(f"{_DMAP}/{table}/csv")


def ghgrp_wastewater() -> pd.DataFrame:
    """Port of data-raw/GHGRP_wastewater.R (subpart II). No subsetting."""
    return _read_epa_csv("ghg.ii_subpart_level_information")


def ghgrp_combustion_emissions() -> pd.DataFrame:
    """Port of data-raw/GHGRP_combustion_emissions.R (subpart C), methane only."""
    df = _read_epa_csv("ghg.c_subpart_level_information")
    return make_consistent(df)


def ghgrp_landfills() -> pd.DataFrame:
    """Port of data-raw/GHGRP_landfills.R (subpart HH + gas-collection details).

    Methane-only subpart HH emissions merged with the HH-6 (generation-first) and
    HH-8 (collection-first) equation results, renamed to match the R columns.
    """
    hh = make_consistent(_read_epa_csv("ghg.hh_subpart_level_information"))
    details = _read_epa_csv("ghg.hh_gas_collection_system_detls")
    details = details[
        ["facility_id", "reporting_year", "equation_hh6_result", "equation_hh8_result"]
    ]
    merged = hh.merge(
        details,
        left_on=["facility_id", "year"],
        right_on=["facility_id", "reporting_year"],
        how="left",
    )
    return merged.rename(
        columns={
            "equation_hh6_result": "generation_first_HH6",
            "equation_hh8_result": "collection_first_HH8",
        }
    )


_SCRAPERS = {
    "ghgrp_wastewater": ghgrp_wastewater,
    "ghgrp_combustion_emissions": ghgrp_combustion_emissions,
    "ghgrp_landfills": ghgrp_landfills,
}


def main(argv: list[str]) -> None:
    if not argv or argv[0] not in _SCRAPERS:
        print("usage: python -m _data_raw.scrapers <name> [out_dir]")
        print("available:", ", ".join(_SCRAPERS))
        raise SystemExit(2)
    name = argv[0]
    df = _SCRAPERS[name]()
    print(f"{name}: {df.shape[0]} rows x {df.shape[1]} cols")
    if len(argv) > 1:
        out = Path(argv[1]) / f"{name}.parquet"
        df.to_parquet(out, index=False)
        print("wrote", out)


if __name__ == "__main__":
    main(sys.argv[1:])
