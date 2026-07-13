"""Live scraper validation (network). Deselected by default: `-m "not network"`.

Confirms a ported data-raw scraper reproduces the shipped dataset from its
upstream source. Upstream tables can grow over time, so we compare on stable
invariants (schema + aggregate over the years present in the shipped snapshot)
rather than exact row-for-row equality.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from _data_raw import scrapers  # noqa: E402

from m3t import datasets  # noqa: E402

pytestmark = pytest.mark.network


def test_ghgrp_wastewater_matches_shipped():
    try:
        live = scrapers.ghgrp_wastewater()
    except Exception as exc:  # pragma: no cover - network flakiness
        pytest.skip(f"EPA dmapservice unreachable: {exc}")

    ship = datasets.load("GHGRP_wastewater")
    assert list(live.columns) == list(ship.columns)

    # compare methane totals over the years captured in the shipped snapshot
    years = sorted(ship["reporting_year"].unique())
    live_y = live[live["reporting_year"].isin(years)]
    assert live_y["ghg_quantity"].sum() == pytest.approx(
        ship["ghg_quantity"].sum(), rel=1e-6
    )
