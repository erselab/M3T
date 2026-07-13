"""Access to the packaged reference datasets.

Python equivalent of the R package's ``LazyData`` datasets (``M3T::EIA_SEDS``
etc.). The data ships as parquet/JSON under ``m3t/data/`` (converted from the R
``.rda`` by ``_data_raw/build_data.py``; see that module for the rationale).

Usage::

    from m3t import data
    seds = data.load("EIA_SEDS")                 # -> DataFrame
    ng = data.load("GHGI_NG_distribution")       # -> dict[str, DataFrame]
    adj = data.load("Neighboring_states")        # -> DataFrame indexed by state
    data.available()                             # -> sorted list of names

Datasets are cached after first load. Descriptions mirror ``R/data.R``.
"""

from __future__ import annotations

import json
from functools import lru_cache
from importlib import resources
from typing import Union

import pandas as pd

# Nested-list datasets stored as JSON (dict of year-indexed tables).
_GHGI_LISTS = {"GHGI_NG_distribution", "GHGI_NG_transmission"}
# Matrix dataset stored as parquet indexed by state code.
_MATRIX = {"Neighboring_states"}

DatasetType = Union[pd.DataFrame, dict[str, pd.DataFrame]]


def _data_files():
    return resources.files("m3t.data")


def available() -> list[str]:
    """Sorted names of all packaged datasets."""
    names = set()
    for entry in _data_files().iterdir():
        if entry.name.endswith(".parquet"):
            names.add(entry.name[: -len(".parquet")])
        elif entry.name.endswith(".json"):
            names.add(entry.name[: -len(".json")])
    return sorted(names)


@lru_cache(maxsize=None)
def load(name: str) -> DatasetType:
    """Load a packaged dataset by name (cached).

    Returns a :class:`pandas.DataFrame` for tabular datasets, a
    ``dict[str, DataFrame]`` for the GHGI nested-list datasets, and a
    state-indexed DataFrame for ``Neighboring_states``.
    """
    if name in _GHGI_LISTS:
        return _load_ghgi_list(name)
    if name in _MATRIX:
        with resources.as_file(_data_files() / f"{name}.parquet") as p:
            return pd.read_parquet(p)  # index (state) preserved by pyarrow
    res = _data_files() / f"{name}.parquet"
    if not res.is_file():
        raise KeyError(f"Unknown dataset {name!r}. Available: {', '.join(available())}")
    with resources.as_file(res) as p:
        return pd.read_parquet(p)


def _load_ghgi_list(name: str) -> dict[str, pd.DataFrame]:
    """Reconstruct a GHGI nested-list dataset -> {element: DataFrame}.

    Each element is a table of activity/emission-factor values with rows labelled
    by component category (``.rowname`` in the stored JSON) and columns by year.
    """
    with resources.as_file(_data_files() / f"{name}.json") as p:
        raw = json.loads(p.read_text())
    out: dict[str, pd.DataFrame] = {}
    for element, columns in raw.items():
        df = pd.DataFrame(columns)
        if ".rowname" in df.columns:
            df = df.rename(columns={".rowname": "component"}).set_index("component")
        out[element] = df
    return out
