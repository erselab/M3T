"""Validate the shipped Python datasets against the R ground truth.

The reference (tests/golden/rda_reference.json) is produced by R itself
(export_rda_reference.R): shapes, column names, per-numeric-column sums and
NA-counts, row names. Asserting the parquet/JSON the Python package ships matches
those R-computed values is a non-circular check that the .rda -> parquet
conversion is faithful.

Regenerate the reference with:
    conda run -n M3T Rscript python/_data_raw/export_rda_reference.R
"""

from __future__ import annotations

import json
import math
from pathlib import Path

import pandas as pd
import pytest

from m3t import datasets

_REF_PATH = Path(__file__).parent / "golden" / "rda_reference.json"
pytestmark = pytest.mark.golden

_TABULAR = None  # filled from reference (data.frame datasets)


@pytest.fixture(scope="module")
def ref():
    if not _REF_PATH.exists():
        pytest.skip(
            "rda_reference.json missing; run "
            "`conda run -n M3T Rscript python/_data_raw/export_rda_reference.R`"
        )
    return json.loads(_REF_PATH.read_text())


def _tabular_names(ref) -> list[str]:
    return [k for k, v in ref.items() if "nrow" in v and v.get("class") != "matrix/array"]


def test_all_datasets_present(ref):
    assert set(datasets.available()) == set(ref.keys())


def test_tabular_shapes_and_columns(ref):
    for name in _tabular_names(ref):
        df = datasets.load(name)
        r = ref[name]
        assert df.shape == (r["nrow"], r["ncol"]), f"{name} shape"
        assert list(df.columns) == list(r["colnames"]), f"{name} columns"


def test_tabular_numeric_sums_match_r(ref):
    for name in _tabular_names(ref):
        df = datasets.load(name)
        for col, r_sum in ref[name]["numeric_col_sums"].items():
            py_sum = float(pd.to_numeric(df[col], errors="coerce").sum(skipna=True))
            # rel tol tight; abs floor for sums near zero
            assert math.isclose(py_sum, r_sum, rel_tol=1e-9, abs_tol=1e-6), (
                f"{name}.{col}: python {py_sum} != R {r_sum}"
            )


def test_tabular_na_counts_match_r(ref):
    for name in _tabular_names(ref):
        df = datasets.load(name)
        for col, r_na in ref[name]["na_counts"].items():
            assert int(df[col].isna().sum()) == r_na, f"{name}.{col} NA count"


def test_neighboring_states_matrix(ref):
    r = ref["Neighboring_states"]
    adj = datasets.load("Neighboring_states")
    assert adj.shape == (r["nrow"], r["ncol"])
    assert list(adj.index) == list(r["rownames"])
    assert list(adj.columns) == list(r["colnames"])
    assert float(adj.to_numpy().sum()) == pytest.approx(r["total"])


@pytest.mark.parametrize("name", ["GHGI_NG_distribution", "GHGI_NG_transmission"])
def test_ghgi_lists_structure_and_sums(ref, name):
    r = ref[name]
    obj = datasets.load(name)
    assert isinstance(obj, dict)
    assert set(obj.keys()) == set(r["names"])
    for element, edf in obj.items():
        er = r["elements"][element]
        # stored with the category column moved to the index
        assert edf.shape[0] == er["nrow"]
        for col, r_sum in er["numeric_col_sums"].items():
            py_sum = float(pd.to_numeric(edf[col], errors="coerce").sum(skipna=True))
            assert math.isclose(py_sum, r_sum, rel_tol=1e-9, abs_tol=1e-6), (
                f"{name}.{element}.{col}"
            )
