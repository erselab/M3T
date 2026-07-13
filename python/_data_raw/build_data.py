"""Convert the R package's packaged `.rda` datasets into the Python package's
shipped data (parquet for tables, JSON for the nested/matrix datasets).

Why convert rather than re-scrape: the R package *ships* these `.rda` files as
its data (the `data-raw/*.R` scrapers are dev-time provenance, not part of a
run). Converting the shipped `.rda` guarantees the Python package uses data
*identical* to R — re-scraping could drift if the upstream government sources
changed, which would break output parity. The scraper ports in this directory
exist to document provenance and enable deliberate refreshes.

Run from the repo root (in the m3t-py env):
    python python/_data_raw/build_data.py

Reads:  data/*.rda  (+ python/_data_raw/ghgi_lists/*.json for the datasets
        pyreadr cannot read, produced by export_rda_reference.R)
Writes: python/src/m3t/data/<name>.parquet | <name>.json
"""

from __future__ import annotations

import json
from pathlib import Path

import pandas as pd
import pyreadr

REPO_ROOT = Path(__file__).resolve().parents[2]
RDA_DIR = REPO_ROOT / "data"
GHGI_JSON_DIR = Path(__file__).resolve().parent / "ghgi_lists"
OUT_DIR = REPO_ROOT / "python" / "src" / "m3t" / "data"

# Datasets pyreadr cannot read (nested lists / matrix). Handled from the R-
# exported JSON in ghgi_lists/ instead.
_NON_DATAFRAME = {"GHGI_NG_distribution", "GHGI_NG_transmission", "Neighboring_states"}


def _load_json_columns(path: Path) -> dict:
    """Load a jsonlite dataframe='columns' export -> plain dict of column lists.

    jsonlite injects a ``_row`` column holding non-default row names; drop it
    (we carry row labels explicitly in ``.rowname``).
    """
    obj = json.loads(path.read_text())

    def _strip(d: dict) -> dict:
        return {k: v for k, v in d.items() if k != "_row"}

    # either a flat {col: [...]} (matrix) or {element: {col: [...]}} (GHGI list)
    if obj and all(isinstance(v, dict) for v in obj.values()):
        return {el: _strip(cols) for el, cols in obj.items()}
    return _strip(obj)


# Datasets whose R rownames carry meaning and must survive as a real column.
# GHGI_stationary_combustion is row-labelled by year and has no year column of its
# own -- the R selects the year with `rownames(x) == GHGI_data_yr`, so dropping the
# index (pandas' default) leaves 12 indistinguishable "US_EPA" rows. Every other
# packaged frame's rownames are just leftover row numbers from an R subset.
_ROWNAME_COLUMN = {"GHGI_stationary_combustion": "year"}


def convert_dataframes() -> list[str]:
    """pyreadr-readable data.frames -> parquet. Returns the names written."""
    written = []
    for rda in sorted(RDA_DIR.glob("*.rda")):
        name = rda.stem
        if name in _NON_DATAFRAME:
            continue
        result = pyreadr.read_r(str(rda))
        if not result:
            raise RuntimeError(f"pyreadr read nothing from {rda.name}; is it a list/matrix?")
        obj_name, df = next(iter(result.items()))

        rowname_col = _ROWNAME_COLUMN.get(name)
        if rowname_col:
            # pyreadr drops rownames, so take the R-exported copy that keeps them
            src = GHGI_JSON_DIR / f"{name}.json"
            if not src.exists():
                raise RuntimeError(
                    f"{name} carries data in its R rownames but {src} is missing; run "
                    "`conda run -n M3T Rscript python/_data_raw/export_rda_reference.R`"
                )
            cols = _load_json_columns(src)
            df = pd.DataFrame(cols)
            df.insert(0, rowname_col, pd.to_numeric(df.pop(".rowname")))

        out = OUT_DIR / f"{name}.parquet"
        df.to_parquet(out, index=False)
        written.append(name)
    return written


def convert_ghgi_lists() -> list[str]:
    """The two GHGI nested-list datasets -> JSON dict of {element: {col: [...]}}.

    Each element is a year-indexed table whose row labels (pipeline/component
    categories) are preserved in the `.rowname` column.
    """
    written = []
    for name in ("GHGI_NG_distribution", "GHGI_NG_transmission"):
        src = GHGI_JSON_DIR / f"{name}.json"
        content = _load_json_columns(src)  # {element_name: {col: [values]}}
        (OUT_DIR / f"{name}.json").write_text(json.dumps(content))
        written.append(name)
    return written


def convert_matrix() -> list[str]:
    """Neighboring_states matrix -> parquet with a state-code index column."""
    src = GHGI_JSON_DIR / "Neighboring_states.json"
    cols = _load_json_columns(src)  # includes .rowname column
    df = pd.DataFrame(cols)
    df = df.rename(columns={".rowname": "state"}).set_index("state")
    df.to_parquet(OUT_DIR / "Neighboring_states.parquet")
    return ["Neighboring_states"]


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    df_names = convert_dataframes()
    ghgi_names = convert_ghgi_lists()
    mat_names = convert_matrix()
    total = df_names + ghgi_names + mat_names
    print(f"wrote {len(total)} datasets to {OUT_DIR}")
    for n in sorted(total):
        print(f"  {n}")


if __name__ == "__main__":
    main()
