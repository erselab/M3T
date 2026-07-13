"""Download helpers.

Port of ``R/utils.R`` (``Trycatch_downloader``, ``make_consistent``) plus the
Zenodo companion-data and Vulcan fetchers that live inline in
``R/CH4_inventory_build.R``.

The R code used ``curl``/``utils::download.file``/``jsonlite``/``terra::vect``.
Here we use ``requests`` for bytes/JSON and ``geopandas`` for vector reads.
"""

from __future__ import annotations

import io
import time
import zipfile
from pathlib import Path
from typing import Any, Literal

import geopandas as gpd
import pandas as pd
import requests

Method = Literal["save", "json", "vect"]

# Companion Zenodo record (see doi:10.5281/zenodo.17328718). Matches the R
# default Zenodo_record in CH4_inventory_build().
DEFAULT_ZENODO_RECORD = "17328718"
# Vulcan v4.0 CO2 inventory record (see doi:10.5281/zenodo.15446748).
VULCAN_ZENODO_RECORD = "15446748"

# Only these four Vulcan sectors are needed by M3T.
_VULCAN_FILES = (
    "v4.res.co2.usa.1km.lcc.mn.allyrs.zip",
    "v4.com.co2.usa.1km.lcc.mn.allyrs.zip",
    "v4.ind.co2.usa.1km.lcc.mn.allyrs.zip",
    "v4.elc.co2.usa.1km.lcc.mn.allyrs.zip",
)


def trycatch_downloader(
    url: str,
    output_location: str | Path | None = None,
    method: Method = "save",
    error_message: str = "",
    *,
    max_attempts: int = 5,
    retry_delay: float = 2.0,
    timeout: float = 60 * 20,
) -> Any:
    """Download ``url`` with retries. Port of R ``Trycatch_downloader``.

    Parameters
    ----------
    url:
        Source URL.
    output_location:
        Destination path (required for ``method="save"``).
    method:
        ``"save"`` writes bytes to ``output_location`` and returns None;
        ``"json"`` parses and returns JSON; ``"vect"`` reads a vector layer via
        geopandas and returns a :class:`geopandas.GeoDataFrame`.
    error_message:
        Raised (as ``RuntimeError``) if all attempts fail.
    max_attempts, retry_delay, timeout:
        Retry policy. R attempted up to 5 times with a 2s delay.

    Notes
    -----
    Unlike R (which swallowed warnings and returned NA to trigger a retry), any
    non-2xx response or exception here counts as a failed attempt.
    """
    if method == "save" and output_location is None:
        raise ValueError("output_location is required when method='save'")

    last_exc: Exception | None = None
    for attempt in range(1, max_attempts + 1):
        try:
            if method == "json":
                resp = requests.get(url, timeout=timeout)
                resp.raise_for_status()
                return resp.json()
            if method == "vect":
                # geopandas/pyogrio can read directly from a URL for many
                # formats; fall back to bytes for zipped shapefiles handled by
                # callers. Here we read directly.
                return gpd.read_file(url)
            # method == "save"
            resp = requests.get(url, timeout=timeout, stream=True)
            resp.raise_for_status()
            out = Path(output_location)
            out.parent.mkdir(parents=True, exist_ok=True)
            with open(out, "wb") as fh:
                for chunk in resp.iter_content(chunk_size=1 << 20):
                    if chunk:
                        fh.write(chunk)
            return None
        except Exception as exc:  # noqa: BLE001 - mirror R's broad retry
            last_exc = exc
            if attempt < max_attempts:
                time.sleep(retry_delay)

    raise RuntimeError(error_message or f"Failed to download {url} after {max_attempts} attempts") from last_exc


def _zenodo_file_url(record: str, filename: str) -> str:
    return f"https://zenodo.org/api/records/{record}/files/{filename}/content"


def download_and_unzip(url: str, extract_dir: str | Path, *, timeout: float = 60 * 20) -> None:
    """Download a zip to memory and extract into ``extract_dir``."""
    resp = requests.get(url, timeout=timeout)
    resp.raise_for_status()
    extract_dir = Path(extract_dir)
    extract_dir.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(io.BytesIO(resp.content)) as zf:
        zf.extractall(extract_dir)


def download_zenodo_companion(
    input_directory: str | Path,
    record: str = DEFAULT_ZENODO_RECORD,
    *,
    timeout: float = 60 * 20,
) -> None:
    """Fetch the M3T companion Zenodo processed data + readme into ``input_directory``.

    Port of the ``if(Zenodo_folder=="")`` branch in ``CH4_inventory_build``.
    """
    input_directory = Path(input_directory)
    download_and_unzip(
        _zenodo_file_url(record, "M3T_Processed.zip"), input_directory, timeout=timeout
    )
    trycatch_downloader(
        _zenodo_file_url(record, "M3T_Readme.pdf"),
        output_location=input_directory / "M3T_Zenodo_Readme.pdf",
        method="save",
        timeout=timeout,
    )


def download_vulcan(vulcan_directory: str | Path, *, timeout: float = 60 * 20) -> None:
    """Download the four needed Vulcan v4.0 sector zips + readme. Port of ``Download_vulcan``."""
    vulcan_directory = Path(vulcan_directory)
    vulcan_directory.mkdir(parents=True, exist_ok=True)
    for filename in _VULCAN_FILES:
        download_and_unzip(
            _zenodo_file_url(VULCAN_ZENODO_RECORD, filename), vulcan_directory, timeout=timeout
        )
    trycatch_downloader(
        _zenodo_file_url(
            VULCAN_ZENODO_RECORD, "readme.Vulcan.1km.V4.0.May.20.2025.pdf"
        ),
        output_location=vulcan_directory / "readme_Vulcan_1km_V4.0_May_20_2025.pdf",
        method="save",
        timeout=timeout,
    )


def make_consistent(df: pd.DataFrame) -> pd.DataFrame:
    """Normalise a GHGRP subpart table. Port of R ``make_consistent``.

    Renames ``ghg_gas_name`` -> ``ghg_name`` and ``reporting_year`` -> ``year``,
    lower-cases ``ghg_name`` and ``facility_name``, and keeps only methane rows.
    """
    df = df.rename(columns={"ghg_gas_name": "ghg_name", "reporting_year": "year"})
    df["ghg_name"] = df["ghg_name"].str.lower()
    df["facility_name"] = df["facility_name"].str.lower()
    return df[df["ghg_name"] == "methane"].copy()
