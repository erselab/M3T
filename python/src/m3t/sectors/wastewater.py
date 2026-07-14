"""Wastewater sector. Port of ``R/Wastewater.R``.

Three independent emission streams, summed into per-variant sector totals:

* **Municipal treatment plants** — facility flow comes from either the Clean
  Watersheds Needs Survey (``CWNS``) or Discharge Monitoring Reports (``DMR``),
  and is turned into methane two ways:

  - ``GHGI``  — disaggregate the GHGI national non-septic total across facilities
    proportionally to flow.
  - ``Moore`` — apply the Moore et al. log-log relationship to each facility's
    flow: ``log10(g/s) = 1.279367 * log10(m³/s) + 0.9257305``.

  Both source × method combinations can be enabled at once, giving up to four
  municipal rasters.

* **Industrial treatment plants** — GHGRP subpart II reported methane, rasterized
  as points.

* **Septic systems** — an emission factor per unit of "developed open space /
  low intensity" NLCD land cover, either national (GHGI total over the national
  area of that cover) or per state (population × septic fraction × GHGI EF).
  Computed by :func:`compute_septic` and passed to :func:`compute_wastewater` via
  ``septic=``, which folds it into the sector totals.

The heavy lifting is in :func:`compute_wastewater`, which takes plain inputs and
returns rasters, so it can be golden-tested without the orchestrator.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

import numpy as np
import pandas as pd
import xarray as xr

from .. import geo
from ..context import RunContext
from ..download import make_consistent
from ._spatial import clip_points_to_domain as _clip_to_domain
from ._spatial import project_points as _project_points

_SEC_PER_YR = 365 * 24 * 60 * 60
_CH4 = 16.043                                # == 12.011 + 1.008 * 4
_MT_TO_MOL_S = 1e6 / (_CH4 * _SEC_PER_YR)    # metric tonnes CH4/yr -> mol/s
_KT_TO_MOL_S = 1e9 / (_CH4 * _SEC_PER_YR)    # kilotonnes CH4/yr -> mol/s
_MGD_TO_M3S = 3785.41178 / (24 * 60 * 60)    # million gal/day -> m³/s

# Moore et al. (2023), refit against the complete 2025 dataset.
_MOORE_SLOPE = 1.279367
_MOORE_INTERCEPT = 0.9257305

# CWNS 2012 records a per-row datum; anything not explicitly WGS84 or NAD27 is
# assumed NAD83 (matching the R, which treats blank/unknown as NAD83).
_CWNS_DATUMS = {
    "World Geodetic System of 1984": "epsg:4326",
    "North American Datum of 1927": "epsg:4267",
}
_CWNS_DEFAULT_DATUM = "epsg:4269"  # NAD83


def _flux_from_points(points: pd.DataFrame, template: xr.DataArray) -> xr.DataArray:
    """Rasterize summed point emissions (mol/s) and convert to flux (nmol/m²/s).

    Port of the R ``rasterize_plus`` tail: sum per cell, ``*1e9/cellSize``, then
    set empty cells to 0. Rows with no emission value are dropped first, matching
    the R ``subset(!is.na(emiss))``.
    """
    pts = points[points["emiss"].notna()]
    if len(pts):
        rast = geo.rasterize_points_sum(
            pts["x"].to_numpy(), pts["y"].to_numpy(), pts["emiss"].to_numpy(), template
        )
    else:
        rast = geo.grid_like(template, fill=np.nan)
    flux = rast * 1e9 / geo.cell_area(template, unit="m")
    flux = flux.fillna(0.0)
    flux.name = "methane_emissions"
    return flux


def _cwns_to_wgs84(cwns: pd.DataFrame, cwns_yr: int) -> pd.DataFrame:
    """Return CWNS rows with lon/lat in EPSG:4326.

    The 2022 survey is uniformly NAD83; the 2012 survey mixes datums per row and
    the R projects each group separately before combining.
    """
    import geopandas as gpd

    df = cwns[cwns["LONGITUDE"].notna() & cwns["LATITUDE"].notna()].copy()

    if cwns_yr == 2022:
        groups = [(df, _CWNS_DEFAULT_DATUM)]
    else:
        datum = df["HORIZONTAL_COORDINATE_DATUM"]
        groups = [
            (df[datum == name], epsg) for name, epsg in _CWNS_DATUMS.items()
        ]
        known = datum.isin(_CWNS_DATUMS)
        groups.append((df[~known], _CWNS_DEFAULT_DATUM))

    out = []
    for part, epsg in groups:
        if not len(part):
            continue
        gdf = gpd.GeoDataFrame(
            part,
            geometry=gpd.points_from_xy(part["LONGITUDE"], part["LATITUDE"]),
            crs=epsg,
        )
        if epsg != "epsg:4326":
            gdf = gdf.to_crs("epsg:4326")
        part = part.copy()
        part["LONGITUDE"] = gdf.geometry.x.to_numpy()
        part["LATITUDE"] = gdf.geometry.y.to_numpy()
        out.append(part)

    return pd.concat(out, ignore_index=True)


def _moore_emissions(flow_mgd: pd.Series) -> pd.Series:
    """Moore et al. log-log flow→emission relationship. Returns mol/s.

    Zero/negative flow gives ``log10 -> -inf`` and hence 0 emission, matching R.
    """
    flow_m3s = flow_mgd * _MGD_TO_M3S
    with np.errstate(divide="ignore", invalid="ignore"):
        log_g_s = _MOORE_SLOPE * np.log10(flow_m3s) + _MOORE_INTERCEPT
    return (10.0**log_g_s) / _CH4


def _municipal_csv(points: pd.DataFrame, source: str, methods: list[str]) -> pd.DataFrame:
    """Assemble the per-facility municipal CSV in the R's column order."""
    out = pd.DataFrame(
        {
            "Facility_name": points["Facility_name"],
            "Million_gallons_per_day_flow": points["flow"],
            "GHGI_emissions_mol_per_s": points.get("emiss_GHGI", np.nan),
            "Moore_Emissions_mol_per_s": points.get("emiss_Moore", np.nan),
            "longitude": points["longitude"],
            "latitude": points["latitude"],
        }
    )
    out["Source"] = {
        "CWNS": "Clean Watershed Needs Survey",
        "DMR": "Discharge Monitoring Reports",
    }[source]
    # R drops whichever method column is unused
    unused = {
        "GHGI": "GHGI_emissions_mol_per_s",
        "Moore": "Moore_Emissions_mol_per_s",
    }
    drop = [col for method, col in unused.items() if method not in methods]
    return out.drop(columns=drop)


def _to_crs(gdf, dst):
    """Domain (GeoDataFrame or bbox tuple) as polygons in ``dst``."""
    return geo.as_polygons(gdf, dst)


def septic_state_info(
    *,
    state_septic_1990: pd.DataFrame,
    reported_state_info: pd.DataFrame,
    national_info: pd.DataFrame,
    state_population: pd.DataFrame,
    state_lookup: pd.DataFrame,
    states: list[str],
    inventory_year: int,
    ghgi_data_yr: int,
) -> tuple[pd.DataFrame, float]:
    """Per-state septic fraction + population, and the national 1990→now scaling.

    States that report to the American Housing Survey (within ±1 year, since it's
    biennial) use their reported fraction; the rest scale their 1990 census
    fraction by the change in the *national* septic fraction since 1990.
    """
    info = state_septic_1990.sort_values("State").copy()
    info["Method"] = "scaled"

    reported = reported_state_info[
        reported_state_info["Year"].between(inventory_year - 1, inventory_year + 1)
        & reported_state_info["State"].isin(states)
    ]
    if len(reported):
        # average the two surrounding years when both are present
        reported = reported.groupby("State", as_index=False).mean(numeric_only=True)
        idx = info["State"].isin(reported["State"])
        lookup = reported.set_index("State")["Septic_Fraction"]
        info.loc[idx, "Septic_Fraction"] = info.loc[idx, "State"].map(lookup)
        info.loc[idx, "Method"] = "reported"

    # national: keep 1990 plus the nearest available current year, collapsing
    # multiple current-year rows to their mean
    nat = national_info
    if nat["Year"].max() < inventory_year - 1:
        current = nat[nat["Year"] == nat["Year"].max()]
    else:
        current = nat[nat["Year"].between(inventory_year - 1, inventory_year + 1)]
    base = nat[nat["Year"] == 1990]["Septic_Fraction"].iloc[0]
    national_scale = float(current["Septic_Fraction"].mean()) / float(base)

    info = info[info["State"].isin(states)].sort_values("State").copy()

    # population joins census (by state NAME) to the tigerlines, ordered by STUSPS
    # so it lines up with `info`, which is sorted by the same code.
    pop = state_population.merge(state_lookup[["NAME", "STUSPS"]], on="NAME")
    pop = pop[pop["STUSPS"].isin(states)].sort_values("STUSPS")
    info["Population"] = pop[f"POPESTIMATE{ghgi_data_yr}"].to_numpy()

    info["Updated_septic_frac"] = np.where(
        info["Method"] == "reported",
        info["Septic_Fraction"],
        info["Septic_Fraction"] * national_scale,
    )
    return info, national_scale


def _septic_to_domain(
    flux: xr.DataArray,
    *,
    nlcd_domain,
    domain,
    domain_template: xr.DataArray,
    domain_crs: str,
) -> xr.DataArray:
    """Septic-specific tail: NLCD grid -> domain grid, then NA outside the domain.

    The reprojection itself is the shared partial-coverage sequence (identical to
    ``save_data`` in Stationary_combustion.R); only the trailing NA step below is
    specific to septic.
    """
    flux = geo.project_partial_to_grid(flux, nlcd_domain, domain_template)

    # averaging leaves small non-zero values just outside a polygon domain; the R
    # NAs out only the cells that are *both* outside and exactly zero, keeping any
    # real signal that bled across the boundary.
    outside = geo.mask_geometries(flux, _to_crs(domain, domain_crs), updatevalue=np.nan)
    flux = flux.where(~(outside.isnull() & (flux == 0)), np.nan)
    flux.name = "methane_emissions"
    return flux


def compute_septic(
    *,
    suburbia: xr.DataArray,
    septic_epa_emiss: float,
    total_national_area: float,
    state_info: pd.DataFrame | None,
    state_total_areas: pd.DataFrame | None,
    state_tigerlines,
    domain,
    domain_template: xr.DataArray,
    domain_crs: str,
    ghgi_ef: float,
    national: bool = True,
    by_state: bool = False,
) -> dict[str, xr.DataArray]:
    """Septic-system flux from NLCD 'developed open/low-intensity' land cover.

    Two ways to get an emission factor for that land cover:

    * ``national`` — spread the GHGI national septic total evenly over the
      national area of this land cover (mol/s/km²).
    * ``by_state`` — per state, ``population × septic_fraction × GHGI_EF`` divided
      by that state's area of the land cover.

    Either way the factor multiplies the fractional-cover raster and is projected
    onto the domain grid.
    """
    nlcd_crs = suburbia.rio.crs
    nlcd_states = _to_crs(state_tigerlines, nlcd_crs)
    dom = _to_crs(domain, domain_crs)

    # The R has a separate branch for domains much larger than the state set
    # (CONUS), where reprojecting the domain polygon is unstable.
    ratio = np.array(dom.total_bounds) / np.array(_to_crs(state_tigerlines, domain_crs).total_bounds)
    if np.any(np.abs(ratio) > 1.1):
        raise NotImplementedError(
            "septic: the large-domain (CONUS) branch of R/Wastewater.R is not ported; "
            "the domain must not greatly exceed its state set"
        )

    nlcd_domain = _to_crs(dom, nlcd_crs)

    out: dict[str, xr.DataArray] = {}

    if national:
        # mol/s/km2, then mol/km2/s -> nmol/m2/s
        flux = suburbia * septic_epa_emiss / total_national_area
        flux = flux * 1e9 * 1e-6
        out["Wastewater_dom_septic_national"] = _septic_to_domain(
            flux,
            nlcd_domain=nlcd_domain,
            domain=dom,
            domain_template=domain_template,
            domain_crs=domain_crs,
        )

    if by_state:
        if state_info is None or state_total_areas is None:
            raise ValueError("by-state septic needs state_info and state_total_areas")

        areas = state_total_areas.sort_values("X")
        info = state_info.sort_values("State")
        # GHGI EF is g CH4/capita/day -> mol/s, per km2 of this land cover
        ef = (
            info["Population"].to_numpy()
            * info["Updated_septic_frac"].to_numpy()
            * ghgi_ef
            / (_CH4 * 24 * 60 * 60)
            / areas["open_or_low_int_area"].to_numpy()
        )
        states = nlcd_states.merge(
            pd.DataFrame({"STUSPS": info["State"].to_numpy(), "Combined_Septic_EF": ef}),
            on="STUSPS",
            how="right",
        )

        # R rasterizes the states at 200 m over the *national* NLCD extent and
        # aggregates 5x back to 1 km, so border cells get a blended factor. Doing
        # that nationally would be a 375M-cell grid, so we build the 200 m grid
        # over the cropped window instead: crop_snap_out preserves 1 km alignment,
        # so the 5x5 aggregation blocks land identically and the result is the same.
        window = geo.crop_snap_out(suburbia, tuple(nlcd_domain.total_bounds))
        wxmin, wymin, wxmax, wymax = geo.ext(window)
        fine = geo.make_grid((wxmin, wymin, wxmax, wymax), 200.0, nlcd_crs)
        fine = geo.rasterize(states, fine, field="Combined_Septic_EF", touches=True)
        fine = fine.fillna(0.0)
        state_ef = geo.aggregate(fine, 5, fun="mean")

        flux = window * state_ef.values
        flux = flux * 1e9 * 1e-6
        out["Wastewater_dom_septic_bystate"] = _septic_to_domain(
            flux,
            nlcd_domain=nlcd_domain,
            domain=dom,
            domain_template=domain_template,
            domain_crs=domain_crs,
        )

    return out


def compute_wastewater(
    *,
    ghgrp_wastewater: pd.DataFrame,
    ghgrp_facility_data: pd.DataFrame,
    ghgi_wastewater_data: pd.DataFrame,
    ghgi_data_yr: int,
    domain_template: xr.DataArray,
    domain: Any,
    domain_crs: str = "epsg:4326",
    cwns: pd.DataFrame | None = None,
    cwns_yr: int = 2022,
    dmr: pd.DataFrame | None = None,
    use_cwns: bool = False,
    use_dmr: bool = True,
    method_ghgi: bool = False,
    method_moore: bool = True,
    septic: dict[str, xr.DataArray] | None = None,
) -> dict[str, Any]:
    """Compute wastewater flux rasters.

    Returns ``{name: DataArray}`` for each enabled municipal variant
    (``Wastewater_<source>_<method>_dom_central``), the industrial raster
    (``Wastewater_ind``), and — when ``septic`` is supplied — one sector total per
    source × method × septic combination. Two CSV tables are returned under the
    ``_csv`` keys.
    """
    ghgi = ghgi_wastewater_data
    row = ghgi[ghgi["year"] == ghgi_data_yr]
    central_emiss = float(row["Nonseptic Emissions"].iloc[0]) * _KT_TO_MOL_S  # mol/s

    methods = [m for m, on in (("GHGI", method_ghgi), ("Moore", method_moore)) if on]
    sources: dict[str, pd.DataFrame] = {}

    # --- municipal: normalise CWNS / DMR to a common (name, flow, lon, lat) frame -- #
    if use_cwns:
        if cwns is None:
            raise ValueError("Wastewater_use_CWNS is set but no CWNS table was provided")
        wgs = _cwns_to_wgs84(cwns, cwns_yr)
        sources["CWNS"] = pd.DataFrame(
            {
                "Facility_name": wgs["FACILITY_NAME"],
                "flow": pd.to_numeric(wgs["EXIST_MUNICIPAL"], errors="coerce"),
                "longitude": wgs["LONGITUDE"],
                "latitude": wgs["LATITUDE"],
            }
        )
    if use_dmr:
        if dmr is None:
            raise ValueError("Wastewater_use_DMR is set but no DMR table was provided")
        valid = dmr[dmr["Facility_Longitude"].notna() & dmr["Facility_Latitude"].notna()]
        sources["DMR"] = pd.DataFrame(
            {
                "Facility_name": valid["Facility_Name"],
                "flow": pd.to_numeric(valid["Average_Daily_Flow__MGD_"], errors="coerce"),
                "longitude": valid["Facility_Longitude"],
                "latitude": valid["Facility_Latitude"],
            }
        )

    out: dict[str, Any] = {}
    municipal: dict[tuple[str, str], xr.DataArray] = {}
    csv_parts: list[pd.DataFrame] = []

    for source, facilities in sources.items():
        # The GHGI split uses the *national* flow total, computed before the
        # domain crop — so a facility's share doesn't depend on the domain.
        total_flow = facilities["flow"].sum(skipna=True)

        emissions = pd.DataFrame(index=facilities.index)
        if method_ghgi:
            emissions["emiss_GHGI"] = central_emiss * facilities["flow"] / total_flow
        if method_moore:
            emissions["emiss_Moore"] = _moore_emissions(facilities["flow"])

        pts = _project_points(
            pd.concat([facilities, emissions], axis=1),
            "longitude",
            "latitude",
            domain_crs,
            keep=["Facility_name", "flow", "longitude", "latitude", *emissions.columns],
        )
        pts = _clip_to_domain(pts, domain, domain_crs)

        for method in methods:
            flux = _flux_from_points(pts.rename(columns={f"emiss_{method}": "emiss"}), domain_template)
            municipal[(source, method)] = flux
            out[f"Wastewater_{source}_{method}_dom_central"] = flux

        csv_parts.append(_municipal_csv(pts, source, methods))

    if csv_parts:
        municipal_csv = pd.concat(csv_parts, ignore_index=True)
        municipal_csv = municipal_csv.sort_values("Facility_name", kind="stable")
    else:
        municipal_csv = pd.DataFrame()
    out["_csv_municipal"] = municipal_csv

    # --- industrial: GHGRP subpart II point sources ------------------------- #
    ghgrp_data = make_consistent(ghgrp_wastewater)
    all_data = ghgrp_facility_data.merge(
        ghgrp_data, on=["facility_id", "year"], how="inner", suffixes=("", "_emis")
    )
    ind = all_data[all_data["year"] == ghgi_data_yr].copy()
    for col in ("latitude", "longitude", "ghg_quantity"):
        ind[col] = pd.to_numeric(ind[col], errors="coerce")
    ind["emiss"] = ind["ghg_quantity"] * _MT_TO_MOL_S

    ind_pts = _project_points(
        ind,
        "longitude",
        "latitude",
        domain_crs,
        keep=["facility_id", "facility_name", "state", "emiss", "longitude", "latitude"],
    )
    ind_pts = _clip_to_domain(ind_pts, domain, domain_crs)

    ind_flux = _flux_from_points(ind_pts, domain_template)
    out["Wastewater_ind"] = ind_flux

    ind_csv = ind_pts.rename(
        columns={
            "facility_id": "GHGRP_ID",
            "emiss": "Emissions_mol_per_s",
        }
    )[["GHGRP_ID", "facility_name", "state", "Emissions_mol_per_s", "longitude", "latitude"]]
    out["_csv_industrial"] = ind_csv.sort_values("facility_name", kind="stable")

    # --- septic + sector totals: municipal + septic + industrial ------------- #
    _SEPTIC_FILES = {
        "national": "Wastewater_dom_septic_national",
        "state": "Wastewater_dom_septic_bystate",
    }
    for kind, flux in (septic or {}).items():
        out[_SEPTIC_FILES[kind]] = flux

    for (source, method), muni in municipal.items():
        for kind, septic_flux in (septic or {}).items():
            total = muni.fillna(0) + septic_flux.fillna(0) + ind_flux.fillna(0)
            total.name = "methane_emissions"
            out[f"Wastewater_sector_total_{source}_{method}_{kind}"] = total

    return out


@dataclass
class WastewaterSector:
    """RunContext wrapper: read shared inputs, compute, write NetCDF + CSV outputs."""

    key: str = "wastewater"
    name: str = "Wastewater"
    process_flag: str = "Process_wastewater"

    def _septic(self, ctx: RunContext) -> dict[str, xr.DataArray]:
        """Septic rasters, keyed ``national`` / ``bystate`` (as the totals name them)."""
        from .. import datasets

        cfg = ctx.config
        if not (cfg.Wastewater_national_septic or cfg.Wastewater_state_septic):
            return {}

        ghgi = cfg.GHGI_wastewater_data
        row = ghgi[ghgi["year"] == ctx.shared["ghgi_data_yr"]]
        states = ctx.shared["state_name_list"]

        info, _ = septic_state_info(
            state_septic_1990=datasets.load("Wastewater_1990_state_septic"),
            reported_state_info=cfg.Wastewater_reported_State_info,
            national_info=cfg.National_wastewater_info,
            state_population=ctx.shared["state_population"],
            state_lookup=ctx.shared["state_tigerlines"],
            states=states,
            inventory_year=ctx.inventory_year,
            ghgi_data_yr=ctx.shared["ghgi_data_yr"],
        )

        septic = compute_septic(
            suburbia=ctx.shared["nlcd_suburbia"],
            septic_epa_emiss=float(row["Septic Emissions"].iloc[0]) * _KT_TO_MOL_S,
            total_national_area=ctx.shared["septic_total_national_area"],
            state_info=info,
            state_total_areas=ctx.shared["septic_state_areas"],
            state_tigerlines=ctx.shared["state_tigerlines"],
            domain=ctx.domain,
            domain_template=ctx.domain_template,
            domain_crs=ctx.domain_crs,
            ghgi_ef=float(row["EF"].iloc[0]),
            national=cfg.Wastewater_national_septic,
            by_state=cfg.Wastewater_state_septic,
        )
        # re-key to the suffixes the sector-total filenames use (R: _national / _state)
        return {
            kind: septic[name]
            for kind, name in (
                ("national", "Wastewater_dom_septic_national"),
                ("state", "Wastewater_dom_septic_bystate"),
            )
            if name in septic
        }

    def run(self, ctx: RunContext) -> None:
        from .. import datasets

        cfg = ctx.config

        cwns = cwns_yr = None
        if cfg.Wastewater_use_CWNS:
            if cfg.Source_CWNS != "M3T":
                raise NotImplementedError(
                    "Source_CWNS: only the packaged 'M3T' tables are ported so far"
                )
            # packaged CWNS comes in two vintages; take whichever is nearer the run year
            cwns_yr = min((2012, 2022), key=lambda y: abs(ctx.inventory_year - y))
            cwns = datasets.load(f"CWNS_{cwns_yr}")

        dmr = ctx.shared["dmr_data"] if cfg.Wastewater_use_DMR else None

        septic = self._septic(ctx) or None

        results = compute_wastewater(
            ghgrp_wastewater=datasets.load("GHGRP_wastewater"),
            ghgrp_facility_data=ctx.shared["ghgrp_facility_data"],
            ghgi_wastewater_data=cfg.GHGI_wastewater_data,
            ghgi_data_yr=ctx.shared["ghgi_data_yr"],
            domain_template=ctx.domain_template,
            domain=ctx.domain,
            domain_crs=ctx.domain_crs,
            cwns=cwns,
            cwns_yr=cwns_yr or 2022,
            dmr=dmr,
            use_cwns=cfg.Wastewater_use_CWNS,
            use_dmr=cfg.Wastewater_use_DMR,
            method_ghgi=cfg.Wastewater_Municipal_Method_GHGI,
            method_moore=cfg.Wastewater_Municipal_Method_Moore,
            septic=septic,
        )

        rasters = {k: v for k, v in results.items() if isinstance(v, xr.DataArray)}
        for name, da in rasters.items():
            if name.startswith("Wastewater_sector_total"):
                ctx.write_output(da, f"{name}.nc")
            else:
                ctx.write_output(da, f"{name}.nc", subdir="Wastewater")

        out_dir = ctx.output_directory / "Wastewater"
        out_dir.mkdir(parents=True, exist_ok=True)
        results["_csv_municipal"].to_csv(
            out_dir / "Municipal_watewater_treatment.csv", index=False
        )
        results["_csv_industrial"].to_csv(
            out_dir / "GHGRP_industrial_watewater_treatment.csv", index=False
        )

        # The combine step reads one raster per sector. Several method variants can
        # be enabled at once, so pick the first sector total in R's own preference
        # order (Moore over GHGI -- the R does the same when labelling plots).
        totals = [k for k in rasters if k.startswith("Wastewater_sector_total")]
        primary = next(
            (
                f"Wastewater_sector_total_{s}_{m}_{k}"
                for s in ("CWNS", "DMR")
                for m in ("Moore", "GHGI")
                for k in ("national", "state")
                if f"Wastewater_sector_total_{s}_{m}_{k}" in rasters
            ),
            None,
        )
        if primary is None:
            raise RuntimeError(f"no wastewater sector total was produced (have {totals})")
        ctx.write_output(rasters[primary], f"{self.key}.nc")

        ctx.shared.setdefault("sector_results", {})[self.key] = results


def register() -> None:
    from . import base

    base.register(WastewaterSector())
