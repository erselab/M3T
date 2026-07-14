"""Stationary combustion sector. Port of ``R/Stationary_combustion.R``.

Fuel burned in buildings and power plants. Nothing measures its methane directly,
so it is built from three datasets, each supplying one piece:

1. **How much fuel each state burns** — EIA SEDS, by sector (residential,
   commercial, industrial, electric) x fuel (coal, gas, petroleum, wood). Scaled
   so the national total matches the GHGI, then turned into methane with an
   emission factor per sector-fuel. That gives 14 state totals in mol/s.
2. **Which county inside the state** — NEI county-level *carbon monoxide* from the
   same sector-fuel. CO is a combustion tracer; only the county/state ratio is
   used, so its units never matter.
3. **Where inside the county** — a gridded sectoral *CO2* inventory (Vulcan or
   ACES), via :mod:`m3t.disaggregation`.

Emissions can be split from state totals (``by_state``) or from a single domain
total (``by_domain``); both can run, giving separate outputs.

Outputs: one raster per subsector x level x inventory (up to 14 each), plus a
fossil-fuel total (coal + gas + petroleum) and a wood total per level/inventory.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd
import xarray as xr

from .. import geo
from ..context import RunContext
from ..disaggregation import county_cover_weights, inventory_based_disaggregation

_SEC_PER_YR = 365 * 24 * 60 * 60
_CH4 = 16.043
_TBTU_TO_GJ = 1e9 / 947.8170777491506  # trillion Btu -> GJ

# Higher- to lower-heating-value conversion, per fuel (R hardcodes these).
_HHV_TO_LHV = {"coal": 0.95, "petr": 0.95, "gas": 0.90, "wood": 0.90}

# SEDS series ids, in the order the R's reshape produces them.
_SEDS_SERIES = [
    "CLCCB", "CLEIB", "CLICB", "NGCCB", "NGEIB", "NGICB", "PACCB", "PAEIB",
    "PAICB", "PARCB", "WDRCB", "WWCCB", "WWEIB", "WWICB",
]
_SEDS_COLUMNS = [
    "com_coal", "elec_coal", "ind_coal", "com_gas", "elec_gas", "ind_gas",
    "com_petr", "elec_petr", "ind_petr", "res_petr", "res_wood", "com_wood",
    "elec_wood", "ind_wood",
]

# The 14 sector-fuel combinations that exist (no residential coal in the US; and
# residential gas lives in the NG-distribution sector, not here).
SUBSECTORS = {
    "res": ["res_petr_ER", "res_wood_ER"],
    "com": ["com_coal_ER", "com_petr_ER", "com_gas_ER", "com_wood_ER"],
    "ind": ["ind_coal_ER", "ind_petr_ER", "ind_gas_ER", "ind_wood_ER"],
    "elec": ["elec_coal_ER", "elec_petr_ER", "elec_gas_ER", "elec_wood_ER"],
}
ALL_SUBSECTORS = [t for group in SUBSECTORS.values() for t in group]

# NEI sector names -> our sector_fuel keys. Some map to keys we never use
# (residential gas, and the "Other" fuels), but the R names them all anyway.
_NEI_SECTOR_MAP = {
    "Fuel Comb - Comm/Institutional - Biomass": "com_wood_ER",
    "Fuel Comb - Comm/Institutional - Coal": "com_coal_ER",
    "Fuel Comb - Comm/Institutional - Natural Gas": "com_gas_ER",
    "Fuel Comb - Comm/Institutional - Oil": "com_petr_ER",
    "Fuel Comb - Comm/Institutional - Other": "com_other",
    "Fuel Comb - Electric Generation - Biomass": "elec_wood_ER",
    "Fuel Comb - Electric Generation - Coal": "elec_coal_ER",
    "Fuel Comb - Electric Generation - Natural Gas": "elec_gas_ER",
    "Fuel Comb - Electric Generation - Oil": "elec_petr_ER",
    "Fuel Comb - Electric Generation - Other": "elec_other",
    "Fuel Comb - Industrial Boilers, ICEs - Biomass": "ind_wood_ER",
    "Fuel Comb - Industrial Boilers, ICEs - Coal": "ind_coal_ER",
    "Fuel Comb - Industrial Boilers, ICEs - Natural Gas": "ind_gas_ER",
    "Fuel Comb - Industrial Boilers, ICEs - Oil": "ind_petr_ER",
    "Fuel Comb - Industrial Boilers, ICEs - Other": "ind_other",
    "Fuel Comb - Residential - Natural Gas": "res_gas",
    "Fuel Comb - Residential - Oil": "res_petr_ER",
    "Fuel Comb - Residential - Other": "res_other",
    "Fuel Comb - Residential - Wood": "res_wood_ER",
}
REQUIRED_NEI_SECTORS = list(_NEI_SECTOR_MAP)


def prepare_seds(
    eia_seds: pd.DataFrame,
    ghgi: pd.DataFrame,
    emission_factors: pd.DataFrame,
    *,
    seds_yr: int,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """State and "domain" CH4 totals (mol/s) per subsector, from SEDS energy use.

    SEDS reports energy consumed per state/sector/fuel. The national SEDS total is
    reconciled against the GHGI (``GHGI_national / SEDS_national``) and every state
    scaled by that ratio, so the sum matches the national inventory. Energy then
    becomes methane via the per-sector-fuel emission factor.

    **No state filtering**, deliberately. With the packaged data the R keeps every
    state here, so its "domain" total is really the *national* total (and likewise
    the NEI county shares in :func:`prepare_nei` are national). Restricting either
    one to the run's states — the obvious-looking thing to do — silently changes
    every ``by_domain`` output. See the note in :func:`prepare_nei`.

    Returns ``(state_total_ch4, domain_total_ch4)`` in long form.
    """
    seds = eia_seds[eia_seds["period"] == seds_yr]

    wide = seds.pivot_table(
        index="stateId", columns="seriesId", values="value", aggfunc="first"
    )
    missing = [s for s in _SEDS_SERIES if s not in wide.columns]
    if missing:
        raise ValueError(f"SEDS is missing series {missing} for {seds_yr}")
    wide = wide[_SEDS_SERIES]
    wide.columns = _SEDS_COLUMNS
    wide = wide.reset_index().rename(columns={"stateId": "State"})
    wide["State"] = wide["State"].replace({"US": "US_SEDS"})

    # SEDS is billion Btu; the GHGI is trillion Btu
    fuel_cols = _SEDS_COLUMNS
    wide[fuel_cols] = wide[fuel_cols].astype("float64") / 1000.0

    ghgi_yr = ghgi[ghgi["year"] == seds_yr].drop(columns="year")
    stat = pd.concat([wide, ghgi_yr], ignore_index=True).sort_values("State")

    # match R's rounding: the two national rows to whole TBtu, everything to 1 dp
    is_seds_national = stat["State"] == "US_SEDS"
    stat.loc[is_seds_national, fuel_cols] = stat.loc[is_seds_national, fuel_cols].round(0)
    stat[fuel_cols] = stat[fuel_cols].round(1)

    epa = stat.loc[stat["State"] == "US_EPA", fuel_cols].iloc[0]
    seds_national = stat.loc[stat["State"] == "US_SEDS", fuel_cols].iloc[0]
    ratio = epa / seds_national

    adj = stat[~stat["State"].isin(["US_EPA", "US_SEDS"])].copy()
    adj[fuel_cols] = adj[fuel_cols].mul(ratio, axis=1)

    # energy (TBtu) -> CH4 (mol/s)
    for col in fuel_cols:
        fuel = col.split("_")[1]
        ef = float(emission_factors[col].iloc[0])
        adj[f"{col}_ER"] = (
            adj[col] * _HHV_TO_LHV[fuel] * _TBTU_TO_GJ * ef / (_CH4 * _SEC_PER_YR)
        )

    state_total = adj.melt(
        id_vars="State",
        value_vars=ALL_SUBSECTORS,
        var_name="Sector",
        value_name="state_ch4_emiss",
    )
    domain_total = (
        state_total.groupby("Sector", as_index=False)["state_ch4_emiss"]
        .sum()
        .rename(columns={"state_ch4_emiss": "domain_ch4_emiss"})
    )
    return state_total, domain_total


def prepare_nei(nei: pd.DataFrame, *, nei_year: int) -> pd.DataFrame:
    """County shares of state (and "domain") CO emissions, per subsector.

    CO is only a spatial proxy, so the output is a *fraction*, not an emission.
    Three wrinkles the R has and we keep:

    * county/sector combinations absent from the NEI mean zero, not missing, and
      must be materialised or they silently vanish from the merge;
    * a state/sector whose CO total is zero cannot be split proportionally, so its
      counties get an equal share each (the CO2 inventory still decides *where*
      inside each county);
    * **every US county is kept**, not just the run's. With the packaged NEI the R
      applies no state filter, so ``emiss_frac_domain`` is a county's share of the
      *national* CO total, and ``by_domain`` therefore disaggregates the national
      total. (Its ``"download"`` branch *does* filter to the run's states, so the
      same config yields different by_domain numbers depending on where the NEI
      came from — an inconsistency in the R. We reproduce the packaged path, which
      is the one M3T ships.) ``by_state`` is unaffected either way, since
      within-state shares don't depend on the other states.
    """
    nei = nei[nei["INVENTORY YEAR"] == nei_year]
    nei = nei[["SECTOR", "STATE", "STATE FIPS", "COUNTY FIPS", "EMISSIONS"]].rename(
        columns={
            "STATE FIPS": "STATE_FIPS",
            "COUNTY FIPS": "COUNTY_FIPS",
            "EMISSIONS": "CO_EMISSIONS",
        }
    )

    # materialise the full county x required-sector grid (missing == zero)
    counties = nei[["STATE", "STATE_FIPS", "COUNTY_FIPS"]].drop_duplicates()
    grid = counties.merge(pd.DataFrame({"SECTOR": REQUIRED_NEI_SECTORS}), how="cross")
    nei = grid.merge(
        nei, on=["STATE", "STATE_FIPS", "COUNTY_FIPS", "SECTOR"], how="left"
    )
    nei["CO_EMISSIONS"] = nei["CO_EMISSIONS"].fillna(0.0)

    by_state = nei.groupby(["SECTOR", "STATE_FIPS"])["CO_EMISSIONS"]
    by_domain = nei.groupby("SECTOR")["CO_EMISSIONS"]

    state_sum = by_state.transform("sum")
    domain_sum = by_domain.transform("sum")
    state_count = by_state.transform("size")
    domain_count = by_domain.transform("size")

    # zero CO in the whole state/domain for this sector -> spread evenly
    nei["emiss_frac"] = np.where(
        state_sum > 0, nei["CO_EMISSIONS"] / state_sum.where(state_sum > 0, 1), 1 / state_count
    )
    nei["emiss_frac_domain"] = np.where(
        domain_sum > 0,
        nei["CO_EMISSIONS"] / domain_sum.where(domain_sum > 0, 1),
        1 / domain_count,
    )

    nei["SECTOR"] = nei["SECTOR"].map(_NEI_SECTOR_MAP)
    return nei


def county_emissions(
    nei: pd.DataFrame, state_total: pd.DataFrame, domain_total: pd.DataFrame
) -> pd.DataFrame:
    """County CH4 (mol/s) per subsector, wide, for both disaggregation levels."""
    merged = nei.merge(
        state_total, left_on=["STATE", "SECTOR"], right_on=["State", "Sector"]
    ).merge(domain_total, left_on="SECTOR", right_on="Sector")

    merged["bystate"] = merged["state_ch4_emiss"] * merged["emiss_frac"]
    merged["bydomain"] = merged["domain_ch4_emiss"] * merged["emiss_frac_domain"]

    wide = merged.pivot_table(
        index=["STATE_FIPS", "COUNTY_FIPS"],
        columns="SECTOR",
        values=["bystate", "bydomain"],
        aggfunc="first",
    ).fillna(0.0)
    wide.columns = [f"{lvl}.{sector}" for lvl, sector in wide.columns]
    return wide.reset_index()


def compute_stationary_combustion(
    *,
    inventories: dict[str, xr.DataArray],
    county_ch4: pd.DataFrame,
    county_tigerlines,
    domain,
    domain_template: xr.DataArray,
    domain_crs: str,
    inventory_name: str,
    by_state: bool = True,
    by_domain: bool = False,
    mass_conserving: bool = True,
    verbose: bool = False,
) -> dict[str, xr.DataArray]:
    """Disaggregate county CH4 onto the domain grid using a CO2 inventory.

    ``inventories`` maps ``res``/``com``/``ind``/``elec`` to that sector's gridded
    CO2 (all on one grid). Returns ``{raster_name: DataArray}`` in nmol/m²/s,
    named as the R does: ``stat_comb_<sector>_<fuel>_by<level>_<inventory>``.
    """
    import geopandas as gpd

    inv_crs = next(iter(inventories.values())).rio.crs

    counties = county_tigerlines.merge(
        county_ch4,
        left_on=["STATEFP", "COUNTYFP"],
        right_on=["STATE_FIPS", "COUNTY_FIPS"],
        how="inner",
    ).sort_values(["STATEFP", "COUNTYFP"])

    dom = geo.as_polygons(domain, domain_crs)
    # keep only counties at least partly inside the domain, then move to the CO2 grid
    counties = gpd.clip(counties, dom)
    counties = counties[~counties.geometry.is_empty].to_crs(inv_crs)

    # The R runs on the full CONUS-wide inventory, relying on terra's extract to
    # touch only what each polygon covers. We crop to the domain first (with a
    # margin for the later zero-buffer), which is numerically identical -- every
    # cell outside the counties is zero-weighted, and crop_snap_out preserves grid
    # alignment -- but keeps the sub-sampled coverage maths tractable.
    box_bounds = geo.ext(geo.project_to_crs(domain_template, inv_crs))
    margin = 20 * max(geo.res(next(iter(inventories.values()))))
    window = (
        min(box_bounds[0], counties.total_bounds[0]) - margin,
        min(box_bounds[1], counties.total_bounds[1]) - margin,
        max(box_bounds[2], counties.total_bounds[2]) + margin,
        max(box_bounds[3], counties.total_bounds[3]) + margin,
    )
    inventories = {k: geo.crop_snap_out(v, window) for k, v in inventories.items()}
    grid = next(iter(inventories.values()))

    # one coverage raster per county, reused across every sector and level
    covers = county_cover_weights(grid, counties)

    # the R reprojects onto the *bounding box* of the domain in the inventory CRS
    # (not the domain polygon) -- see save_data()
    box = gpd.GeoDataFrame(
        geometry=gpd.GeoSeries.from_wkt([_bbox_wkt(box_bounds)], crs=inv_crs),
        crs=inv_crs,
    )

    levels = [lvl for lvl, on in (("state", by_state), ("domain", by_domain)) if on]
    out: dict[str, xr.DataArray] = {}

    for level in levels:
        for sector, totals in SUBSECTORS.items():
            cols = {t: f"by{level}.{t}" for t in totals}
            frame = counties.copy()
            for t, col in cols.items():
                frame[t] = frame[col] if col in frame else np.nan

            ch4 = inventory_based_disaggregation(
                inventories[sector],
                totals,
                frame,
                covers,
                progress=f"{inventory_name} {sector} by{level}" if verbose else None,
            )
            for total, da in ch4.items():
                flux = _to_flux(da, box, domain_template, mass_conserving=mass_conserving)
                flux.name = "methane_emissions"
                fuel = total.replace("_ER", "")
                out[f"stat_comb_{fuel}_by{level}_{inventory_name}"] = flux

    # fossil-fuel and wood sector totals, per level
    for level in levels:
        for label, keep in (
            ("fossil_fuel", ("coal", "gas", "petr")),
            ("wood", ("wood",)),
        ):
            parts = [
                da
                for name, da in out.items()
                if name.endswith(f"by{level}_{inventory_name}")
                and any(f"_{k}_" in name for k in keep)
            ]
            if not parts:
                continue
            total = sum(p.fillna(0.0) for p in parts)
            total.name = "methane_emissions"
            out[
                f"Stationary_combustion_sector_{label}_total_{inventory_name.upper()}_by{level}"
            ] = total

    return out


def _to_flux(
    ch4: xr.DataArray, box, domain_template: xr.DataArray, *, mass_conserving: bool
) -> xr.DataArray:
    """Disaggregated CH4 (mol/s per inventory pixel) -> flux on the domain grid.

    ``mass_conserving=True`` (the default) redistributes the mass conservatively and
    then divides by each target cell's true geodesic area, so the raster's total
    mass equals the county totals that went in.

    ``mass_conserving=False`` reproduces the R exactly, and is kept so the golden
    tests can still check parity. Two things make the R's version lose the books:

    * it treats each 1 km Vulcan/ACES pixel as exactly 1 km² (``* 1000`` to go from
      "mol/s per pixel" to nmol/m²/s). Those grids are Lambert Conformal Conic —
      *conformal*, not equal-area — so a "1 km" pixel is really ~1.009 km² at
      CT/RI's latitude, overstating the flux by ~0.9%;
    * it then area-*averages* the result onto the domain grid and weights cells by
      their coverage of the domain's bounding box, neither of which conserves mass.

    Together those inflate the gridded total by ~1.4% relative to the county totals.
    """
    if not mass_conserving:
        return geo.project_partial_to_grid(ch4, box, domain_template) * 1000.0

    mass = geo.project_mass_to_grid(ch4, domain_template)  # mol/s per target cell
    return mass * 1e9 / geo.cell_area(domain_template, unit="m")


def _bbox_wkt(bounds) -> str:
    xmin, ymin, xmax, ymax = bounds
    return (
        f"POLYGON(({xmin} {ymin},{xmax} {ymin},{xmax} {ymax},"
        f"{xmin} {ymax},{xmin} {ymin}))"
    )


@dataclass
class StationaryCombustionSector:
    """RunContext wrapper: read shared inputs, compute, write NetCDF outputs."""

    key: str = "stationary_combustion"
    name: str = "Stationary combustion"
    process_flag: str = "Process_stationary_combustion"

    def run(self, ctx: RunContext) -> None:
        from .. import datasets

        cfg = ctx.config
        seds_yr = ctx.shared["ghgi_data_yr"]

        state_total, domain_total = prepare_seds(
            datasets.load("EIA_SEDS"),
            datasets.load("GHGI_stationary_combustion"),
            cfg.stationary_combustion_emission_factors,
            seds_yr=seds_yr,
        )
        nei = prepare_nei(
            datasets.load("NEI_all_years"),
            nei_year=ctx.shared["nei_year"],
        )
        county_ch4 = county_emissions(nei, state_total, domain_total)

        results: dict[str, xr.DataArray] = {}
        for inv_name, key in (("aces", "Use_ACES"), ("vulcan", "Use_Vulcan")):
            if not getattr(cfg, key):
                continue
            results |= compute_stationary_combustion(
                inventories=ctx.shared[f"{inv_name}_inventories"],
                county_ch4=county_ch4,
                county_tigerlines=ctx.shared["county_tigerlines"],
                domain=ctx.domain,
                domain_template=ctx.domain_template,
                domain_crs=ctx.domain_crs,
                inventory_name=inv_name,
                by_state=cfg.stationary_combustion_by_state,
                by_domain=cfg.stationary_combustion_by_domain,
                mass_conserving=cfg.Mass_conserving_regrid,
                verbose=ctx.verbose,
            )

        for name, da in results.items():
            if name.startswith("Stationary_combustion_sector"):
                ctx.write_output(da, f"{name}.nc")
            else:
                ctx.write_output(da, f"{name}.nc", subdir="stationary_combustion")

        # the combine step reads one raster per sector: fossil fuel + wood
        totals = [
            da for n, da in results.items() if n.startswith("Stationary_combustion_sector")
        ]
        combined = sum(t.fillna(0.0) for t in totals)
        combined.name = "methane_emissions"
        ctx.write_output(combined, f"{self.key}.nc")

        ctx.shared.setdefault("sector_results", {})[self.key] = results


def register() -> None:
    from . import base

    base.register(StationaryCombustionSector())
