"""Natural gas distribution sector. Port of ``R/Natural_Gas_Distribution.R``.

The gas network between the city gate and the burner tip. Six leak types, each
built by multiplying an *activity count* by an *emission factor*:

======================  ===========================  =========================
subsector               activity (source)            emission factor
======================  ===========================  =========================
``mains``               miles of main, by material   Weller et al. (leaks/mile
                        (PHMSA)                      x mol/s/leak)
``serv``                number of services, by        GHGI
                        material (PHMSA)
``MnR``                 metering & regulating         GHGI, split by pressure
                        stations (GHGRP, see below)   and above/below grade
``meter``               customer meters (EIA)         GHGI
``upset``               miles of pipe (PHMSA)         GHGI (relief valves,
                                                      blowdowns, dig-ins)
``post_meter``          gas delivered (EIA)           config (residential only;
                                                      the commercial EF is 0)
======================  ===========================  =========================

Each is then split residential/commercial by that state's ratio of residential
to commercial *customers*, because the CO2 inventories used to place the
emissions on a grid are sectoral. That gives 12 rasters per level per inventory.

**M&R stations** are the awkward one: the GHGRP reports them only for the large
LDCs, and it reports a company's *headquarters*, which for a multi-state utility
is often the wrong state. So the R re-derives an operating state from the facility
name (``Atmos Energy Corporation - Kentucky`` operates in KY, not TX), computes
stations-per-mile among reporters in each state, and applies that rate to PHMSA's
mile counts. States with no reporters at all borrow the average of their
neighbours, iteratively, until every state has a rate.

Not ported: ``NG_distribution_by_LDC``, which reads the output of a separate
semi-manual R script (``NG_distribution_by_LDC_prep.R``) that is not part of the
package.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd
import xarray as xr

from .. import geo
from ..context import RunContext
from ..disaggregation import county_cover_weights, inventory_based_disaggregation

# The 49 states M3T covers (CONUS + DC), used to fill M&R gaps from neighbours.
FULL_STATE_LIST = [
    "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "IA", "ID",
    "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS",
    "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR",
    "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV",
    "WY",
]

SUBSECTORS = ["mains", "serv", "MnR", "meter", "upset", "post_meter"]
RES_TOTALS = [f"{s}_ER_total_res" for s in SUBSECTORS]
COM_TOTALS = [f"{s}_ER_total_com" for s in SUBSECTORS]

# M&R station types: above-grade ones are everything that isn't a vault.
_MNR_ABOVE = ["M&R >300", "M&R 100-300", "M&R <100", "Reg >300", "Reg 100-300",
              "Reg 40-100", "Reg <40"]
_MNR_BELOW = ["R-Vault >300", "R-Vault 100-300", "R-Vault 40-100"]

_PIPE_MATERIALS = ["bare_steel", "iron", "coat_steel", "plastic"]
_SERVICE_MATERIALS = ["unp_steel", "cp_steel", "plastic", "copper_iron"]


def resolve_ghgi_tables(ghgi: dict[str, pd.DataFrame], year: int) -> dict[str, pd.DataFrame]:
    """Pick the run year out of the year-columned GHGI NG-distribution tables."""
    col = str(year)
    mnr = pd.DataFrame(
        {
            "EF": ghgi["GHGI_MnR_EF"][col],
            "Total_stations": ghgi["GHGI_MnR_Activity"][col],
        }
    )
    return {
        "MnR": mnr,
        "services": ghgi["GHGI_services"][[col]].rename(columns={col: "EF"}),
        "meters": ghgi["GHGI_meters"][[col]].rename(columns={col: "EF"}),
        "maintenance": ghgi["GHGI_maintenance"][[col]].rename(columns={col: "EF"}),
    }


def _operating_state(facility_name: pd.Series, reported_state: pd.Series) -> pd.Series:
    """Re-derive the state an LDC actually operates in, from its name.

    GHGRP facilities report a headquarters, so ``Atmos Energy Corporation -
    Kentucky`` comes back as TX. The R splits the name on a handful of separators
    and matches the tail against state names/abbreviations; anything that doesn't
    match keeps the reported state.
    """
    import re

    split_re = re.compile(
        r"- |\(|of |NorthWestern Energy.? |Summit Utilities Inc\., |Duke Energy "
    )
    tail = facility_name.fillna("").map(lambda s: split_re.split(s)[-1])
    tail = tail.map(lambda s: s.split("-")[-1])
    for junk in (" LDC", " Gas Operation", " Gas Distribution", ")"):
        tail = tail.str.replace(junk, "", regex=False)

    abbr = {s: s for s in FULL_STATE_LIST}
    names = {
        n.lower(): a
        for n, a in zip(_STATE_NAMES, _STATE_ABBR)
    }

    out = reported_state.copy()
    hit_abbr = tail.isin(abbr)
    out[hit_abbr] = tail[hit_abbr]
    hit_name = tail.str.lower().isin(names)
    out[hit_name] = tail[hit_name].str.lower().map(names)
    return out


# R's datasets::state.abb / state.name (50 states; DC is not in them).
_STATE_ABBR = [
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID",
    "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
    "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
    "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
    "WI", "WY",
]
_STATE_NAMES = [
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
    "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
    "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
    "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming",
]


def stations_per_mile(
    ghgrp_facility: pd.DataFrame,
    ghgrp_subpartw: pd.DataFrame,
    ghgrp_ldc: pd.DataFrame,
    neighbours: pd.DataFrame,
    *,
    year: int,
) -> tuple[pd.Series, pd.Series]:
    """Above- and below-grade M&R stations per mile of main, per state.

    States with no GHGRP reporter take the mean rate of their neighbours; that can
    cascade (a state all of whose neighbours are also empty), so the R iterates
    until every state has a value. Returns two Series indexed by state.
    """
    w = ghgrp_subpartw[
        (ghgrp_subpartw["reporting_year"] == year)
        & (ghgrp_subpartw["industry_segment"] == "Natural gas distribution [98.230(a)(8)]")
    ]
    w = (
        w.groupby(["facility_id", "facility_name"], as_index=False)["total_reported_ch4_emissions"]
        .sum()
        .rename(columns={"total_reported_ch4_emissions": "Reported_CH4"})
    )
    w = w[w["Reported_CH4"] > 0]

    ldc = ghgrp_ldc[ghgrp_ldc["reporting_year"] == year].drop(columns="reporting_year")
    w = w.merge(ldc, on="facility_id")

    fac = ghgrp_facility[ghgrp_facility["year"] == year]
    csv = fac.merge(w, on="facility_id", suffixes=("", "_w"))
    csv["operating_state"] = _operating_state(csv["facility_name"], csv["state"])

    grouped = csv.groupby("operating_state")
    miles = grouped["Miles_of_Mains"].sum(min_count=1)
    above = grouped[
        ["N_of_above_grade_T_D_transfer_stations", "N_of_above_grade_non_T_D_MR_stations"]
    ].sum().sum(axis=1)
    below = grouped[
        ["N_of_below_grade_non_T_D_MR_stations", "N_of_below_grade_T_D_transfer_stations"]
    ].sum().sum(axis=1)

    rates = {
        "above": (above / miles).replace([np.inf, -np.inf], np.nan),
        "below": (below / miles).replace([np.inf, -np.inf], np.nan),
    }

    out = {}
    for key, rate in rates.items():
        rate = rate.reindex(FULL_STATE_LIST)
        for _ in range(11):  # the R caps the cascade at 10 rounds then stops
            missing = rate.index[rate.isna()]
            if not len(missing):
                break
            filled = {}
            for state in missing:
                nb = neighbours.loc[state]
                nb_states = [s for s in nb.index[nb.astype(bool)] if s in rate.index]
                vals = rate.loc[nb_states].dropna()
                if len(vals):
                    filled[state] = float(vals.mean())
            if not filled:
                break
            rate.loc[list(filled)] = pd.Series(filled)
        else:
            raise RuntimeError(f"{key}-grade M&R rates did not converge")
        out[key] = rate

    return out["above"], out["below"]


def prepare_activity(
    eia: pd.DataFrame,
    phmsa: pd.DataFrame,
    *,
    year: int,
    states: list[str],
    above_rate: pd.Series,
    below_rate: pd.Series,
) -> pd.DataFrame:
    """Per-state activity data: EIA customers/volumes + PHMSA pipe/services."""
    e = eia[eia["Year"] == year]
    e = e.groupby("State", as_index=False).sum(numeric_only=True)

    p = phmsa[(phmsa["REPORT_YEAR"] == year) & (phmsa["STOP"].isin(states))].copy()
    # total miles including services, from the average service length (ft -> miles)
    p["Miles_main_and_serv"] = (
        p["MMILES_TOTAL"] + p["NUM_SRVCS_TOTAL"] * p["AVERAGE_LENGTH"] / 5280.0
    )
    p = p.groupby("STOP", as_index=False).sum(numeric_only=True)

    m = e.merge(p, left_on="State", right_on="STOP")
    m = m[m["State"].isin(states)].sort_values("State").set_index("State")

    # allocate the state's stations-per-mile rate across its mains
    m["GHGRP_MnR_above"] = m["MMILES_TOTAL"] * above_rate.reindex(m.index)
    m["GHGRP_MnR_below"] = m["MMILES_TOTAL"] * below_rate.reindex(m.index)
    return m


def compute_emissions(
    activity: pd.DataFrame,
    ghgi: dict[str, pd.DataFrame],
    pipeline_ef: pd.DataFrame,
    *,
    res_post_meter_ef: float,
    com_post_meter_ef: float,
) -> pd.DataFrame:
    """Activity -> CH4 (mol/s) per subsector, split residential/commercial."""
    a = activity
    out = pd.DataFrame(index=a.index)

    # mains: miles x leaks/mile x mol/s/leak
    mains = sum(
        a[f"MMILES_{mat}"]
        * pipeline_ef.iloc[i]["Leaks_per_mile"]
        * pipeline_ef.iloc[i]["Avg_emissions_mol_per_s"]
        for i, mat in enumerate(_PIPE_MATERIALS)
    )
    # services: count x mol/s/service
    serv = sum(
        a[f"NUM_SRVS_{mat}"] * ghgi["services"]["EF"].iloc[i]
        for i, mat in enumerate(_SERVICE_MATERIALS)
    )

    # M&R: stations x type-fraction x EF, above and below grade separately
    mnr_tbl = ghgi["MnR"]
    above_total = mnr_tbl.loc[_MNR_ABOVE, "Total_stations"].sum()
    below_total = mnr_tbl.loc[_MNR_BELOW, "Total_stations"].sum()
    mnr = 0.0
    for types, stations, total in (
        (_MNR_ABOVE, a["GHGRP_MnR_above"], above_total),
        (_MNR_BELOW, a["GHGRP_MnR_below"], below_total),
    ):
        for t in types:
            row = mnr_tbl.loc[t]
            mnr = mnr + stations * (row["Total_stations"] / total) * row["EF"]

    # meters: customers x mol/s/meter
    meters_ef = ghgi["meters"]["EF"]
    res_meter = a["Residential_Total_Customers"] * meters_ef.iloc[0]
    com_meter = a["Commercial_Total_Customers"] * meters_ef.iloc[1]
    ind_meter = a["Industrial_Total_Customers"] * meters_ef.iloc[2]

    # upsets: miles x mol/s/mile
    maint_ef = ghgi["maintenance"]["EF"]
    upset = (
        a["MMILES_TOTAL"] * maint_ef.iloc[0]
        + a["Miles_main_and_serv"] * maint_ef.iloc[1]
        + a["Miles_main_and_serv"] * maint_ef.iloc[2]
    )

    # post-meter: Mcf delivered -> cubic feet -> mol/s
    out["post_meter_ER_total_res"] = (
        a["Residential_Total_Volume_(Mcf)"] * 1000 * res_post_meter_ef
    )
    out["post_meter_ER_total_com"] = (
        a["Commercial_Total_Volume_(Mcf)"] * 1000 * com_post_meter_ef
    )

    # everything else splits by the residential:commercial *customer* ratio
    customers = a["Residential_Total_Customers"] + a["Commercial_Total_Customers"]
    res_share = a["Residential_Total_Customers"] / customers
    com_share = a["Commercial_Total_Customers"] / customers
    for name, value in (("mains", mains), ("serv", serv), ("MnR", mnr), ("upset", upset)):
        out[f"{name}_ER_total_res"] = value * res_share
        out[f"{name}_ER_total_com"] = value * com_share

    # Industrial meters are a handful of big point sources, many not even on gas,
    # so rather than map them with the industrial CO2 grid the R shares them across
    # res/com in proportion to their *meter* emissions (not their customer counts,
    # which would tilt the split residential).
    meter_total = res_meter + com_meter
    out["meter_ER_total_res"] = res_meter + ind_meter * res_meter / meter_total
    out["meter_ER_total_com"] = com_meter + ind_meter * com_meter / meter_total

    return out


def compute_ng_distribution(
    *,
    emissions: pd.DataFrame,
    state_tigerlines,
    inventories: dict[str, xr.DataArray],
    domain,
    domain_template: xr.DataArray,
    domain_crs: str,
    inventory_name: str,
    by_state: bool = True,
    by_domain: bool = False,
    verbose: bool = False,
) -> dict[str, xr.DataArray]:
    """Disaggregate state (or domain) CH4 totals onto the domain grid.

    ``inventories`` needs the ``res`` and ``com`` CO2 grids. Returns
    ``{raster_name: DataArray}`` in nmol/m²/s, named as the R does:
    ``NG_dist_<subsector>_<res|com>_by<level>_<inventory>``.
    """
    import geopandas as gpd

    inv_crs = next(iter(inventories.values())).rio.crs
    states = state_tigerlines.set_index("STUSPS").loc[list(emissions.index)].reset_index()
    states = gpd.GeoDataFrame(
        states.join(emissions.reset_index(drop=True)), crs=state_tigerlines.crs
    )

    # crop the inventory to the domain (see the note in stationary_combustion:
    # identical results, tractable coverage maths)
    box_bounds = geo.ext(geo.project_to_crs(domain_template, inv_crs))
    margin = 20 * max(geo.res(next(iter(inventories.values()))))
    poly_bounds = states.to_crs(inv_crs).total_bounds
    window = (
        min(box_bounds[0], poly_bounds[0]) - margin,
        min(box_bounds[1], poly_bounds[1]) - margin,
        max(box_bounds[2], poly_bounds[2]) + margin,
        max(box_bounds[3], poly_bounds[3]) + margin,
    )
    inventories = {k: geo.crop_snap_out(v, window) for k, v in inventories.items()}

    box = gpd.GeoDataFrame(
        geometry=gpd.GeoSeries.from_wkt(
            [
                "POLYGON(({0} {1},{2} {1},{2} {3},{0} {3},{0} {1}))".format(
                    box_bounds[0], box_bounds[1], box_bounds[2], box_bounds[3]
                )
            ],
            crs=inv_crs,
        ),
        crs=inv_crs,
    )

    levels = []
    if by_state:
        levels.append(("state", states.to_crs(inv_crs)))
    if by_domain:
        # one polygon, one row: the states dissolved, with their totals summed
        merged = states.dissolve()
        for col in emissions.columns:
            merged[col] = float(emissions[col].sum(skipna=True))
        levels.append(("domain", merged.to_crs(inv_crs)))

    out: dict[str, xr.DataArray] = {}
    for level, polys in levels:
        covers = county_cover_weights(next(iter(inventories.values())), polys)
        for sector, totals in (("res", RES_TOTALS), ("com", COM_TOTALS)):
            ch4 = inventory_based_disaggregation(
                inventories[sector],
                totals,
                polys,
                covers,
                progress=f"{inventory_name} {sector} by{level}" if verbose else None,
            )
            for total, da in ch4.items():
                flux = geo.project_partial_to_grid(da, box, domain_template) * 1000.0
                flux.name = "methane_emissions"
                name = total.replace("_ER_total", "")
                out[f"NG_dist_{name}_by{level}_{inventory_name}"] = flux

    return out


@dataclass
class NGDistributionSector:
    """RunContext wrapper: read shared inputs, compute, write NetCDF outputs."""

    key: str = "natural_gas_distribution"
    name: str = "Natural gas distribution"
    process_flag: str = "Process_natural_gas_distribution"

    def run(self, ctx: RunContext) -> None:
        from .. import datasets

        cfg = ctx.config
        if cfg.NG_distribution_by_LDC:
            raise NotImplementedError(
                "NG_distribution_by_LDC reads the output of NG_distribution_by_LDC_prep.R, "
                "a semi-manual script outside the package; not ported"
            )

        year = ctx.shared["ghgi_data_yr"]
        states = ctx.shared["state_name_list"]
        ghgi = resolve_ghgi_tables(datasets.load("GHGI_NG_distribution"), year)

        above, below = stations_per_mile(
            ctx.shared["ghgrp_facility_data"],
            ctx.shared["ghgrp_subpartW_emissions"],
            datasets.load("GHGRP_LDC"),
            datasets.load("Neighboring_states"),
            year=year,
        )
        activity = prepare_activity(
            datasets.load("EIA_NG_data"),
            datasets.load("PHMSA_natural_gas_distribution"),
            year=year,
            states=states,
            above_rate=above,
            below_rate=below,
        )
        emissions = compute_emissions(
            activity,
            ghgi,
            cfg.natural_gas_pipeline_emission_factors,
            res_post_meter_ef=cfg.natural_gas_res_post_meter_emission_factor,
            com_post_meter_ef=cfg.natural_gas_com_post_meter_emission_factor,
        )

        results: dict[str, xr.DataArray] = {}
        for inv_name, flag in (("aces", "Use_ACES"), ("vulcan", "Use_Vulcan")):
            if not getattr(cfg, flag):
                continue
            inventories = ctx.shared[f"{inv_name}_inventories"]
            results |= compute_ng_distribution(
                emissions=emissions,
                state_tigerlines=ctx.shared["state_tigerlines"],
                inventories={k: inventories[k] for k in ("res", "com")},
                domain=ctx.domain,
                domain_template=ctx.domain_template,
                domain_crs=ctx.domain_crs,
                inventory_name=inv_name,
                by_state=cfg.NG_distribution_by_state,
                by_domain=cfg.NG_distribution_by_domain,
                verbose=ctx.verbose,
            )

        for name, da in results.items():
            ctx.write_output(da, f"{name}.nc", subdir="NG_distribution")

        combined = sum(da.fillna(0.0) for da in results.values())
        combined.name = "methane_emissions"
        ctx.write_output(combined, f"{self.key}.nc")

        ctx.shared.setdefault("sector_results", {})[self.key] = results


def register() -> None:
    from . import base

    base.register(NGDistributionSector())
