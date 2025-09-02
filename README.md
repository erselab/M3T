
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MMMT

<!-- badges: start -->

<!-- badges: end -->

This is an R package designed to help create an urban-focused gridded
methane inventory for any region within the continental United States.
Typical users should only need to modify a configuration file and define
certain inputs when running the main function.

The config allows a user to select the emitting sectors they want to
include, emission factors to use, and methods for sectors where multiple
are available.

When running the main function you set the directories to save data, the
year, resolution, and area you want an inventory for. Years should be
between 2011 and 2022 (as of this writing) and the resolution should not
be greater than 0.01 degrees or 1 square kilometer.

## Background

This tool is developed using a wide variety of published research,
publicly available datasets, and models. If you are knowledgeable about
any particular sector we would greatly appreciate you alerting us to
newer information that could be incorporated using the Github issues
page.

For a full, detailed explanation of the package at the time of its
development, you should see the published article that is freely
available at Earth System Science Data ().

Other particularly relevant articles for the various sectors are
mentioned below when describing the functions in this package.

## Functions

1.  General Functions
    - CH4_inventory_config.R
      - Place to modify a variety of settings that affect how emissions
        are calculated
    - CH4_inventory_main.R
      - Calls config and runs all sector functions
    - Prepare_ACES_Vulcan.R
      - Download the ACES and Vulcan CO₂ 1 km² inventories for different
        sectors. Convert hourly ACES values into annual. Strongly
        recommend against using given the large amount of data
        downloaded (\>100 GB).
      - See publications for
        [ACES](https://doi.org/10.1002/2017JD027359) and
        [Vulcan](https://doi.org/10.1029/2020JD032974). Datasets are
        publicly available for
        [ACES](https://doi.org/10.3334/ORNLDAAC/1943) and
        [Vulcan](https://doi.org/10.3334/ORNLDAAC/1741) as well.
    - Inventory_based_aggregation.R
      - Helper function to disaggregate emissions e.g., from the state
        total to pixels using the CO₂ inventories.
    - Plotting_individual_sectors.R
      - Plotting functions to provide log-scale and linear-scale visuals
2.  Landfills
    - Landfill_emissions_r1.R
      - Calculates emissions from municipal solid waste facilities
3.  Natural Gas Distribution
    - NG_distribution_byLDC_prep.R
      - Only for those who wish to calculate emissions in more detail -
        disaggregating from the Local Distribution Company (LDC) values
        rather than state totals. This may perform reasonably by
        default, but generally requires user edits to appropriately
        match facilities across datasets and update LDC coverage maps.
    - NG_distribution_emissions_r4.R
      - Calculates emissions from the natural gas distribution system,
        including residential household leaks
      - See publications [Weller et
        al.](https://doi.org/10.1021/acs.est.0c00437) discussing
        emissions measurements for distribution pipelines, [Fischer et
        al.](https://doi.org/10.1021/acs.est.8b03217) discussing
        whole-home emissions measurements, and the
        [Vulcan](https://doi.org/10.1029/2020JD032974) and
        [ACES](https://doi.org/10.1002/2017JD027359) CO₂ inventories.
4.  Natural Gas Transmission
    - NG_transmission_emissions_r1.R
      - Calculates emissions from the natural gas transmission system
        (both pipelines and compressor stations)
5.  Stationary Combustion
    - stationary_combustion_r4.R
      - Calculates emissions from stationary combustion, excluding
        residential coal (the U.S. has none) and residential natural gas
        (handled separately)
6.  Wastewater
    - NLCD_fractions_by_state.R
      - Processes the National Land Cover Database (NLCD) to define
        areas to assign septic emissions
    - WWTP_emissions_r3.R
      - Calculates emissions from municipal wastewater treatment plants,
        septic systems, and industrial wastewater treatment plants
      - See publications [Homer et
        al.](https://doi.org/10.1016/j.isprsjprs.2020.02.019) discussing
        the National Land Cover Database (NLCD) and [Moore et
        al.](https://doi.org/10.1021/acs.est.2c05373) discussing
        emissions measurements from municipal wastewater treatment
        plants
7.  Wetlands and Inland Waters
    - WETCHARTS_downscaling.R
      - Disaggregates the WetCHARTs modeled wetland emissions from 0.5
        degrees to 0.1 degrees using land cover
      - The dataset is publicly available for
        [WetCHARTs](https://doi.org/10.3334/ORNLDAAC/2346)
    - Wetland_fraction_r2_WIP.R
      - Calculates freshwater and wetland land area using the National
        Wetland Inventory
    - Wetland_emissions_r2.R
      - Combines freshwater and wetland land area with emission factors
        to calculate emissions
      - See the publications [McDonald et
        al.](https://doi.org/10.4319/lo.2012.57.2.0597) discussing
        freshwater lakes in the United States and [Rosentreter et
        al.](https://doi.org/10.1038/s41561-021-00715-2) discussing
        freshwater emissions. The [State Of the Carbon Cycle Report
        version
        1](https://www.carboncyclescience.us/state-carbon-cycle-report-soccr)
        and [State of the Carbon Cycle Report version
        2](https://carbon2018.globalchange.gov/) are publicly available
8.  Gridded EPA (GEPA)
    - Prepare_GEPA.R
      - Downloads and processes the appropriate gridded EPA emissions
        data for sectors not included here. This includes industrial
        landfills, agricultural emissions, mobile combustion, fossil
        fuel exploration, fossil fuel production, fossil fuel refining,
        petroleum transport, the petrochemical industry and the
        ferroalloy industry.
      - See the publication [Maasakkers et
        al.](https://doi.org/10.1021/acs.est.3c05138). The
        [GEPA](https://doi.org/10.5281/zenodo.8367082) dataset is
        publicly available.
9.  Combining sectors
    - Combiner.R
      - Combines output from across all sectors and all variations to
        create all unique combinations of inventories

## Output

For most sectors emissions are calculated for various subsectors before
they’re combined. Some sectors also have partial output. To keep the
output organized, each sector has its own folder in the main output
folder. Subsectors and partial output is saved here while sector totals
(all variations as defined by the config) are saved in the main output
folder. The sector total values are named to clarify the combination of
subsectors used. The below lists the subsector output for each sector,
including all possible variations. Minor variations in the filenames to
differentiate subsectors are in **bold**. If you are interested in
methodology, we recommend you to the paper mentioned in the background
section or the help files for each individual function.

1.  Landfills
    - MSW_GHGRP\_**method**.nc
      - Municipal Solid Waste (MSW) emissions from the GreenHouse Gas
        Reporting Program (GHGRP) using 1 of 3 methods.
      - **method**
        - **Reported**
          - Uses values as reported to the GHGRP. Facilities can choose
            either method below as their reported value.
        - **Modeled**
          - Uses GHGRP method HH-6 emissions which are based on a first
            order decay model
        - **Collection_efficiency**
          - Uses GHGRP method HH-8 emissions which are based on an
            assumed landfill gas capture efficiency and the known
            quantity of gas captured
    - MSW_LMOP.nc
      - Municipal Solid Waste (MSW) emissions assigned to Landfill
        Methane Outreach Program (LMOP) facilities
2.  Natural Gas Distribution
    - NG_dist\_**type_sector_variation_inventory**.nc
      - **type**
        - **upset**
          - upsets - relief valves, blowdowns, and mishaps like dig-ins
        - **serv**
          - service pipeline
        - **post_meter**
          - all residential emissions after the gas has passed the
            home’s gas meter including leaks from furnaces, stoves,
            water heaters, pipelines, etc.
        - **MnR**
          - Metering and Regulating (MnR) stations
        - **mains**
          - main pipeline
      - **sector**
        - **res**
          - residential
        - **com**
          - commercial
      - **variation**
        - **byLDC**
          - disaggregating from Local Distribution Company (LDC) total
            emissions to pixels
        - **bystate**
          - disaggregating from state total emissions to pixels
        - **bydomain**
          - disaggregating from entire domain total emissions to pixels
      - **inventory**
        - **ACES**
          - using ACES sectoral CO₂ inventories to disaggregate to
            pixels
        - **Vulcan**
          - using Vulcan sectoral CO₂ inventories to disaggregate to
            pixels
3.  Natural Gas Transmission
    - NG_trans_compressors.nc
      - Natural gas transmission compressor emissions
    - NG_trans_pipes.nc
      - Natural gas transmission pipeline emissions
4.  Stationary Combustion
    - Stat_comb\_**sector_fuel_variation_inventory**.nc
      - **sector**
        - **res**
          - residential
        - **com**
          - commercial
        - **elec**
          - electric
        - **ind**
          - industrial
      - **fuel**
        - **wood**
        - **coal**
        - **petr**
          - petroleum
        - **gas**
          - natural gas
      - **variation**
        - **bystate**
          - disaggregating from state total emissions to pixels
        - **bydomain**
          - disaggregating from entire domain total emissions to pixels
      - **inventory**
        - **ACES**
          - using ACES sectoral CO₂ inventories to disaggregate to
            pixels
        - **Vulcan**
          - using Vulcan sectoral CO₂ inventories to disaggregate to
            pixels
5.  Wastewater
    - Wastewater_ind.nc
      - Industrial wastewater facility emissions
    - Wastewater\_**input_method**\_dom_central.nc
      - Municipal wastewater treatment facility emissions
      - **input**
        - **CWNS**
          - Uses the Clean Watershed Needs Survey (CWNS) to get the flow
            handled by each facility
        - **DMR**
          - Uses the Discharge Monitoring Reports (DMR) to get the flow
            handled by each facility
      - **method**
        - **GHGI**
          - Disaggregates the GreenHouse Gas Inventory (GHGI) national
            total emissions to each facility assuming they’re
            proportional to flow handled
        - **ML**
          - Calculates emissions using the Moore et al. log-log
            Linear (ML) relationship between flow handled and emissions
            determined using measurements
    - Wastewater_dom_septic\_**scale**.nc
      - Septic emissions
      - **scale**
        - **bystate**
          - calculated using state-specific septic data
        - **national**
          - calculated using the fraction of open / low density urban
            land cover relative to the national total
6.  Wetlands and Inland Waters
    - Wetcharts\_<b>landcover</b>\_downscaled_subset\_<b>N</b>.nc
      - Wetland emissions from WetCHARTs downscaled from 0.5 degress to
        0.1 degrees using landcover
      - **landcover**
        - **NLDC**
          - National Land Cover Database (NLCD)
        - **NACLMS**
          - North American Land Change Monitoring System (NALCMS)
      - **N**
        - Sequential. You can select different subsets of WetCHARTs
          models and these will be calculated sequentially as input in
          the config.
    - SOCCR1.nc
      - Wetland emissions using the National Wetlands Inventory (NWI)
        and State Of the Carbon Cycle Report (SOCCR) version 1 emission
        factors
    - SOCCR2.nc
      - Wetland emissions using the National Wetlands Inventory (NWI)
        and State Of the Carbon Cycle Report (SOCCR) version 2 emission
        factors
    - Freshwater.nc
      - Freshwater emissions using the National Wetlands Inventory (NWI)
        and [Rosentreter et
        al.](https://doi.org/10.1038/s41561-021-00715-2) emission
        factors
7.  Gridded EPA (GEPA)
    - GEPA_ind_landfill.nc
      - industrial landfill emissions
    - GEPA_non_thermo.nc
      - non-thermogenic emissions (composting, manure, enteric
        fermentation, rice cultivation, and field burning)
    - GEPA_thermo.nc
      - thermogenic emissions (mobile combustion, fossil fuel
        exploration, fossil fuel production, fossil fuel refining,
        petroleum transport, the petrochemical industry and the
        ferroalloy industry)
8.  Combining sectors
    - a separate folder will be created with each unique combination of
      sectors saved numerically. A csv will also be saved which provides
      a clear legend detailing which variations are in each inventory
      file.

## Installation

You can install the development version of MMMT from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Kristian-hajny/methane_inventory")
```









# NIST default information:

# NIST Open-Source Software Repository Template

Use of GitHub by NIST employees for government work is subject to the
[Rules of Behavior for
GitHub](https://odiwiki.nist.gov/pub/ODI/GitHub/GHROB.pdf). This is the
recommended template for NIST employees, since it contains required
files with approved text. For details, please consult the Office of Data
& Informatics’ [Quickstart Guide to GitHub at
NIST](https://odiwiki.nist.gov/ODI/GitHub.html).

Please click on the green **Use this template** button above to create a
new repository under the [usnistgov](https://github.com/usnistgov)
organization for your own open-source work. Please do not “fork” the
repository directly, and do not create the templated repository under
your individual account.

The key files contained in this repository – which will also appear in
templated copies – are listed below, with some things to know about
each.

------------------------------------------------------------------------

## README

Each repository will contain a plain-text [README
file](https://en.wikipedia.org/wiki/README), preferably formatted using
[GitHub-flavored Markdown](https://github.github.com/gfm/) and named
`README.md` (this file) or `README`.

Per the [GitHub ROB](https://odiwiki.nist.gov/pub/ODI/GitHub/GHROB.pdf)
and [NIST Suborder
1801.02](https://inet.nist.gov/adlp/directives/review-data-intended-publication),
your README should contain:

1.  Software or Data description
    - Statements of purpose and maturity
    - Description of the repository contents
    - Technical installation instructions, including operating system or
      software dependencies
2.  Contact information
    - PI name, NIST OU, Division, and Group names
    - Contact email address at NIST
    - Details of mailing lists, chatrooms, and discussion forums, where
      applicable
3.  Related Material
    - URL for associated project on the NIST website or other Department
      of Commerce page, if available
    - References to user guides if stored outside of GitHub
4.  Directions on appropriate citation with example text
5.  References to any included non-public domain software modules, and
    additional license language if needed, *e.g.*
    [BSD](https://opensource.org/licenses/bsd-license),
    [GPL](https://opensource.org/licenses/gpl-license), or
    [MIT](https://opensource.org/licenses/mit-license)

The more detailed your README, the more likely our colleagues around the
world are to find it through a Web search. For general advice on writing
a helpful README, please review [*Making Readmes
Readable*](https://github.com/18F/open-source-guide/blob/18f-pages/pages/making-readmes-readable.md)
from 18F and Cornell’s [*Guide to Writing README-style
Metadata*](https://data.research.cornell.edu/content/readme).

## LICENSE

Each repository will contain a plain-text file named `LICENSE.md` or
`LICENSE` that is phrased in compliance with the Public Access to NIST
Research [*Copyright, Fair Use, and Licensing Statement for SRD, Data,
and Software*](https://www.nist.gov/open/license#software), which
provides up-to-date official language for each category in a blue box.

- The version of [LICENSE.md](LICENSE.md) included in this repository is
  approved for use.
- Updated language on the [Licensing
  Statement](https://www.nist.gov/open/license#software) page supersedes
  the copy in this repository. You may transcribe the language from the
  appropriate “blue box” on that page into your README.

If your repository includes any software or data that is licensed by a
third party, create a separate file for third-party licenses
(`THIRD_PARTY_LICENSES.md` is recommended) and include copyright and
licensing statements in compliance with the conditions of those
licenses.

## CODEOWNERS

This template repository includes a file named [CODEOWNERS](CODEOWNERS),
which visitors can view to discover which GitHub users are “in charge”
of the repository. More crucially, GitHub uses it to assign reviewers on
pull requests. GitHub documents the file (and how to write one)
[here](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners).

***Please update that file*** to point to your own account or team, so
that the [Open-Source
Team](https://github.com/orgs/usnistgov/teams/opensource-team) doesn’t
get spammed with spurious review requests. *Thanks!*

## CODEMETA

Project metadata is captured in `CODEMETA.yaml`, used by the NIST
Software Portal to sort your work under the appropriate thematic
homepage. ***Please update this file*** with the appropriate “theme” and
“category” for your code/data/software. The Tier 1 themes are:

- [Advanced
  communications](https://www.nist.gov/advanced-communications)
- [Bioscience](https://www.nist.gov/bioscience)
- [Buildings and
  Construction](https://www.nist.gov/buildings-construction)
- [Chemistry](https://www.nist.gov/chemistry)
- [Electronics](https://www.nist.gov/electronics)
- [Energy](https://www.nist.gov/energy)
- [Environment](https://www.nist.gov/environment)
- [Fire](https://www.nist.gov/fire)
- [Forensic Science](https://www.nist.gov/forensic-science)
- [Health](https://www.nist.gov/health)
- [Information Technology](https://www.nist.gov/information-technology)
- [Infrastructure](https://www.nist.gov/infrastructure)
- [Manufacturing](https://www.nist.gov/manufacturing)
- [Materials](https://www.nist.gov/materials)
- [Mathematics and
  Statistics](https://www.nist.gov/mathematics-statistics)
- [Metrology](https://www.nist.gov/metrology)
- [Nanotechnology](https://www.nist.gov/nanotechnology)
- [Neutron research](https://www.nist.gov/neutron-research)
- [Performance excellence](https://www.nist.gov/performance-excellence)
- [Physics](https://www.nist.gov/physics)
- [Public safety](https://www.nist.gov/public-safety)
- [Resilience](https://www.nist.gov/resilience)
- [Standards](https://www.nist.gov/standards)
- [Transportation](https://www.nist.gov/transportation)

------------------------------------------------------------------------

[usnistgov/opensource-repo](https://github.com/usnistgov/opensource-repo/)
is developed and maintained by the
[opensource-team](https://github.com/orgs/usnistgov/teams/opensource-team),
principally:

- Gretchen Greene, @GRG2
- Yannick Congo, @faical-yannick-congo
- Trevor Keller, @tkphd

Please reach out with questions and comments.

<!-- References -->
