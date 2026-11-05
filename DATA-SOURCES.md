# Data sources and reuse

JoonHo Lee ([jlee296@ua.edu](mailto:jlee296@ua.edu))

This compendium accompanies [Prioritizing Places under Uncertainty (2026)](https://doi.org/10.5281/zenodo.23966864). Source data retain their original attribution and terms. The code license does not replace those terms.

## Opportunity Atlas

The historical observations come from Raj Chetty, John Friedman, Nathaniel Hendren, Maggie R. Jones, and Sonya R. Porter, *Replication Data for: The Opportunity Atlas: Mapping the Childhood Roots of Social Mobility*, Harvard Dataverse, [doi:10.7910/DVN/NKCQM1](https://doi.org/10.7910/DVN/NKCQM1), version 1.0. Credit also belongs to Opportunity Insights and the U.S. Census Bureau. The selected county, tract, and tract-covariate files are covered by [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

| Selected original file | Use in this study | Dataverse file ID |
|---|---|---:|
| [county_outcomes_simple.csv](https://opportunityinsights.org/wp-content/uploads/2018/10/county_outcomes_simple.csv) | Alabama county estimates and standard errors | 5793982 |
| [tract_outcomes_simple.csv](https://opportunityinsights.org/wp-content/uploads/2018/10/tract_outcomes_simple.csv) | Alabama tract estimates and standard errors | 5793994 |
| [tract_covariates.csv](https://opportunityinsights.org/wp-content/uploads/2018/10/tract_covariates.csv) | The Alabama tract-key universe used to document coverage | 5794540 |

The extracts select Alabama, pooled race and gender, and parental income at the national 25th percentile. The cohort was born in 1978–1983. The income outcome is the published adult household income rank; incarceration is the published April 1, 2010 snapshot. These describe historical outcomes.

Processing adds zero-padded identifiers, separates unavailable inputs from modeled observations, selects the decision universes, and fits the measurement models. Original estimates and standard errors remain on their fraction scale. Display values multiply them by 100. Negative noisy observations are retained. Posterior estimates and review bands are products of this study and are distinguished from the source observations. These changes do not imply endorsement by the source authors or agencies.

## Current statistical and geographic context

- **Census ACS:** 2020–2024 five-year estimates, with published margins of error. The county child-poverty display uses [table B17006](https://api.census.gov/data/2024/acs/acs5/groups/B17006.json). Its numerator and denominator both refer to related children under 18 in families. The accompanying [table-based summary file](https://www2.census.gov/programs-surveys/acs/summary_file/2024/table-based-SF/data/5YRData/acsdt5y2024-b17006.dat) provides the estimates and margins of error used in the calculation.
- **HUD:** the official [2016–2020 Low- and Moderate-Income Summary Data service](https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/LMISD_Map/FeatureServer/4). LOWMOD, LOWMODUNIV, and LOWMOD_PCT describe the HUD special-tabulation universe. They do not share the ACS poverty denominator.
- **Census geography:** [TIGER/Line](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html) and [cartographic boundary files](https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html). Actual Census vectors are public-domain data. The file dictionary identifies vintage, coordinate reference system, and any repair or display simplification.

Only the statistical cells and geometry needed for reproduction are included. Source attribution, variable definitions, and component-specific notices remain applicable. Geometry simplification affects drawing only; values are not transferred across boundary vintages.

## ADECA program materials

The public source documents are the [2026 CDBG Action Plan](https://adeca.alabama.gov/wp-content/uploads/2026-CDBG-Action-Plan.docx), [rating form](https://adeca.alabama.gov/wp-content/uploads/Rating-Form-Small-City-Large-City-and-County-Funds.docx), and [applications received by July 22, 2026](https://adeca.alabama.gov/wp-content/uploads/PY2026-CDBG-Grant-Applications-Received-by-July-22-2026-Deadline.xlsx).

The compendium provides selected factual fields, source locators, and retrieval instructions. The original DOCX and XLSX files are not redistributed. A document being publicly accessible does not establish permission to relicense the complete document. Requested amounts are application requests, not awards or total project costs. `program-context.csv` records the County Fund allocation ($2,793,000; Action Plan P0021) and maximum requested grant ($500,000; P0157) as program context. They do not determine the study review capacities.

## Study outputs

JoonHo Lee's documentation, posterior draws, saved reports, tables, figures, and selection annotations are provided under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/), subject to the upstream component terms above. Code is covered separately by the repository's MIT license. Large posterior and geographic files belong to the companion Zenodo data deposit; its version and checksums are identified in the data manifest.
