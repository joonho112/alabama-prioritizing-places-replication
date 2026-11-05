# Data guide

JoonHo Lee ([jlee296@ua.edu](mailto:jlee296@ua.edu))

Use [DATA-SOURCES.md](../DATA-SOURCES.md) for source links, attribution, and reuse terms. Small inputs are kept in `data/`; large posterior and geographic files are supplied through the [companion dataset, version 1.0.0](https://doi.org/10.5281/zenodo.23984957). Use the dataset version and file checksums recorded in the manifest.

## Analysis inputs

| File | Records and role |
|---|---|
| `data/analysis/county-observations.csv` | 67 county observations used by the county measurement models |
| `data/analysis/county-universe.csv` | 67 counties with a flag identifying the 64 candidates for review |
| `data/analysis/tract-observations.csv` | 1,173 tracts with usable estimates and positive standard errors for both criteria |
| `data/metadata/tract-coverage.csv` | 1,181 tract keys, including the eight without model estimates |
| `data/analysis/pilot-tract-keys.csv` | The 90 tracts in the common pilot decision universe |
| `data/analysis/acs-county-child-poverty.csv` | County B17006 numerator, denominator, and margins of error for Figure 8C |
| `data/analysis/applications.csv` | Selected factual fields for 72 CDBG applications |
| `data/analysis/current-context.csv` | Selected ACS/HUD native factual cells; no unavailable S1701 placeholder rows |
| `data/analysis/current-context-display.csv` | The 8,236 context records used for the supplemental display |
| `data/analysis/program-context.csv` | County Fund allocation and request ceiling with their source locators |

The observation tables are wide: one row per entity, with `entity_id`, `county_id`, `income_estimate_fraction`, `income_se_fraction`, `incarceration_estimate_fraction`, and `incarceration_se_fraction`. County `entity_id` equals `county_id`; tract `entity_id` equals `tract_id`, which is also retained. IDs are character strings so leading zeroes survive reading and joining. Native source identifiers and coverage fields document their construction.

The county models include Jefferson (`01073`), Mobile (`01097`), and Shelby (`01117`), but these counties are excluded from the 64-county review universe. Their posterior estimates remain available; they do not receive review bands in that universe.

The tract coverage table includes four keys with no outcome-source row and four source rows with unavailable model inputs. Missing posteriors remain explicit. All 1,173 modeled tracts contribute to the tract measurement models, while only the specified 90 pilot tracts receive review bands. The 123 negative published incarceration estimates among modeled tracts are retained as noisy observations.

## Units and direction

Model inputs use native fractions, including their standard errors. A posterior income value of 0.40 is displayed as 40 percentile points; a posterior incarceration value of 0.03 is displayed as 3 percentage points. Display scaling does not turn the published standard error into a binomial standard error.

For the review decision, greater historical need means lower income and higher incarceration. The decision adapter therefore multiplies income by −100 and incarceration by +100 once. The observation tables themselves remain in the natural outcome direction.

## Saved posterior draws and reports

Each criterion is fitted separately. The full posterior files preserve iteration × chain × entity dimensions and their original chain and Stan-iteration identifiers. Entity vectors within a draw stay together. Draws from different criteria do not define a scientific joint posterior.

Pool A selects the reports; pool B evaluates the selected reports as fixed actions. Both pools use the same observed data. The independent computations are not independent empirical validation samples. An extended A pool replaces its initial version rather than being concatenated with it.

The tract model checks select reports from their own A pools and evaluate them against the same main-model B pool. There is no separate stress-model B pool. The data manifest connects each file to its model, criterion, pool, dimensions, seed, and checksum.

## Fixed pair-precision domain

`data/metadata/selection-sensitive-pairs.csv` identifies the 1,993 main-tract pairs used for Appendix C's coefficient-precision checks. `pair_id` joins the canonical 11-character `left_id` and `right_id` with `__`. The two membership flags document an A-only historical screen over five policy settings and both directions: `abs(weighted_error - tau) <= .02` or `abs(weighted_error - .5) <= .02`. Each screen retained 1,891 pairs, including 1,789 shared and 102 unique to each screen. Their union was frozen before report optimization. The initial native arrays are not included; these identifiers are a fixed design input, not a screen reconstructed from adopted or fresh results.

`data/saved/pair-precision-summary.csv` contains the two published A/B reference rows. Script 04b recomputes their counts and finite weighted-coefficient MCSE maxima from the adopted main-tract posterior arrays. Each pair expands to two directions and five policies, giving 19,930 weighted rows per pool. Generated detailed rows stay in the external output directory. See [the reproduction guide](reproduce.md#optional-appendix-c-coefficient-precision) for commands and the distinction between unavailable indicator MCSE and a finite precision miss.

## Current context

Figure 8C uses the percentage of related children under 18 in families who are below poverty. With `n = B17006_E002`, `d = B17006_E001`, and their published margins of error `m_n` and `m_d`, the displayed estimate is `100 * n / d`. Its 90% margin of error is

```text
r = n / d
a = m_n^2 - r^2 * m_d^2
MOE = 100 * sqrt(a) / d
```

When `a` is negative, the calculation uses the addition fallback, replacing `a` with `m_n^2 + r^2 * m_d^2`. The denominator is from the same table and universe as the numerator. B09001 total children is not substituted. The current-context map can display the published B17006 count; that is a different quantity from the county percentage.

Atlas outcomes use 2010 geography, HUD context uses 2020 block groups, and ACS context retains its own native geographic definitions. Place boundaries provide jurisdictional context. Display layers and native full geometries have separate roles; no ACS or HUD values are apportioned to Atlas tracts.

## Application facts

The roster contains 72 applications: 39 municipal, 14 county, and 19 unknown or mixed government types. Seventy-one have a resolved county key. The source spelling `Talledega` remains unresolved rather than being silently reassigned. Source sheet and row locators make the extraction traceable.

Keep requested amounts in exact cents for aggregation and round only for presentation. The roster supplies no project-site coordinates, beneficiary counts, awards, or final rating scores. Review bands do not establish project eligibility, readiness, or an award recommendation.

## Rebuild the small extracts

The source catalog, `data/metadata/source-files.csv`, records official URLs and SHA-256 checksums. Supply those original files in a local directory, or add `--download` to acquire missing files explicitly. Existing files are verified before they are read.

```sh
Rscript scripts/01_prepare_data.R --check
Rscript scripts/01_prepare_data.R --from-source=/path/to/official-files --atlas --acs --context --applications --output=/path/to/rebuilt
```

Rebuilt files are written below the selected external output directory. Add `--write-inputs` only when deliberately replacing the included input tables with their rebuilt equivalents. Parsing the original applications workbook requires the optional `readxl` dependency; ordinary use of its supplied factual extract does not.

The native context extract selects the three available ACS count metrics and HUD low/moderate-income fields. Unavailable S1701 request placeholders are omitted. Figure 8C is calculated separately from B17006's compatible numerator and denominator. Eighteen HUD block groups have zero LOWMODUNIV; their published percentage is retained in the factual input and omitted from the display as undefined. The HUD service does not establish the confidence level of its MOE field, so no such level is assigned.

## Geographic files

`data/metadata/geography-layers.csv` identifies ten full or display layers and their vintages. `data/metadata/geography-sources.csv` gives official retrieval URLs and checksum scope. The companion geographic archive preserves the accepted polygons in `geo/geography.gpkg`. The lazy `rep_geo()` reader requires `sf` only when a map or geometry is requested.

The optional rebuild reads official Census source files, projects them to EPSG:5070, repairs invalid geometries, and retains full polygons when a generalized display file omits a native key. For the two preserved 2020 cartographic sources, component hashes identify the shapefiles; a hash of an independently fetched ZIP container is not asserted.

```sh
Rscript scripts/01_prepare_data.R --from-source=/path/to/official-files --geometry --output=/path/to/rebuilt
```

The rebuilt GeoPackage goes to the external output directory. No display simplification is applied in the deposited polygons; the plotting code applies its documented drawing simplification once. Projection-library differences can change serialized coordinates, so file hashes identify the frozen deposit while a source rebuild should also be checked for keys, geometry coverage, and the intended native vintages.
