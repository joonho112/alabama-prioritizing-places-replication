# Tables

JoonHo Lee ([jlee296@ua.edu](mailto:jlee296@ua.edu))

Each printed table has a readable HTML version, an unrounded CSV, and a reusable TeX fragment. Open the HTML files in a browser. TeX fragments require `booktabs`, `tabularx`, and `array`; Table E.1 continues across two floats to preserve its font size.

| Paper table | HTML | CSV |
|---|---|---|
| 1. First-band tracts | [View](first-band-tracts.html) | [Data](first-band-tracts.csv) |
| 2. Project readiness | [View](project-readiness-summary.html) | [Data](project-readiness-summary.csv) |
| 3. County bands and applications | [View](county-bands-applications.html) | [Data](county-bands-applications.csv) |
| B.1. Tract-model population parameters | [View](hyperparameters.html) | [Data](hyperparameters.csv) |
| D.1. Reporting-rule comparisons | [View](reporting-rule-comparisons.html) | [Data](reporting-rule-comparisons.csv) |
| E.1. Policy sensitivity | [View](policy-sensitivity.html) | [Data](policy-sensitivity.csv) |
| E.2. Model and direction sensitivity | [View](model-direction-sensitivity.html) | [Data](model-direction-sensitivity.csv) |
| F.1. Rating instrument | [View](rating-instrument.html) | [Data](rating-instrument.csv) |

[The data index](data-index.csv) points to complete county/tract results, diagnostic records, sensitivity results, and policy/application details. Its paths are relative to the compendium root. These CSV files retain detail that would require small text in a printed table; column definitions are in [the saved-data dictionary](../data/saved/data-dictionary.csv) and [the input dictionary](../data/metadata/input-dictionary.csv).

Rebuild from the repository root:

```sh
Rscript scripts/05_tables.R
```

The default writes to the external run directory configured by `--output`. Add `--write-public` to update this folder. No GIS software, sampler, solver, or bulk-data download is required. Table B.1 is recomputed from the compact actual hyperparameter draws in `data/saved/`; the other statistical tables use the small saved results. Tables 2 and F.1 retain the paper's authored policy summaries.

To use another run, `--posterior-dir` replaces both central county/tract summaries from script 03, `--report-dir` replaces all reports and sensitivity results from script 04, and `--fit-dir` replaces Table B.1 with validated central tract-fit summaries from script 02. Each option replaces its stated layer; omitted layers use the paper's saved results. Supply all three directories to display a complete new analysis. Incomplete populations, missing files, and unaccepted or altered fit summaries are rejected.

Income rank uses percentile points; incarceration uses percentage points. Entity intervals are separate-criterion 95% intervals. Population-parameter intervals are 90% intervals on the logit scale. Review bands and capacity counts retain whole bands. Application amounts are requests, and the rating subtotal is nested within the overall cap.

Appendix C coefficient precision is available as [a compact A/B summary](../data/saved/pair-precision-summary.csv). Script `04b_pair_precision.R` regenerates its detailed rows from the archived main-tract draws and the fixed historical pair domain; [the reproduction guide](../docs/reproduce.md#optional-appendix-c-coefficient-precision) explains this optional diagnostic.
