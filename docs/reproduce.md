# Reproducing the analysis

JoonHo Lee ([jlee296@ua.edu](mailto:jlee296@ua.edu))

Start with the [software guide](software.md). Commands below run from the compendium root. Each script also locates the project from its own path, so an absolute script path works from another working directory. Quote paths containing spaces.

Three routes keep the article's archived results distinct from a new computation.

| Route | Required files | Starts |
|---|---|---|
| Rebuild tables and figures | Small supplied results; geographic archive for maps | No sampler or solver |
| Recompute from archived posterior draws | Relevant full posterior archives | Pair calculations; HiGHS only with `--reselect` |
| Refit and reselect | Included observations or their verified source rebuild; all recorded software | CmdStan, pair calculations, and HiGHS |

## Choose external working locations

The default cache is R's user cache for `alabama-prioritizing-places`; results go under its `results/` directory. Every command accepts `--cache` and `--output`. These examples use neighboring directories:

```sh
export ALABAMA_CACHE="$PWD/../alabama-data-cache"
export ALABAMA_RUN="$PWD/../alabama-replay-results"
```

Use the same cache for commands in one analysis. The dataset cache key includes the manifest's version and content hash. Reimport archives after changing a dataset manifest. Heavy computation uses one shared lock in this cache; do not bypass it by launching concurrent fit or selection runs with different caches.

## 1. Rebuild the article's tables and figures

The eight tables and non-map statistical figures use small files already included in `data/`. Table B.1 recalculates its population-parameter summaries from the compact hyperparameter draws.

```sh
Rscript scripts/01_prepare_data.R --check
Rscript scripts/05_tables.R --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
Rscript scripts/06_figures.R --figure=fig3,fig5,fig6,fig7,figS3,figS4 --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
```

The outputs are under `$ALABAMA_RUN/tables` and `$ALABAMA_RUN/figures`. HTML tables provide readable display versions; CSV files preserve unrounded values. [The table index](../tables/README.md) links full results and diagnostic records. [The figure index](../figures/README.md) gives paper numbers and script keys.

Maps need the geographic archive. Import files explicitly from a local directory containing the companion deposit files:

```sh
Rscript scripts/00_data_access.R --download --files=geographic-inputs.zip --cache="$ALABAMA_CACHE"
Rscript scripts/06_figures.R --all --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
```

`--all` includes Figure 1 and therefore requires `pdflatex`. Statistical figures require no TeX. `--png` adds PNG companions; the primary figures are PDFs with vector map boundaries and matrix cells.

[Dataset version 1.0.0](https://doi.org/10.5281/zenodo.23984957) is published on Zenodo. The [manifest](../data/metadata/zenodo-manifest.json) pins its record-specific file URLs and SHA-256 checksums. Use `--download` in the commands above and below; each file is verified before import. The script allows at least one hour per archive on slower connections and preserves any longer R timeout. For offline reproduction, replace `--download` with `--archive-dir="/path/to/deposit-files"` to use local copies. The dataset DOI is separate from the article's preprint DOI.

## 2. Recompute from the archived posterior draws

Import all five archives for the complete analysis:

```sh
Rscript scripts/00_data_access.R --download --cache="$ALABAMA_CACHE"
Rscript scripts/03_posterior_summaries.R --central --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
Rscript scripts/04_review_reports.R --saved-only --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
Rscript scripts/05_tables.R --posterior-dir="$ALABAMA_RUN/posterior" --report-dir="$ALABAMA_RUN/review-reports/saved-only" --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
Rscript scripts/06_figures.R --all --posterior-dir="$ALABAMA_RUN/posterior" --report-dir="$ALABAMA_RUN/review-reports/saved-only" --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
```

Script 03 writes full central county and tract summaries under `posterior/`. Script 04 recomputes A/B metrics and Monte Carlo diagnostics for the fixed saved reports, plus capacity and stability tables under `review-reports/saved-only/`. It calculates pair probabilities but does not reselect reports. The last two commands explicitly connect those recalculated results to the displays; the maps and workflow retain the geographic and TeX requirements from route 1.

To repeat selection with the same archived draws, choose another empty result location:

```sh
Rscript scripts/04_review_reports.R --reselect --cache="$ALABAMA_CACHE" --output="$PWD/../alabama-archived-reselection"
```

Omitting `--scenario` runs all 14 cases. `--scenario=center` requests the two central universes; a case ID such as `county-center` requests one. Restricted alternatives automatically include the corresponding center needed for comparisons. A complete stability readout requires all eight tract policy/model cases. Solver seeds and controls are in [config/scenarios.csv](../config/scenarios.csv) and [config/analysis.json](../config/analysis.json).

## Optional: Appendix C coefficient precision

The supplied 1,993-pair diagnostic domain was fixed before the original tract report optimization. An A-only screen flagged pairs within .02 of either the policy withholding threshold or .5, across five policies and both directions. The union of the initial and adopted A screens was retained; B did not select the pairs. The initial native computation is not distributed. The supplied IDs and screen-membership flags preserve this historical design, and adopted A/B arrays suffice to recompute the published precision counts.

```sh
Rscript scripts/04b_pair_precision.R --check
Rscript scripts/04b_pair_precision.R --run --cache="$ALABAMA_CACHE" --output="$PWD/../alabama-pair-reference"
```

The first command reads only small design/reference files. The second requires the main-tract A/B archives and writes criterion rows, weighted policy rows, a summary and provenance under `pair-precision/`. It uses bounded pair blocks and the shared heavy-work lock. The default both-pool run has 15,944 criterion-direction rows and 39,860 weighted-policy-direction rows. `--pool=A` or `--pool=B` selects one stream; `--block-size=128` can be changed from 1 to 256.

For new fits, add `--draws-dir="$ALABAMA_RUN"` and choose a different output directory. This checks the same historical domain; it does not screen fresh reports or establish precision for every newly near-threshold pair. Observed constant indicators retain unavailable MCSE. Criterion-specific precision, the .005 weighted-coefficient target, and fixed-report loss precision are separate diagnostics. Archived mode checks the supplied two-row reference; new-fit mode records its actual results without requiring agreement.

## 3. Refit the unified measurement program

Check [the model guide](model.md), configure CmdStan 2.38.0, and choose a new run directory. The packets run serially; each fit has its own files and status.

```sh
export ALABAMA_RUN="$PWD/../alabama-fresh-results"
Rscript scripts/02_fit_models.R --check --cache="$ALABAMA_CACHE"
Rscript scripts/02_fit_models.R --group=county --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
Rscript scripts/02_fit_models.R --group=tract-main --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
Rscript scripts/02_fit_models.R --group=tract-stress --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
```

There are four county fits, four main-tract fits, and six sensitivity A fits. No sensitivity B is fitted. Sampling settings and seeds are recorded per fit. `--fit=FIT_ID` runs an individual row of [config/fit-settings.csv](../config/fit-settings.csv). `--predictive` additionally saves the replicated noisy observations as an RDS object; the ordinary native target arrays suffice for the reported review calculations.

Each output fit directory contains native posterior arrays, original iteration/chain coordinates, settings, summaries, HMC diagnostics, source/input hashes, and raw CmdStan CSVs. Keep enough space for these large files. Existing nonempty fit directories are rejected. A failed HMC gate does not authorize adoption: inspect `diagnostics.json` and `run-status.json`, preserve the attempt, and use a separate new directory for any justified repair. Completed fit execution and acceptable HMC are recorded separately.

After all required fits pass:

```sh
Rscript scripts/03_posterior_summaries.R --central --draws-dir="$ALABAMA_RUN" --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
Rscript scripts/04_review_reports.R --reselect --draws-dir="$ALABAMA_RUN" --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
Rscript scripts/05_tables.R --posterior-dir="$ALABAMA_RUN/posterior" --report-dir="$ALABAMA_RUN/review-reports/reselect" --fit-dir="$ALABAMA_RUN/fits" --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
Rscript scripts/06_figures.R --all --posterior-dir="$ALABAMA_RUN/posterior" --report-dir="$ALABAMA_RUN/review-reports/reselect" --cache="$ALABAMA_CACHE" --output="$ALABAMA_RUN"
```

The figure command requires both new-run directories, the complete 14-case report run, and its 720-row stability readout. It replaces posterior, band, risk, capacity and stability values together; geographic boundaries, observed application facts and ACS context remain the same study inputs. Import the geographic archive as in route 1 before drawing fresh maps. Archived posterior archives are not needed when the required new fits are supplied.

Fresh figures derive their band counts and first-band membership from the new reports. B3 and P share one plotted point only when their fresh pair relations agree. Units and publication calibrations are retained; preflight stops before writing PDFs if fresh point means fall outside supported display ranges, so an unsupported result is not silently clipped.

Explicit `--draws-dir` never falls back to archived draws. Script 04 selects on each model's A and evaluates fixed actions on the corresponding main-model B. The A and B computations use the same observed data; B is numerical evaluation, not empirical validation on held-out communities. Use a fresh output directory when repeating report selection; a nonempty report run is rejected.

## Rebuild observations from the official sources

[DATA-SOURCES.md](../DATA-SOURCES.md) and [the data guide](data.md) identify the exact source files and transformations. Rebuild the extracts to another directory first:

```sh
Rscript scripts/01_prepare_data.R --from-source="/path/to/official-files" --atlas --acs --context --applications --download --cache="$ALABAMA_CACHE" --output="$PWD/../alabama-rebuilt-inputs"
```

`--download` is explicit; supplied files are checked before parsing. Omit it for an entirely local rebuild. Add `--geometry` to rebuild the deposited geographic layers. ADECA inputs have source-specific redistribution conditions, so the repository supplies factual extracts and links rather than distributing the original workbook and documents.

The fit script reads the included `data/analysis/` inputs. After comparing rebuilt inputs with them, `--write-inputs` deliberately replaces those small extracts for a source-to-fit run. Keep a clean source copy and preserve the original checksums when doing this.

## Assess agreement

Saved-output replay should preserve IDs, missing values, bands, application counts, units and printed rounding. New fits need not be byte-identical. Evaluate HMC first, then differences relative to criterion-specific Monte Carlo uncertainty and solver tolerances. A changed but equivalent band labeling can have identical pair relations; compare the relations and objective bounds. Do not tune the model, suppress a diagnostic failure, or overwrite the archived results to make a new run resemble the article.

The supplied draws, full-vector dependence and original coordinates support these checks. Constant-indicator MCSEs remain missing unless the quantity is algebraically constant. The finite-draw solver certificate, HMC diagnostics, and Monte Carlo precision answer separate numerical questions. See [model.md](model.md) for their definitions.
