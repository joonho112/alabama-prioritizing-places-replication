# Saved analysis results

JoonHo Lee ([jlee296@ua.edu](mailto:jlee296@ua.edu))

These files preserve the numerical results used in the paper. They support figure and table reproduction without fitting models again. The posterior-to-report workflow can recompute the statistical summaries and review reports from the companion dataset. Definitions and units are in `data-dictionary.csv`.

- `county-results.csv` contains all 67 counties. Only 64 are candidates for the county report. `county-review-results.csv` adds the central report's whole-band status at capacities 3 and 5.
- `tract-results.csv` retains all 1,181 tract keys, including eight without posterior estimates. `pilot90-tract-results.csv` and `first-band-tracts.csv` are explicit subsets. Only the 90 pilot tracts receive bands.
- `report-metrics.csv` compares B0–B3 and P at the central settings. `sensitivity.csv` records policy changes, three tract-model alternatives, and the separate direction constraint. Selection uses A; fixed-report evaluation uses B. Model alternatives share the main-model B reference.
- `capacity-status.csv` records intact-band review status. `tract-stability-cases.csv` contains 90 tracts across eight cases. The exploratory direction constraint is excluded from these frequencies.
- `hmc-production-attempts.csv` summarizes all 20 relevant production attempts: six county and fourteen tract attempts. Fourteen are selected. The parameter and chain diagnostic files provide the corresponding detail, including superseded and failed attempts.
- `pair-precision-summary.csv` records the Appendix C A/B coefficient-precision counts. Script 04b recomputes them from the main-tract draws and the supplied fixed historical pair domain; unavailable indicator MCSE remains explicit.
- `hyperparameters.csv` records the six tract-model population summaries in Table B.1. The table builder recomputes them from `hyperparameter-draws.rds`: two actual selection-stream arrays with 4,000 retained iterations, four chains, and three parameters (alpha, county SD, and tract SD). Iteration/chain coordinates and source-file hashes are retained; no fitting is needed. `readiness-summary.csv` and `rating-summary.csv` preserve the authored text and structure of Tables 2 and F.1. `applications-by-fund.csv` summarizes the 72 received applications; amounts are requests.

Outcome summaries use natural points: percentile points for income rank and percentage points for incarceration. Entity intervals are separate-criterion 95% equal-tailed intervals. Hyperparameter intervals are 90% intervals on the logit scale. Posterior adjustment is the posterior mean minus the published estimate; it is not change over time.

Expected reporting loss includes the costs of wrong disclosed directions and withheld comparisons. It is not an error probability. Band labels are meaningful within their stated candidate set. A withheld comparison does not establish equality, and the bands do not establish project eligibility or an award recommendation.
