# Prioritizing Places under Uncertainty

JoonHo Lee ([jlee296@ua.edu](mailto:jlee296@ua.edu))

Replication materials for [*Prioritizing Places under Uncertainty: Historical Opportunity Indicators and the Order of Community Development Review in Alabama* (2026)](https://doi.org/10.5281/zenodo.23966864).

This compendium contains one annotated Stan program, the R analysis scripts, small Alabama observation tables, saved review results, and the article's tables and figures. The county models cover 67 counties; the county review covers 64 candidates. The tract models cover 1,173 tracts, and the common tract review covers 90 pilot tracts. Eight unmodeled tract keys remain explicit in the coverage data.

Large posterior and geographic files are in the companion [Zenodo dataset, version 1.0.0](https://doi.org/10.5281/zenodo.23984957). The [dataset manifest](data/metadata/zenodo-manifest.json) identifies the required archives and checksums. The [data guide](docs/data.md) explains their contents and provenance.

## Start here

Use R 4.6.0. In R, install the small bootstrap dependencies if needed:

```r
install.packages(c("renv", "jsonlite", "digest"), repos = "https://cloud.r-project.org")
```

From this repository's root:

```sh
Rscript scripts/00_setup.R --restore
Rscript scripts/00_setup.R --check
Rscript scripts/05_tables.R
Rscript scripts/06_figures.R --figure=fig3
```

Scripts print their output locations. Results and downloaded files go to an external cache by default; `--cache` and `--output` select other locations. These examples use small supplied results and start no model fit or optimization.

The [reproduction guide](docs/reproduce.md) gives commands for all figures, posterior-based report selection, and complete refitting. Maps require the geographic archive and optional GIS dependencies; Figure 1 requires TeX. See the [software guide](docs/software.md) before installing these components.

Browse the [table index](tables/README.md), [figure index](figures/README.md), and [model explanation](docs/model.md). The [postrank website](https://joonho112.github.io/postrank/) documents the R package; the [postrank explorer](https://joonho112.github.io/postrank-explorer/) illustrates the framework.

Code is MIT licensed. Author-created data products and documentation are CC BY 4.0; upstream terms and attribution are in [DATA-SOURCES.md](DATA-SOURCES.md). Cite the article using [CITATION.cff](CITATION.cff).
