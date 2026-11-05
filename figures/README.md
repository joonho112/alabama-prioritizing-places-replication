# Figures

JoonHo Lee (jlee296@ua.edu)

The PDFs reproduce the article and supplementary figures. The three larger
supplements show complete county/tract identifiers in the pair matrices and all
90 pilot tracts in one stability chart. See [the output index](../docs/output-index.csv)
for the correspondence between paper numbers, filenames and script keys.

From the repository root, after installing the documented R dependencies:

```sh
Rscript scripts/06_figures.R --figure=fig3 --output=.
Rscript scripts/06_figures.R --all --output=. --cache=/path/to/data-cache
```

`--output=.` writes the PDFs here. Without `--output`, results go to the analysis
cache. `--png` adds 500 dpi PNG companions. Rebuilding these figures uses saved
summaries and starts neither model fitting nor report selection.

For a complete new analysis, pass both the stage 03 posterior directory and the
stage 04 report directory (`saved-only` or `reselect`), using a new output location:

```sh
Rscript scripts/06_figures.R --all --posterior-dir=/path/to/posterior --report-dir=/path/to/review-reports/reselect --output=/path/to/new-plots
```

The run must contain all 14 cases, 67 fixed reports, and 720 tract-case rows.
Fresh risks, bands, capacities, stability, and posterior summaries are used
together; missing statistics stop the build. Band displays support up to five
occupied bands. B3 and P are shown separately when their relations differ, and
Figure 7 gains row space if the new first band is larger. The outcome panels
retain the article's calibration: means outside the supported ranges stop the
build before any PDF, so a substantially different fit receives an explicit
display review. `--saved-dir` applies only to archived summaries.

| Script key | Output |
|---|---|
| `fig1` | Workflow (Figure 1; requires `pdflatex`) |
| `fig2`–`fig8` | Corresponding main-text figure |
| `figS1` | Tract precision maps (Figure B.1) |
| `figS2` | Posterior adjustment maps (Figure B.2) |
| `figS3` | Pair matrices (Figures D.1–D.2), plus two larger identifier versions |
| `figS4` | Tract stability (Figure E.1, three parts), plus the complete 90-tract sheet |
| `figS5` | Current conditions at their source geographies (Figure A.1) |

Maps (`fig2`, `fig4`, `fig8`, `figS1`, `figS2`, `figS5`) additionally require the
geography archive in the data cache. The remaining figures work without loading
GIS libraries. Map simplification changes display geometry only; values remain
attached to the original geography keys. Missing estimates and places outside
the county-government route retain their distinct encodings.

The plotting code is [R/figures.R](../R/figures.R); the workflow source is
[fig_src/fig1_workflow.tex](../fig_src/fig1_workflow.tex). R opens a Cairo vector
PDF device, with the native Quartz vector device as an explicit fallback on
macOS when Cairo is unavailable. Both use Helvetica. Matrix cells remain
vector rectangles, including the reversed row axis: the upper-right blue cells
report the row place as higher need, and the lower-left red cells report the
column place as higher need.
