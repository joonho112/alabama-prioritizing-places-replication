# JoonHo Lee (jlee296@ua.edu)
# Regenerate the article figures from documented saved summaries and context.
# Examples:
#   Rscript scripts/06_figures.R --figure=fig3
#   Rscript scripts/06_figures.R --figure=fig8 --cache=/path/to/cache
#   Rscript scripts/06_figures.R --all --output=/path/to/new/run
#   Add both --report-dir=/path/to/04/output and --posterior-dir=/path/to/03/output
#   to use a complete fresh analysis instead of the archived scientific summaries.
# --png also writes 500 dpi PNG companions. GIS is required only for requested maps.
# Figure 1 additionally needs pdflatex. No model fitting or solver is started.
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE))
source(file.path(dirname(script_path), "_bootstrap.R"))
allowed <- c("figure", "all", "png", "saved-dir", "cache", "output", "report-dir", "posterior-dir")
if (any(!names(opt) %in% allowed)) stop("Unknown figure option.")
if (isTRUE(opt$all) == !is.null(opt$figure)) {
  stop("Choose exactly one of --all and --figure=KEYS.")
}
figures <- if (isTRUE(opt$all)) NULL else strsplit(opt$figure, ",", fixed = TRUE)[[1L]]
rep_make_figures(figures, saved_dir = if (is.null(opt[["saved-dir"]])) {
  rep_path("data", "saved")
} else {
  opt[["saved-dir"]]
}, png = isTRUE(opt$png), report_dir = opt[["report-dir"]], posterior_dir = opt[["posterior-dir"]])
