# JoonHo Lee (jlee296@ua.edu)
# Recompute fixed published reports, or explicitly repeat report selection.
# Inputs: archived/native posterior arrays, scenario settings and raw estimates.
# Outputs: reports, risk/MCSE/capacity/stability tables outside the source tree.
# Examples:
#   Rscript scripts/04_review_reports.R --saved-only
#   Rscript scripts/04_review_reports.R --reselect --scenario=center
#   Rscript scripts/04_review_reports.R --reselect --draws-dir=/path/to/new/run
# A restricted run also includes its center reference. Only --reselect starts HiGHS.
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE))
source(file.path(dirname(script_path), "_bootstrap.R"))
if (file.exists(rep_path("R", "direction.R"))) source(rep_path("R", "direction.R"))
allowed <- c("saved-only", "reselect", "scenario", "draws-dir", "cache", "output")
if (any(!names(opt) %in% allowed)) stop("Unknown review-report option.")
if (isTRUE(opt[["saved-only"]]) == isTRUE(opt$reselect)) {
  stop("Choose exactly one of --saved-only and --reselect.")
}
mode <- if (isTRUE(opt$reselect)) "reselect" else "saved-only"
scenarios <- if (is.null(opt$scenario)) NULL else strsplit(opt$scenario, ",", fixed = TRUE)[[1L]]
rep_run_review_reports(mode, scenarios, opt[["draws-dir"]])
