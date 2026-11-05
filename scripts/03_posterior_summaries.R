# JoonHo Lee (jlee296@ua.edu)
# Regenerate native posterior summaries from archived or explicitly supplied draws.
# Inputs: dataset posterior archives and the package's small observation CSVs.
# Outputs: CSV summaries under the requested external result directory.
# Examples:
#   Rscript scripts/03_posterior_summaries.R --central
#   Rscript scripts/03_posterior_summaries.R --model=PV-TM1 --pool=B
#   Rscript scripts/03_posterior_summaries.R --central --draws-dir=/path/to/new/run
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE))
source(file.path(dirname(script_path), "_bootstrap.R"))
allowed <- c("central", "model", "pool", "draws-dir", "cache", "output")
if (any(!names(opt) %in% allowed)) stop("Unknown posterior summary option.")
models <- if (isTRUE(opt$central)) c("M1", "PV-TM1") else opt$model
if (is.null(models)) stop("Use --central or --model=MODEL.")
pool <- if (is.null(opt$pool)) "A" else opt$pool
if (!pool %in% c("A", "B")) stop("Choose pool A or B.")
for (model in models) {
  summaries <- rep_posterior_summaries(model, pool, opt[["draws-dir"]])
  path <- rep_output("posterior", paste0(model, "-", pool, "-summary.csv"))
  rep_write_csv(summaries, path)
  cat("Wrote", path, "\n")
}
