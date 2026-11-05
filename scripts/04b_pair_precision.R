# JoonHo Lee (jlee296@ua.edu)
# Appendix C's optional coefficient-precision diagnostic on fixed tract pairs.
# --check reads only small design/reference files. --run opens authenticated
# main-tract A/B arrays after acquiring the shared heavy-work lock.
# Examples:
#   Rscript scripts/04b_pair_precision.R --check
#   Rscript scripts/04b_pair_precision.R --run --cache=/path/to/cache
#   Rscript scripts/04b_pair_precision.R --run --draws-dir=/path/to/fresh/run --output=/path/to/new/results
# Fresh draws use the same historical pair domain; no new screen or report
# selection is performed. Computed row tables stay in external output.
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE))
source(file.path(dirname(script_path), "_bootstrap.R"))
if (!exists("rep_pair_precision", mode = "function")) source(rep_path("R", "pair_precision.R"))
allowed <- c("check", "run", "draws-dir", "pool", "block-size", "cache", "output")
if (any(!names(opt) %in% allowed)) stop("Unknown pair-precision option.")
if (isTRUE(opt$check) == isTRUE(opt$run)) stop("Choose exactly one of --check and --run.")
if (isTRUE(opt$check)) {
  if (any(c("draws-dir", "pool", "block-size") %in% names(opt))) {
    stop("--check validates small design files only; run options belong with --run.")
  }
  cat(jsonlite::toJSON(rep_check_pair_precision(), auto_unbox = TRUE, pretty = TRUE), "\n")
} else {
  pool <- if (is.null(opt$pool)) "both" else opt$pool
  if (!pool %in% c("A", "B", "both")) stop("Choose --pool=A, B, or both.")
  pools <- if (pool == "both") c("A", "B") else pool
  block <- if (is.null(opt[["block-size"]])) 128L else as.numeric(opt[["block-size"]])
  result <- rep_pair_precision(opt[["draws-dir"]], pools, block)
  print(result$summary, row.names = FALSE)
  cat("Wrote", result$output_dir, "\n")
}
