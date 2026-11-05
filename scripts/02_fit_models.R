# JoonHo Lee (jlee296@ua.edu)
# Explicitly fit one recorded measurement model or a serial packet.
# Input: package observation CSVs and config/fit-settings.csv.
# Output: a new external run folder with native arrays and diagnostics.
# Examples:
#   Rscript scripts/02_fit_models.R --check
#   Rscript scripts/02_fit_models.R --fit=M1-A-income --output=/path/to/run
#   Rscript scripts/02_fit_models.R --group=tract-main --output=/path/to/run
# CmdStan 2.38.0 and a C++ toolchain are required only for actual fitting.
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE))), "_bootstrap.R"))

allowed <- c("check", "fit", "group", "compile-only", "predictive", "cache", "output")
if (any(!names(opt) %in% allowed)) stop("Unknown fitting option.")
if (isTRUE(opt$check)) {
  print(rep_fit_settings()[, c("fit_id", "seed", "N", "latent_branch",
                               "iter_sampling", "adapt_delta", "max_treedepth")],
        row.names = FALSE)
} else if (isTRUE(opt[["compile-only"]])) {
  lock <- rep_acquire_lock("compile-model")
  tryCatch(rep_compile_model(quiet = FALSE), finally = rep_release_lock(lock))
} else if (!is.null(opt$fit) && is.null(opt$group)) {
  result <- rep_fit_one(opt$fit, save_predictive = isTRUE(opt$predictive))
  if (!isTRUE(result$diagnostics$hard_hmc_passed)) quit(status = 2L)
} else if (!is.null(opt$group) && is.null(opt$fit)) {
  rep_fit_group(opt$group, save_predictive = isTRUE(opt$predictive))
} else {
  stop("Use --check, --compile-only, --fit=ID, or --group=NAME.")
}
