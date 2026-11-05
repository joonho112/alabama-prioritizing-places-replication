# JoonHo Lee (jlee296@ua.edu)
# Locate this compendium from the executable script, independent of getwd().
file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(file_arg) != 1L) stop("Run a numbered script with Rscript.")
script_file <- normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg),
                                  fixed = TRUE), mustWork = TRUE)
root <- dirname(dirname(script_file))
source(file.path(root, "R", "io.R"))
opt <- rep_options()
rep_init(root, cache = opt$cache, output = opt$output)
# Source definitions in an explicit order; none of these files starts computation.
modules <- c("data.R", "model.R", "posterior.R", "reports.R", "pair_precision.R", "direction.R", "tables.R", "figures.R")
for (module in modules) {
  path <- rep_path("R", module)
  if (file.exists(path)) source(path)
}
