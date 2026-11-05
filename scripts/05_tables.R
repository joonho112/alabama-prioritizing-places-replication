# JoonHo Lee (jlee296@ua.edu)
# Rebuild the eight printed tables as unrounded CSV, readable HTML, and TeX.
# Default output is outside the source tree; --write-public updates tables/.
# Examples:
#   Rscript scripts/05_tables.R
#   Rscript scripts/05_tables.R --write-public
#   Rscript scripts/05_tables.R --report-dir=/path/to/review-reports --posterior-dir=/path/to/posterior --fit-dir=/path/to/fits
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script <- normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE))
source(file.path(dirname(script), "_bootstrap.R"))
allowed <- c("write-public", "report-dir", "posterior-dir", "fit-dir", "cache", "output")
if (any(!names(opt) %in% allowed)) stop("Unknown table-building option.")
output <- if (isTRUE(opt[["write-public"]])) rep_path("tables") else rep_output("tables")
rep_build_tables(output, opt[["report-dir"]], opt[["posterior-dir"]], opt[["fit-dir"]])
