# JoonHo Lee (jlee296@ua.edu)
# Check the included inputs or rebuild selected extracts from pinned official files.
# Examples:
#   Rscript scripts/01_prepare_data.R --check
#   Rscript scripts/01_prepare_data.R --from-source=/path/to/sources --atlas
#   Rscript scripts/01_prepare_data.R --from-source=/path/to/sources --acs --context --applications
# Add --download to acquire missing official files explicitly.
# Rebuilt files go to --output=/path/to/rebuilt-data by default. --write-inputs
# deliberately replaces the included analysis inputs with their rebuilt versions.
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script <- normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE))
source(file.path(dirname(script), "_bootstrap.R"))
allowed <- c("check", "from-source", "atlas", "acs", "context", "applications", "download",
             "write-inputs", "geometry", "cache", "output")
if (any(!names(opt) %in% allowed)) stop("Unknown data-preparation option.")
if (isTRUE(opt$check)) {
  print(rep_check_inputs())
} else {
  if (is.null(opt[["from-source"]])) stop("Supply --from-source=/path/to/official-files.")
  output <- if (isTRUE(opt[["write-inputs"]])) rep_path("data") else rep_output("prepared-data")
  requested <- c(atlas = isTRUE(opt$atlas), acs = isTRUE(opt$acs),
                 applications = isTRUE(opt$applications), context = isTRUE(opt$context), geometry = isTRUE(opt$geometry))
  if (!any(requested)) stop("Choose --atlas, --acs, --context, --applications, or --geometry.")
  if (requested[["atlas"]]) rep_prepare_atlas(opt[["from-source"]], output, isTRUE(opt$download))
  if (requested[["acs"]]) rep_prepare_acs_poverty(opt[["from-source"]], output, isTRUE(opt$download))
  if (requested[["context"]]) rep_prepare_current_context(opt[["from-source"]], output, isTRUE(opt$download))
  if (requested[["applications"]]) rep_prepare_applications(opt[["from-source"]], output, isTRUE(opt$download))
  if (requested[["geometry"]]) rep_prepare_geography(opt[["from-source"]],
    rep_output("geography", "geography.gpkg"), isTRUE(opt$download))
  message("Prepared inputs: ", output)
}
