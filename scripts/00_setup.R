# JoonHo Lee (jlee296@ua.edu)
# Check the software environment, or restore the recorded R packages explicitly.
# Example: Rscript scripts/00_setup.R --check
#          Rscript scripts/00_setup.R --restore
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE))), "_bootstrap.R"))

allowed <- c("check", "restore", "maps", "fits", "sources", "cache", "output")
if (any(!names(opt) %in% allowed)) stop("Unknown setup option.")

wanted <- rep_read_csv(rep_path("config", "software.csv"))
roles <- c("core", if (isTRUE(opt$maps)) "maps", if (isTRUE(opt$fits)) "fits",
           if (isTRUE(opt$sources)) "sources")
wanted <- wanted[wanted$role %in% roles, , drop = FALSE]

if (isTRUE(opt$restore)) {
  rep_require(c("renv", "jsonlite", "digest"))
  rep_assert_sha256(rep_path(rep_config()$postrank_source$path),
                    rep_config()$postrank_source$sha256)
  rep_dir(rep_cache("library"))
  # Resolve the vendored archive in memory. The committed lockfile stays
  # relative and portable, while restoration also works from another cwd.
  lockfile <- renv::lockfile_read(rep_path("renv.lock"))
  lockfile$Packages$postrank$RemoteUrl <- rep_path(rep_config()$postrank_source$path)
  renv::restore(project = root, library = rep_cache("library"),
                lockfile = lockfile, packages = wanted$package, prompt = FALSE)
} else {
  installed <- vapply(wanted$package, function(p) {
    if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA_character_
  }, character(1))
  match_version <- mapply(function(a, b) {
    !is.na(a) && utils::compareVersion(a, b) == 0L
  }, installed, wanted$version, USE.NAMES = FALSE)
  print(data.frame(package = wanted$package, recorded = wanted$version,
                   installed = unname(installed), matches = match_version,
                   row.names = NULL))
  cat("R:", as.character(getRversion()), "(recorded: 4.6.0)\n")
  cat("Use --maps for GIS, --fits for CmdStanR, --sources for workbook parsing.\n")
  if (isTRUE(opt$fits)) {
    rep_require("cmdstanr")
    cat("CmdStan:", as.character(cmdstanr::cmdstan_version()), "(recorded: 2.38.0)\n")
  }
  if (any(!match_version)) quit(status = 1L)
}
