# JoonHo Lee (jlee296@ua.edu)
# Paths, small files, and explicit run directories shared by the analysis scripts.
# Reading this file neither installs software nor starts an analysis.

.rep <- new.env(parent = emptyenv())

rep_init <- function(root, cache = NULL, output = NULL) {
  root <- normalizePath(root, mustWork = TRUE)
  stopifnot(file.exists(file.path(root, "CITATION.cff")))
  .rep$root <- root
  if (is.null(cache) || !nzchar(cache)) {
    cache <- tools::R_user_dir("alabama-prioritizing-places", "cache")
  }
  .rep$cache <- rep_absolute_path(cache)
  local_library <- file.path(.rep$cache, "library")
  if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))
  if (is.null(output) || !nzchar(output)) output <- file.path(.rep$cache, "results")
  .rep$output <- rep_absolute_path(output)
  invisible(.rep)
}

# Resolve relative paths before scripts change their working directory.
rep_absolute_path <- function(path) {
  path <- path.expand(path)
  absolute <- grepl("^(/|[A-Za-z]:)", path) || startsWith(path, "\\\\")
  if (!absolute) path <- file.path(getwd(), path)
  normalizePath(path, mustWork = FALSE)
}

rep_path <- function(...) file.path(.rep$root, ...)
rep_cache <- function(...) file.path(.rep$cache, ...)
rep_output <- function(...) file.path(.rep$output, ...)

rep_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, mustWork = TRUE)
}

rep_require <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

rep_read_csv <- function(path, ...) {
  # IDs are text by default: numeric conversion would remove their leading zero.
  utils::read.csv(path, colClasses = "character", check.names = FALSE,
                  na.strings = c("", "NA"), stringsAsFactors = FALSE, ...)
}

rep_write_csv <- function(x, path) {
  rep_dir(dirname(path))
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

rep_read_json <- function(path, simplifyVector = TRUE) {
  rep_require("jsonlite")
  jsonlite::read_json(path, simplifyVector = simplifyVector)
}

rep_write_json <- function(x, path) {
  rep_require("jsonlite")
  rep_dir(dirname(path))
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null", digits = NA)
  invisible(path)
}

rep_sha256 <- function(path) {
  rep_require("digest")
  digest::digest(file = path, algo = "sha256")
}

rep_assert_sha256 <- function(path, expected) {
  if (!file.exists(path) || !identical(rep_sha256(path), expected)) {
    stop("Missing file or SHA-256 mismatch: ", path)
  }
  invisible(path)
}

rep_options <- function(args = commandArgs(trailingOnly = TRUE)) {
  # Named --key=value arguments keep example commands short and unambiguous.
  out <- list()
  for (arg in args) {
    if (!grepl("^--[a-z][a-z0-9-]*(=|$)", arg)) stop("Invalid argument: ", arg)
    key <- sub("=.*$", "", sub("^--", "", arg))
    if (!is.null(out[[key]])) stop("Repeated argument: ", key)
    out[[key]] <- if (grepl("=", arg)) sub("^[^=]*=", "", arg) else TRUE
  }
  out
}

rep_config <- function() rep_read_json(rep_path("config", "analysis.json"))

rep_acquire_lock <- function(kind) {
  # All heavy runs share one lock. A stale lock must be inspected explicitly.
  path <- rep_cache("heavy-job.lock")
  rep_dir(dirname(path))
  if (!dir.create(path, showWarnings = FALSE)) {
    stop("A heavy-job lock already exists: ", path)
  }
  rep_write_json(list(kind = kind, pid = Sys.getpid(),
                     started = format(Sys.time(), tz = "UTC", usetz = TRUE)),
                 file.path(path, "owner.json"))
  path
}

rep_release_lock <- function(path) unlink(path, recursive = TRUE)

# Include the content manifest in the cache key to isolate dataset revisions.
rep_dataset <- function(...) {
  manifest_path <- rep_path("data", "metadata", "zenodo-manifest.json")
  manifest <- rep_read_json(manifest_path)
  version <- manifest$version
  if (!is.character(version) || !grepl("^[A-Za-z0-9._-]+$", version)) {
    stop("Invalid dataset version.")
  }
  key <- paste0(version, "-", substr(rep_sha256(manifest_path), 1L, 12L))
  rep_cache("dataset", key, ...)
}
