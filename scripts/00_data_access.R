# JoonHo Lee (jlee296@ua.edu)
# Obtain and verify the frozen analytic dataset without starting fitting.
# --archive-dir supplies the local deposit files before their publication.
# --download uses the versioned public file URLs recorded in the manifest.
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE))), "_bootstrap.R"))

allowed <- c("archive-dir", "download", "files", "cache", "output")
if (any(!names(opt) %in% allowed)) stop("Unknown data-access option.")

manifest <- rep_read_json(rep_path("data", "metadata", "zenodo-manifest.json"), FALSE)
# A configured DOI may still belong to a draft. Explicit downloads will work
# once Zenodo publishes the record; local archive import also works beforehand.
if (isTRUE(opt$download) && !manifest$status %in% c("configured", "published")) {
  stop("Dataset DOI and file URLs are not configured. Supply --archive-dir instead.")
}
if (is.null(opt$`archive-dir`) && !isTRUE(opt$download)) {
  stop("Specify --archive-dir=/path/to/files or --download.")
}
if (!length(manifest$files)) stop("Dataset files have not been configured.")
records <- manifest$files
if (!is.null(opt$files)) {
  requested <- strsplit(opt$files, ",", fixed = TRUE)[[1L]]
  available <- vapply(records, function(x) x$name, character(1))
  if (any(!requested %in% available)) stop("Unknown dataset archive name.")
  records <- records[match(requested, available)]
}
for (record in records) {
  dest <- rep_cache("downloads", record$name)
  rep_dir(dirname(dest))
  if (!is.null(opt$`archive-dir`)) {
    origin <- file.path(opt$`archive-dir`, record$name)
    rep_assert_sha256(origin, record$sha256)
    if (!file.copy(origin, dest, overwrite = TRUE)) stop("Copy failed: ", origin)
  } else if (!file.exists(dest)) {
    if (is.null(record$url) || !nzchar(record$url)) stop("File URL is missing.")
    # Only a verified complete file becomes a reusable cache entry. A failed
    # download must not make later calls skip an incomplete final path.
    temporary <- paste0(dest, ".part")
    # Full posterior archives can take several minutes on slower connections.
    # Keep a longer user timeout and restore the original setting afterwards.
    previous_timeout <- getOption("timeout")
    options(timeout = max(3600, previous_timeout))
    tryCatch({
      utils::download.file(record$url, temporary, mode = "wb", method = "libcurl")
      rep_assert_sha256(temporary, record$sha256)
      if (!file.rename(temporary, dest)) stop("Cannot store verified dataset archive.")
    }, finally = {
      options(timeout = previous_timeout)
      unlink(temporary)
    })
  }
  rep_assert_sha256(dest, record$sha256)
  # Archives are prepared from known relative paths. Refuse traversal on import.
  entries <- utils::unzip(dest, list = TRUE)$Name
  if (any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", entries))) stop("Unsafe archive path.")
  utils::unzip(dest, exdir = rep_dataset())
  message("Verified and imported ", record$name)
}
