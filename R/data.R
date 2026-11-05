# JoonHo Lee (jlee296@ua.edu)
# Read the study inputs, rebuild the Atlas extracts, and parse public context data.
# Inputs remain on their native scales. Source files are downloaded only when
# requested explicitly, and their checksums are verified before parsing.
# This module defines functions only. GIS packages are loaded only by rep_geo().

# ---- Source acquisition and geographic identifiers ----

rep_sources <- function() {
  rep_read_csv(rep_path("data", "metadata", "source-files.csv"))
}

rep_source_file <- function(source_id, source_dir, download = FALSE) {
  catalog <- rep_sources()
  item <- catalog[catalog$source_id == source_id, , drop = FALSE]
  if (nrow(item) != 1L) stop("Unknown source: ", source_id)
  file <- file.path(source_dir, item$filename)
  if (!file.exists(file)) {
    if (!isTRUE(download)) stop("Supply the official source file: ", file)
    rep_dir(source_dir)
    temporary <- paste0(file, ".part")
    on.exit(unlink(temporary), add = TRUE)
    utils::download.file(item$url, temporary, mode = "wb", method = "libcurl")
    rep_assert_sha256(temporary, item$sha256)
    if (!file.rename(temporary, file)) stop("Cannot store downloaded source.")
  }
  rep_assert_sha256(file, item$sha256)
  file
}

rep_pad_code <- function(x, width) {
  x <- as.character(x)
  if (anyNA(x) || any(!grepl(paste0("^[0-9]{1,", width, "}$"), x))) {
    stop("Invalid native geographic code.")
  }
  sprintf(paste0("%0", width, "d"), as.integer(x))
}

rep_atlas_keys <- function(x, tract = FALSE) {
  county <- paste0(rep_pad_code(x$state, 2L), rep_pad_code(x$county, 3L))
  if (tract) paste0(county, rep_pad_code(x$tract, 6L)) else county
}

rep_atlas_extract <- function(file, tract = FALSE) {
  x <- rep_read_csv(file)
  columns <- c("state", "county", if (tract) "tract",
               "kfr_pooled_pooled_p25", "kfr_pooled_pooled_p25_se",
               "jail_pooled_pooled_p25", "jail_pooled_pooled_p25_se")
  if (!all(columns %in% names(x))) stop("Atlas source schema changed.")
  selected <- which(x$state == "1")
  x <- x[selected, , drop = FALSE]
  ids <- rep_atlas_keys(x, tract)
  if (anyDuplicated(ids)) stop("Duplicated Atlas source key.")
  result <- data.frame(entity_id = ids, county_id = substr(ids, 1L, 5L),
    state_native = x$state, county_native = x$county,
    source_row_ordinal = selected,
    income_estimate_fraction = x$kfr_pooled_pooled_p25,
    income_se_fraction = x$kfr_pooled_pooled_p25_se,
    incarceration_estimate_fraction = x$jail_pooled_pooled_p25,
    incarceration_se_fraction = x$jail_pooled_pooled_p25_se,
    stringsAsFactors = FALSE)
  if (tract) {
    result$tract_id <- ids
    result$tract_native <- x$tract
  }
  result[order(result$entity_id), , drop = FALSE]
}

rep_complete_atlas <- function(x) {
  fields <- c("income_estimate_fraction", "income_se_fraction",
              "incarceration_estimate_fraction", "incarceration_se_fraction")
  values <- lapply(x[fields], as.numeric)
  Reduce(`&`, lapply(values, is.finite)) & values[[2]] > 0 & values[[4]] > 0
}

# ---- Historical Atlas observations and coverage ----

rep_prepare_atlas <- function(source_dir, output_dir, download = FALSE) {
  # The covariate file supplies coverage keys, not a third model criterion.
  county <- rep_atlas_extract(rep_source_file("atlas-county", source_dir, download))
  tract <- rep_atlas_extract(rep_source_file("atlas-tract", source_dir, download), TRUE)
  covariates <- rep_read_csv(rep_source_file("atlas-covariates", source_dir, download))
  original_row <- which(covariates$state == "1")
  covariates <- covariates[original_row, , drop = FALSE]
  keys <- rep_atlas_keys(covariates, TRUE)
  if (anyDuplicated(keys) || !all(tract$entity_id %in% keys)) {
    stop("Tract coverage keys disagree with the selected outcome source.")
  }
  index <- match(keys, tract$entity_id)
  present <- !is.na(index)
  available <- rep_complete_atlas(tract)
  eligible <- present & available[ifelse(present, index, 1L)]
  coverage <- data.frame(entity_id = keys, tract_id = keys,
    county_id = substr(keys, 1L, 5L),
    state_native = covariates$state, county_native = covariates$county,
    tract_native = covariates$tract, source_row_ordinal = tract$source_row_ordinal[index],
    covariate_row_ordinal = original_row, outcome_source_present = present,
    model_eligible = eligible,
    coverage_status = ifelse(!present, "covariate_key_without_outcome_row",
                      ifelse(eligible, "complete_input", "incomplete_source_row")),
    stringsAsFactors = FALSE)
  reasons <- vector("list", length(keys))
  for (criterion in c("income", "incarceration")) {
    y <- as.numeric(tract[[paste0(criterion, "_estimate_fraction")]])[index]
    se <- as.numeric(tract[[paste0(criterion, "_se_fraction")]])[index]
    coverage[[paste0(criterion, "_estimate_missing")]] <- !is.finite(y)
    coverage[[paste0(criterion, "_se_missing")]] <- !is.finite(se)
    coverage[[paste0(criterion, "_se_nonpositive")]] <- is.finite(se) & se <= 0
    coverage[[paste0(criterion, "_negative")]] <- ifelse(is.finite(y), y < 0, NA)
    for (i in which(present)) {
      if (!is.finite(y[i])) reasons[[i]] <- c(reasons[[i]], paste0(criterion, "_estimate_missing"))
      if (!is.finite(se[i])) reasons[[i]] <- c(reasons[[i]], paste0(criterion, "_se_missing"))
      if (is.finite(se[i]) && se[i] <= 0) reasons[[i]] <- c(reasons[[i]], paste0(criterion, "_se_nonpositive"))
    }
  }
  coverage$exclusion_reason <- vapply(seq_along(keys), function(i) {
    if (!present[i]) "outcome_source_row_absent" else paste(reasons[[i]], collapse = ";")
  }, character(1))
  coverage <- coverage[order(coverage$tract_id), , drop = FALSE]
  lookup <- rep_read_csv(rep_path("data", "metadata", "county-names.csv"))
  county$county_name <- lookup$county_name[match(county$county_id, lookup$county_id)]
  if (anyNA(county$county_name) || any(!rep_complete_atlas(county))) {
    stop("County names or published model inputs are incomplete.")
  }
  universe <- county[c("entity_id", "county_id", "county_name")]
  universe$county_government_candidate <- !universe$county_id %in% rep_config()$policy$county_exclusions
  universe$route_status <- ifelse(universe$county_government_candidate,
    "POTENTIAL_NON_ENTITLEMENT_COUNTY_ROUTE", "EXCLUDED_NAMED_ENTITLEMENT_COUNTY")
  universe$full_project_eligibility_status <- "UNVERIFIED_PROJECT_SERVICE_AREA"
  modeled <- tract[available, , drop = FALSE]
  pilot_counties <- rep_read_csv(rep_path("data", "metadata", "pilot-counties.csv"))
  pilot <- modeled[modeled$county_id %in% pilot_counties$county_id,
                   c("entity_id", "tract_id", "county_id"), drop = FALSE]
  pilot$analysis_role <- "historical_need_review_pilot"
  stopifnot(nrow(county) == 67L, nrow(modeled) == 1173L, nrow(coverage) == 1181L,
            sum(universe$county_government_candidate) == 64L, nrow(pilot) == 90L,
            sum(as.numeric(modeled$incarceration_estimate_fraction) < 0) == 123L)
  files <- list("analysis/county-observations.csv" = county,
                "analysis/county-universe.csv" = universe,
                "analysis/tract-observations.csv" = modeled,
                "metadata/tract-coverage.csv" = coverage,
                "analysis/pilot-tract-keys.csv" = pilot)
  for (name in names(files)) rep_write_csv(files[[name]], file.path(output_dir, name))
  invisible(files)
}

# ---- County child-poverty percentage and margin of error ----

rep_acs_county_poverty <- function(table) {
  # Numerator and denominator have the same related-child-in-family universe.
  # Published margins of error are 90% MOEs; no confidence conversion is needed.
  keep <- startsWith(table$GEO_ID, "0500000US01")
  x <- table[keep, c("GEO_ID", "B17006_E001", "B17006_M001",
                    "B17006_E002", "B17006_M002"), drop = FALSE]
  if (nrow(x) != 67L) stop("Expected 67 Alabama county B17006 records.")
  denominator <- as.numeric(x$B17006_E001)
  numerator <- as.numeric(x$B17006_E002)
  denominator_moe <- as.numeric(x$B17006_M001)
  numerator_moe <- as.numeric(x$B17006_M002)
  if (any(!is.finite(denominator) | denominator <= 0) ||
      any(!is.finite(numerator) | numerator < 0 | numerator > denominator) ||
      any(!is.finite(denominator_moe) | denominator_moe < 0) ||
      any(!is.finite(numerator_moe) | numerator_moe < 0)) {
    stop("Invalid B17006 subset-proportion inputs.")
  }
  ratio <- numerator / denominator
  radicand <- numerator_moe^2 - ratio^2 * denominator_moe^2
  fallback <- radicand < 0
  radicand[fallback] <- numerator_moe[fallback]^2 +
    ratio[fallback]^2 * denominator_moe[fallback]^2
  x$county_id <- substr(x$GEO_ID, 10L, 14L)
  x$den <- denominator; x$den_moe <- denominator_moe
  x$num_ <- numerator; x$num_moe <- numerator_moe
  x$share <- 100 * ratio
  x$share_moe <- 100 * sqrt(radicand) / denominator
  x$moe_confidence_level <- 90
  x$moe_addition_fallback <- fallback
  x[order(x$county_id), , drop = FALSE]
}

rep_prepare_acs_poverty <- function(source_dir, output_dir, download = FALSE) {
  file <- rep_source_file("acs-B17006", source_dir, download)
  table <- utils::read.table(file, sep = "|", header = TRUE, colClasses = "character",
                             check.names = FALSE, quote = "", comment.char = "")
  answer <- rep_acs_county_poverty(table)
  rep_write_csv(answer, file.path(output_dir, "analysis", "acs-county-child-poverty.csv"))
  invisible(answer)
}

# ---- Public application roster ----

rep_normalize_county <- function(x) {
  # Match only case, spacing and an optional County suffix. Never fuzzy-match.
  x <- tolower(trimws(gsub("[[:space:]]+", " ", x)))
  sub(" county$", "", x)
}

rep_prepare_applications <- function(source_dir, output_dir, download = FALSE) {
  rep_require("readxl")
  file <- rep_source_file("adeca-applications", source_dir, download)
  temporary <- tempfile("workbook-")
  dir.create(temporary)
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  utils::unzip(file, files = "xl/workbook.xml", exdir = temporary)
  xml <- paste(readLines(file.path(temporary, "xl", "workbook.xml"), warn = FALSE), collapse = "")
  if (grepl('date1904="(1|true)"', xml)) stop("The pinned workbook uses the 1900 date system.")
  cells <- as.data.frame(readxl::read_excel(file, sheet = "Sheet1", range = "A1:G124",
    col_names = FALSE, col_types = rep("text", 7L), trim_ws = FALSE, .name_repair = "minimal"))
  names(cells) <- c("date", "applicant", "county", "amount", "activity", "project", "financial")
  regions <- data.frame(fund = c("Small City", "Large City", "County", "Community Enhancement", "Planning"),
    first = c(4L,39L,61L,89L,117L), last = c(36L,58L,86L,114L,124L))
  lookup <- rep_read_csv(rep_path("data", "metadata", "county-names.csv"))
  lookup_key <- rep_normalize_county(lookup$county_name)
  catalog <- rep_sources(); sha <- catalog$sha256[catalog$source_id == "adeca-applications"]
  result <- list()
  for (i in seq_len(nrow(cells))) {
    region <- which(i >= regions$first & i <= regions$last)
    fields <- unlist(cells[i, 1:6], use.names = FALSE)
    if (length(region) != 1L || anyNA(fields) || any(!nzchar(fields))) next
    if (!grepl("^[A-Z]{2}-[A-Z]{2}-[A-Z]{2}-26-[0-9]{3}$", cells$project[i])) {
      stop("Unrecognized nonempty application row: ", i)
    }
    serial <- suppressWarnings(as.numeric(cells$date[i]))
    if (!is.finite(serial) || serial != floor(serial) || serial == 60) stop("Invalid Excel date.")
    received <- as.Date(serial, origin = "1899-12-30")
    if (received < as.Date("2026-01-01") || received > as.Date("2026-07-22")) stop("Date outside roster cutoff.")
    dollars <- as.numeric(cells$amount[i])
    cents <- round(dollars * 100)
    if (!is.finite(cents) || cents < 0 || abs(dollars * 100 - cents) > 1e-5) {
      stop("Requested amount is not nonnegative whole cents.")
    }
    county <- match(rep_normalize_county(cells$county[i]), lookup_key)
    fund <- regions$fund[region]
    named_county <- grepl(" County$", cells$applicant[i]) &&
      rep_normalize_county(cells$applicant[i]) %in% lookup_key
    type <- if (fund == "County" || named_county) "county_government" else if (
      fund %in% c("Small City", "Large City")) "municipal_government" else "unknown_or_mixed"
    result[[length(result) + 1L]] <- data.frame(
      application_id = sprintf("ADECA-PY2026-Sheet1-%04d", i),
      source_id = "applications-received-2026", source_sha256 = sha,
      source_sheet = "Sheet1", source_row = i, fund_section = fund,
      date_received_iso = format(received, "%Y-%m-%d"),
      applicant_name_raw = cells$applicant[i], county_name_raw = cells$county[i],
      amount_requested_cents_exact = sprintf("%.0f", cents),
      amount_requested_usd_exact = sprintf("%.2f", cents / 100),
      amount_requested_usd = cents / 100, currency = "USD",
      activity_raw = cells$activity[i], project_number_raw = cells$project[i],
      county_id = lookup$county_id[county], county_name_resolved = lookup$county_name[county],
      county_resolution_status = ifelse(is.na(county), "unresolved_no_exact_match", "resolved_exact_normalized_name"),
      government_type = type, stringsAsFactors = FALSE)
  }
  answer <- do.call(rbind, result)
  stopifnot(nrow(answer) == 72L, sum(!is.na(answer$county_id)) == 71L,
    sum(answer$government_type == "municipal_government") == 39L,
    sum(answer$government_type == "county_government") == 14L,
    sum(answer$government_type == "unknown_or_mixed") == 19L)
  if (anyDuplicated(answer$application_id) || anyDuplicated(answer$project_number_raw)) {
    stop("Duplicate application identifier.")
  }
  rep_write_csv(answer, file.path(output_dir, "analysis", "applications.csv"))
  invisible(answer)
}


# ---- ACS and HUD native context ----

rep_context_number <- function(x) {
  x <- gsub("+/-", "", as.character(x), fixed = TRUE)
  x <- gsub("[% ,]", "", x)
  value <- suppressWarnings(as.numeric(x))
  value[!is.finite(value) | value <= -100000000] <- NA_real_
  value
}

rep_prepare_current_context <- function(source_dir, output_dir, download = FALSE) {
  # Only the published metrics used by this study are selected. An unavailable
  # S1701 request is not substituted for the B17006 same-universe calculation.
  metrics <- data.frame(table = c("B01003", "B09001", "B17006"),
    number = c("001", "001", "002"),
    metric = c("total_population", "under18_population", "related_under18_below_poverty_count"),
    universe = c("Total population", "Population under 18 years",
      "Related children under 18 in families"))
  catalog <- rep_sources()
  pieces <- list()
  for (i in seq_len(nrow(metrics))) {
    source_id <- paste0("acs-", metrics$table[i])
    file <- rep_source_file(source_id, source_dir, download)
    table <- utils::read.table(file, sep = "|", header = TRUE, colClasses = "character",
      check.names = FALSE, quote = "", comment.char = "")
    selected <- startsWith(table$GEO_ID, "0500000US01") | startsWith(table$GEO_ID, "1400000US01")
    x <- table[selected, , drop = FALSE]
    kind <- ifelse(startsWith(x$GEO_ID, "050"), "county", "tract")
    id <- substring(x$GEO_ID, 10L)
    raw_y <- x[[paste0(metrics$table[i], "_E", metrics$number[i])]]
    raw_moe <- x[[paste0(metrics$table[i], "_M", metrics$number[i])]]
    y <- rep_context_number(raw_y); moe <- rep_context_number(raw_moe)
    pieces[[length(pieces) + 1L]] <- data.frame(
      source_system = "Census_ACS", source_id = paste0("ACS20245Y_", metrics$table[i]),
      data_period = "2020-2024", estimate_year = "2024", geography_type = kind,
      geography_vintage = "2024 reference; tract2020 Census basis; legal areas2024-01-01",
      geoid = id, county_id = substr(id, 1L, 5L),
      tract_id = ifelse(kind == "tract", id, NA_character_), block_group_id = NA_character_,
      metric_id = metrics$metric[i], estimate = y, moe = moe, moe_confidence_level = "90",
      unit = "persons", moe_unit = "persons", universe = metrics$universe[i],
      estimate_variable = paste0(metrics$table[i], "_", metrics$number[i], "E"),
      moe_variable = paste0(metrics$table[i], "_", metrics$number[i], "M"),
      availability_status = ifelse(is.finite(y), ifelse(is.finite(moe), "available",
        ifelse(raw_moe == "-555555555", "controlled_estimate_MOE_not_applicable",
               "estimate_available_MOE_published_special_value")), "published_special_value"),
      source_sha256 = catalog$sha256[catalog$source_id == source_id], stringsAsFactors = FALSE)
  }
  for (page in c("0000", "2000")) {
    source_id <- paste0("hud-bg-alabama-offset", page)
    file <- rep_source_file(source_id, source_dir, download)
    payload <- rep_read_json(file, simplifyVector = FALSE)
    if (!is.null(payload$error)) stop("HUD source is an error response.")
    fields <- c("STATE", "COUNTY", "TRACT", "BLKGRP", "LOWMOD", "LOWMODUNIV", "LOWMOD_PCT", "MOE_LOWMODPCT")
    rows <- lapply(payload$features, function(feature) {
      values <- lapply(fields, function(name) {
        value <- feature$attributes[[name]]
        if (is.null(value)) NA_character_ else as.character(value)
      })
      names(values) <- fields
      as.data.frame(values, stringsAsFactors = FALSE)
    })
    x <- do.call(rbind, rows)
    id <- paste0(rep_pad_code(x$STATE, 2L), rep_pad_code(x$COUNTY, 3L),
                 rep_pad_code(x$TRACT, 6L), rep_pad_code(x$BLKGRP, 1L))
    denominator <- rep_context_number(x$LOWMODUNIV)
    numerator <- rep_context_number(x$LOWMOD)
    percent <- rep_context_number(x$LOWMOD_PCT)
    stopifnot(all(numerator >= 0 & numerator <= denominator))
    positive <- denominator > 0
    stopifnot(all(abs(percent[positive] - 100 * numerator[positive] / denominator[positive]) <= 0.050000001))
    for (metric in c("LOWMOD", "LOWMODUNIV", "LOWMOD_PCT")) {
      pct <- metric == "LOWMOD_PCT"
      y <- rep_context_number(x[[metric]])
      pieces[[length(pieces) + 1L]] <- data.frame(
        source_system = "HUD_LMISD", source_id = "HUD_Official_LMISD_Map_BG",
        data_period = "2016-2020", estimate_year = "2020", geography_type = "block_group",
        geography_vintage = "2020 Census block group", geoid = id, county_id = substr(id, 1L, 5L),
        tract_id = substr(id, 1L, 11L), block_group_id = id, metric_id = metric,
        estimate = y, moe = if (pct) rep_context_number(x$MOE_LOWMODPCT) else NA_real_,
        moe_confidence_level = NA_character_, unit = if (pct) "percent" else "persons",
        moe_unit = if (pct) "percentage_points" else NA_character_,
        universe = if (pct) "HUD LOWMODUNIV; not ACS poverty universe" else "HUD special-tabulation low/mod income universe",
        estimate_variable = metric, moe_variable = if (pct) "MOE_LOWMODPCT" else NA_character_,
        availability_status = ifelse(is.finite(y),
          ifelse(pct & denominator == 0, "published_percent_zero_universe_not_interpretable_for_eligibility", "available"),
          "published_missing"),
        source_sha256 = catalog$sha256[catalog$source_id == source_id], stringsAsFactors = FALSE)
    }
  }
  context <- do.call(rbind, pieces)
  context <- context[order(context$source_system, context$geography_type, context$geoid, context$metric_id, method = "radix"), ]
  key <- paste(context$source_system, context$geography_type, context$geoid, context$metric_id)
  stopifnot(!anyDuplicated(key), nrow(context) == 16287L)
  selected <- (context$source_system == "Census_ACS" & context$geography_type == "tract") |
    (context$source_system == "HUD_LMISD" & context$metric_id == "LOWMOD_PCT")
  display <- context[selected, , drop = FALSE]
  denominator <- context[context$source_system == "HUD_LMISD" & context$metric_id == "LOWMODUNIV", ]
  hud <- display$source_system == "HUD_LMISD"
  display$HUD_LOWMODUNIV_zero <- NA
  display$HUD_LOWMODUNIV_zero[hud] <- denominator$estimate[match(display$geoid[hud], denominator$geoid)] == 0
  undefined <- hud & !is.na(display$HUD_LOWMODUNIV_zero) & display$HUD_LOWMODUNIV_zero
  display$display_estimate <- display$estimate
  display$display_estimate[undefined] <- NA_real_
  display$display_value_status <- ifelse(undefined, "undefined_percent_zero_native_denominator",
    ifelse(is.finite(display$estimate), "available_native_context_estimate", "native_source_estimate_unavailable"))
  stopifnot(nrow(display) == 8236L, sum(undefined) == 18L)
  rep_write_csv(context, file.path(output_dir, "analysis", "current-context.csv"))
  rep_write_csv(display, file.path(output_dir, "analysis", "current-context-display.csv"))
  invisible(list(context = context, display = display))
}

rep_current_context <- function(display = FALSE) {
  filename <- if (display) "current-context-display.csv" else "current-context.csv"
  rep_read_csv(rep_path("data", "analysis", filename))
}


# ---- Native-vintage and display geometries ----

rep_geographic_source <- function(source_id, source_dir, download = FALSE) {
  catalog <- rep_read_csv(rep_path("data", "metadata", "geography-sources.csv"))
  row <- catalog[catalog$source_id == source_id, , drop = FALSE]
  if (nrow(row) != 1L) stop("Unknown geographic source.")
  expected <- jsonlite::fromJSON(row$sha256_by_filename)
  unpacked <- file.path(source_dir, paste0("geo-", source_id))
  component_scope <- row$checksum_scope == "shapefile_components"
  if (component_scope && dir.exists(unpacked)) {
    # Existing user-supplied components are checked, never replaced on failure.
    for (name in names(expected)) rep_assert_sha256(file.path(unpacked, name), expected[[name]])
    return(file.path(unpacked, row$member_filename))
  }

  file <- file.path(source_dir, row$filename)
  archive <- file
  newly_downloaded <- !file.exists(file)
  if (newly_downloaded) {
    if (!isTRUE(download)) stop("Supply official geographic source: ", file)
    rep_dir(source_dir)
    archive <- tempfile(paste0(".", row$filename, "-"), tmpdir = source_dir, fileext = ".part")
    on.exit(unlink(archive), add = TRUE)
    status <- utils::download.file(row$url, archive, mode = "wb", method = "libcurl")
    if (status != 0L) stop("Geographic source download did not complete.")
  }
  if (!component_scope) rep_assert_sha256(archive, expected[[1L]])

  if (grepl("[.]zip$", row$filename, ignore.case = TRUE)) {
    # Validate a complete extraction in a sibling temporary directory before
    # publishing either a new ZIP or its unpacked directory. Failed attempts
    # leave no final cache entry that a subsequent call could mistake as ready.
    staging <- tempfile(paste0(".geo-", source_id, "-"), tmpdir = source_dir)
    dir.create(staging)
    on.exit(unlink(staging, recursive = TRUE), add = TRUE)
    checked_unzip <- function(...) withCallingHandlers(utils::unzip(...),
      warning = function(warning) stop(conditionMessage(warning), call. = FALSE))
    listing <- checked_unzip(archive, list = TRUE)
    names_in_zip <- listing$Name
    if (!length(names_in_zip) || anyDuplicated(names_in_zip) ||
        any(grepl("\\", names_in_zip, fixed = TRUE)) ||
        any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", names_in_zip))) {
      stop("Invalid source archive paths.")
    }
    checked_unzip(archive, exdir = staging)
    members <- names_in_zip[!endsWith(names_in_zip, "/")]
    extracted <- file.info(file.path(staging, members))
    lengths <- listing$Length[match(members, names_in_zip)]
    if (!length(members) || anyNA(extracted$size) || any(extracted$isdir) ||
        any(extracted$size != lengths)) stop("Geographic source extraction is incomplete.")
    if (component_scope) {
      for (name in names(expected)) rep_assert_sha256(file.path(staging, name), expected[[name]])
      member <- row$member_filename
      if (!member %in% names(expected)) stop("The declared shapefile is not a checked component.")
    } else {
      shapes <- members[grepl("[.]shp$", members, ignore.case = TRUE)]
      if (length(shapes) != 1L) stop("Expected a single shapefile in the source archive.")
      member <- shapes[1L]
    }
    if (file.exists(unpacked)) {
      if (!dir.exists(unpacked)) stop("Geographic extraction path already exists as a file: ", unpacked)
      # A pre-existing directory must match the validated archive. Preserve its
      # contents on mismatch so the reader can inspect it or choose a new cache.
      for (name in members) rep_assert_sha256(file.path(unpacked, name),
        rep_sha256(file.path(staging, name)))
    }
    if (newly_downloaded) {
      if (file.exists(file) || !file.rename(archive, file)) stop("Cannot store downloaded geographic source.")
    }
    if (!dir.exists(unpacked) && !file.rename(staging, unpacked)) {
      stop("Cannot publish validated geographic extraction.")
    }
    return(file.path(unpacked, member))
  }
  if (newly_downloaded && (file.exists(file) || !file.rename(archive, file))) {
    stop("Cannot store downloaded geographic source.")
  }
  file
}

rep_prepare_geography <- function(source_dir, output_file, download = FALSE) {
  # Rebuild full native-vintage polygons and drawing layers from Census files.
  # The companion deposit preserves the accepted geometry bytes; this optional
  # rebuild records the same transformations without any cross-vintage overlay.
  rep_require(c("sf", "jsonlite"))
  if (file.exists(output_file)) stop("Choose a new geographic output file.")
  rep_dir(dirname(output_file))
  sources <- c("county2010_full", "tract2010_full", "tract2010_display",
    "tract2020_full", "tract2020_display", "bg2020_full", "bg2020_display",
    "place2026_full", "county2024_AL_query", "tract2024_full")
  converted <- list()
  for (id in sources) {
    file <- rep_geographic_source(id, source_dir, download)
    x <- sf::st_read(file, quiet = TRUE, stringsAsFactors = FALSE)
    if (id %in% c("county2010_full", "tract2010_full")) {
      key <- as.character(x$GEOID10)
    } else if (id == "tract2010_display") {
      key <- paste0(rep_pad_code(x$STATE, 2L), rep_pad_code(x$COUNTY, 3L), rep_pad_code(x$TRACT, 6L))
    } else key <- as.character(x$GEOID)
    if (anyNA(key) || anyDuplicated(key) || any(!startsWith(key, "01"))) stop("Invalid Census geometry keys.")
    native_valid <- sf::st_is_valid(x)
    x <- sf::st_transform(x, 5070)
    bad <- which(is.na(native_valid) | !native_valid)
    if (length(bad)) sf::st_geometry(x)[bad] <- sf::st_make_valid(sf::st_geometry(x)[bad])
    if (any(sf::st_is_empty(x)) || any(!sf::st_is_valid(x))) stop("Geometry validation failed.")
    if (startsWith(id, "county")) {
      z <- sf::st_sf(county_id = key, geom = sf::st_geometry(x))
    } else if (startsWith(id, "place")) {
      z <- sf::st_sf(place_id = key, NAME = x$NAME, CLASSFP = x$CLASSFP, geom = sf::st_geometry(x))
    } else if (startsWith(id, "bg")) {
      z <- sf::st_sf(block_group_id = key, tract_id = substr(key, 1L, 11L),
        county_id = substr(key, 1L, 5L), geom = sf::st_geometry(x))
    } else {
      z <- sf::st_sf(tract_id = key, county_id = substr(key, 1L, 5L), geom = sf::st_geometry(x))
    }
    converted[[id]] <- z
  }
  # Generalized boundary products omit some native water polygons. Keep the
  # full Census polygon for such keys; never silently drop an analysis key.
  for (kind in c("tract2010", "tract2020", "bg2020")) {
    full <- converted[[paste0(kind, "_full")]]
    display <- converted[[paste0(kind, "_display")]]
    key <- if (kind == "bg2020") "block_group_id" else "tract_id"
    index <- match(full[[key]], display[[key]])
    present <- !is.na(index)
    z <- full
    sf::st_geometry(z)[present] <- sf::st_geometry(display[index[present], ])
    z$display_geometry_source <- ifelse(present, "official_or_cached_cartographic", "full_tiger_fallback_for_missing_cartographic")
    converted[[paste0(kind, "_display")]] <- z
  }
  mapping <- c(county2010 = "county2010_full", tract2010 = "tract2010_full",
    tract2010_display = "tract2010_display", tract2020 = "tract2020_full",
    tract2020_display = "tract2020_display", block_group2020 = "bg2020_full",
    block_group2020_display = "bg2020_display", place2026 = "place2026_full",
    county2024 = "county2024_AL_query", tract2024 = "tract2024_full")
  expected <- rep_read_csv(rep_path("data", "metadata", "geography-layers.csv"))
  for (layer in names(mapping)) {
    x <- converted[[mapping[[layer]]]]
    stopifnot(nrow(x) == as.integer(expected$features[expected$layer == layer]))
    sf::st_write(x, output_file, layer = layer, append = FALSE, quiet = TRUE)
  }
  invisible(output_file)
}

rep_geo <- function(layer, crs = NULL) {
  # GIS is optional for model fitting and table-only reproduction.
  rep_require("sf")
  file <- rep_dataset("geo", "geography.gpkg")
  if (!file.exists(file)) stop("Import the geographic data archive first.")
  allowed <- rep_read_csv(rep_path("data", "metadata", "geography-layers.csv"))
  if (!layer %in% allowed$layer) stop("Unknown geographic layer: ", layer)
  answer <- sf::st_read(file, layer = layer, quiet = TRUE, stringsAsFactors = FALSE)
  if (!is.null(crs)) answer <- sf::st_transform(answer, crs)
  answer
}

# ---- Input validation ----

rep_check_inputs <- function(data_dir = rep_path("data")) {
  read <- function(name) rep_read_csv(file.path(data_dir, name))
  county <- read("analysis/county-observations.csv")
  universe <- read("analysis/county-universe.csv")
  tract <- read("analysis/tract-observations.csv")
  coverage <- read("metadata/tract-coverage.csv")
  pilot <- read("analysis/pilot-tract-keys.csv")
  poverty <- read("analysis/acs-county-child-poverty.csv")
  apps <- read("analysis/applications.csv")
  check_key <- function(ids, width, label, missing = FALSE) {
    pattern <- if (width == 5L) "^01[0-9]{3}$" else "^01[0-9]{9}$"
    if (!is.character(ids) || (!missing && anyNA(ids)) ||
        any(!is.na(ids) & !grepl(pattern, ids))) {
      stop("Invalid Alabama ", label, ": preserve the leading zero and full geographic key.")
    }
  }
  for (name in c("county", "universe", "poverty")) {
    x <- get(name)
    check_key(x$county_id, 5L, paste(name, "county_id"))
    if (anyDuplicated(x$county_id)) stop("Duplicated county key in ", name, ".")
  }
  for (name in c("county", "universe")) {
    x <- get(name)
    check_key(x$entity_id, 5L, paste(name, "entity_id"))
    if (!identical(x$entity_id, x$county_id)) stop("County entity and county keys disagree.")
  }
  for (name in c("tract", "coverage", "pilot")) {
    x <- get(name)
    check_key(x$county_id, 5L, paste(name, "county_id"))
    check_key(x$tract_id, 11L, paste(name, "tract_id"))
    check_key(x$entity_id, 11L, paste(name, "entity_id"))
    if (!identical(x$entity_id, x$tract_id) ||
        any(substr(x$tract_id, 1L, 5L) != x$county_id)) {
      stop("Tract, entity, and county-prefix keys disagree in ", name, ".")
    }
  }
  check_key(apps$county_id, 5L, "application county_id", missing = TRUE)
  county_ids <- county$county_id
  if (!all(vapply(list(universe$county_id, poverty$county_id,
                      coverage$county_id, tract$county_id),
                  function(ids) setequal(ids, county_ids), logical(1))) ||
      any(!pilot$county_id %in% county_ids) ||
      any(!apps$county_id[!is.na(apps$county_id)] %in% county_ids)) {
    stop("County keys disagree across the observation, coverage, policy, poverty, or application inputs.")
  }
  stopifnot(nrow(county) == 67L, nrow(universe) == 67L, nrow(tract) == 1173L,
    nrow(coverage) == 1181L, nrow(pilot) == 90L, nrow(poverty) == 67L, nrow(apps) == 72L, sum(!is.na(apps$county_id)) == 71L,
    sum(tolower(universe$county_government_candidate) == "true") == 64L,
    sum(tolower(coverage$model_eligible) == "true") == 1173L,
    sum(as.numeric(tract$incarceration_estimate_fraction) < 0) == 123L,
    !anyDuplicated(county$entity_id), !anyDuplicated(tract$entity_id),
    !anyDuplicated(coverage$tract_id), !anyDuplicated(pilot$tract_id),
    setequal(coverage$tract_id[tolower(coverage$model_eligible) == "true"], tract$tract_id),
    all(pilot$tract_id %in% tract$tract_id),
    all(tract$entity_id == tract$tract_id), all(county$entity_id == county$county_id))
  for (x in list(county, tract)) {
    for (criterion in c("income", "incarceration")) {
      stopifnot(all(is.finite(as.numeric(x[[paste0(criterion, "_estimate_fraction")]]))),
        all(is.finite(as.numeric(x[[paste0(criterion, "_se_fraction")]])) &
            as.numeric(x[[paste0(criterion, "_se_fraction")]]) > 0))
    }
  }
  invisible(list(counties = 67L, county_candidates = 64L, modeled_tracts = 1173L,
    tract_keys = 1181L, pilot_tracts = 90L, negative_incarceration = 123L,
    applications = 72L))
}
