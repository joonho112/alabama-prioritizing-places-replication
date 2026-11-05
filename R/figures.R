# JoonHo Lee (jlee296@ua.edu)
# Reproduce the article's figures from documented inputs and saved results.
# Map data are read only for requested maps. Model fitting and report selection
# are separate stages; sourcing this module starts no computation.
#
# The article's palettes, physical sizes, panel arrangements and encodings are
# preserved. Each plot takes a documented data list and an output directory.
# Helpers keep styling and file writing separate from the statistical inputs.

rep_figure_theme_house <- function(base_size = 8.3) {
  theme_minimal(base_size = base_size, base_family = "Helvetica") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.28),
      axis.ticks = element_blank(),
      axis.title = element_text(size = base_size, colour = "grey20"),
      axis.text = element_text(size = base_size - 0.5, colour = "grey30"),
      plot.title = element_blank(), plot.subtitle = element_blank(),
      plot.tag = element_text(face = "bold", size = base_size + 1.7, family = "Helvetica"),
      plot.tag.position = c(0, 1),
      legend.position = "bottom", legend.title = element_text(size = base_size - 0.3),
      legend.text = element_text(size = base_size - 0.5),
      legend.key.size = unit(3.4, "mm"), legend.margin = margin(0, 0, 0, 0),
      strip.text = element_text(face = "bold", hjust = 0, size = base_size, colour = "grey15"),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(4, 4, 2, 4)
    )
}

rep_figure_theme_map <- function(base_size = 8.3) {
  rep_figure_theme_house(base_size) + theme(
    panel.grid = element_blank(), axis.text = element_blank(),
    axis.title = element_blank(), panel.background = element_blank()
  )
}

rep_figure_annot <- function(x, y, label, size = 2.55, hjust = 0, vjust = 1, colour = "grey15", ...) {
  annotate("label",
    x = x, y = y, label = label, size = size, hjust = hjust, vjust = vjust,
    family = "Helvetica", colour = colour, fill = "white", alpha = .92, linewidth = 0,
    label.padding = unit(1.2, "mm"), ...
  )
}

rep_figure_mm <- function(x) x / 25.4

rep_figure_legend_band <- function(pal, title) {
  scale_fill_manual(
    values = pal, breaks = names(pal), name = title, drop = FALSE, na.value = "#EFEFEF",
    guide = guide_legend(nrow = 1, override.aes = list(colour = "grey40", linewidth = .2))
  )
}

rep_figure_city_layer <- function(city_pts, size = 2.1) {
  list(
    geom_point(data = city_pts, aes(X, Y), inherit.aes = FALSE, shape = 21, fill = "white", colour = "black", size = 1.1, stroke = .3),
    geom_text(data = city_pts, aes(X, Y, label = NAME), inherit.aes = FALSE, size = size, family = "Helvetica", colour = "black", hjust = -0.15, vjust = -0.3)
  )
}

rep_figure_black_belt_label <- function(state_bbox, size = 2.1) {
  annotate("text",
    x = state_bbox["xmin"] + .40 * (state_bbox["xmax"] - state_bbox["xmin"]), y = state_bbox["ymin"] + .36 * (state_bbox["ymax"] - state_bbox["ymin"]),
    label = "Black Belt", size = size, family = "Helvetica", fontface = "italic", colour = "white"
  )
}

rep_figure_scale_bar <- function(x0, y0, len_km, label, size = 2.3) {
  list(
    annotate("rect", xmin = x0, xmax = x0 + len_km * 1000, ymin = y0, ymax = y0 + len_km * 40, fill = "black", colour = "black", linewidth = .2),
    annotate("rect", xmin = x0 + len_km * 500, xmax = x0 + len_km * 1000, ymin = y0, ymax = y0 + len_km * 40, fill = "white", colour = "black", linewidth = .2),
    annotate("text", x = x0 + len_km * 1000 + len_km * 80, y = y0 + len_km * 20, label = label, hjust = 0, vjust = .5, size = size, family = "Helvetica", colour = "grey20")
  )
}

rep_figure_num <- function(x) suppressWarnings(as.numeric(x))

rep_figure_or <- function(x, alternative) if (is.null(x)) alternative else x

# ---- Small input readers and an explicit, lazy figure context ----------------

# Fresh runs supply all scientific summaries together. The table adapter already
# replaces every posterior field and central grade vector; the additions here
# translate the report stage's long capacity/stability schemas for plotting.
rep_figure_fresh_sources <- function(report_dir, posterior_dir) {
  fail <- function(...) stop("Fresh figure inputs: ", ..., call. = FALSE)
  report_dir <- rep_absolute_path(report_dir)
  posterior_dir <- rep_absolute_path(posterior_dir)
  status <- rep_read_json(rep_table_find(report_dir, "run-status.json"))
  cases <- rep_read_json(rep_table_find(report_dir, "review-reports.json"), FALSE)$cases
  config <- rep_report_scenarios()
  if (!all(c("status", "case_count", "method_count", "completed_cases") %in% names(status))) {
    fail("report run-status.json is missing completion fields.")
  }
  if (!identical(status$status, "complete") || status$case_count != 14L ||
    status$method_count != 67L || !setequal(status$completed_cases, config$case_id) ||
    !setequal(names(cases), config$case_id)) {
    fail("use a complete fourteen-case report run (67 fixed reports).")
  }
  source <- rep_table_sources(report_dir, posterior_dir)
  metric_fields <- c("case_id", "scenario_id", "universe", "method", "n_entities", "N", "M", "Goccupied", "disclosure_fraction", "B_Rtau", "B_Rtau_mcse")
  sensitivity_fields <- c(
    "case_id", "scenario_id", "scenario_kind", "universe", "method",
    "n_entities", "N", "M", "Goccupied", "disclosure_fraction", "B_Rtau", "B_Rtau_mcse",
    paste0("P_vs_centerP_", c("tie_to_disclosure", "disclosure_to_tie", "direction_reversals", "relation_changes"))
  )
  if (!all(metric_fields %in% names(source$metrics)) || !all(sensitivity_fields %in% names(source$sensitivity))) {
    fail("report metrics or sensitivity are missing required plotting fields.")
  }
  if (nrow(source$metrics) != 67L || anyDuplicated(paste(source$metrics$case_id, source$metrics$method)) ||
    !setequal(source$metrics$case_id, config$case_id) || nrow(source$sensitivity) != 14L ||
    anyDuplicated(source$sensitivity$case_id) || !setequal(source$sensitivity$case_id, config$case_id)) {
    fail("metrics and sensitivity must cover the complete case inventory.")
  }
  pilot_keys <- rep_read_csv(rep_path("data", "analysis", "pilot-tract-keys.csv"))
  county_keys <- rep_read_csv(rep_path("data", "analysis", "county-universe.csv"))
  # Pair counts depend only on the report grades. Check them before accepting
  # a metric CSV from another stage or run; no posterior arrays are needed.
  check_counts <- function(record, grade, label) {
    n <- length(grade)
    M <- choose(n, 2L)
    N <- M - sum(choose(as.numeric(table(grade)), 2L))
    expected <- c(n_entities = n, M = M, N = N, Goccupied = length(unique(grade)), disclosure_fraction = N / M)
    if (nrow(record) != 1L) fail("missing or duplicate metric row: ", label)
    actual <- vapply(names(expected), function(field) rep_figure_num(record[[field]]), numeric(1))
    if (any(!is.finite(actual)) || any(abs(actual - expected) > 1e-12)) {
      fail("metric counts disagree with report grades: ", label)
    }
  }
  for (universe in c("county", "tract")) {
    frame <- source[[universe]]
    modeled <- if (universe == "county") rep(TRUE, nrow(frame)) else tolower(frame$posterior_available) == "true"
    model <- if (universe == "county") "M1" else "PV-TM1"
    for (criterion in c("income", "incarceration")) {
      if (anyNA(frame[[paste0(criterion, "_posterior_fit_id")]][modeled]) ||
        any(frame[[paste0(criterion, "_posterior_fit_id")]][modeled] != paste(model, "A", criterion, sep = "-"))) {
        fail("posterior summaries must come from the central A fits: ", universe)
      }
    }
  }
  for (i in seq_len(nrow(config))) {
    row <- config[i, ]
    case <- cases[[row$case_id]]
    required_case <- c("selection_pool", "evaluation_pool", "selection_model", "evaluation_model", "weights", "tau", "G_max", "reports", "include_in_stability")
    if (!all(required_case %in% names(case))) fail("case provenance is incomplete: ", row$case_id)
    weights <- unlist(case$weights, use.names = TRUE)
    expected_weights <- c(income = row$weight_income, incarceration = row$weight_incarceration)
    if (!identical(case$selection_pool, "A") || !identical(case$evaluation_pool, "B") ||
      !identical(case$selection_model, row$model_id) || !identical(case$evaluation_model, row$evaluation_model) ||
      !setequal(names(weights), names(expected_weights)) ||
      any(abs(weights[names(expected_weights)] - expected_weights) > 1e-12) ||
      case$tau != row$tau || case$G_max != row$G_max ||
      !identical(case$include_in_stability, row$include_in_tract_8case_stability)) {
      fail("case provenance differs from the declared settings: ", row$case_id)
    }
    methods <- if (row$kind == "exploratory") c("B3", "P") else c("B0", "B1", "B2", "B3", "P")
    if (!setequal(names(case$reports), methods) ||
      !setequal(source$metrics$method[source$metrics$case_id == row$case_id], methods)) {
      fail("missing reporting method in ", row$case_id)
    }
    ids <- if (row$universe == "county") county_keys$entity_id[tolower(county_keys$county_government_candidate) == "true"] else pilot_keys$entity_id
    for (method in methods) {
      grade <- unlist(case$reports[[method]]$grade, use.names = TRUE)
      if (anyDuplicated(names(grade)) || !setequal(names(grade), ids) ||
        any(!is.finite(grade) | grade < 1 | grade > 5 | grade != floor(grade)) ||
        !identical(sort(unique(as.integer(grade))), seq_len(length(unique(grade))))) {
        fail("reports need complete entities and compact bands 1 through at most 5: ", row$case_id)
      }
      metric <- source$metrics[source$metrics$case_id == row$case_id & source$metrics$method == method, , drop = FALSE]
      check_counts(metric, grade, paste(row$case_id, method))
      if (metric$universe != row$universe || metric$scenario_id != row$scenario_id) {
        fail("metric case labels disagree with the report: ", row$case_id)
      }
      if (method == "P") {
        sensitivity <- source$sensitivity[source$sensitivity$case_id == row$case_id, , drop = FALSE]
        check_counts(sensitivity, grade, paste(row$case_id, "sensitivity"))
        if (sensitivity$universe != row$universe || sensitivity$scenario_id != row$scenario_id || sensitivity$method != "P") {
          fail("sensitivity labels disagree with the report: ", row$case_id)
        }
        for (field in c("B_Rtau", "B_Rtau_mcse")) {
          a <- rep_figure_num(metric[[field]])
          b <- rep_figure_num(sensitivity[[field]])
          equal <- (is.na(a) && is.na(b)) || (is.finite(a) && is.finite(b) && abs(a - b) <= 1e-12)
          if (!equal) fail("sensitivity risk/MCSE differs from its metric row: ", row$case_id)
        }
        center <- unlist(cases[[paste0(row$universe, "-center")]]$reports$P$grade, use.names = TRUE)
        comparison <- rep_relation_comparison(grade, center)
        for (field in c("tie_to_disclosure", "disclosure_to_tie", "direction_reversals", "relation_changes")) {
          value <- rep_figure_num(sensitivity[[paste0("P_vs_centerP_", field)]])
          if (!is.finite(value) || value != comparison[[field]]) {
            fail("relation-change counts differ from the fresh center report: ", row$case_id)
          }
        }
      }
    }
  }
  source$metrics <- source$metrics[source$metrics$case_id %in% c("county-center", "tract-center"), , drop = FALSE]
  source$sensitivity$scenario_kind <- ifelse(source$sensitivity$scenario_kind == "posterior", "model",
    ifelse(source$sensitivity$scenario_kind == "exploratory", "direction", source$sensitivity$scenario_kind)
  )
  source$sensitivity$policy_id <- source$sensitivity$scenario_id
  changes <- c("tie_to_disclosure", "disclosure_to_tie", "direction_reversals", "relation_changes")
  for (field in changes) {
    source$sensitivity[[paste0("P_vs_main_center_", field)]] <- source$sensitivity[[paste0("P_vs_centerP_", field)]]
  }
  capacity <- source$capacity
  required_capacity <- c("universe", "case_id", "method", "entity_id", "review_capacity_K", "capacity_status")
  if (!all(required_capacity %in% names(capacity))) fail("capacity data are incomplete.")
  capacity$review_capacity_K <- as.integer(capacity$review_capacity_K)
  if (anyNA(capacity[required_capacity])) fail("capacity data contain missing keys or statuses.")
  for (i in seq_len(nrow(source$sensitivity))) {
    row <- source$sensitivity[i, ]
    Ks <- if (row$universe == "county") c(3L, 5L) else c(5L, 10L)
    keys <- if (row$universe == "county") county_keys else pilot_keys
    grade <- unlist(cases[[row$case_id]]$reports$P$grade, use.names = TRUE)
    expected <- rep_capacity_readout(grade, keys, Ks, row$universe)
    for (j in seq_along(Ks)) {
      z <- capacity[capacity$universe == row$universe & capacity$case_id == row$scenario_id &
        capacity$method == "P" & capacity$review_capacity_K == Ks[j], , drop = FALSE]
      wanted <- expected[expected$review_capacity_K == Ks[j], ]
      if (anyDuplicated(z$entity_id) || !setequal(z$entity_id, keys$entity_id) ||
        !identical(z$capacity_status[match(wanted$entity_id, z$entity_id)], wanted$capacity_status)) {
        fail("capacity rows disagree with the fresh intact-band report: ", row$case_id)
      }
      for (state in c("core", "boundary")) {
        source$sensitivity[[paste0("capacity", j, "_", state, "_ids")]][i] <-
          paste(sort(z$entity_id[z$capacity_status == state]), collapse = ";")
      }
    }
  }
  stability <- rep_read_csv(rep_table_find(report_dir, "tract-stability-cases.csv"))
  required <- c("tract_id", "case_id", "scientifically_ready", "grade_original", "capacity_K5", "capacity_K10")
  stability_ids <- config$scenario_id[config$include_in_tract_8case_stability]
  if (!all(required %in% names(stability)) || nrow(stability) != 720L ||
    anyDuplicated(paste(stability$tract_id, stability$case_id)) ||
    !setequal(stability$case_id, stability_ids) || anyNA(stability[required]) || any(tolower(stability$scientifically_ready) != "true")) {
    fail("supply all 720 valid tract-case rows from the same complete report run.")
  }
  for (case_id in stability_ids) {
    z <- stability[stability$case_id == case_id, , drop = FALSE]
    case_key <- config$case_id[config$universe == "tract" & config$scenario_id == case_id]
    grade <- unlist(cases[[case_key]]$reports$P$grade, use.names = TRUE)
    if (!setequal(z$tract_id, pilot_keys$entity_id) ||
      any(as.numeric(z$grade_original) != grade[z$tract_id])) {
      fail("stability grades differ from ", case_id)
    }
    for (K in c(5L, 10L)) {
      wanted <- rep_capacity_readout(grade, pilot_keys, K, "tract")
      if (!identical(z[[paste0("capacity_K", K)]], wanted$capacity_status[match(z$tract_id, wanted$entity_id)])) {
        fail("stability capacity differs from the fresh report: ", case_id)
      }
    }
  }
  source$stability <- stability
  source$cases <- cases
  source
}

rep_figure_palette <- function(palette, grade) {
  occupied <- sort(unique(as.integer(grade[!is.na(grade)])))
  if (!identical(occupied, seq_len(length(occupied))) || length(occupied) > 5L) {
    stop("Figure bands must be compact labels 1 through at most 5.", call. = FALSE)
  }
  n <- length(occupied)
  colours <- if (n <= length(palette)) unname(palette[seq_len(n)]) else grDevices::colorRampPalette(unname(palette))(n)
  stats::setNames(colours, paste("Band", seq_len(n)))
}

# The article uses fixed outcome calibrations so comparison across panels is
# meaningful. A substantially different fit needs a deliberate display review.
rep_figure_preflight <- function(data, figures) {
  if (!isTRUE(data$fresh)) {
    return(invisible(TRUE))
  }
  fail <- function(...) stop("Fresh figure display needs review: ", ..., call. = FALSE)
  inside <- function(values, limits) all(is.finite(values) & values >= limits[1] & values <= limits[2])
  if ("fig2" %in% figures && !inside(data$tract$incarceration_posterior_mean_points[data$tract$modeled], c(.5, 6.5))) {
    fail("Figure 2 posterior incarceration means exceed its calibrated 0.5–6.5 point scale.")
  }
  if ("fig3" %in% figures) {
    county <- data$county[data$county$candidate, ]
    tract <- data$tract[data$tract$pilot, ]
    if (!inside(county$income_posterior_mean_points, c(29.6, 44.3)) ||
      !inside(county$incarceration_posterior_mean_points, c(-.35, 6)) ||
      !inside(tract$income_posterior_mean_points, c(24.3, 49.5)) ||
      !inside(tract$incarceration_posterior_mean_points, c(-.2, 9.1))) {
      fail("Figure 3 means exceed the published comparison ranges; review the new fit and plotting calibration.")
    }
  }
  if ("fig6" %in% figures && any(!is.finite(data$report_metrics$B_Rtau) |
    !is.finite(data$report_metrics$B_Rtau_mcse) | data$report_metrics$B_Rtau_mcse < 0)) {
    fail("Figure 6 needs finite fixed-action risks and identified MCSE; no zero is substituted for unavailable MCSE.")
  }
  invisible(TRUE)
}

rep_figure_cast <- function(data, name) {
  dictionary <- rep_read_csv(rep_path("data", "saved", "data-dictionary.csv"))
  fields <- dictionary$column[dictionary$file == name &
    dictionary$type %in% c("numeric", "integer")]
  for (field in intersect(fields, names(data))) data[[field]] <- as.numeric(data[[field]])
  data
}

rep_figure_read_saved <- function(name, saved_dir) {
  rep_figure_cast(rep_read_csv(file.path(saved_dir, name)), name)
}

# Open an actual vector device before printing. Some macOS R builds advertise
# Cairo support even when optional XQuartz libraries are unavailable. Quartz is
# a native vector fallback on those systems; other platforms require Cairo.
rep_figure_device <- function(path, width_mm, height_mm) {
  previous <- grDevices::dev.cur()
  cairo_error <- tryCatch(
    {
      if (!isTRUE(capabilities("cairo"))) stop("Cairo support is unavailable.")
      withCallingHandlers(
        grDevices::cairo_pdf(
          filename = path, width = rep_figure_mm(width_mm), height = rep_figure_mm(height_mm),
          family = "Helvetica", bg = "white", onefile = TRUE
        ),
        warning = function(w) stop(conditionMessage(w), call. = FALSE)
      )
      if (grDevices::dev.cur() == previous) stop("Cairo did not open a PDF device.")
      NULL
    },
    error = function(e) e
  )
  if (is.null(cairo_error)) {
    return("Cairo")
  }
  if (grDevices::dev.cur() != previous) grDevices::dev.off()
  if (file.exists(path)) unlink(path)
  if (Sys.info()[["sysname"]] == "Darwin" && isTRUE(capabilities("aqua"))) {
    grDevices::quartz(
      type = "pdf", file = path, width = rep_figure_mm(width_mm), height = rep_figure_mm(height_mm),
      family = "Helvetica", bg = "white"
    )
    if (grDevices::dev.cur() == previous) stop("Quartz did not open a PDF device.")
    return("Quartz")
  }
  stop(
    "Unable to open Cairo PDF: ", conditionMessage(cairo_error),
    " Install an R build with working Cairo support."
  )
}

rep_figure_save <- function(p, stem, width_mm = 174, height_mm = 100, dpi = 500, output_dir, png = FALSE) {
  pdf_path <- file.path(output_dir, paste0(stem, ".pdf"))
  device <- rep_figure_device(pdf_path, width_mm, height_mm)
  tryCatch(
    withCallingHandlers(print(p), warning = function(w) {
      # The nested, fixed-aspect map layout in Figure 4 triggers a known
      # patchwork unit-assignment warning. It concerns panel alignment rather
      # than observations. Keep the accepted layout and leave other warnings
      # visible; match both this message and its exact layout call.
      fixed_map_layout <- identical(stem, "fig4_review_bands_maps") &&
        identical(conditionMessage(w), "number of items to replace is not a multiple of replacement length") &&
        any(vapply(sys.calls(), function(call) {
          identical(call[[1L]], as.name("set_panel_dimensions"))
        }, logical(1L)))
      if (fixed_map_layout) invokeRestart("muffleWarning")
    }),
    finally = grDevices::dev.off()
  )
  if (!file.exists(pdf_path) || file.info(pdf_path)$size == 0) {
    stop("The PDF device did not produce ", pdf_path)
  }
  paths <- pdf_path
  if (png) {
    rep_require("ragg")
    rep_dir(file.path(output_dir, "png"))
    png_path <- file.path(output_dir, "png", paste0(stem, ".png"))
    ggplot2::ggsave(png_path, p,
      device = ragg::agg_png,
      width = rep_figure_mm(width_mm), height = rep_figure_mm(height_mm), units = "in", dpi = dpi, bg = "white"
    )
    paths <- c(paths, png_path)
  }
  message("Wrote ", basename(pdf_path), " (", width_mm, " x ", height_mm, " mm; ", device, " vector PDF)")
  invisible(structure(paths, device = device))
}

rep_figure_simplify <- function(x, keep) {
  # Display simplification does not compute or redistribute any statistical value.
  y <- rmapshaper::ms_simplify(x, keep = keep, keep_shapes = TRUE, sys = FALSE)
  stopifnot(nrow(y) == nrow(x), !any(sf::st_is_empty(y)))
  y
}

rep_figure_poverty <- function() {
  # The source preparation stage already computes the same-universe ratio and
  # the published 90% subset-proportion MOE, with the documented fallback.
  data <- rep_read_csv(rep_path("data", "analysis", "acs-county-child-poverty.csv"))
  for (field in c("den", "den_moe", "num_", "num_moe", "share", "share_moe")) {
    data[[field]] <- as.numeric(data[[field]])
  }
  stopifnot(nrow(data) == 67L)
  data
}

rep_figure_data <- function(output_dir, saved_dir, maps = FALSE, png = FALSE,
                            report_dir = NULL, posterior_dir = NULL) {
  fresh <- !is.null(report_dir) || !is.null(posterior_dir)
  if (xor(is.null(report_dir), is.null(posterior_dir))) {
    stop("Supply both report_dir and posterior_dir for fresh figures.", call. = FALSE)
  }
  source <- if (fresh) rep_figure_fresh_sources(report_dir, posterior_dir) else NULL
  packages <- c("ggplot2", "patchwork", "dplyr", "tidyr", "scales", "ggrepel", "grid")
  rep_require(packages)
  for (package in packages) {
    suppressPackageStartupMessages(
      library(package, character.only = TRUE)
    )
  }
  context <- list()
  context$FONT <- "Helvetica"
  context$BASE <- 8.3
  context$okabe <- c(
    orange = "#E69F00", sky = "#56B4E9", green = "#009E73",
    yellow = "#F0E442", blue = "#0072B2", vermillion = "#D55E00",
    purple = "#CC79A7", black = "#000000"
  )
  context$pal_county <- c(
    `Band 1` = "#08306B", `Band 2` = "#2F6FA8",
    `Band 3` = "#9ECAE1", `Band 4` = "#E3EEF8"
  )
  context$pal_tract <- c(`Band 1` = "#3F007D", `Band 2` = "#8073AC", `Band 3` = "#D8CFE8")
  context$grey_outside <- "#D9D9D9"
  context$grey_na <- "#EFEFEF"
  context$line_county <- "#4D4D4D"
  context$line_tract <- "white"
  context$pilot_ids <- c(`01087` = "Macon", `01101` = "Montgomery", `01113` = "Russell")

  context$fresh <- fresh
  county <- if (fresh) rep_figure_cast(source$county, "county-results.csv") else rep_figure_read_saved("county-results.csv", saved_dir)
  county$candidate <- tolower(county$county_government_candidate) == "true"
  county$county_short <- sub(" County$", "", county$county_name)
  county$band <- ifelse(is.na(county$grade_P), NA_character_, paste("Band", county$grade_P))
  tract <- if (fresh) rep_figure_cast(source$tract, "tract-results.csv") else rep_figure_read_saved("tract-results.csv", saved_dir)
  tract$modeled <- tolower(tract$posterior_available) == "true"
  tract$pilot <- tolower(tract$in_pooled90) == "true"
  tract$band <- ifelse(is.na(tract$grade_P), NA_character_, paste("Band", tract$grade_P))
  tract$county_short <- unname(context$pilot_ids[tract$county_id])
  tract$tract_label <- sub("\\.?0+$", "", formatC(
    as.numeric(substr(tract$tract_id, 6L, 11L)) / 100,
    format = "f", digits = 2L
  ))
  context$county <- county
  context$tract <- tract
  context$first_band <- tract[tract$pilot & !is.na(tract$grade_P) & tract$grade_P == 1, , drop = FALSE]
  context$first_band <- context$first_band[order(context$first_band$tract_id), , drop = FALSE]
  context$report_metrics <- if (fresh) rep_figure_cast(source$metrics, "report-metrics.csv") else rep_figure_read_saved("report-metrics.csv", saved_dir)
  context$sensitivity <- if (fresh) rep_figure_cast(source$sensitivity, "sensitivity.csv") else rep_figure_read_saved("sensitivity.csv", saved_dir)
  context$stability_cases <- if (fresh) rep_figure_cast(source$stability, "tract-stability-cases.csv") else rep_figure_read_saved("tract-stability-cases.csv", saved_dir)
  if (fresh) {
    context$pal_county <- rep_figure_palette(context$pal_county, county$grade_P[county$candidate])
    context$pal_tract <- rep_figure_palette(context$pal_tract, tract$grade_P[tract$pilot])
  }
  context$B3_P_equal <- c(
    county = all(sign(outer(county$grade_B3[county$candidate], county$grade_B3[county$candidate], "-")) == sign(outer(county$grade_P[county$candidate], county$grade_P[county$candidate], "-"))),
    tract = all(sign(outer(tract$grade_B3[tract$pilot], tract$grade_B3[tract$pilot], "-")) == sign(outer(tract$grade_P[tract$pilot], tract$grade_P[tract$pilot], "-")))
  )
  context$applications <- rep_read_csv(rep_path("data", "analysis", "applications.csv"))
  context$context_display <- rep_current_context(display = TRUE)
  context$program_context <- rep_read_csv(rep_path("data", "analysis", "program-context.csv"))
  context$program_context$value <- as.numeric(context$program_context$value)
  context$county_poverty <- rep_figure_poverty()
  stopifnot(
    sum(county$candidate) == 64L, sum(tract$pilot) == 90L,
    sum(tract$modeled) == 1173L
  )
  if (!fresh) stopifnot(nrow(context$first_band) == 13L)

  if (maps) {
    rep_require(c("sf", "rmapshaper"))
    suppressPackageStartupMessages(library(sf))
    context$geo_county <- rep_geo("county2010", 5070)
    context$geo_tract <- rep_geo("tract2010_display", 5070)
    context$geo_county <- rep_figure_simplify(context$geo_county, 0.30)
    context$geo_tract <- rep_figure_simplify(context$geo_tract, 0.18)
    context$state_bbox <- sf::st_bbox(context$geo_county)
    context$places <- rep_geo("place2026", 5070)
    cities <- context$places[context$places$NAME %in%
      c("Birmingham", "Montgomery", "Mobile", "Huntsville"), , drop = FALSE]
    points <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(cities)))
    context$city_pts <- data.frame(NAME = cities$NAME, X = points[, 1L], Y = points[, 2L])
  }
  context
}

# ---- Historical outcomes by childhood tract ------------

rep_plot_fig2 <- function(data, output_dir, png = FALSE) {
  FONT <- data$FONT
  BASE <- data$BASE
  grey_na <- data$grey_na
  tract <- data$tract
  geo_county <- data$geo_county
  geo_tract <- data$geo_tract
  state_bbox <- data$state_bbox


  d <- geo_tract %>% left_join(tract %>% select(
    tract_id, modeled, income_raw_estimate_points, income_posterior_mean_points,
    incarceration_raw_estimate_points, incarceration_posterior_mean_points
  ), by = "tract_id")
  stopifnot(sum(!d$modeled) == 8L)
  long <- bind_rows(
    d %>% transmute(tract_id, modeled, criterion = "income", quantity = "Published estimate", value = income_raw_estimate_points),
    d %>% transmute(tract_id, modeled, criterion = "income", quantity = "Posterior mean", value = income_posterior_mean_points),
    d %>% transmute(tract_id, modeled, criterion = "incarceration", quantity = "Published estimate", value = incarceration_raw_estimate_points),
    d %>% transmute(tract_id, modeled, criterion = "incarceration", quantity = "Posterior mean", value = incarceration_posterior_mean_points)
  )
  long$value[!long$modeled] <- NA
  neg <- sum(long$criterion == "incarceration" & long$quantity == "Published estimate" & !is.na(long$value) & long$value < 0)
  if (!data$fresh) stopifnot(neg == 123L)
  panel <- function(crit, qty, tag, limits, option, dir, legend_title, cities = FALSE, belt = FALSE) {
    z <- long %>% filter(criterion == crit, quantity == qty)
    g <- ggplot(z) +
      geom_sf(aes(fill = value), colour = NA) +
      geom_sf(data = geo_county, fill = NA, colour = "white", linewidth = .12) +
      scale_fill_viridis_c(
        option = option, direction = dir, limits = limits, oob = squish, na.value = grey_na, name = legend_title,
        guide = guide_colourbar(barwidth = unit(28, "mm"), barheight = unit(2.4, "mm"), title.position = "top")
      ) +
      rep_figure_theme_map() +
      labs(tag = tag) +
      theme(legend.position = "bottom", legend.title = element_text(size = BASE - .6, lineheight = .95), legend.text = element_text(size = BASE - 1)) +
      annotate("text", x = state_bbox["xmax"] - 2000, y = state_bbox["ymin"] + 90000, label = qty, hjust = 1, vjust = 1, size = 2.6, family = FONT, colour = "grey15")
    if (cities) g <- g + rep_figure_city_layer(data$city_pts)
    if (belt) g <- g + rep_figure_black_belt_label(data$state_bbox)
    g
  }
  inc_lim <- range(long$value[long$criterion == "income"], na.rm = TRUE)
  jail_lim <- c(-12.5, 18.3)
  jail_post <- range(long$value[long$criterion == "incarceration" & long$quantity == "Posterior mean"], na.rm = TRUE)
  if (!data$fresh) stopifnot(jail_post[1] > 0.6, jail_post[2] < 6.3)
  pA <- panel("income", "Published estimate", "A", inc_lim, "mako", 1, "Adult household income rank\n(percentile points)", cities = TRUE, belt = TRUE)
  pB <- panel("income", "Posterior mean", "B", inc_lim, "mako", 1, "Adult household income rank\n(percentile points)")
  pC <- panel("incarceration", "Published estimate", "C", jail_lim, "rocket", -1, "Incarceration, published estimate\n(percentage points; privacy noise included)", cities = TRUE)
  pD <- panel("incarceration", "Posterior mean", "D", c(0.5, 6.5), "rocket", -1, "Incarceration, posterior mean\n(percentage points; own scale)")
  p <- (pA + pB + plot_layout(guides = "collect")) / (pC + pD) & theme(legend.position = "bottom")
  rep_figure_save(p, "fig2_historical_outcomes", 174, 196, output_dir = output_dir, png = png)
}

# ---- Two criteria with their marginal uncertainty ------------

rep_plot_fig3 <- function(data, output_dir, png = FALSE) {
  FONT <- data$FONT
  pal_county <- data$pal_county
  pal_tract <- data$pal_tract
  county <- data$county
  tract <- data$tract
  pilot_ids <- data$pilot_ids


  cA <- county %>%
    filter(candidate) %>%
    mutate(band = factor(band, names(pal_county)), focal = county_id %in% names(pilot_ids))
  if (!data$fresh) stopifnot(all(table(cA$band) == c(2, 31, 30, 1)))
  tB <- tract %>%
    filter(pilot) %>%
    mutate(band = factor(band, names(pal_tract)), county_short = factor(county_short, c("Montgomery", "Russell", "Macon")))
  if (!data$fresh) stopifnot(all(table(tB$band) == c(13, 69, 8)))
  pA <- ggplot(cA, aes(income_posterior_mean_points, incarceration_posterior_mean_points)) +
    geom_segment(aes(x = income_posterior_q025_points, xend = income_posterior_q975_points, yend = incarceration_posterior_mean_points, colour = band), linewidth = .35, alpha = .75) +
    geom_segment(aes(y = incarceration_posterior_q025_points, yend = incarceration_posterior_q975_points, xend = income_posterior_mean_points, colour = band), linewidth = .35, alpha = .75) +
    geom_point(aes(fill = band), shape = 21, colour = "white", size = 2.1, stroke = .3) +
    geom_point(data = filter(cA, focal), aes(fill = band), shape = 21, colour = "black", size = 2.6, stroke = .5) +
    geom_text_repel(
      data = filter(cA, focal), aes(label = county_short), size = 2.6, family = FONT, fontface = "bold",
      min.segment.length = 0, box.padding = .5, point.padding = .3, segment.colour = "grey40", segment.size = .25, seed = 11
    ) +
    scale_colour_manual(values = pal_county, drop = FALSE, guide = "none") +
    scale_fill_manual(values = pal_county, drop = FALSE, name = "Review band", guide = guide_legend(nrow = 1, override.aes = list(size = 2.6))) +
    scale_x_continuous(breaks = seq(30, 44, 2)) +
    scale_y_continuous(breaks = seq(0, 5, 1)) +
    coord_cartesian(xlim = c(29.6, 44.3), ylim = c(-0.35, 6.0), clip = "off") +
    labs(
      tag = "A", x = "Adult household income rank (percentile points)",
      y = "Incarceration (percentage points)"
    ) +
    rep_figure_theme_house() +
    theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = .25))
  pB <- ggplot(tB, aes(income_posterior_mean_points, incarceration_posterior_mean_points)) +
    geom_segment(aes(x = income_posterior_q025_points, xend = income_posterior_q975_points, yend = incarceration_posterior_mean_points, colour = band), linewidth = .3, alpha = .55) +
    geom_segment(aes(y = incarceration_posterior_q025_points, yend = incarceration_posterior_q975_points, xend = income_posterior_mean_points, colour = band), linewidth = .3, alpha = .55) +
    geom_point(aes(fill = band, shape = county_short), colour = "white", size = 2.0, stroke = .3) +
    geom_point(data = filter(tB, band == "Band 1"), aes(fill = band, shape = county_short), colour = "black", size = 2.3, stroke = .5) +
    geom_text_repel(
      data = filter(tB, tract_id %in% c("01101001000", "01113030800")), aes(label = paste(county_short, tract_label)), size = 2.4, family = FONT, fontface = "bold",
      min.segment.length = 0, box.padding = .6, nudge_y = 1.2, nudge_x = 1.5, segment.colour = "grey40", segment.size = .25, seed = 5
    ) +
    scale_shape_manual(values = c(Montgomery = 21, Russell = 22, Macon = 24), name = "County", guide = guide_legend(nrow = 1, override.aes = list(fill = "grey55", size = 2.4))) +
    scale_colour_manual(values = pal_tract, drop = FALSE, guide = "none") +
    scale_fill_manual(values = pal_tract, drop = FALSE, name = "Review band", guide = guide_legend(nrow = 1, override.aes = list(shape = 21, size = 2.6))) +
    coord_cartesian(xlim = c(24.3, 49.5), ylim = c(-0.2, 9.1), clip = "off") +
    labs(
      tag = "B", x = "Adult household income rank (percentile points)",
      y = "Incarceration (percentage points)"
    ) +
    rep_figure_theme_house() +
    theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = .25), legend.box = "vertical", legend.spacing.y = unit(1.2, "mm"), legend.key.width = unit(3, "mm"), legend.key.spacing.x = unit(1, "mm"))
  p <- pA / pB + plot_layout(heights = c(1, 1.1))
  rep_figure_save(p, "fig3_two_criteria_uncertain", 146, 165, output_dir = output_dir, png = png)
}

# ---- Geography of the central review bands ------------

rep_plot_fig4 <- function(data, output_dir, png = FALSE) {
  FONT <- data$FONT
  okabe <- data$okabe
  pal_county <- data$pal_county
  pal_tract <- data$pal_tract
  grey_outside <- data$grey_outside
  line_tract <- data$line_tract
  county <- data$county
  tract <- data$tract
  pilot_ids <- data$pilot_ids
  geo_county <- data$geo_county
  geo_tract <- data$geo_tract
  places <- data$places
  state_bbox <- data$state_bbox


  cmap <- geo_county %>%
    left_join(county %>% select(county_id, candidate, band), by = "county_id") %>%
    mutate(
      fill_cat = ifelse(candidate, band, "Outside the county-government route"),
      fill_cat = factor(fill_cat, c(names(pal_county), "Outside the county-government route"))
    )
  pal_c <- c(pal_county, `Outside the county-government route` = grey_outside)
  stopifnot(sum(cmap$fill_cat == "Outside the county-government route") == 3L)
  # Key the label nudges to place IDs rather than geometry file row order.
  focal <- cmap[match(c("01101", "01087", "01113"), cmap$county_id), ]
  lab_pts <- st_centroid(st_geometry(focal)) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    mutate(name = pilot_ids[focal$county_id])
  bb <- state_bbox
  pA <- ggplot(cmap) +
    geom_sf(aes(fill = fill_cat), colour = "white", linewidth = .18) +
    geom_sf(data = focal, fill = NA, colour = okabe["vermillion"], linewidth = .55) +
    geom_label_repel(
      data = lab_pts, aes(X, Y, label = name), size = 2.5, family = FONT, fontface = "bold", fill = alpha("white", .85), label.size = 0,
      min.segment.length = 0, nudge_x = c(-95000, 0, 95000), nudge_y = c(-95000, -120000, 60000), segment.colour = "grey30", segment.size = .3, seed = 3
    ) +
    scale_fill_manual(
      values = pal_c, name = "County review bands (64 candidates)", drop = FALSE,
      guide = guide_legend(nrow = if (length(pal_c) > 5L) 2 else 1, override.aes = list(colour = "grey40", linewidth = .2))
    ) +
    geom_label_repel(
      data = st_centroid(st_geometry(cmap %>% filter(county_id == "01041"))) %>% st_coordinates() %>% as.data.frame() %>% mutate(name = paste0("Crenshaw (", cmap$band[cmap$county_id == "01041"], ")")),
      aes(X, Y, label = name), size = 2.2, family = FONT, fill = alpha("white", .85), label.size = 0, min.segment.length = 0, nudge_x = -70000, nudge_y = -140000, segment.colour = "grey30", segment.size = .3, seed = 4
    ) +
    rep_figure_scale_bar(bb["xmin"] + 5000, bb["ymin"] + 8000, 80, "80 km") +
    rep_figure_theme_map() +
    labs(tag = "A") +
    theme(legend.position = "bottom")
  tmap <- geo_tract %>%
    filter(county_id %in% names(pilot_ids)) %>%
    left_join(tract %>% select(tract_id, pilot, band), by = "tract_id") %>%
    mutate(band = factor(band, names(pal_tract)))
  stopifnot(nrow(tmap) == 90L, all(tmap$pilot))
  panel <- function(cid, tag, len_km, show_legend = TRUE) {
    z <- tmap %>% filter(county_id == cid)
    bz <- st_bbox(z)
    n <- table(z$band)
    band_numbers <- seq_along(n)
    count_label <- if (length(n) == 3L) {
      sprintf("%s: %d tracts\nBands 1 / 2 / 3 = %d / %d / %d", pilot_ids[cid], nrow(z), n[1], n[2], n[3])
    } else if (length(n) <= 3L) {
      sprintf("%s: %d tracts\nBands %s = %s", pilot_ids[cid], nrow(z), paste(band_numbers, collapse = " / "), paste(n, collapse = " / "))
    } else {
      sprintf("%s: %d tracts\nBands %s\nCounts %s", pilot_ids[cid], nrow(z), paste(band_numbers, collapse = " / "), paste(n, collapse = " / "))
    }
    header_space <- if (length(n) > 3L) .36 else .25
    city <- if (cid == "01101") places %>% filter(NAME == "Montgomery") else NULL
    ggplot(z) +
      geom_sf(aes(fill = band), colour = line_tract, linewidth = .18, show.legend = show_legend) +
      geom_sf(data = geo_county %>% filter(county_id == cid), fill = NA, colour = "grey35", linewidth = .35) +
      {
        if (!is.null(city)) geom_sf(data = city, fill = NA, colour = okabe["orange"], linewidth = .45, linetype = "22")
      } +
      {
        if (!is.null(city)) annotate("text", x = bz["xmax"] - .02 * (bz["xmax"] - bz["xmin"]), y = bz["ymin"] + .05 * (bz["ymax"] - bz["ymin"]), label = "dashed: City of Montgomery boundary (2026)", hjust = 1, vjust = 0, size = 2.0, family = FONT, colour = okabe["orange"])
      } +
      scale_fill_manual(
        values = pal_tract, name = "Common tract review bands (one report for all 90 pilot tracts)", drop = FALSE,
        guide = guide_legend(nrow = if (length(pal_tract) > 3L) 2 else 1, override.aes = list(colour = "grey40", linewidth = .2))
      ) +
      rep_figure_scale_bar(bz["xmin"] + 1500, bz["ymin"] + 1500, len_km, paste(len_km, "km")) +
      annotate("text",
        x = bz["xmin"] + .13 * (bz["xmax"] - bz["xmin"]), y = bz["ymax"] + (header_space - .01) * (bz["ymax"] - bz["ymin"]), label = count_label,
        hjust = 0, vjust = 1, size = 2.45, family = FONT, colour = "grey15", lineheight = .95
      ) +
      coord_sf(ylim = c(bz["ymin"], bz["ymax"] + header_space * (bz["ymax"] - bz["ymin"])), expand = FALSE, clip = "off") +
      rep_figure_theme_map() +
      labs(tag = tag) +
      theme(legend.position = "bottom")
  }
  pB <- panel("01101", "B", 10)
  pC <- panel("01087", "C", 10, FALSE)
  pD <- panel("01113", "D", 10, FALSE)
  design <- "AABB\nAACD"
  p <- pA + pB + pC + pD + plot_layout(design = design, guides = "collect") & theme(legend.position = "bottom", legend.box = "vertical")
  height <- 138 + 6 * (length(pal_c) > 5L) + 6 * (length(pal_tract) > 3L)
  rep_figure_save(p, "fig4_review_bands_maps", 174, height, output_dir = output_dir, png = png)
}

# ---- Intact-band readout at an assumed review capacity ------------

rep_plot_fig5 <- function(data, output_dir, png = FALSE) {
  FONT <- data$FONT
  okabe <- data$okabe
  pal_county <- data$pal_county
  pal_tract <- data$pal_tract
  county <- data$county
  tract <- data$tract


  ladder <- function(sizes, pal, Ks, universe_label, n_label, tag) {
    d <- tibble(band = names(pal)[seq_along(sizes)], n = sizes) %>% mutate(
      start = cumsum(lag(n, default = 0)), end = cumsum(n),
      status_K1 = case_when(end <= Ks[1] ~ "core", start < Ks[1] & end > Ks[1] ~ "boundary", TRUE ~ "outside"),
      status_K2 = case_when(end <= Ks[2] ~ "core", start < Ks[2] & end > Ks[2] ~ "boundary", TRUE ~ "outside")
    )
    label_colours <- stats::setNames(ifelse(names(pal) %in% c("Band 1", "Band 2"), "white", "grey15"), names(pal))
    if (length(pal) > if (universe_label == "county") 4L else 3L) {
      rgb <- grDevices::col2rgb(pal) / 255
      linear <- ifelse(rgb <= .04045, rgb / 12.92, ((rgb + .055) / 1.055)^2.4)
      luminance <- colSums(linear * c(.2126, .7152, .0722))
      label_colours <- stats::setNames(ifelse(luminance > .179, "black", "white"), names(pal))
    }
    rows <- bind_rows(d %>% mutate(K = Ks[1], status = status_K1), d %>% mutate(K = Ks[2], status = status_K2)) %>%
      mutate(Klab = factor(paste0("Assumed review capacity K = ", K), paste0("Assumed review capacity K = ", Ks)))
    ggplot(rows) +
      geom_rect(aes(xmin = start, xmax = end, ymin = .55, ymax = 1.45, fill = band), colour = "white", linewidth = .4) +
      geom_text(data = rows %>% filter(n >= 3), aes(x = (start + end) / 2, y = 1, label = paste0(band, "\n", n)), size = 2.4, family = FONT, colour = unname(label_colours[rows$band[rows$n >= 3]]), lineheight = .9) +
      geom_text(data = rows %>% filter(n < 3, start > 0), aes(x = end + .8, y = 1, label = paste0(band, " (", n, ")")), size = 2.2, family = FONT, colour = "grey25", hjust = 0) +
      geom_vline(aes(xintercept = K), linetype = "22", colour = okabe["vermillion"], linewidth = .5) +
      geom_text(aes(x = K, y = 1.62, label = paste0("K = ", K)), size = 2.4, family = FONT, colour = okabe["vermillion"], hjust = -.1) +
      geom_text(aes(x = (start + end) / 2, y = .38, label = ifelse(n < 3 & start == 0, paste0(band, " (", n, "): ", status), status), hjust = ifelse(n < 3 & start == 0, 0, .5)), size = 2.3, family = FONT, colour = "grey30", fontface = "italic") +
      facet_wrap(~Klab, ncol = 1) +
      scale_fill_manual(values = pal, guide = "none") +
      scale_x_continuous(breaks = seq(0, sum(sizes), by = 10), expand = expansion(mult = c(0, .11))) +
      coord_cartesian(ylim = c(.25, 1.75)) +
      labs(tag = tag, x = sprintf("Position in the review order, from highest historical need (%s)", n_label), y = NULL) +
      rep_figure_theme_house() +
      theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(), strip.text = element_text(face = "plain", colour = "grey25"))
  }
  csz <- as.integer(table(county$grade_P[county$candidate]))
  tsz <- as.integer(table(tract$grade_P[tract$pilot]))
  pA <- ladder(csz, pal_county, c(3, 5), "county", "64 candidate county governments", "A")
  pB <- ladder(tsz, pal_tract, c(5, 10), "tract", "90 pilot tracts in Macon, Montgomery and Russell", "B")
  p <- pA / pB
  rep_figure_save(p, "fig5_capacity_readout", 174, 112, output_dir = output_dir, png = png)
}

# ---- Disclosure and expected reporting loss ------------

rep_plot_fig6 <- function(data, output_dir, png = FALSE) {
  FONT <- data$FONT
  BASE <- data$BASE
  okabe <- data$okabe
  county <- data$county
  tract <- data$tract
  report_metrics <- data$report_metrics


  rd <- function(which_universe, lab) {
    report_metrics %>%
      filter(universe == which_universe) %>%
      transmute(
        universe = lab, collapse = unname(data$B3_P_equal[which_universe]), method, N = rep_figure_num(N), M = rep_figure_num(M), disclosure = rep_figure_num(disclosure_fraction),
        risk = rep_figure_num(B_Rtau), mcse = rep_figure_num(B_Rtau_mcse)
      )
  }
  d <- bind_rows(
    rd("county", "County review (64 candidate county governments)"),
    rd("tract", "Tract review (90 pilot tracts)")
  ) %>%
    mutate(point = ifelse(method %in% c("B3", "P") & collapse, "B3 = P", method), universe = factor(universe, unique(universe)))


  # A fresh selection need not make B3 and P identical. Collapse only an
  # independently verified equal relation vector, never merely equal risk.
  dd <- d %>% filter(!(method == "B3" & collapse))
  cols <- c(B0 = "grey45", B1 = unname(okabe["vermillion"]), B2 = unname(okabe["blue"]), `B3 = P` = "#3F007D", B3 = unname(okabe["green"]), P = "#3F007D")
  labs_txt <- c(
    B0 = "B0: every comparison withheld", B1 = "B1: published-estimate ranking, fixed quintile bands",
    B2 = "B2: posterior-mean ranking, fixed quintile bands", `B3 = P` = "B3 = P: posterior review order with withheld comparisons",
    B3 = "B3: optimal cuts of the fixed expected-rank order", P = "P: free posterior review partition"
  )
  risk_limits <- c(.085, .235)
  risk_breaks <- seq(.1, .225, .025)
  if (data$fresh) {
    range <- range(c(dd$risk - 2 * dd$mcse, dd$risk + 2 * dd$mcse, .2))
    if (range[1] < risk_limits[1] || range[2] > risk_limits[2]) {
      padding <- max(diff(range) * .08, .005)
      risk_limits <- c(max(0, range[1] - padding), range[2] + padding)
      risk_breaks <- pretty(risk_limits, 5)
    }
  }
  height <- if (all(data$B3_P_equal)) 92 else 100
  p <- ggplot(dd, aes(disclosure, risk, colour = point)) +
    geom_hline(yintercept = .2, linetype = "22", colour = "grey55", linewidth = .35) +
    geom_errorbar(aes(ymin = risk - 2 * mcse, ymax = risk + 2 * mcse), width = 0, linewidth = .4) +
    geom_point(size = 2.6) +
    geom_text_repel(aes(label = point), size = 2.5, family = FONT, fontface = "bold", show.legend = FALSE, min.segment.length = 10, nudge_y = -.006, seed = 2) +
    facet_wrap(~universe) +
    scale_colour_manual(values = cols, labels = labs_txt, name = NULL, guide = guide_legend(ncol = 2, byrow = TRUE)) +
    scale_x_continuous(labels = label_percent(accuracy = 1), limits = c(-.03, 1.03), breaks = seq(0, 1, .25)) +
    scale_y_continuous(limits = risk_limits, breaks = risk_breaks) +
    labs(x = "Share of unordered pairs with a disclosed direction", y = "Expected reporting loss (lower is better)") +
    rep_figure_theme_house() +
    theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = .25), legend.text = element_text(size = BASE - .8))
  rep_figure_save(p, "fig6_disclosure_loss", 174, height, output_dir = output_dir, png = png)
}

# ---- Stability across the prespecified cases ------------

rep_plot_fig7 <- function(data, output_dir, png = FALSE) {
  FONT <- data$FONT
  BASE <- data$BASE
  okabe <- data$okabe
  county <- data$county
  tract <- data$tract
  first_band <- data$first_band
  sensitivity <- data$sensitivity
  stability_cases <- data$stability_cases
  pilot_ids <- data$pilot_ids


  t13 <- sensitivity
  case_lab <- c(
    center = "Center", `PV-w25` = "Income weight .25", `PV-w75` = "Income weight .75", `PV-t10` = "Withholding cost .10",
    `PV-t30` = "Withholding cost .30", `PV-TS1` = "Single-level model", `PV-TS2` = "SE × 1.25", `PV-TS3` = "Within-county error ρ = .25"
  )
  # (A) county core membership under the five policy settings (K = 3 and K = 5)
  cp <- t13 %>%
    filter(universe == "county", method == "P") %>%
    select(policy_id, capacity1_core_ids, capacity1_boundary_ids, capacity2_core_ids, capacity2_boundary_ids)
  stopifnot(nrow(cp) == 5L)
  status_of <- function(id, core, bdy) ifelse(id %in% strsplit(rep_figure_or(core, ""), ";")[[1]], "core", ifelse(id %in% strsplit(rep_figure_or(bdy, ""), ";")[[1]], "boundary", "outside"))
  cA <- expand_grid(policy_id = cp$policy_id, county_id = c("01101", "01113", "01087")) %>%
    rowwise() %>%
    mutate(
      K3 = status_of(county_id, cp$capacity1_core_ids[cp$policy_id == policy_id], cp$capacity1_boundary_ids[cp$policy_id == policy_id]),
      K5 = status_of(county_id, cp$capacity2_core_ids[cp$policy_id == policy_id], cp$capacity2_boundary_ids[cp$policy_id == policy_id])
    ) %>%
    ungroup() %>%
    pivot_longer(c(K3, K5), names_to = "K", values_to = "status") %>%
    mutate(
      K = ifelse(K == "K3", "K = 3", "K = 5"), case = factor(case_lab[policy_id], case_lab[1:5]),
      county = factor(pilot_ids[county_id], c("Montgomery", "Russell", "Macon")), status = factor(status, c("core", "boundary", "outside"))
    )
  if (!data$fresh) stopifnot(all(cA$status[cA$county %in% c("Montgomery", "Russell")] == "core"))
  pal_s <- c(core = "#08306B", boundary = "#9ECAE1", outside = "#F2F2F2")
  pA <- ggplot(cA, aes(case, county, fill = status)) +
    geom_tile(colour = "white", linewidth = .8) +
    geom_text(aes(label = status, colour = status == "core"), size = 2.3, family = FONT) +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey25"), guide = "none") +
    facet_wrap(~K, nrow = 1) +
    scale_fill_manual(values = pal_s, name = NULL, drop = FALSE, guide = "none") +
    scale_y_discrete(limits = rev) +
    labs(tag = "A", x = NULL, y = NULL) +
    rep_figure_theme_house() +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")
  # (B) membership of the central first-band tracts across the eight valid cases
  pc <- stability_cases %>% filter(tract_id %in% first_band$tract_id)
  stopifnot(nrow(pc) == nrow(first_band) * 8L, all(pc$scientifically_ready == "TRUE"))
  cB <- pc %>%
    select(tract_id, case_id, capacity_K5, capacity_K10, grade_original) %>%
    pivot_longer(c(capacity_K5, capacity_K10), names_to = "K", values_to = "status") %>%
    mutate(
      K = ifelse(K == "capacity_K5", "K = 5", "K = 10"), K = factor(K, c("K = 5", "K = 10")), case = factor(case_lab[case_id], case_lab),
      status = factor(status, c("core", "boundary", "outside")),
      tract = paste0(pilot_ids[substr(tract_id, 1, 5)], " ", first_band$tract_label[match(tract_id, first_band$tract_id)]),
      tract = factor(tract, paste0(pilot_ids[substr(first_band$tract_id, 1, 5)], " ", first_band$tract_label))
    )
  n_always_K5 <- pc %>%
    group_by(tract_id) %>%
    summarise(ok = all(capacity_K5 != "outside")) %>%
    summarise(n = sum(ok)) %>%
    pull(n)
  n_always_K10 <- pc %>%
    group_by(tract_id) %>%
    summarise(ok = all(capacity_K10 != "outside")) %>%
    summarise(n = sum(ok)) %>%
    pull(n)

  cnt <- cB %>%
    group_by(tract, K) %>%
    summarise(`in scope (core or boundary)` = sum(status != "outside"), core = sum(status == "core"), .groups = "drop") %>%
    pivot_longer(c(`in scope (core or boundary)`, core), names_to = "what", values_to = "n") %>%
    mutate(what = factor(what, c("in scope (core or boundary)", "core")))
  pB <- ggplot(cnt, aes(n, tract, colour = what)) +
    geom_vline(xintercept = 8, linetype = "22", colour = "grey60", linewidth = .35) +
    geom_segment(data = cnt %>% pivot_wider(names_from = what, values_from = n), aes(x = core, xend = `in scope (core or boundary)`, y = tract, yend = tract), inherit.aes = FALSE, colour = "grey75", linewidth = .6) +
    geom_point(size = 2.3) +
    facet_wrap(~K, nrow = 1) +
    scale_colour_manual(values = c(`in scope (core or boundary)` = "#9ECAE1", core = "#08306B"), name = "Number of the eight cases in which the tract is") +
    scale_x_continuous(breaks = 0:8, limits = c(-.2, 8.3)) +
    scale_y_discrete(limits = rev) +
    labs(tag = "B", x = "Cases (of 8)", y = NULL) +
    rep_figure_theme_house() +
    theme(panel.grid.major.y = element_line(colour = "grey94", linewidth = .25), panel.grid.major.x = element_blank(), axis.text.y = element_text(size = BASE - 1))
  # (C) pair relations changed from the center report
  pol <- t13 %>%
    filter(universe == "tract", method == "P", scenario_kind == "policy", policy_id != "center") %>%
    transmute(
      case = case_lab[policy_id], `withheld to disclosed` = rep_figure_num(P_vs_centerP_tie_to_disclosure), `disclosed to withheld` = rep_figure_num(P_vs_centerP_disclosure_to_tie),
      `direction reversed` = rep_figure_num(P_vs_centerP_direction_reversals), total = rep_figure_num(P_vs_centerP_relation_changes)
    )
  mod <- t13 %>%
    filter(universe == "tract", method == "P", scenario_kind == "model") %>%
    transmute(
      case = case_lab[scenario_id], `withheld to disclosed` = rep_figure_num(P_vs_main_center_tie_to_disclosure), `disclosed to withheld` = rep_figure_num(P_vs_main_center_disclosure_to_tie),
      `direction reversed` = rep_figure_num(P_vs_main_center_direction_reversals), total = rep_figure_num(P_vs_main_center_relation_changes)
    )
  ch <- bind_rows(pol, mod) %>% mutate(case = factor(case, rev(case_lab[-1])))
  stopifnot(nrow(ch) == 7L, all(ch$total == ch$`withheld to disclosed` + ch$`disclosed to withheld` + ch$`direction reversed`))

  chl <- ch %>%
    pivot_longer(c(`withheld to disclosed`, `disclosed to withheld`, `direction reversed`), names_to = "type", values_to = "n") %>%
    mutate(type = factor(type, c("withheld to disclosed", "disclosed to withheld", "direction reversed")))
  pC <- ggplot(chl, aes(n, case, fill = type)) +
    geom_col(width = .66) +
    geom_text(data = ch, aes(x = total, y = case, label = format(total, big.mark = ",")), inherit.aes = FALSE, hjust = -.15, size = 2.4, family = FONT, colour = "grey20") +
    scale_fill_manual(values = c(`withheld to disclosed` = unname(okabe["blue"]), `disclosed to withheld` = "grey60", `direction reversed` = unname(okabe["vermillion"])), name = "Change relative to the center report") +
    scale_x_continuous(limits = c(0, 4005), breaks = c(0, 1000, 2000, 3000, 4005), labels = label_comma(), expand = expansion(mult = c(0, .02))) +
    labs(tag = "C", x = "Unordered tract pairs whose reported relation changed (of 4,005)", y = NULL) +
    rep_figure_theme_house() +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92", linewidth = .25), legend.text = element_text(size = BASE - .8))
  # Keep the published layout exactly for its 13 rows. Fresh first bands get
  # enough row space while retaining fixed overhead for axes and the legend.
  rows <- nrow(first_band)
  panel_heights <- if (rows == 13L) c(1.0, 2.2, 1.9) else c(1.0, .8 + 1.4 * rows / 13, 1.9)
  height <- if (rows == 13L) 185 else 185 * sum(panel_heights) / sum(c(1, 2.2, 1.9))
  p <- (pA / pB / pC) + plot_layout(heights = panel_heights)
  rep_figure_save(p, "fig7_review_scope_stability", 174, height, output_dir = output_dir, png = png)
}

# ---- Applications and current county conditions ------------

rep_plot_fig8 <- function(data, output_dir, png = FALSE) {
  FONT <- data$FONT
  okabe <- data$okabe
  pal_county <- data$pal_county
  grey_outside <- data$grey_outside
  county <- data$county
  applications <- data$applications
  program_context <- data$program_context
  county_poverty <- data$county_poverty
  pilot_ids <- data$pilot_ids
  geo_county <- data$geo_county
  state_bbox <- data$state_bbox


  apps <- applications %>% mutate(usd = rep_figure_num(amount_requested_usd_exact))
  stopifnot(nrow(apps) == 72L)
  byfund <- apps %>%
    group_by(fund = fund_section) %>%
    summarise(n = n(), usd = sum(usd)) %>%
    mutate(fund = factor(fund, c("Small City", "Large City", "County", "Community Enhancement", "Planning")))
  alloc <- program_context$value[program_context$metric_id == "county_fund_allocation_usd"] / 1e6
  pA <- ggplot(byfund, aes(usd / 1e6, fund)) +
    geom_col(fill = okabe["blue"], width = .62) +
    geom_text(aes(label = sprintf("%d applications; US$%.2f million", n, usd / 1e6)), hjust = -.04, size = 2.4, family = FONT, colour = "grey20") +
    annotate("segment", x = alloc, xend = alloc, y = 2.55, yend = 3.45, colour = okabe["vermillion"], linewidth = .7) +
    annotate("text", x = alloc, y = 2.5, label = sprintf("County Fund allocation US$%.2f million", alloc), hjust = 0, vjust = 1, size = 2.2, family = FONT, colour = okabe["vermillion"]) +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(limits = c(0, 25), expand = expansion(mult = c(0, 0)), breaks = seq(0, 15, 5)) +
    labs(tag = "A", x = "Amount requested by 22 July 2026 (US$ million)", y = NULL) +
    rep_figure_theme_house() +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92", linewidth = .25))
  cm <- geo_county %>%
    left_join(county %>% select(county_id, candidate, band, n_app = matched_received_application_count), by = "county_id") %>%
    mutate(n_cat = factor(ifelse(is.na(n_app), "0", as.character(pmin(n_app, 4))), c("0", "1", "2", "3", "4")))
  stopifnot(sum(cm$n_app) == 71L)
  # Key the label nudges to place IDs rather than geometry file row order.
  focal <- cm[match(c("01101", "01087", "01113"), cm$county_id), ]
  lab_pts <- st_centroid(st_geometry(focal)) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    mutate(name = pilot_ids[focal$county_id])
  bb <- state_bbox
  pal_n <- c(`0` = "#FFFFFF", `1` = "#C6DBEF", `2` = "#6BAED6", `3` = "#2171B5", `4` = "#08306B")
  outl <- cm %>%
    filter(candidate) %>%
    mutate(band_line = factor(ifelse(band == "Band 1", "Band 1", ifelse(band == "Band 2", "Band 2", "Later bands")), c("Band 1", "Band 2", "Later bands")))
  pB <- ggplot(cm) +
    geom_sf(aes(fill = n_cat), colour = "grey70", linewidth = .15) +
    geom_sf(data = outl %>% filter(band_line != "Later bands"), aes(colour = band_line), fill = NA, linewidth = .5) +
    geom_sf_text(data = cm %>% filter(!candidate), label = "×", size = 2.6, colour = "grey35", family = FONT) +
    scale_colour_manual(
      values = c(`Band 1` = unname(okabe["vermillion"]), `Band 2` = "#08306B"), name = "County review band (outline)", drop = TRUE,
      guide = guide_legend(nrow = 1, title.position = "top", override.aes = list(fill = NA))
    ) +
    geom_label_repel(
      data = lab_pts, aes(X, Y, label = name), size = 2.4, family = FONT, fontface = "bold", fill = alpha("white", .85), label.size = 0,
      min.segment.length = 0, nudge_x = c(-95000, 0, 95000), nudge_y = c(-95000, -120000, 60000), segment.colour = "grey30", segment.size = .3, seed = 3
    ) +
    scale_fill_manual(
      values = pal_n, name = "Matched applications\n(× = outside the county-government route)", drop = FALSE, labels = c("0", "1", "2", "3", "4"),
      guide = guide_legend(nrow = 1, title.position = "top", override.aes = list(colour = "grey40", linewidth = .2))
    ) +
    rep_figure_scale_bar(bb["xmin"] + 5000, bb["ymin"] + 8000, 80, "80 km") +
    rep_figure_theme_map() +
    labs(tag = "B") +
    theme(legend.box = "vertical", legend.spacing.y = unit(0, "mm"))
  # (C) current related-child poverty share by county band
  cp <- county_poverty %>%
    left_join(county %>% select(county_id, county_short, candidate, band), by = "county_id") %>%
    mutate(group = factor(ifelse(candidate, band, "Outside route"), c(names(pal_county), "Outside route")))
  pC <- ggplot(cp, aes(group, share)) +
    geom_errorbar(aes(ymin = share - share_moe, ymax = share + share_moe), width = 0, colour = "grey70", linewidth = .35, position = position_jitter(width = .18, height = 0, seed = 7)) +
    geom_point(aes(fill = group), shape = 21, colour = "white", size = 2.1, stroke = .3, position = position_jitter(width = .18, height = 0, seed = 7)) +
    geom_text_repel(data = cp %>% filter(county_id %in% c("01101", "01113", "01087")), aes(label = county_short), size = 2.3, family = FONT, fontface = "bold", position = position_jitter(width = .18, height = 0, seed = 7), min.segment.length = 0, seed = 9) +
    scale_fill_manual(values = c(pal_county, `Outside route` = grey_outside), guide = "none") +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, NA)) +
    labs(tag = "C", x = "County review band (center report)", y = "Related children under 18 below poverty,\nACS 2020-2024 (share, 90% margin of error)") +
    rep_figure_theme_house() +
    theme(panel.grid.major.x = element_blank())
  p <- (pA + pB + plot_layout(widths = c(1.05, 1))) / pC + plot_layout(heights = c(1, .9))
  rep_figure_save(p, "fig8_applications_snapshot", 174, 175, output_dir = output_dir, png = png)
}

# ---- Precision of the tract estimates ------------

rep_plot_figS1 <- function(data, output_dir, png = FALSE) {
  FONT <- data$FONT
  BASE <- data$BASE
  grey_na <- data$grey_na
  tract <- data$tract
  geo_county <- data$geo_county
  geo_tract <- data$geo_tract
  state_bbox <- data$state_bbox

  # precision: published SE and posterior interval width
  d <- geo_tract %>% left_join(tract %>% select(
    tract_id, modeled, income_raw_se_points, incarceration_raw_se_points,
    income_posterior_interval_width_points, incarceration_posterior_interval_width_points
  ), by = "tract_id")
  long <- bind_rows(
    d %>% transmute(modeled, crit = "income", q = "Published standard error", v = income_raw_se_points),
    d %>% transmute(modeled, crit = "income", q = "95% posterior interval width", v = income_posterior_interval_width_points),
    d %>% transmute(modeled, crit = "incarceration", q = "Published standard error", v = incarceration_raw_se_points),
    d %>% transmute(modeled, crit = "incarceration", q = "95% posterior interval width", v = incarceration_posterior_interval_width_points)
  )
  long$v[!long$modeled] <- NA
  panel <- function(crit, q, tag, unit, lim, brk) {
    z <- long %>% filter(crit == !!crit, q == !!q)
    stopifnot(sum(!is.na(z$v)) == 1173L)
    ggplot(z) +
      geom_sf(aes(fill = v), colour = NA) +
      geom_sf(data = geo_county, fill = NA, colour = "white", linewidth = .12) +
      scale_fill_viridis_c(
        option = "viridis", limits = lim, oob = squish, na.value = grey_na, name = paste0(q, "\n(", unit, "; ", lim[2], "+ = ", lim[2], " or more)"),
        breaks = brk, labels = function(b) ifelse(b >= lim[2], paste0(b, "+"), format(b)),
        guide = guide_colourbar(barwidth = unit(28, "mm"), barheight = unit(2.4, "mm"), title.position = "top")
      ) +
      rep_figure_theme_map() +
      labs(tag = tag) +
      theme(legend.title = element_text(size = BASE - .6, lineheight = .95), legend.text = element_text(size = BASE - 1)) +
      annotate("text", x = state_bbox["xmax"] - 2000, y = state_bbox["ymin"] + 90000, label = paste(if (crit == "income") "Income" else "Incarceration"), hjust = 1, vjust = 1, size = 2.6, family = FONT, colour = "grey15")
  }
  pA <- panel("income", "Published standard error", "A", "percentile points", c(0, 6), c(0, 2, 4, 6))
  pB <- panel("income", "95% posterior interval width", "B", "percentile points", c(0, 16), c(0, 4, 8, 12, 16))
  pC <- panel("incarceration", "Published standard error", "C", "percentage points", c(0, 6), c(0, 2, 4, 6))
  pD <- panel("incarceration", "95% posterior interval width", "D", "percentage points", c(0, 10), c(0, 5, 10))
  p <- (pA + pB) / (pC + pD)
  rep_figure_save(p, "figS1_precision_maps", 174, 196, output_dir = output_dir, png = png)
}

# ---- Posterior mean minus the published estimate ------------

rep_plot_figS2 <- function(data, output_dir, png = FALSE) {
  FONT <- data$FONT
  BASE <- data$BASE
  okabe <- data$okabe
  grey_na <- data$grey_na
  tract <- data$tract
  geo_county <- data$geo_county
  geo_tract <- data$geo_tract
  state_bbox <- data$state_bbox

  # posterior mean minus published estimate
  d <- geo_tract %>% left_join(tract %>% select(tract_id, modeled, income_shrinkage_signed_points, incarceration_shrinkage_signed_points), by = "tract_id")
  panel <- function(v, tag, unit, lim, lab) {
    z <- d
    z$v <- v
    z$v[!z$modeled] <- NA
    ggplot(z) +
      geom_sf(aes(fill = v), colour = NA) +
      geom_sf(data = geo_county, fill = NA, colour = "grey70", linewidth = .12) +
      scale_fill_gradient2(
        low = okabe["vermillion"], mid = "white", high = okabe["blue"], midpoint = 0, limits = lim, oob = squish, na.value = grey_na,
        name = paste0("Posterior mean minus published estimate\n(", unit, ")"), guide = guide_colourbar(barwidth = unit(34, "mm"), barheight = unit(2.4, "mm"), title.position = "top")
      ) +
      rep_figure_theme_map() +
      labs(tag = tag) +
      theme(legend.title = element_text(size = BASE - .6), legend.text = element_text(size = BASE - 1)) +
      annotate("text", x = state_bbox["xmax"] - 2000, y = state_bbox["ymin"] + 90000, label = lab, hjust = 1, vjust = 1, size = 2.6, family = FONT, colour = "grey15")
  }
  pA <- panel(d$income_shrinkage_signed_points, "A", "percentile points", c(-8, 8), "Income")
  pB <- panel(d$incarceration_shrinkage_signed_points, "B", "percentage points", c(-8, 8), "Incarceration")
  rep_figure_save(pA + pB, "figS2_posterior_adjustment", 174, 102, output_dir = output_dir, png = png)
}

# ---- Publication and full-identifier pair-relation matrices ------------

rep_plot_figS3 <- function(data, output_dir, png = FALSE) {
  okabe <- data$okabe
  county <- data$county
  tract <- data$tract

  # pair-relation matrices implied by the center reports
  rel_matrix <- function(ids, grades, labels, pal_name, tag, title, detailed = FALSE) {
    o <- order(grades, ids)
    ids <- ids[o]
    grades <- grades[o]
    labels <- labels[o]
    g <- expand_grid(i = seq_along(ids), j = seq_along(ids)) %>%
      filter(i != j) %>%
      mutate(
        rel = case_when(grades[i] < grades[j] ~ "row entity reported as higher need", grades[i] > grades[j] ~ "column entity reported as higher need", TRUE ~ "comparison withheld (same band)"),
        rel = factor(rel, c("row entity reported as higher need", "column entity reported as higher need", "comparison withheld (same band)"))
      )
    n_disc <- sum(g$rel != "comparison withheld (same band)") / 2
    cuts <- cumsum(table(grades))[-length(unique(grades))] + .5
    counts <- as.integer(table(grades))
    mids <- cumsum(counts) - (counts - 1) / 2
    band_labels <- paste0("Band ", seq_along(counts), "\n(n = ", counts, ")")
    breaks <- if (detailed) seq_along(ids) else mids
    axis_labels <- if (detailed) labels else band_labels
    # Explicit vector cells retain the row/column meaning with a reversed y axis.
    # The diagonal is left white because a place is not compared with itself.
    ggplot(g, aes(j, i, fill = rel)) +
      geom_tile(width = 1, height = 1, colour = NA) +
      geom_hline(yintercept = cuts, colour = "white", linewidth = .5) +
      geom_vline(xintercept = cuts, colour = "white", linewidth = .5) +
      scale_fill_manual(values = c(`row entity reported as higher need` = unname(okabe["blue"]), `column entity reported as higher need` = unname(okabe["vermillion"]), `comparison withheld (same band)` = "grey88"), name = NULL, guide = guide_legend(nrow = 3)) +
      scale_y_reverse(breaks = breaks, labels = axis_labels, expand = c(0, 0)) +
      scale_x_continuous(breaks = breaks, labels = axis_labels, expand = c(0, 0), position = "top") +
      coord_equal() +
      labs(tag = tag, x = NULL, y = NULL) +
      rep_figure_theme_house(if (detailed) 10 else 10.5) +
      # The enlarged identifier sheets need a small tag inset so the glyph
      # bounding box stays within the page as well as the plot area.
      theme(plot.tag.position = c(if (detailed) .005 else 0, 1), panel.grid = element_blank(), axis.text.x = element_text(angle = if (detailed) 90 else 0, hjust = if (detailed) 0 else .5, vjust = .5, size = if (detailed) 9 else 10), axis.text.y = element_text(size = if (detailed) 9 else 10), legend.position = "bottom", plot.margin = if (detailed) margin(4, 4, 2, 4) else margin(6, 26, 4, 6))
  }
  cc <- county %>% filter(candidate)
  tt <- tract %>% filter(pilot)
  pA <- rel_matrix(cc$county_id, as.integer(cc$grade_P), cc$county_short, "county", "A", sprintf("County report (%d candidates, %d bands)", nrow(cc), length(unique(cc$grade_P))))
  pB <- rel_matrix(tt$tract_id, as.integer(tt$grade_P), paste(tt$county_short, tt$tract_label), "tract", "B", sprintf("Common tract report (%d pilot tracts, %d bands)", nrow(tt), length(unique(tt$grade_P))))
  if (!data$fresh) stopifnot(sum(as.integer(cc$grade_P)[rep(1:64, each = 64)] != as.integer(cc$grade_P)[rep(1:64, 64)]) / 2 == 1115)
  rep_figure_save(pA, "figS3a_relations_county", 146, 160, output_dir = output_dir, png = png)
  rep_figure_save(pB, "figS3b_relations_tract", 146, 160, output_dir = output_dir, png = png)
  rep_figure_save(rel_matrix(cc$county_id, as.integer(cc$grade_P), cc$county_id, "county", "A", "County report", TRUE), "figS3a_relations_county_detailed", 300, 320, output_dir = output_dir, png = png)
  rep_figure_save(rel_matrix(tt$tract_id, as.integer(tt$grade_P), tt$tract_id, "tract", "B", "Tract report", TRUE), "figS3b_relations_tract_detailed", 380, 410, output_dir = output_dir, png = png)
}

# ---- All90 tract memberships, and three continued publication panels ------------

rep_plot_figS4 <- function(data, output_dir, png = FALSE) {
  FONT <- data$FONT
  county <- data$county
  tract <- data$tract
  stability_cases <- data$stability_cases

  # membership of all 90 pilot tracts across the eight cases
  pc <- stability_cases
  case_lab <- c(
    center = "Center", `PV-w25` = "Income weight .25", `PV-w75` = "Income weight .75", `PV-t10` = "Withholding cost .10",
    `PV-t30` = "Withholding cost .30", `PV-TS1` = "Single-level model", `PV-TS2` = "SE × 1.25", `PV-TS3` = "Within-county error ρ = .25"
  )
  tt <- tract %>%
    filter(pilot) %>%
    select(tract_id, county_short, tract_label, grade_P)
  d <- pc %>%
    left_join(tt, by = "tract_id") %>%
    select(tract_id, county_short, tract_label, grade_P, case_id, capacity_K5, capacity_K10, grade_original) %>%
    pivot_longer(c(capacity_K5, capacity_K10), names_to = "K", values_to = "status") %>%
    mutate(
      K = factor(ifelse(K == "capacity_K5", "K = 5", "K = 10"), c("K = 5", "K = 10")), case = factor(case_lab[case_id], case_lab),
      status = factor(status, c("core", "boundary", "outside")), tract = paste(county_short, tract_label),
      county_short = factor(county_short, c("Montgomery", "Russell", "Macon"))
    )
  ord <- tt %>%
    mutate(county_short = factor(county_short, c("Montgomery", "Russell", "Macon"))) %>%
    arrange(county_short, as.integer(grade_P), tract_id) %>%
    mutate(tract = paste(county_short, tract_label)) %>%
    pull(tract)
  d$tract <- factor(d$tract, rev(ord))
  pal_s <- c(core = "#08306B", boundary = "#9ECAE1", outside = "#F2F2F2")
  p <- ggplot(d, aes(case, tract, fill = status)) +
    geom_tile(colour = "white", linewidth = .4, show.legend = TRUE) +
    geom_text(aes(label = grade_original, colour = status == "core"), size = 3.2, family = FONT) +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey25"), guide = "none") +
    facet_wrap(~K, nrow = 1) +
    scale_fill_manual(values = pal_s, name = "Membership of the review scope", drop = FALSE, guide = guide_legend(nrow = 1, override.aes = list(colour = "grey50"))) +
    labs(x = NULL, y = NULL) +
    rep_figure_theme_house(10.5) +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1, size = 10), axis.text.y = element_text(size = 9))
  rep_figure_save(p, "figS4_membership_all90", 300, 380, output_dir = output_dir, png = png)
  # Publication panels preserve every cell and the original row ordering; the full matrix
  # above remains an additional replication figure. Short setting labels are defined in the caption.
  short_lab <- c("Center", "w=.25", "w=.75", "t=.10", "t=.30", "TS1", "TS2", "TS3")
  names(short_lab) <- case_lab
  stopifnot(nrow(d) == 90L * 8L * 2L, length(unique(d$tract_id)) == 90L)
  for (part in 1:3) {
    rows <- ord[seq.int((part - 1L) * 30L + 1L, part * 30L)]
    z <- d %>%
      filter(as.character(tract) %in% rows) %>%
      mutate(tract = factor(as.character(tract), rev(rows)))
    stopifnot(nrow(z) == 30L * 8L * 2L)
    panel <- ggplot(z, aes(case, tract, fill = status)) +
      geom_tile(colour = "white", linewidth = .25, show.legend = TRUE) +
      geom_text(aes(label = grade_original, colour = status == "core"), size = 3.3, family = FONT) +
      scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey25"), guide = "none") +
      facet_wrap(~K, nrow = 1) +
      scale_fill_manual(values = pal_s, name = NULL, drop = FALSE, guide = guide_legend(nrow = 1, override.aes = list(colour = "grey50"))) +
      scale_x_discrete(labels = short_lab) +
      labs(x = NULL, y = NULL) +
      rep_figure_theme_house(10.5) +
      theme(
        panel.grid = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9), legend.text = element_text(size = 10),
        panel.spacing.x = unit(3, "mm"), plot.margin = margin(4, 4, 4, 4)
      )
    rep_figure_save(panel, paste0("figS4_membership_part", part), 146, 162, output_dir = output_dir, png = png)
  }
}

# ---- Current context at each source geography ------------

rep_plot_figS5 <- function(data, output_dir, png = FALSE) {
  BASE <- data$BASE
  grey_na <- data$grey_na
  tract <- data$tract
  context_display <- data$context_display
  geo_county <- data$geo_county

  # current context at native geographies
  ctx <- context_display
  acs_t <- ctx %>%
    filter(source_system == "Census_ACS", geography_type == "tract", metric_id == "related_under18_below_poverty_count") %>%
    transmute(tract_id = geoid, v = rep_figure_num(display_estimate))
  hud <- ctx %>%
    filter(source_system == "HUD_LMISD", geography_type == "block_group") %>%
    transmute(block_group_id = geoid, v = rep_figure_num(display_estimate), zero = HUD_LOWMODUNIV_zero)
  g24 <- rep_geo("tract2020_display", 5070) %>% select(tract_id, geom)
  gbg <- rep_geo("block_group2020_display", 5070) %>% select(block_group_id, geom)
  stopifnot(nrow(g24) == 1437L, nrow(gbg) == 3925L)
  g24 <- rep_figure_simplify(g24, 0.18)
  gbg <- rep_figure_simplify(gbg, 0.12)
  a <- g24 %>% left_join(acs_t, by = "tract_id")
  h <- gbg %>% left_join(hud, by = "block_group_id")
  pA <- ggplot(a) +
    geom_sf(aes(fill = v), colour = NA) +
    geom_sf(data = geo_county, fill = NA, colour = "white", linewidth = .12) +
    scale_fill_viridis_c(
      option = "mako", direction = -1, trans = "sqrt", breaks = c(0, 250, 500, 1000), na.value = grey_na, name = "Related children <18 in poverty\nACS 2020-2024; 2020 tracts",
      guide = guide_colourbar(barwidth = unit(34, "mm"), barheight = unit(2.4, "mm"), title.position = "top")
    ) +
    rep_figure_theme_map() +
    labs(tag = "A") +
    theme(plot.tag.position = "topleft", legend.title = element_text(size = BASE - .6), legend.text = element_text(size = BASE - 1))
  pB <- ggplot(h) +
    geom_sf(aes(fill = v), colour = NA) +
    geom_sf(data = geo_county, fill = NA, colour = "white", linewidth = .12) +
    scale_fill_viridis_c(
      option = "mako", direction = -1, limits = c(0, 100), na.value = grey_na, name = "Low- and moderate-income persons\n(%; HUD LMISD 2016-2020; 2020 block groups)",
      guide = guide_colourbar(barwidth = unit(34, "mm"), barheight = unit(2.4, "mm"), title.position = "top")
    ) +
    rep_figure_theme_map() +
    labs(tag = "B") +
    theme(plot.tag.position = "topleft", legend.title = element_text(size = BASE - .6), legend.text = element_text(size = BASE - 1))
  rep_figure_save(pA + pB, "figS5_current_context", 174, 102, output_dir = output_dir, png = png)
}

# ---- Workflow and explicit figure build --------------------------------------

rep_figure_workflow <- function(output_dir) {
  # TeX is needed only for the authored workflow. Compilation artifacts stay in
  # the cache; the output directory receives the single final PDF.
  executable <- Sys.which("pdflatex")
  if (!nzchar(executable)) stop("Install a TeX distribution to rebuild Figure 1.")
  source <- rep_path("fig_src", "fig1_workflow.tex")
  work <- rep_dir(rep_cache("figure-workflow", rep_sha256(source)))
  if (!file.copy(source, file.path(work, "fig1_workflow.tex"), overwrite = TRUE)) {
    stop("Cannot copy the workflow source to the build cache.")
  }
  previous <- getwd()
  on.exit(setwd(previous), add = TRUE)
  setwd(work)
  status <- system2(executable, c(
    "-interaction=nonstopmode", "-halt-on-error",
    "fig1_workflow.tex"
  ),
  stdout = "compile.stdout", stderr = "compile.stderr"
  )
  if (status != 0L || !file.exists("fig1_workflow.pdf")) {
    stop("Workflow compilation failed; inspect the figure-workflow cache.")
  }
  target <- file.path(rep_dir(output_dir), "fig1_workflow.pdf")
  if (!file.copy("fig1_workflow.pdf", target, overwrite = TRUE)) stop("Cannot save Figure 1.")
  invisible(target)
}

rep_make_figures <- function(figures = NULL, output_dir = rep_output("figures"),
                             saved_dir = rep_path("data", "saved"), png = FALSE,
                             report_dir = NULL, posterior_dir = NULL) {
  available <- c("fig1", paste0("fig", 2:8), paste0("figS", 1:5))
  if (is.null(figures)) figures <- available
  if (anyNA(figures) || anyDuplicated(figures) || any(!figures %in% available)) {
    stop("Choose figure keys: ", paste(available, collapse = ", "))
  }
  if (xor(is.null(report_dir), is.null(posterior_dir))) {
    stop("Supply both --report-dir and --posterior-dir for fresh figures.", call. = FALSE)
  }
  fresh <- !is.null(report_dir)
  output_dir <- rep_absolute_path(output_dir)
  saved_dir <- rep_absolute_path(saved_dir)
  if (fresh && saved_dir != rep_absolute_path(rep_path("data", "saved"))) {
    stop("--saved-dir is for archived figures; fresh runs use the public context keys.", call. = FALSE)
  }
  requested <- setdiff(figures, "fig1")
  map_ids <- c("fig2", "fig4", "fig8", "figS1", "figS2", "figS5")
  context <- NULL
  if (fresh) {
    # Validate every fresh scientific input before the first PDF, even Fig1.
    context <- rep_figure_data(output_dir, saved_dir,
      maps = any(requested %in% map_ids),
      png = png, report_dir = report_dir, posterior_dir = posterior_dir
    )
    rep_figure_preflight(context, requested)
  }
  rep_dir(output_dir)
  if ("fig1" %in% figures) rep_figure_workflow(output_dir)
  if (length(requested)) {
    if (is.null(context)) {
      context <- rep_figure_data(output_dir, saved_dir,
        maps = any(requested %in% map_ids), png = png
      )
    }
    for (id in requested) {
      plot <- get(paste0("rep_plot_", id), mode = "function")
      plot(context, output_dir = output_dir, png = png)
    }
  }
  invisible(list.files(output_dir, pattern = "\\.pdf$", full.names = TRUE))
}
