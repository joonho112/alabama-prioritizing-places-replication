# JoonHo Lee (jlee296@ua.edu)
# Reproduce the eight printed tables and index their full machine-readable data.
# Sources: small saved results, authored policy summaries, and compact actual
# hyperparameter arrays. Optional directories replace them with new run results.
# No GIS package, solver, or model fitting is needed for the table build.

# ---- Small source tables and optional new-run overlays -----------------------

rep_table_read <- function(name, location = "saved") {
  rep_read_csv(rep_path("data", location, name))
}

rep_table_number <- function(x) suppressWarnings(as.numeric(x))
rep_table_true <- function(x) !is.na(x) & tolower(as.character(x)) == "true"

rep_table_find <- function(directory, candidates) {
  files <- file.path(directory, candidates)
  found <- files[file.exists(files)]
  if (!length(found)) stop("Required new-run file is missing: ", candidates[1L])
  found[1L]
}

rep_table_choose <- function(x, candidates) {
  answer <- rep(NA_real_, nrow(x))
  for (name in candidates[candidates %in% names(x)]) {
    value <- rep_table_number(x[[name]])
    take <- !is.finite(answer) & is.finite(value)
    answer[take] <- value[take]
  }
  answer
}

rep_table_sources <- function(report_dir = NULL, posterior_dir = NULL) {
  source <- list(county = rep_table_read("county-results.csv"),
    tract = rep_table_read("tract-results.csv"), metrics = rep_table_read("report-metrics.csv"),
    sensitivity = rep_table_read("sensitivity.csv"), capacity = rep_table_read("capacity-status.csv"))
  if (!is.null(posterior_dir)) {
    for (universe in c("county", "tract")) {
      model <- if (universe == "county") "M1" else "PV-TM1"
      filename <- paste0(model, "-A-summary.csv")
      newer <- rep_read_csv(rep_table_find(posterior_dir, c(filename, file.path("posterior", filename))))
      key <- if (universe == "county") "county_id" else "tract_id"
      expected <- rep_table_read(paste0(universe, "-observations.csv"), "analysis")
      if (anyDuplicated(newer$entity_id) ||
          !setequal(newer$entity_id, expected$entity_id) ||
          !identical(newer$county_id, expected$county_id[match(newer$entity_id, expected$entity_id)])) {
        stop("New posterior summaries must contain the complete ", universe, " population with correct county keys.")
      }
      fields <- grep("_(posterior_|raw_|shrinkage_)", names(source[[universe]]), value = TRUE)
      if (!all(fields %in% names(newer))) stop("New posterior summary is missing required statistics.")
      numeric_fields <- setdiff(fields, grep("_fit_id$", fields, value = TRUE))
      if (any(!is.finite(as.matrix(data.frame(lapply(newer[numeric_fields], rep_table_number)))))) {
        stop("New posterior summaries contain missing or invalid modeled statistics.")
      }
      index <- match(source[[universe]][[key]], newer$entity_id)
      # Replace every raw/posterior field together. Eight uncovered tract keys
      # remain explicit NA rows, rather than inheriting archived estimates.
      for (name in fields) source[[universe]][[name]] <- newer[[name]][index]
    }
  }
  if (!is.null(report_dir)) {
    source$metrics <- rep_read_csv(rep_table_find(report_dir, "report-metrics.csv"))
    source$sensitivity <- rep_read_csv(rep_table_find(report_dir, "sensitivity.csv"))
    source$capacity <- rep_read_csv(rep_table_find(report_dir, "capacity-status.csv"))
    cases <- rep_read_json(rep_table_find(report_dir, "review-reports.json"), FALSE)$cases
    for (universe in c("county", "tract")) {
      center <- cases[[paste0(universe, "-center")]]
      if (is.null(center)) stop("New-run report data need both central reports.")
      key <- if (universe == "county") "county_id" else "tract_id"
      expected <- if (universe == "county") {
        source$county$county_id[rep_table_true(source$county$county_government_candidate)]
      } else rep_table_read("pilot-tract-keys.csv", "analysis")$entity_id
      if (!setequal(names(center$reports), c("B0", "B1", "B2", "B3", "P"))) {
        stop("New central reports must contain all five methods.")
      }
      for (method in c("B0", "B1", "B2", "B3", "P")) {
        grade <- unlist(center$reports[[method]]$grade, use.names = TRUE)
        if (anyDuplicated(names(grade)) || !setequal(names(grade), expected) ||
            any(!is.finite(grade)) || any(grade != floor(grade)) || any(grade < 1 | grade > 5)) {
          stop("New central report has missing entities or invalid bands.")
        }
        source[[universe]][[paste0("grade_", method)]] <- grade[match(source[[universe]][[key]], names(grade))]
      }
    }
  }
  source
}

rep_table_hyperparameters <- function(fit_dir = NULL) {
  variables <- c("alpha", "sigma_county", "sigma_tract")
  rows <- list()
  if (is.null(fit_dir)) {
    packet <- readRDS(rep_path("data", "saved", "hyperparameter-draws.rds"))
    if (packet$model_id != "PV-TM1" || packet$pool != "A") stop("Wrong population-parameter draw source.")
    for (criterion in c("income", "incarceration")) {
      block <- packet$criteria[[criterion]]
      draws <- block$draws
      if (!identical(class(draws), "array") || !identical(dim(draws), c(4000L, 4L, 3L)) ||
          !identical(dimnames(draws), list(as.character(1001:5000), as.character(1:4), variables)) ||
          any(!is.finite(draws))) stop("Hyperparameter draw coordinates changed.")
      for (variable in variables) {
        values <- as.numeric(draws[, , variable])
        interval <- stats::quantile(values, c(.05, .95), names = FALSE, type = 7L)
        rows[[length(rows) + 1L]] <- data.frame(criterion = criterion, variable = variable,
          mean = mean(values), q5 = interval[1L], q95 = interval[2L], fit_id = block$fit_id)
      }
    }
  } else {
    # A new fit already computes these summaries directly from its full draws.
    # Do not substitute the paper's archived parameters for a new computation.
    for (criterion in c("income", "incarceration")) {
      fit_id <- paste0("PV-TM1-A-", criterion)
      summary_path <- rep_table_find(fit_dir, c(file.path(fit_id, "summary.csv"), file.path("fits", fit_id, "summary.csv")))
      folder <- dirname(summary_path)
      metadata <- rep_read_json(file.path(folder, "metadata.json"))
      if (!identical(metadata$fit_id, fit_id) || is.null(metadata$output_sha256)) {
        stop("New population parameters need matching fit metadata and output hashes.")
      }
      rep_assert_sha256(summary_path, metadata$output_sha256[["summary.csv"]])
      diagnostics_path <- file.path(folder, "diagnostics.json")
      rep_assert_sha256(diagnostics_path, metadata$output_sha256[["diagnostics.json"]])
      diagnostics <- rep_read_json(diagnostics_path)
      if (!isTRUE(diagnostics$hard_hmc_passed)) stop("New population parameters need an accepted HMC fit.")
      summary <- rep_read_csv(summary_path)
      name <- sub("\\[1\\]$", "", summary$variable)
      for (variable in variables) {
        row <- summary[name == variable, , drop = FALSE]
        if (nrow(row) != 1L) stop("Missing new-fit population parameter.")
        rows[[length(rows) + 1L]] <- data.frame(criterion = criterion, variable = variable,
          mean = rep_table_number(row$mean), q5 = rep_table_number(row$q5),
          q95 = rep_table_number(row$q95), fit_id = fit_id)
      }
    }
  }
  answer <- do.call(rbind, rows)
  rownames(answer) <- NULL
  if (any(!is.finite(as.matrix(answer[c("mean", "q5", "q95")])))) stop("Invalid population-parameter summary.")
  answer
}

# ---- Display formatting leaves the CSV values unrounded ----------------------

rep_table_decimal <- function(x, digits = 2L, signed = FALSE) {
  value <- rep_table_number(x)
  answer <- formatC(value, format = "f", digits = digits, big.mark = ",")
  answer[!is.finite(value)] <- "--"
  if (signed) answer[is.finite(value) & value > 0] <- paste0("+", answer[is.finite(value) & value > 0])
  answer
}

rep_table_integer <- function(x) {
  value <- rep_table_number(x)
  answer <- formatC(value, format = "f", digits = 0L, big.mark = ",")
  answer[!is.finite(value)] <- "--"
  answer
}

rep_table_mcse <- function(x) {
  value <- rep_table_number(x)
  answer <- trimws(formatC(value, format = "g", digits = 2L))
  answer[!is.finite(value)] <- "--"
  answer
}

rep_table_interval <- function(mean, lower, upper, digits = 2L) {
  paste0(rep_table_decimal(mean, digits), " [", rep_table_decimal(lower, digits),
         ", ", rep_table_decimal(upper, digits), "]")
}

rep_table_tract_label <- function(ids) {
  sub("\\.?0+$", "", sprintf("%.2f", as.numeric(substr(ids, 6L, 11L)) / 100))
}

rep_table_panel <- function(data, title = NULL, row_classes = NULL, grouped_header = FALSE) {
  list(data = data, title = title, row_classes = row_classes, grouped_header = grouped_header)
}

rep_table_record <- function(stem, number, label, title, csv, panels, note) {
  list(stem = stem, number = number, label = label, title = title, csv = csv,
       panels = panels, note = note)
}

# ---- Table data and the original printed structures ---------------------------

rep_paper_tables <- function(report_dir = NULL, posterior_dir = NULL, fit_dir = NULL) {
  x <- rep_table_sources(report_dir, posterior_dir)
  lookup <- rep_read_csv(rep_path("data", "metadata", "county-names.csv"))
  county_names <- stats::setNames(sub(" County$", "", lookup$county_name), lookup$county_id)
  tables <- list()

  # Table 1: retain the geographic keys in CSV; show the three readable columns.
  first <- x$tract[rep_table_true(x$tract$in_pooled90) &
                     !is.na(x$tract$grade_P) & rep_table_number(x$tract$grade_P) == 1, ]
  first <- first[order(first$tract_id), ]
  first$county <- unname(county_names[first$county_id])
  first$tract_label <- rep_table_tract_label(first$tract_id)
  fields <- c("tract_id", "county_id", "county", "tract_label", "income_posterior_mean_points",
    "income_posterior_q025_points", "income_posterior_q975_points",
    "incarceration_posterior_mean_points", "incarceration_posterior_q025_points", "incarceration_posterior_q975_points")
  first <- first[fields]
  panels <- lapply(unique(first$county), function(name) {
    z <- first[first$county == name, ]
    rep_table_panel(data.frame(Tract = z$tract_label,
      `Income rank` = rep_table_interval(z$income_posterior_mean_points, z$income_posterior_q025_points, z$income_posterior_q975_points),
      Incarceration = rep_table_interval(z$incarceration_posterior_mean_points, z$incarceration_posterior_q025_points, z$incarceration_posterior_q975_points), check.names = FALSE), paste(name, "County"))
  })
  tables[["firstband"]] <- rep_table_record("first-band-tracts", "1", "tab:firstband",
    "The first band of the common tract report", first, panels,
    "Posterior means with separate-criterion 95% intervals. Income rank is in percentile points and incarceration in percentage points. The historical cohort, pooled race/gender, and parental-income condition follow the paper. Full identifiers, published estimates, and standard errors remain in the detailed CSV data. Boundary bands are not split at review capacities.")

  # Table 2: the authored evidence/action structure is part of the paper.
  readiness <- rep_table_read("readiness-summary.csv")
  display <- readiness
  names(display) <- c("Element", "Evidence from the review bands", "Further evidence and action")
  tables[["readiness"]] <- rep_table_record("project-readiness-summary", "2", "tab:readiness",
    "Evidence from the review bands and next steps in project development", readiness,
    list(rep_table_panel(display)), "Review bands initiate inquiry. Applicant status, project eligibility, service areas, costs, beneficiaries, and completed official scores require further evidence.")

  # Table 3: exact applicant matching; retain the unmatched roster row separately.
  county <- x$county
  candidate <- rep_table_true(county$county_government_candidate)
  rows <- lapply(sort(unique(rep_table_number(county$grade_P[candidate]))), function(band) {
    z <- county[candidate & !is.na(county$grade_P) & rep_table_number(county$grade_P) == band, ]
    apps <- rep_table_number(z$matched_received_application_count); apps[is.na(apps)] <- 0
    fund <- rep_table_number(z$matched_county_fund_application_count); fund[is.na(fund)] <- 0
    data.frame(review_band = paste("Band", band), counties = nrow(z),
      all_routes_counties_applying = sum(apps > 0), all_routes_applications = sum(apps),
      county_fund_counties_applying = sum(fund > 0), county_fund_applications = sum(fund))
  })
  outside <- county[!candidate, ]
  outside_apps <- rep_table_number(outside$matched_received_application_count); outside_apps[is.na(outside_apps)] <- 0
  rows[[length(rows) + 1L]] <- data.frame(review_band = "Outside route", counties = nrow(outside),
    all_routes_counties_applying = sum(outside_apps > 0), all_routes_applications = sum(outside_apps),
    county_fund_counties_applying = NA_real_, county_fund_applications = NA_real_)
  band_apps <- do.call(rbind, rows)
  band_apps <- rbind(band_apps, data.frame(review_band = "All counties", counties = sum(band_apps$counties),
    all_routes_counties_applying = sum(band_apps$all_routes_counties_applying),
    all_routes_applications = sum(band_apps$all_routes_applications),
    county_fund_counties_applying = sum(band_apps$county_fund_counties_applying, na.rm = TRUE),
    county_fund_applications = sum(band_apps$county_fund_applications, na.rm = TRUE)))
  stopifnot(tail(band_apps$counties, 1L) == 67, tail(band_apps$all_routes_applications, 1L) == 71,
            tail(band_apps$county_fund_applications, 1L) == 12)
  display <- data.frame(`Review band` = band_apps$review_band,
    Counties = rep_table_integer(band_apps$counties),
    `Counties applying (all routes)` = rep_table_integer(band_apps$all_routes_counties_applying),
    `Applications (all routes)` = rep_table_integer(band_apps$all_routes_applications),
    `Counties applying (County Fund)` = rep_table_integer(band_apps$county_fund_counties_applying),
    `Applications (County Fund)` = rep_table_integer(band_apps$county_fund_applications), check.names = FALSE)
  tables[["bands-applications"]] <- rep_table_record("county-bands-applications", "3", "tab:bandsapps",
    "County review bands and applications received by the 22 July 2026 deadline", band_apps,
    list(rep_table_panel(display, grouped_header = TRUE, row_classes = c(rep("", nrow(display) - 1L), "total"))),
    "Bands use the central settings. Applications are located by the applicant's recorded county, including municipal applicants. One unmatched Talledega row is excluded from the county counts. Jefferson, Mobile, and Shelby are outside the County Fund route. Requested dollars are application requests, not awards; 12 County Fund applications requested $5,649,796.90 against a $2,793,000 program allocation.")

  # Table B.1: recompute the six full-precision values from actual small arrays.
  hyper <- rep_table_hyperparameters(fit_dir)
  parameters <- c("alpha", "sigma_county", "sigma_tract")
  labels <- c("Statewide level (alpha)", "Between-county spread (sigma_c)", "Within-county tract spread (sigma_t)")
  display <- data.frame(Parameter = labels, check.names = FALSE)
  for (criterion in c("income", "incarceration")) {
    z <- hyper[hyper$criterion == criterion, ]; z <- z[match(parameters, z$variable), ]
    display[[if (criterion == "income") "Income" else "Incarceration"]] <- rep_table_interval(z$mean, z$q5, z$q95, 3L)
  }
  tables[["hyperparameters"]] <- rep_table_record("hyperparameters", "B.1", "tab:B-hyper",
    "Population-level parameters of the tract model PV-TM1", hyper,
    list(rep_table_panel(display)), "Posterior means and 90% intervals use 16,000 selection-stream draws per criterion and are on the logit scale. The spreads are standard deviations of latent logits. Entity-level outcome tables use natural points and 95% intervals instead.")

  # Table D.1: two candidate universes, with the same five reporting rules.
  metrics <- x$metrics
  if ("case_id" %in% names(metrics)) metrics <- metrics[metrics$case_id %in% c("county-center", "tract-center"), ]
  metrics <- metrics[order(match(metrics$universe, c("county", "tract")), match(metrics$method, c("B0", "B1", "B2", "B3", "P"))), ]
  panels <- lapply(c("county", "tract"), function(universe) {
    z <- metrics[metrics$universe == universe, ]
    excess <- paste0(rep_table_decimal(z$B_difference_vs_P, 4L), " (", rep_table_mcse(z$B_paired_mcse_vs_P), ")")
    excess[z$method == "P"] <- "--"
    rep_table_panel(data.frame(Rule = z$method, `Disclosed pairs` = rep_table_integer(z$N),
      Bands = rep_table_integer(z$Goccupied),
      `Loss on B (MCSE)` = paste0(rep_table_decimal(z$B_Rtau, 4L), " (", rep_table_mcse(z$B_Rtau_mcse), ")"),
      `Excess loss vs P (paired MCSE)` = excess, check.names = FALSE),
      if (universe == "county") "Counties: 64 candidates, 2,016 pairs" else "Tracts: 90 candidates, 4,005 pairs")
  })
  stopifnot(nrow(metrics) == 10L)
  tables[["comparators"]] <- rep_table_record("reporting-rule-comparisons", "D.1", "tab:D-comparators",
    "Reporting rules under equal criterion weights, withholding cost .20, and at most five bands", metrics, panels,
    "B0 withholds all comparisons. B1 uses published-estimate rank quintiles, B2 posterior-mean rank quintiles, B3 optimal cuts of posterior expected ranks, and P the unrestricted common report. Stream B evaluates fixed reports. Loss includes wrong disclosed directions and withheld comparisons; it is not an error probability. In the archived central results, B3 and P have the same pair relations and their paired difference is exactly zero. Undefined MCSEs remain missing.")

  # Tables E.1/E.2: separate policy objectives from common-reference model checks.
  sensitivity <- x$sensitivity
  kind <- ifelse(sensitivity$scenario_kind == "posterior", "model", ifelse(sensitivity$scenario_kind == "exploratory", "direction", sensitivity$scenario_kind))
  policy <- sensitivity[kind == "policy", ]
  setting <- if ("policy_id" %in% names(policy)) policy$policy_id else policy$scenario_id
  policy$setting <- setting
  order_ids <- c("center", "PV-w25", "PV-w75", "PV-t10", "PV-t30")
  policy <- policy[order(match(policy$universe, c("county", "tract")), match(policy$setting, order_ids)), ]
  setting_labels <- c(center = "Center", `PV-w25` = "Income weight .25", `PV-w75` = "Income weight .75", `PV-t10` = "Cost .10", `PV-t30` = "Cost .30")
  policy$setting_label <- unname(setting_labels[policy$setting])
  policy$relations_changed <- rep_table_choose(policy, c("P_vs_centerP_relation_changes", "relation_changes_vs_center"))
  policy$newly_disclosed <- rep_table_choose(policy, "P_vs_centerP_tie_to_disclosure")
  policy$newly_withheld <- rep_table_choose(policy, "P_vs_centerP_disclosure_to_tie")
  policy$reversed <- rep_table_choose(policy, "P_vs_centerP_direction_reversals")
  for (j in 1:2) {
    policy[[paste0("core", j)]] <- rep_table_choose(policy, paste0("capacity", j, "_core_n"))
    policy[[paste0("boundary", j)]] <- rep_table_choose(policy, paste0("capacity", j, "_boundary_n"))
  }
  if (!is.null(report_dir)) {
    for (i in seq_len(nrow(policy))) {
      K <- if (policy$universe[i] == "county") c(3L, 5L) else c(5L, 10L)
      for (j in 1:2) {
        z <- x$capacity[x$capacity$universe == policy$universe[i] & x$capacity$case_id %in% c(policy$setting[i], policy$case_id[i]) &
                          x$capacity$method == "P" & rep_table_number(x$capacity$review_capacity_K) == K[j], ]
        policy[[paste0("core", j)]][i] <- sum(z$capacity_status == "core")
        policy[[paste0("boundary", j)]][i] <- sum(z$capacity_status == "boundary")
      }
    }
  }
  panels <- list()
  for (universe in c("county", "tract")) {
    z <- policy[policy$universe == universe, ]
    panels[[length(panels) + 1L]] <- rep_table_panel(data.frame(Setting = z$setting_label,
      `Disclosed pairs` = rep_table_integer(z$N), `Share (%)` = rep_table_decimal(100 * rep_table_number(z$disclosure_fraction), 1L),
      Bands = rep_table_integer(z$Goccupied), `Loss on B` = rep_table_decimal(z$B_Rtau, 4L),
      `Relations changed` = ifelse(z$setting == "center", "--", rep_table_integer(z$relations_changed)), check.names = FALSE),
      paste("A. Disclosure and loss:", if (universe == "county") "64 counties" else "90 tracts"))
  }
  for (universe in c("county", "tract")) {
    z <- policy[policy$universe == universe, ]
    panels[[length(panels) + 1L]] <- rep_table_panel(data.frame(Setting = z$setting_label,
      `Newly disclosed` = ifelse(z$setting == "center", "--", rep_table_integer(z$newly_disclosed)),
      `Newly withheld` = ifelse(z$setting == "center", "--", rep_table_integer(z$newly_withheld)),
      Reversed = ifelse(z$setting == "center", "--", rep_table_integer(z$reversed)),
      `Core / boundary K1` = paste(rep_table_integer(z$core1), rep_table_integer(z$boundary1), sep = " / "),
      `Core / boundary K2` = paste(rep_table_integer(z$core2), rep_table_integer(z$boundary2), sep = " / "), check.names = FALSE),
      paste("B. Changes and review scope:", if (universe == "county") "counties K1=3, K2=5" else "tracts K1=5, K2=10"))
  }
  stopifnot(nrow(policy) == 10L)
  tables[["policy"]] <- rep_table_record("policy-sensitivity", "E.1", "tab:E-policy",
    "Prespecified policy settings, reported comparisons, and review scope", policy, panels,
    "The center uses income weight .5 and withholding cost .20. Incarceration receives the remaining weight. Each alternative changes one setting. Every loss uses that row's own objective, so losses across different objectives are not common-reference risk differences. Core and boundary counts retain complete bands; five occupied bands reach the cap.")

  models <- sensitivity[kind %in% c("model", "direction"), ]
  model_id <- if ("model_id" %in% names(models)) models$model_id else models$selection_model
  models$alternative <- ifelse(models$scenario_kind %in% c("direction", "exploratory"), "Direction c=.5", sub("^PV-", "", model_id))
  models$relations_changed <- rep_table_choose(models, c("relation_changes_vs_center", "P_vs_main_center_relation_changes", "P_vs_centerP_relation_changes"))
  models$excess_loss <- rep_table_choose(models, c("P_minus_main_center_P_reference_B", "B_delta_vs_center_same_objective"))
  models$paired_mcse <- rep_table_choose(models, c("P_minus_main_center_P_reference_B_paired_mcse", "B_delta_paired_mcse"))
  for (j in 1:2) {
    models[[paste0("core", j)]] <- rep_table_choose(models, c(paste0("core", j, "_n"), paste0("capacity", j, "_core_n")))
    models[[paste0("boundary", j)]] <- rep_table_choose(models, c(paste0("boundary", j, "_n"), paste0("capacity", j, "_boundary_n")))
  }
  if (!is.null(report_dir)) {
    for (i in seq_len(nrow(models))) for (j in 1:2) {
      K <- c(5L, 10L)[j]
      z <- x$capacity[x$capacity$universe == "tract" & x$capacity$case_id %in% c(models$scenario_id[i], models$case_id[i]) &
                        x$capacity$method == "P" & rep_table_number(x$capacity$review_capacity_K) == K, ]
      models[[paste0("core", j)]][i] <- sum(z$capacity_status == "core")
      models[[paste0("boundary", j)]][i] <- sum(z$capacity_status == "boundary")
    }
  }
  stopifnot(nrow(models) == 4L)
  pA <- rep_table_panel(data.frame(Alternative = models$alternative,
    `Disclosed pairs` = rep_table_integer(models$N), `Share (%)` = rep_table_decimal(100 * rep_table_number(models$disclosure_fraction), 1L),
    Bands = rep_table_integer(models$Goccupied), `Relations changed` = rep_table_integer(models$relations_changed), check.names = FALSE), "A. Selected reports")
  pB <- rep_table_panel(data.frame(Alternative = models$alternative,
    `Excess loss` = rep_table_decimal(models$excess_loss, 4L, signed = TRUE), `Paired MCSE` = rep_table_mcse(models$paired_mcse),
    `Core / boundary K=5` = paste(rep_table_integer(models$core1), rep_table_integer(models$boundary1), sep = " / "),
    `Core / boundary K=10` = paste(rep_table_integer(models$core2), rep_table_integer(models$boundary2), sep = " / "), check.names = FALSE), "B. Common-reference loss and review scope")
  tables[["model"]] <- rep_table_record("model-direction-sensitivity", "E.2", "tab:E-model",
    "Model alternatives and the exploratory direction constraint", models, list(pA, pB),
    "TS1 omits county effects; TS2 multiplies published SEs by 1.25; TS3 uses within-county measurement-error correlation .25. Each report is selected from its own A and compared with the center on the same main-model B. The direction screen disallows finite-A weighted wrong-direction probabilities above .5. It is nonbinding in the archived results and is excluded from the eight-case stability frequencies.")

  # Table F.1: display the 130 subtotal and 200 cap without summing them twice.
  rating <- rep_table_read("rating-summary.csv")
  points <- rep_table_number(rating$points)
  stopifnot(sum(points[1:4]) == 130, points[5L] == 130, points[6L] + points[7L] + points[5L] == 200, points[8L] == 200)
  display <- rating[c("component", "points", "evidence_assessed")]
  names(display) <- c("Component", "Points", "Evidence assessed")
  classes <- ifelse(rep_table_true(rating$is_overall_cap), "total", ifelse(rep_table_true(rating$is_subtotal), "subtotal", ""))
  tables[["rating"]] <- rep_table_record("rating-instrument", "F.1", "tab:F-rating",
    "Program year 2026 rating form for the Small City, Large City, and County funds", rating,
    list(rep_table_panel(display, row_classes = classes)),
    "The 45-point Project Development category includes 10 points for maps. Nature of Benefits is the 130-point subtotal, not an additional category. The overall cap is 200. Bonus and adjustment provisions are outside this summary; completed project scores are unobserved. LMI means low and moderate income.")
  tables
}

# ---- Readable standalone HTML and reusable LaTeX fragments --------------------

rep_table_html_escape <- function(x) {
  x <- as.character(x); x[is.na(x)] <- "--"
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

rep_table_tex_escape <- function(x) {
  # Escape each original character once, so replacement braces are not re-escaped.
  map <- c("\\" = "\\textbackslash{}", "&" = "\\&", "%" = "\\%", "$" = "\\$",
    "#" = "\\#", "_" = "\\_", "{" = "\\{", "}" = "\\}",
    "~" = "\\textasciitilde{}", "^" = "\\textasciicircum{}")
  vapply(as.character(x), function(value) {
    if (is.na(value)) return("--")
    paste(vapply(strsplit(value, "", fixed = TRUE)[[1L]], function(character) {
      if (character %in% names(map)) map[[character]] else character
    }, character(1)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

rep_table_html <- function(table) {
  title <- paste0("Table ", table$number, ". ", table$title)
  body <- c("<!doctype html>", '<html lang="en"><head><meta charset="utf-8">',
    paste0("<title>", rep_table_html_escape(title), "</title>"),
    '<meta name="author" content="JoonHo Lee (jlee296@ua.edu)">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<style>body{font:16px/1.55 system-ui,-apple-system,sans-serif;color:#243440;background:white;margin:0}main{max-width:1120px;padding:2.2rem 1.4rem;margin:auto}h1{font-size:1.45rem;line-height:1.35;margin:0 0 1.4rem}h2{font-size:1.05rem;margin:1.8rem 0 .65rem}.table-wrap{overflow-x:auto;margin-bottom:1rem}table{border-collapse:collapse;width:100%;font-size:1rem}thead{background:#f1f5f7}th,td{padding:.65rem .75rem;border-bottom:1px solid #dce3e7;text-align:left;vertical-align:top}th{font-weight:650}.subtotal td{border-top:2px solid #a6b6c0;font-weight:650}.total td{border-top:2px solid #6b8494;font-weight:700;background:#f8fafb}.note{font-size:.94rem;max-width:100ch;margin-top:1.4rem}footer{font-size:.9rem;color:#526471;margin-top:2rem}@media print{main{max-width:none;padding:0}thead{display:table-header-group}tr{break-inside:avoid}}</style></head><body><main>',
    paste0("<h1>", rep_table_html_escape(title), "</h1>"))
  for (panel in table$panels) {
    data <- panel$data
    if (!is.null(panel$title)) body <- c(body, paste0("<h2>", rep_table_html_escape(panel$title), "</h2>"))
    body <- c(body, '<div class="table-wrap"><table><thead>')
    if (panel$grouped_header) {
      body <- c(body, '<tr><th rowspan="2">Review band</th><th rowspan="2">Counties</th><th colspan="2">All routes</th><th colspan="2">County Fund</th></tr>', '<tr><th>Counties applying</th><th>Applications</th><th>Counties applying</th><th>Applications</th></tr>')
    } else body <- c(body, paste0("<tr>", paste0("<th>", rep_table_html_escape(names(data)), "</th>", collapse = ""), "</tr>"))
    body <- c(body, "</thead><tbody>")
    for (i in seq_len(nrow(data))) {
      row_class <- if (is.null(panel$row_classes)) "" else panel$row_classes[i]
      body <- c(body, paste0('<tr class="', row_class, '">',
        paste0("<td>", rep_table_html_escape(unlist(data[i, ], use.names = FALSE)), "</td>", collapse = ""), "</tr>"))
    }
    body <- c(body, "</tbody></table></div>")
  }
  c(body, paste0('<p class="note"><strong>Note.</strong> ', rep_table_html_escape(table$note), "</p>"),
    '<footer>JoonHo Lee (jlee296@ua.edu). <a href="https://doi.org/10.5281/zenodo.23966864">Published preprint</a>.</footer>',
    "</main></body></html>")
}

rep_table_tex <- function(table) {
  row_end <- paste0(" ", strrep("\\", 2L))
  body <- c("% JoonHo Lee (jlee296@ua.edu)", "% Requires booktabs, tabularx, and array.")
  # E.1 has two distinct questions and four panels. Continue the same table on
  # a second float instead of making its text smaller or exceeding page height.
  blocks <- if (table$number == "E.1") list(table$panels[1:2], table$panels[3:4]) else list(table$panels)
  for (block in seq_along(blocks)) {
    body <- c(body, "\\begin{table}[htbp]")
    if (block > 1L) body <- c(body, "\\addtocounter{table}{-1}")
    caption <- paste0(table$title, if (block > 1L) " (continued)" else "")
    body <- c(body, paste0("\\caption{", rep_table_tex_escape(caption), "}"))
    if (block == 1L) body <- c(body, paste0("\\label{", table$label, "}"))
    body <- c(body, "\\centering\\small", "\\renewcommand{\\arraystretch}{1.2}")
    for (panel in blocks[[block]]) {
      if (!is.null(panel$title)) body <- c(body, paste0("\\textit{", rep_table_tex_escape(panel$title), "}\\par\\smallskip"))
      data <- panel$data
      columns <- ncol(data)
      specification <- paste0("@{}>{\\raggedright\\arraybackslash}p{.25\\linewidth}",
        paste(rep(">{\\raggedright\\arraybackslash}X", columns - 1L), collapse = ""), "@{}")
      body <- c(body, paste0("\\begin{tabularx}{\\linewidth}{", specification, "}"), "\\toprule")
      if (panel$grouped_header) {
        body <- c(body, paste0("Review band & Counties & \\multicolumn{2}{c}{All routes} & \\multicolumn{2}{c}{County Fund}", row_end),
          paste0(" & & Counties applying & Applications & Counties applying & Applications", row_end))
      } else {
        body <- c(body, paste0(paste(rep_table_tex_escape(names(data)), collapse = " & "), row_end))
      }
      body <- c(body, "\\midrule")
      for (i in seq_len(nrow(data))) {
        if (!is.null(panel$row_classes) && nzchar(panel$row_classes[i])) body <- c(body, "\\midrule")
        body <- c(body, paste0(paste(rep_table_tex_escape(unlist(data[i, ], use.names = FALSE)), collapse = " & "), row_end),
          "\\addlinespace[0.35em]")
      }
      body <- c(body, "\\bottomrule", "\\end{tabularx}", "\\par\\medskip")
    }
    body <- c(body, "\\begin{minipage}{\\linewidth}\\small", paste0("\\textit{Note.} ", rep_table_tex_escape(table$note)),
      "\\end{minipage}", "\\end{table}")
  }
  body
}

rep_table_data_index <- function() {
  definitions <- list(
    c("County results and capacity", "data/saved/county-review-results.csv", "county_id", "67 counties; natural points; review capacities 3 and 5"),
    c("Statewide tract results", "data/saved/tract-results.csv", "tract_id", "1,181 tract keys; 1,173 modeled and eight missing; natural points"),
    c("Pilot tract results", "data/saved/pilot90-tract-results.csv", "tract_id", "90 pilot tracts; published estimates and SEs; posterior 95% intervals"),
    c("First-band detail", "data/saved/first-band-tracts.csv", "tract_id", "13 tracts; full identifiers, published estimates, and posterior outcomes"),
    c("Relevant production attempts", "data/saved/hmc-production-attempts.csv", "fit_id", "20 attempts; 14 selected; source identifiers and diagnostic acceptance"),
    c("Parameter diagnostics", "data/saved/hmc-parameter-diagnostics.csv", "fit_id + variable", "All monitored parameters from relevant production attempts"),
    c("Chain diagnostics", "data/saved/hmc-chain-diagnostics.csv", "fit_id + chain row", "80 chain rows; HMC diagnostic values"),
    c("Full hyperparameter precision", "data/saved/hyperparameters.csv", "criterion + variable", "Six reference rows; means and 90% intervals; printed values are recomputed from compact draws"),
    c("Reporting-rule detail", "data/saved/report-metrics.csv", "universe + method", "10 central reporting rules; A/B losses, MCSEs, bounds, and disclosure"),
    c("Sensitivity detail", "data/saved/sensitivity.csv", "case_id", "14 cases; policy, model, and direction scopes kept distinct"),
    c("Application facts", "data/analysis/applications.csv", "application_id", "72 applications; exact cents and source sheet/row; requested amounts"),
    c("Applications by fund", "data/saved/applications-by-fund.csv", "fund_section", "Five funds and total; counts, exact dollars, and government types"),
    c("Readiness detail", "data/analysis/policy-readiness.csv", "item", "13 rows of evidence, action, and source locators"),
    c("Rating dictionary", "data/analysis/rating-instrument.csv", "rating_element", "130-point subtotal; maps contribute 10 of 45 Project Development points; overall cap 200"),
    c("Appendix C coefficient precision", "data/saved/pair-precision-summary.csv", "pool",
      "Main-tract A/B; 1,993 fixed pairs and 19,930 weighted policy-direction rows per pool; script 04b regenerates detail"))
  do.call(rbind, lapply(definitions, function(row) {
    x <- rep_read_csv(rep_path(row[2L]))
    data.frame(title = row[1L], path_from_compendium_root = row[2L], rows = nrow(x),
               key = row[3L], scope = row[4L], stringsAsFactors = FALSE)
  }))
}

rep_build_tables <- function(output_dir = NULL, report_dir = NULL,
                             posterior_dir = NULL, fit_dir = NULL) {
  if (is.null(output_dir)) output_dir <- rep_output("tables")
  output_dir <- rep_dir(output_dir)
  tables <- rep_paper_tables(report_dir, posterior_dir, fit_dir)
  for (table in tables) {
    rep_write_csv(table$csv, file.path(output_dir, paste0(table$stem, ".csv")))
    writeLines(rep_table_html(table), file.path(output_dir, paste0(table$stem, ".html")))
    writeLines(rep_table_tex(table), file.path(output_dir, paste0(table$stem, ".tex")))
    message("Wrote Table ", table$number, ": ", table$stem)
  }
  index <- rep_table_data_index()
  rep_write_csv(index, file.path(output_dir, "data-index.csv"))
  invisible(tables)
}
