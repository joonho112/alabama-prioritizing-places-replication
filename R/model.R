# JoonHo Lee (jlee296@ua.edu)
# Build criterion-specific measurement inputs and run the recorded Stan fits.
# Inputs: wide native-fraction observations and config/fit-settings.csv.
# Outputs: separate fit folders containing native arrays and numerical diagnostics.
# Sourcing this file defines functions only; fitting always requires an explicit call.

rep_fit_settings <- function(fit_id = NULL) {
  settings <- rep_read_csv(rep_path("config", "fit-settings.csv"))
  integers <- c("seed", "N", "C", "latent_structure", "chains", "iter_warmup",
                "iter_sampling", "thin", "parallel_chains", "threads_per_chain",
                "max_treedepth", "sig_figs")
  numbers <- c("rho", "se_multiplier", "alpha_loc", "alpha_scale",
               "sigma_county_scale", "sigma_tract_scale", "adapt_delta")
  required <- c("fit_id", "source_fit_id", "model_id", "universe", "pool",
                "criterion", "latent_branch", "save_warmup", integers, numbers)
  if (!all(required %in% names(settings)) || anyDuplicated(settings$fit_id)) {
    stop("Fit settings have missing fields or duplicated identifiers.")
  }
  for (column in integers) {
    value <- suppressWarnings(as.numeric(settings[[column]]))
    if (any(!is.finite(value) | value != floor(value))) stop("Invalid integer: ", column)
    settings[[column]] <- as.integer(value)
  }
  for (column in numbers) {
    value <- suppressWarnings(as.numeric(settings[[column]]))
    if (any(!is.finite(value))) stop("Invalid numeric setting: ", column)
    settings[[column]] <- value
  }
  warmup <- tolower(settings$save_warmup)
  if (any(!warmup %in% c("true", "false"))) stop("Invalid save_warmup flag.")
  settings$save_warmup <- warmup == "true"
  if (any(settings$chains != 4L | settings$parallel_chains > 2L |
          settings$threads_per_chain != 1L | settings$thin != 1L |
          settings$save_warmup | settings$seed <= 0L |
          !settings$latent_structure %in% 1:3 |
          !settings$criterion %in% c("income", "incarceration") |
          !settings$universe %in% c("county", "tract"))) {
    stop("Fit settings violate the recorded sampling or model contract.")
  }
  if (is.null(fit_id)) return(settings)
  found <- settings[settings$fit_id %in% fit_id, , drop = FALSE]
  if (nrow(found) != length(unique(fit_id))) stop("Unknown fit identifier.")
  found[match(fit_id, found$fit_id), , drop = FALSE]
}

rep_model_data <- function(observations, fit, validate_population = TRUE) {
  # validate_population=FALSE is reserved for explicit small-input checks.
  # Published production fits always use the complete source entity universe.
  if (is.data.frame(fit)) {
    if (nrow(fit) != 1L) stop("Choose one fit setting.")
    fit <- as.list(fit[1, , drop = FALSE])
  }
  if (is.character(observations) && length(observations) == 1L) {
    observations <- rep_read_csv(observations)
  }
  estimate_column <- paste0(fit$criterion, "_estimate_fraction")
  se_column <- paste0(fit$criterion, "_se_fraction")
  required <- c("entity_id", "county_id", estimate_column, se_column)
  if (!is.data.frame(observations) || !all(required %in% names(observations))) {
    stop("Observation data do not contain the required native fields.")
  }
  if (!is.character(observations$entity_id) || !is.character(observations$county_id)) {
    stop("Read geographic identifiers as text to preserve leading zeros.")
  }
  observations <- observations[order(observations$entity_id), , drop = FALSE]
  rownames(observations) <- NULL
  ids <- observations$entity_id
  counties <- observations$county_id
  pattern <- if (fit$universe == "county") "^01[0-9]{3}$" else "^01[0-9]{9}$"
  if (!length(ids) || anyNA(ids) || anyNA(counties) || anyDuplicated(ids) ||
      any(!grepl(pattern, ids)) || any(!grepl("^01[0-9]{3}$", counties)) ||
      any(substr(ids, 1L, 5L) != counties)) {
    stop("Invalid, duplicated, or mismatched Alabama entity/county identifiers.")
  }
  y <- suppressWarnings(as.numeric(observations[[estimate_column]]))
  published_se <- suppressWarnings(as.numeric(observations[[se_column]]))
  if (any(!is.finite(y)) || any(!is.finite(published_se) | published_se <= 0)) {
    stop("Published estimates must be finite and standard errors positive.")
  }
  levels <- sort(unique(counties))
  county <- match(counties, levels)
  sizes <- tabulate(county, nbins = length(levels))
  starts <- cumsum(c(1L, head(sizes, -1L)))
  if (!identical(as.integer(county), rep(seq_along(levels), sizes))) {
    stop("Source entities must form contiguous county blocks.")
  }
  if (validate_population &&
      (length(ids) != fit$N || length(levels) != fit$C)) {
    stop("Production input differs from the recorded full entity universe.")
  }
  data <- list(N = length(ids), C = length(levels), county = as.integer(county),
               group_start = as.integer(starts), group_size = as.integer(sizes),
               y = y, se = published_se * fit$se_multiplier,
               latent_structure = as.integer(fit$latent_structure),
               alpha_loc = fit$alpha_loc, alpha_scale = fit$alpha_scale,
               sigma_county_scale = fit$sigma_county_scale,
               sigma_tract_scale = fit$sigma_tract_scale, rho = fit$rho)
  list(data = data, entity_ids = ids, county_ids = counties,
       county_levels = levels, published_se = published_se,
       criterion = fit$criterion, source_row_count = nrow(observations),
       technical_subset = !validate_population)
}

rep_compile_model <- function(quiet = TRUE, compile_model_methods = FALSE,
                              force_recompile = FALSE) {
  rep_require(c("cmdstanr", "digest"))
  version <- as.character(cmdstanr::cmdstan_version())
  if (!identical(version, "2.38.0")) {
    stop("Use the recorded CmdStan 2.38.0 installation for model reproduction.")
  }
  source <- rep_path("stan", "measurement_model.stan")
  hash <- rep_sha256(source)
  folder <- rep_dir(rep_cache("stan", hash))
  cached_source <- file.path(folder, "measurement_model.stan")
  if (!file.exists(cached_source)) {
    if (!file.copy(source, cached_source)) stop("Cannot cache the Stan source.")
  }
  rep_assert_sha256(cached_source, hash)
  cmdstanr::cmdstan_model(cached_source, quiet = quiet,
                         cpp_options = list(stan_threads = FALSE),
                         compile_model_methods = compile_model_methods,
                         force_recompile = force_recompile)
}

rep_extract_native_array <- function(fit, variable, entity_ids, settings) {
  array <- as.array(fit$draws(variables = variable, inc_warmup = FALSE,
                            format = "draws_array"))
  variables <- paste0(variable, "[", seq_along(entity_ids), "]")
  if (!all(variables %in% dimnames(array)[[3L]])) stop("Missing Stan entity target.")
  array <- array[, , variables, drop = FALSE]
  expected <- as.integer(c(settings$iter_sampling, settings$chains, length(entity_ids)))
  if (!identical(dim(array), expected) || any(!is.finite(array))) {
    stop("Unexpected native-array dimensions or nonfinite values.")
  }
  # A plain array has no package-specific class or hidden local source paths.
  array <- array(as.numeric(array), dim = dim(array))
  dimnames(array) <- list(as.character(settings$iter_warmup + seq_len(expected[1L])),
                         as.character(seq_len(expected[2L])), entity_ids)
  array
}

rep_model_diagnostics <- function(fit, settings) {
  # Check the same sampled parameters and targets as in the original programs.
  # latent_logit is derived; stochastic y_rep is not a convergence parameter.
  variables <- c("lp__", "alpha", "sigma_tract", "target_native")
  if (settings$latent_structure != 1L) variables <- c(variables, "sigma_county")
  if (settings$latent_structure == 2L) {
    variables <- c(variables, "eta")
  } else {
    variables <- c(variables, "z_tract")
    if (settings$latent_structure == 3L) variables <- c(variables, "z_county")
  }
  summary <- as.data.frame(fit$summary(variables = variables))
  chain <- as.data.frame(fit$diagnostic_summary(quiet = TRUE))
  threshold <- rep_config()$hmc
  finite <- all(is.finite(summary$rhat)) && all(is.finite(summary$ess_bulk)) &&
            all(is.finite(summary$ess_tail)) && all(is.finite(chain$ebfmi))
  major <- summary$variable %in% c("alpha", "sigma_tract", "sigma_county[1]") |
           grepl("^target_native\\[", summary$variable)
  diagnostics <- list(
    rhat_max = max(summary$rhat), ess_bulk_min = min(summary$ess_bulk),
    ess_tail_min = min(summary$ess_tail), divergences = sum(chain$num_divergent),
    treedepth_hits = sum(chain$num_max_treedepth), bfmi_min = min(chain$ebfmi),
    hard_hmc_passed = finite && max(summary$rhat) < threshold$rhat_max &&
      min(summary$ess_bulk) >= threshold$ess_bulk_min &&
      min(summary$ess_tail) >= threshold$ess_tail_min &&
      sum(chain$num_divergent) <= threshold$divergence_max &&
      sum(chain$num_max_treedepth) <= threshold$treedepth_hits_max &&
      min(chain$ebfmi) > threshold$bfmi_min,
    preferred_ess1000_passed = all(summary$ess_bulk[major] >= 1000 &
                                  summary$ess_tail[major] >= 1000),
    monitored_variables = variables, thresholds = threshold,
    interpretation = "Execution completion, HMC acceptance, and Monte Carlo precision are separate.")
  list(summary = summary, chain = chain, diagnostics = diagnostics)
}

rep_fit_execute <- function(fit_id, output_dir = NULL, save_predictive = FALSE) {
  settings <- as.list(rep_fit_settings(fit_id)[1, , drop = FALSE])
  if (is.null(output_dir)) output_dir <- rep_output("fits", fit_id)
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE,
                                                no.. = TRUE))) {
    stop("Fit output already contains files; choose a new run directory: ", output_dir)
  }
  output_dir <- rep_dir(output_dir)
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  state <- list(fit_id = fit_id, status = "running", started_utc = started,
                settings = settings)
  rep_write_json(state, file.path(output_dir, "run-status.json"))
  tryCatch({
    observations <- rep_path("data", "analysis",
                             paste0(settings$universe, "-observations.csv"))
    bundle <- rep_model_data(observations, settings)
    cmdstanr::write_stan_json(bundle$data, file.path(output_dir, "stan-data.json"))
    model <- rep_compile_model()
    raw_dir <- rep_dir(file.path(output_dir, "cmdstan"))
    fit <- model$sample(data = bundle$data, seed = settings$seed,
                        chains = settings$chains,
                        chain_ids = seq_len(settings$chains),
                        parallel_chains = settings$parallel_chains,
                        iter_warmup = settings$iter_warmup,
                        iter_sampling = settings$iter_sampling,
                        thin = settings$thin, save_warmup = FALSE,
                        adapt_delta = settings$adapt_delta,
                        max_treedepth = settings$max_treedepth,
                        sig_figs = settings$sig_figs,
                        output_dir = raw_dir, output_basename = fit_id,
                        refresh = 200, show_messages = TRUE,
                        show_exceptions = TRUE)
    targets <- rep_extract_native_array(fit, "target_native", bundle$entity_ids, settings)
    if (any(targets <= 0 | targets >= 1)) {
      stop("Native targets reached numerical bounds; preserve output and review.")
    }
    saveRDS(targets, file.path(output_dir, "draws.rds"), compress = "gzip")
    if (save_predictive) {
      predictive <- rep_extract_native_array(fit, "y_rep", bundle$entity_ids, settings)
      saveRDS(predictive, file.path(output_dir, "predictive.rds"), compress = "gzip")
    }
    numerical <- rep_model_diagnostics(fit, settings)
    rep_write_csv(numerical$summary, file.path(output_dir, "summary.csv"))
    rep_write_csv(numerical$chain, file.path(output_dir, "chain-diagnostics.csv"))
    rep_write_json(numerical$diagnostics, file.path(output_dir, "diagnostics.json"))
    sampler <- as.array(fit$sampler_diagnostics(inc_warmup = FALSE, format = "draws_array"))
    dimnames(sampler)[[1L]] <- dimnames(targets)[[1L]]
    dimnames(sampler)[[2L]] <- dimnames(targets)[[2L]]
    saveRDS(sampler, file.path(output_dir, "sampler-diagnostics.rds"), compress = "gzip")
    coordinates <- expand.grid(retained_iteration = seq_len(settings$iter_sampling),
                               chain = seq_len(settings$chains),
                               KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    coordinates$stan_iteration <- settings$iter_warmup + coordinates$retained_iteration
    coordinates$fit_id <- fit_id
    coordinates$criterion <- settings$criterion
    rep_write_csv(coordinates, file.path(output_dir, "draw_coordinates.csv"))
    metadata <- list(author = "JoonHo Lee (jlee296@ua.edu)",
                     fit_id = fit_id, reference_source_fit_id = settings$source_fit_id,
                     model_id = settings$model_id, universe = settings$universe,
                     pool = settings$pool, criterion = settings$criterion,
                     entity_ids = bundle$entity_ids, chain_ids = as.character(1:4),
                     dimensions = dim(targets), settings = settings,
                     target_scale = "native fraction", orientation_applied = FALSE,
                     scientific_cross_criterion_joint = FALSE,
                     stan_source_sha256 = rep_sha256(rep_path("stan", "measurement_model.stan")),
                     observations_sha256 = rep_sha256(observations),
                     started_utc = started,
                     completed_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                     software = list(R = as.character(getRversion()),
                                     cmdstan = as.character(cmdstanr::cmdstan_version()),
                                     cmdstanr = as.character(utils::packageVersion("cmdstanr")),
                                     posterior = as.character(utils::packageVersion("posterior"))))
    files <- c("draws.rds", "draw_coordinates.csv", "summary.csv", "stan-data.json",
               "chain-diagnostics.csv", "diagnostics.json", "sampler-diagnostics.rds")
    if (save_predictive) files <- c(files, "predictive.rds")
    metadata$output_sha256 <- as.list(stats::setNames(vapply(file.path(output_dir, files),
                                                            rep_sha256, character(1)), files))
    rep_write_json(metadata, file.path(output_dir, "metadata.json"))
    state$status <- "complete"
    state$hard_hmc_passed <- numerical$diagnostics$hard_hmc_passed
    state$completed_utc <- metadata$completed_utc
    rep_write_json(state, file.path(output_dir, "run-status.json"))
    if (!numerical$diagnostics$hard_hmc_passed) {
      warning("Fit completed but failed a required HMC criterion; do not adopt it.")
    }
    invisible(list(path = output_dir, metadata = metadata,
                   diagnostics = numerical$diagnostics))
  }, error = function(error) {
    state$status <- "failed"
    state$error <- conditionMessage(error)
    state$ended_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    rep_write_json(state, file.path(output_dir, "run-status.json"))
    stop(error)
  })
}

rep_fit_one <- function(fit_id, output_dir = NULL, save_predictive = FALSE) {
  lock <- rep_acquire_lock(paste("fit", fit_id))
  on.exit(rep_release_lock(lock), add = TRUE)
  rep_fit_execute(fit_id, output_dir, save_predictive)
}

rep_fit_group <- function(group, output_dir = NULL, save_predictive = FALSE) {
  settings <- rep_fit_settings()
  selected <- switch(group,
    county = settings$universe == "county",
    `tract-main` = settings$model_id == "PV-TM1",
    `tract-stress` = settings$model_id %in% c("PV-TS1", "PV-TS2", "PV-TS3"),
    stop("Choose group county, tract-main, or tract-stress."))
  if (is.null(output_dir)) output_dir <- rep_output("fits")
  lock <- rep_acquire_lock(paste("fit-group", group))
  on.exit(rep_release_lock(lock), add = TRUE)
  results <- vector("list", sum(selected))
  fit_ids <- settings$fit_id[selected]
  names(results) <- fit_ids
  for (fit_id in fit_ids) {
    results[[fit_id]] <- rep_fit_execute(fit_id, file.path(output_dir, fit_id), save_predictive)
    if (!isTRUE(results[[fit_id]]$diagnostics$hard_hmc_passed)) {
      stop("Stop the packet after a hard HMC failure: ", fit_id)
    }
  }
  invisible(results)
}
