# JoonHo Lee (jlee296@ua.edu)
# Read the archived criterion-specific posterior arrays and regenerate summaries.
# Inputs: the versioned dataset cache, or an explicitly supplied fresh-fit folder.
# Outputs: plain native arrays, summary tables, or a criterionwise postrank input.
# Reading this file does not download data, load large arrays, or start a solver.

rep_fit_manifest <- function(fit_id = NULL) {
  manifest <- rep_read_csv(rep_path("data", "metadata", "fit-manifest.csv"))
  if (anyDuplicated(manifest$fit_id)) stop("Duplicate archived fit identifier.")
  if (is.null(fit_id)) return(manifest)
  result <- manifest[manifest$fit_id == fit_id, , drop = FALSE]
  if (nrow(result) != 1L) stop("Unknown archived fit identifier: ", fit_id)
  result
}

rep_read_posterior <- function(fit_id, draws_dir = NULL) {
  # Supplying draws_dir is explicit: a fresh run can never silently replace the
  # archived posterior used in the paper. Accept a fit root or its fits/ parent.
  if (is.null(draws_dir)) {
    folder <- rep_dataset("posterior", fit_id)
  } else {
    choices <- c(file.path(draws_dir, fit_id),
                 file.path(draws_dir, "fits", fit_id), draws_dir)
    found <- vapply(choices, function(path) {
      file.exists(file.path(path, "metadata.json")) &&
        file.exists(file.path(path, "draws.rds"))
    }, logical(1))
    if (!any(found)) stop("No posterior folder for ", fit_id, " under ", draws_dir)
    folder <- choices[which(found)[1L]]
  }
  metadata_path <- file.path(folder, "metadata.json")
  coordinates_path <- file.path(folder, "draw_coordinates.csv")
  draws_path <- file.path(folder, "draws.rds")
  if (!file.exists(metadata_path)) {
    stop("Posterior files are absent. Fetch the dataset explicitly or supply --draws-dir.")
  }
  metadata <- rep_read_json(metadata_path)
  if (!identical(metadata$fit_id, fit_id)) stop("Posterior fit identity differs.")
  archived <- identical(metadata$record_type, "archived_adopted_posterior")
  if (archived) {
    manifest <- rep_fit_manifest(fit_id)
    if (!isTRUE(metadata$hard_hmc_passed)) stop("Archived fit is not an adopted HMC fit.")
    rep_assert_sha256(metadata_path, manifest$metadata_sha256)
    rep_assert_sha256(draws_path, manifest$draws_sha256)
    rep_assert_sha256(coordinates_path, manifest$coordinates_sha256)
  } else {
    # Fresh outputs use their own recorded hashes and settings, not the paper's
    # native-array hash. Their selection/evaluation pools stay distinct.
    if (is.null(metadata$settings) || is.null(metadata$output_sha256)) {
      stop("Fresh posterior metadata need settings and output hashes.")
    }
    rep_assert_sha256(draws_path, metadata$output_sha256[["draws.rds"]])
    rep_assert_sha256(coordinates_path, metadata$output_sha256[["draw_coordinates.csv"]])
    metadata$seed <- metadata$settings$seed
    metadata$iter_warmup <- metadata$settings$iter_warmup
    metadata$iter_sampling <- metadata$settings$iter_sampling
    metadata$thin <- metadata$settings$thin
    metadata$source_fit_id <- metadata$reference_source_fit_id
    metadata$draws_sha256 <- metadata$output_sha256[["draws.rds"]]
    metadata$coordinates_sha256 <- metadata$output_sha256[["draw_coordinates.csv"]]
    metadata$record_type <- "fresh_computation"
    diagnostics_path <- file.path(folder, "diagnostics.json")
    rep_assert_sha256(diagnostics_path, metadata$output_sha256[["diagnostics.json"]])
    if (!isTRUE(rep_read_json(diagnostics_path)$hard_hmc_passed)) {
      stop("A fresh fit failed required HMC criteria; do not adopt its posterior.")
    }
  }
  required <- c("model_id", "universe", "pool", "criterion", "seed", "entity_ids",
                "chain_ids", "dimensions", "iter_warmup", "iter_sampling", "thin",
                "target_scale", "orientation_applied")
  if (!all(required %in% names(metadata)) ||
      !identical(paste(metadata$model_id, metadata$pool, metadata$criterion, sep = "-"), fit_id) ||
      !identical(metadata$scientific_cross_criterion_joint, FALSE) ||
      !identical(metadata$target_scale, "native fraction") ||
      !identical(metadata$orientation_applied, FALSE) || metadata$thin != 1L) {
    stop("Posterior metadata do not describe unchanged native targets.")
  }
  draws <- readRDS(draws_path)
  expected <- as.integer(c(metadata$iter_sampling, 4L, length(metadata$entity_ids)))
  if (!identical(class(draws), "array") || !identical(dim(draws), expected) ||
      !identical(as.integer(metadata$dimensions), expected) ||
      any(!is.finite(draws)) || any(draws <= 0 | draws >= 1)) {
    stop("Invalid native posterior array or dimensions.")
  }
  iterations <- as.character(metadata$iter_warmup + seq_len(expected[1L]))
  if (!identical(dimnames(draws)[[1L]], iterations) ||
      !identical(dimnames(draws)[[2L]], as.character(1:4)) ||
      !identical(as.character(metadata$chain_ids), as.character(1:4)) ||
      !identical(dimnames(draws)[[3L]], as.character(metadata$entity_ids))) {
    stop("Native entity, chain, or actual Stan iteration coordinates changed.")
  }
  coordinates <- rep_read_csv(coordinates_path)
  needed <- c("fit_id", "criterion", "chain", "retained_iteration", "stan_iteration")
  if (!all(needed %in% names(coordinates)) ||
      nrow(coordinates) != prod(expected[1:2]) ||
      any(coordinates$fit_id != fit_id) ||
      any(coordinates$criterion != metadata$criterion)) {
    stop("Posterior coordinate table has the wrong identity or size.")
  }
  source_key <- paste(coordinates$chain, coordinates$stan_iteration, sep = ":")
  chain <- rep(as.character(1:4), each = expected[1L])
  stan_iteration <- rep(iterations, times = 4L)
  wanted_key <- paste(chain, stan_iteration, sep = ":")
  index <- match(wanted_key, source_key)
  if (anyDuplicated(source_key) || anyNA(index)) stop("Missing/duplicated draw coordinate.")
  coordinates <- coordinates[index, , drop = FALSE]
  rownames(coordinates) <- NULL
  if (!identical(as.integer(coordinates$retained_iteration),
                 rep(seq_len(expected[1L]), times = 4L))) {
    stop("Retained iteration order changed.")
  }
  structure(list(draws = draws, metadata = metadata, coordinates = coordinates,
                 chain = chain, iteration = as.integer(stan_iteration)),
            class = "rep_posterior")
}

rep_summarise_posterior <- function(posterior, observations = NULL) {
  if (!inherits(posterior, "rep_posterior")) stop("Read an authenticated posterior first.")
  metadata <- posterior$metadata
  if (is.null(observations)) {
    observations <- rep_read_csv(rep_path("data", "analysis",
                             paste0(metadata$universe, "-observations.csv")))
  }
  ids <- as.character(metadata$entity_ids)
  index <- match(ids, observations$entity_id)
  if (anyNA(index) || anyDuplicated(observations$entity_id)) {
    stop("Observations do not cover the posterior source entities uniquely.")
  }
  observations <- observations[index, , drop = FALSE]
  array <- posterior$draws
  matrix <- matrix(array, nrow = prod(dim(array)[1:2]), ncol = length(ids))
  # Pool chains only for marginal summaries. The saved array and coordinates
  # remain intact for MCSE and within-criterion pair calculations.
  interval <- apply(matrix, 2L, stats::quantile,
                    probs = c(0.025, 0.5, 0.975), names = FALSE, type = 7L)
  mean <- colMeans(matrix)
  sd <- apply(matrix, 2L, stats::sd)
  criterion <- metadata$criterion
  raw <- as.numeric(observations[[paste0(criterion, "_estimate_fraction")]])
  se <- as.numeric(observations[[paste0(criterion, "_se_fraction")]])
  if (any(!is.finite(raw)) || any(!is.finite(se) | se <= 0)) {
    stop("Summary source estimates/SE must be finite and SE positive.")
  }
  data.frame(entity_id = ids, county_id = observations$county_id,
             criterion = criterion, fit_id = metadata$fit_id,
             raw_estimate_fraction = raw, raw_se_fraction = se,
             raw_estimate_points = 100 * raw, raw_se_points = 100 * se,
             posterior_mean_fraction = mean,
             posterior_median_fraction = interval[2L, ],
             posterior_sd_fraction = sd,
             posterior_q025_fraction = interval[1L, ],
             posterior_q975_fraction = interval[3L, ],
             posterior_mean_points = 100 * mean,
             posterior_median_points = 100 * interval[2L, ],
             posterior_sd_points = 100 * sd,
             posterior_q025_points = 100 * interval[1L, ],
             posterior_q975_points = 100 * interval[3L, ],
             posterior_interval_width_points = 100 * (interval[3L, ] - interval[1L, ]),
             shrinkage_signed_points = 100 * (mean - raw),
             stringsAsFactors = FALSE, row.names = NULL)
}

rep_posterior_summaries <- function(model_id, pool = "A", draws_dir = NULL) {
  # Join the two marginal summaries by their named source entities. This is a
  # table join, not an assertion of cross-criterion posterior dependence.
  criteria <- c("income", "incarceration")
  summaries <- lapply(criteria, function(criterion) {
    fit_id <- paste(model_id, pool, criterion, sep = "-")
    rep_summarise_posterior(rep_read_posterior(fit_id, draws_dir))
  })
  if (!identical(summaries[[1L]]$entity_id, summaries[[2L]]$entity_id)) {
    stop("Criterion summary entity order differs.")
  }
  out <- summaries[[1L]][, c("entity_id", "county_id"), drop = FALSE]
  for (i in seq_along(criteria)) {
    fields <- setdiff(names(summaries[[i]]), c("entity_id", "county_id", "criterion"))
    block <- summaries[[i]][, fields, drop = FALSE]
    names(block) <- paste(criteria[i], ifelse(fields == "fit_id", "posterior_fit_id", fields), sep = "_")
    out <- cbind(out, block)
  }
  out
}

rep_posterior_input <- function(model_id, pool = "A", entity_ids, draws_dir = NULL) {
  # Keep each criterion's original complete entity vector and chain history
  # until applying this deterministic named candidate subset. Equal row numbers
  # across criteria are bookkeeping slots, not scientific joint posterior states.
  rep_require(c("postrank", "digest"))
  ids <- as.character(entity_ids)
  if (length(ids) < 2L || anyNA(ids) || anyDuplicated(ids)) {
    stop("Supply at least two uniquely named candidate entity IDs.")
  }
  criteria <- c("income", "incarceration")
  loaded <- lapply(criteria, function(criterion) {
    rep_read_posterior(paste(model_id, pool, criterion, sep = "-"), draws_dir)
  })
  names(loaded) <- criteria
  matrices <- blocks <- vector("list", length(criteria))
  names(matrices) <- names(blocks) <- criteria
  source_ids <- loaded[[1L]]$metadata$entity_ids
  if (!identical(source_ids, loaded[[2L]]$metadata$entity_ids) ||
      any(!ids %in% source_ids) ||
      loaded[[1L]]$metadata$seed == loaded[[2L]]$metadata$seed) {
    stop("Criterion source vectors or independent numerical stream identities differ.")
  }
  for (criterion in criteria) {
    z <- loaded[[criterion]]
    metadata <- z$metadata
    dimensions <- dim(z$draws)
    source_index <- match(ids, metadata$entity_ids)
    values <- matrix(z$draws, nrow = prod(dimensions[1:2]), ncol = dimensions[3L])
    values <- values[, source_index, drop = FALSE]
    multiplier <- if (criterion == "income") -100 else 100
    values <- values * multiplier
    slots <- paste0(criterion, ":", metadata$fit_id, ":chain-", z$chain,
                    ":iteration-", z$iteration)
    dimnames(values) <- list(draw = slots, entity = ids)
    matrices[[criterion]] <- values
    history <- lapply(as.character(1:4), function(chain) {
      list(chain_id = chain, retained_iterations = z$iteration[z$chain == chain],
           warmup_iterations = seq_len(metadata$iter_warmup),
           discarded_sampling_iterations = integer(),
           selection_note = "All original postwarmup iterations and the full source vector, followed by a deterministic named subset.")
    })
    blocks[[criterion]] <- list(
      criterion_id = criterion,
      origin = list(source_id = paste0("alabama-posterior:", metadata$fit_id),
                    fit_id = metadata$fit_id, model_id = metadata$model_id,
                    source_format = "RDS iteration x original chain x native entity array",
                    source_artifact = paste0("posterior/", metadata$fit_id, "/draws.rds"),
                    source_hash = metadata$draws_sha256,
                    vector_generation_provenance = paste(
                      "One full native entity vector from separate criterion fit",
                      metadata$fit_id, "; source N", length(source_ids),
                      "; deterministic candidate subset N", length(ids)),
                    within_criterion_joint_declared = TRUE),
      sampling = list(regime = "mcmc", target_distribution = "declared_posterior",
                      equal_weight = TRUE, warmup_status = "removed",
                      sampling_provenance = paste(
                        "Separate criterion CmdStan stream, seed", metadata$seed,
                        "; original chains1:4; sampling/chain", metadata$iter_sampling,
                        "; thin1"), chain_history = history),
      draw_index = lapply(seq_along(slots), function(i) {
        list(slot_id = slots[i], original_draw_id = paste0(metadata$fit_id, ":chain-",
             z$chain[i], ":iteration-", z$iteration[i]),
             chain_id = z$chain[i], iteration = z$iteration[i])
      }),
      variable_map = lapply(seq_along(ids), function(i) {
        list(entity_id = ids[i], criterion_id = criterion,
             source_target_variable_id = paste0("target_native[", source_index[i], "]"),
             derivation_note = paste("Native target for", ids[i], ";", multiplier,
                                     "applied exactly once; SE is not a latent target."))
      }))
  }
  fit_ids <- vapply(loaded, function(z) z$metadata$fit_id, character(1))
  hashes <- vapply(loaded, function(z) z$metadata$draws_sha256, character(1))
  target_kind <- if (loaded[[1L]]$metadata$universe == "county") "counties" else "2010 census tracts"
  metadata <- list(
    schema_version = "applied-0.1", input_kind = "equal_weight_draws",
    dependence_mode = "criterionwise_expected_only", requested_functional = "expected_rank_loss",
    entity_ids = ids, criterion_ids = criteria,
    targets = lapply(criteria, function(criterion) {
      list(criterion_id = criterion,
           estimand = if (criterion == "income")
             "Negative conditional mean child household income rank at parental percentile25" else
             "Conditional probability incarcerated on2010-04-01 at parental percentile25",
           entity_population = paste(length(ids), "Alabama", target_kind,
                                     "; pooled race/gender;1978-1983 birth cohorts"),
           time_reference = if (criterion == "income") "Income rank averaged2014-2015" else "Incarceration snapshot2010-04-01",
           units = if (criterion == "income") "negative income percentile points" else "incarceration percentage points",
           source_transform = if (criterion == "income") "-100 times native fraction, once" else "+100 times native fraction, once",
           identification_basis = "Separate criterion working likelihood for public estimates and SE; no identified sampling covariance.",
           orientation = "higher_priority")
    }),
    context = list(cohort_id = "Alabama-1978-1983-parentp25-pooledracegender",
                   data_version = paste(hashes, collapse = "+"),
                   data_conditioning = "Published historical Atlas estimates and SE; current ACS/HUD are separate context.",
                   model_summary = paste(model_id, "separate criterion fits;pool", pool)),
    identification = list(status = "identified_declared",
                          note = "Declared working-model targets; not validation of model truth, unknown covariance, convergence, or policy eligibility."),
    tie_policy = list(rule = "pairwise_tie_free", declared = TRUE,
                     justification = "Continuous logistic-normal targets imply zero-probability exact ties; numerical ties are preserved and reviewed.",
                     observed_tie_action = "reject_and_review"),
    provenance = list(producer = "Alabama replication R/posterior.R",
                      artifact_id = paste0("alabama-needs:", paste(fit_ids, collapse = "+"),
                                           ":subset:", digest::digest(ids, algo = "sha256")),
                      artifact_hash = digest::digest(list(fit_ids = fit_ids, hashes = hashes,
                                                         ids = ids, transform = c(-100, 100)), algo = "sha256"),
                      construction_note = "Each criterion keeps its own complete chain history. Matching row positions do not declare cross-criterion scientific joint states.",
                      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    criterion_blocks = unname(blocks))
  input <- postrank::postrank_draws(matrices, metadata = metadata)
  if (!isTRUE(postrank::validate_postrank_draws(input)$valid)) {
    stop("The criterionwise postrank input failed semantic validation.")
  }
  input
}

rep_input_matrices <- function(input) {
  # The validated public constructor stores each criterion's matrix in its
  # declared block. Read those fields without accessing package internals.
  if (!inherits(input, "postrank_draws") ||
      !identical(input$dependence_mode, "criterionwise_expected_only") ||
      !isTRUE(postrank::validate_postrank_draws(input)$valid)) {
    stop("A valid criterionwise postrank input is required.")
  }
  values <- lapply(input$criterion_blocks, function(block) block$matrix$values)
  names(values) <- vapply(input$criterion_blocks, function(block) block$criterion_id,
                         character(1))
  values[input$criterion_ids]
}

rep_input_coordinates <- function(input) {
  if (!inherits(input, "postrank_draws") ||
      !isTRUE(postrank::validate_postrank_draws(input)$valid)) {
    stop("Read coordinates only from a validated postrank input.")
  }
  values <- lapply(input$criterion_blocks, function(block) {
    data.frame(chain = vapply(block$draw_index, function(draw) draw$chain_id, character(1)),
               iteration = vapply(block$draw_index, function(draw) as.integer(draw$iteration), integer(1)),
               stringsAsFactors = FALSE)
  })
  names(values) <- vapply(input$criterion_blocks, function(block) block$criterion_id,
                         character(1))
  values[input$criterion_ids]
}
