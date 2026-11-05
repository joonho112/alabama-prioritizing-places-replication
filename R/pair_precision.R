# JoonHo Lee (jlee296@ua.edu)
# Recompute Appendix C's coefficient precision on its fixed historical domain.
#
# This diagnostic is separate from report selection and fixed-report loss MCSE.
# Its 1,993 unordered tract pairs were fixed before the original optimization:
# preserve the union of the initial and adopted A-only threshold screens.
# Each screen considers five policies and both directions, retaining a pair
# when abs(weighted_error - tau) <= .02 OR abs(weighted_error - .5) <= .02.
# The two 1,891-pair screens overlap in 1,789 pairs, giving a 1,993-pair union.
# B never chooses this domain. The initial native computation is not distributed. The supplied identifiers make
# the published diagnostic reproducible without reconstructing that history.
#
# No function here chooses a new pair set from fresh draws, B, or report grades.
# Reading this file starts no calculation, download, sampler, or optimizer.

# ---- Small design inputs: safe to check without posterior archives -----------

rep_pair_precision_design <- function() {
  pair_path <- rep_path("data", "metadata", "selection-sensitive-pairs.csv")
  pairs <- rep_read_csv(pair_path)
  fields <- c(
    "pair_id", "left_id", "right_id",
    "in_initial_A_screen", "in_adopted_A_screen"
  )
  keys <- rep_read_csv(rep_path("data", "analysis", "pilot-tract-keys.csv"))
  ids <- sort(keys$entity_id)
  if (!identical(names(pairs), fields) || nrow(pairs) != 1993L ||
    anyNA(pairs) || anyDuplicated(pairs$pair_id) ||
    length(ids) != 90L || anyDuplicated(ids) ||
    any(!pairs$left_id %in% ids | !pairs$right_id %in% ids) ||
    any(pairs$left_id >= pairs$right_id) ||
    !identical(pairs$pair_id, paste(pairs$left_id, pairs$right_id, sep = "__"))) {
    stop("Invalid published pair-precision identifiers or pilot keys.")
  }
  for (field in fields[4:5]) {
    text <- tolower(pairs[[field]])
    if (any(!text %in% c("true", "false"))) stop("Invalid screen membership flag.")
    pairs[[field]] <- text == "true"
  }
  initial <- pairs$in_initial_A_screen
  adopted <- pairs$in_adopted_A_screen
  if (sum(initial) != 1891L || sum(adopted) != 1891L ||
    sum(initial & adopted) != 1789L || any(!initial & !adopted)) {
    stop("Published initial/adopted screen membership changed.")
  }

  # The county rows repeat these policies. Use the five tract rows once.
  settings <- rep_report_scenarios()
  policies <- settings[
    settings$universe == "tract" & settings$kind == "policy",
    c("scenario_id", "weight_income", "weight_incarceration", "tau")
  ]
  expected_ids <- c("center", "PV-w25", "PV-w75", "PV-t10", "PV-t30")
  if (nrow(policies) != 5L || !setequal(policies$scenario_id, expected_ids)) {
    stop("Pair precision requires the five published tract policy settings.")
  }
  policies <- policies[match(expected_ids, policies$scenario_id), , drop = FALSE]
  expected <- cbind(
    weight_income = c(.5, .25, .75, .5, .5),
    weight_incarceration = c(.5, .75, .25, .5, .5),
    tau = c(.2, .2, .2, .1, .3)
  )
  values <- as.matrix(policies[, colnames(expected)])
  if (any(!is.finite(values)) || any(abs(values - expected) > 1e-14)) {
    stop("The frozen diagnostic domain belongs to the published five policies.")
  }
  target <- rep_config()$precision$pair_mcse_target
  if (length(target) != 1L || !is.finite(target) || abs(target - .005) > 1e-14) {
    stop("The published pair-coefficient MCSE target is .005.")
  }
  reference <- rep_read_csv(rep_path("data", "saved", "pair-precision-summary.csv"))
  reference_fields <- c(
    "pool", "model_id", "n_entities", "n_frozen_pairs", "n_policies",
    "weighted_rows", "finite_weighted_rows", "unavailable_weighted_rows",
    "finite_weighted_target_misses", "any_criterion_constant_rows",
    "all_criteria_constant_rows", "max_finite_weighted_mcse",
    "criterion_diagnostic_target_misses", "target_mcse", "income_draws",
    "incarceration_draws", "criterion_direction_rows_with_ties", "precision_status"
  )
  if (!identical(names(reference), reference_fields) || nrow(reference) != 2L ||
    anyNA(reference) || anyDuplicated(reference$pool) ||
    !setequal(reference$pool, c("A", "B")) || any(reference$model_id != "PV-TM1")) {
    stop("Expected the two complete published pair-precision reference pools.")
  }
  numeric_fields <- setdiff(reference_fields, c("pool", "model_id", "precision_status"))
  for (field in numeric_fields) {
    reference[[field]] <- suppressWarnings(as.numeric(reference[[field]]))
    if (any(!is.finite(reference[[field]]) | reference[[field]] < 0)) {
      stop("Invalid numeric pair-precision reference field: ", field)
    }
  }
  if (any(reference$n_entities != 90L | reference$n_frozen_pairs != 1993L |
    reference$n_policies != 5L | reference$weighted_rows != 19930L) ||
    any(reference$finite_weighted_rows + reference$unavailable_weighted_rows !=
      reference$weighted_rows) || any(reference$target_mcse != target) ||
    any(!reference$precision_status %in% c("estimated_target_met", "precision_limited"))) {
    stop("Pair-precision reference counts or target are inconsistent.")
  }
  list(
    pairs = pairs, entity_ids = ids, policies = policies, target = target,
    reference = reference, pair_path = pair_path
  )
}

rep_check_pair_precision <- function() {
  design <- rep_pair_precision_design()
  list(
    valid = TRUE, model = "PV-TM1", n_entities = length(design$entity_ids),
    n_frozen_pairs = nrow(design$pairs), n_policies = nrow(design$policies),
    weighted_rows_per_pool = 2L * nrow(design$pairs) * nrow(design$policies),
    initial_screen_pairs = sum(design$pairs$in_initial_A_screen),
    adopted_screen_pairs = sum(design$pairs$in_adopted_A_screen),
    initial_native_computation_distributed = FALSE,
    initial_and_adopted_union_supplied = TRUE,
    historical_rule = "A-only five-policy/both-direction union: abs(weighted_error-tau)<=.02 OR abs(weighted_error-.5)<=.02; preserve initial and adopted screens",
    fixed_domain_reselected = FALSE, native_arrays_opened = FALSE,
    target_mcse = design$target
  )
}

# ---- One original-chain indicator trace ------------------------------------

rep_pair_trace_mcse <- function(trace) {
  # Coordinates have already been checked once for this criterion. Retain the
  # resulting iteration-by-chain matrix for every pair, rather than pooling it
  # into an IID vector or guessing an effective sample size.
  if (!is.matrix(trace) || any(!is.finite(trace)) || !nrow(trace) ||
    !ncol(trace) || is.null(rownames(trace)) || is.null(colnames(trace))) {
    stop("A finite, named iteration-by-chain matrix is required.")
  }
  constant <- all(trace == trace[1L])
  warnings <- character()
  estimate <- function(fun) {
    tryCatch(withCallingHandlers(as.numeric(fun(trace)), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) {
      warnings <<- c(warnings, conditionMessage(e))
      NA_real_
    })
  }
  if (constant) {
    # An observed constant direction indicator does not certify a probability
    # of zero/one. These empirical traces have no algebraic-zero exception.
    mcse <- ess <- rhat <- NA_real_
    status <- "constant_trace_mcse_unidentified"
  } else if (nrow(trace) < 4L) {
    mcse <- ess <- rhat <- NA_real_
    status <- "insufficient_retained_iterations"
  } else {
    mcse <- estimate(posterior::mcse_mean)
    ess <- estimate(posterior::ess_mean)
    rhat <- estimate(posterior::rhat)
    status <- if (is.finite(mcse) && mcse >= 0) {
      "estimated_chain_preserving"
    } else {
      "nonconstant_mcse_unavailable"
    }
  }
  list(
    mcse = mcse, ess_mean = ess, indicator_rhat = rhat,
    constant = constant, mcse_status = status,
    warnings = paste(unique(warnings), collapse = " | ")
  )
}

# ---- Bounded coefficient calculations, with each criterion's own S ----------

rep_pair_precision_criterion <- function(values, coordinates, pairs, pool,
                                         criterion, block_size = 128L) {
  if (!is.matrix(values) || any(!is.finite(values)) ||
    is.null(colnames(values)) || anyDuplicated(colnames(values)) ||
    !all(c("pair_id", "left_id", "right_id") %in% names(pairs)) ||
    !nrow(pairs) || anyDuplicated(pairs$pair_id) ||
    length(block_size) != 1L || !is.finite(block_size) ||
    block_size != floor(block_size) || block_size < 1L || block_size > 256L) {
    stop("Invalid pair block, matrix, or pair identifiers.")
  }
  left <- match(pairs$left_id, colnames(values))
  right <- match(pairs$right_id, colnames(values))
  if (anyNA(left) || anyNA(right) || any(left == right)) {
    stop("Every fixed pair needs two distinct entities in the criterion matrix.")
  }
  # Validate all actual chain/iteration coordinates once. Matrix positions then
  # refer to this exact map throughout the blocked calculation.
  S <- nrow(values)
  map <- rep_chain_matrix(seq_len(S), coordinates)
  original_row_order <- as.integer(map)
  rows <- list()
  index <- 0L
  for (start in seq.int(1L, nrow(pairs), by = block_size)) {
    at <- seq.int(start, min(start + block_size - 1L, nrow(pairs)))
    a <- values[, left[at], drop = FALSE]
    b <- values[, right[at], drop = FALSE]
    ties <- colSums(a == b)
    counts <- list()
    for (direction in c("left_over_right", "right_over_left")) {
      # Values have already been oriented toward greater need exactly once.
      # A declared higher-need place is wrong when its value is <= the other.
      # Both weak events include an exact numerical tie; never break it by ID.
      event <- if (direction == "left_over_right") a <= b else b <= a
      counts[[direction]] <- colSums(event)
      diagnostics <- lapply(seq_along(at), function(q) {
        trace <- matrix(as.numeric(event[original_row_order, q]),
          nrow = nrow(map), ncol = ncol(map),
          dimnames = dimnames(map)
        )
        rep_pair_trace_mcse(trace)
      })
      index <- index + 1L
      rows[[index]] <- data.frame(
        pool = pool, pair_id = pairs$pair_id[at], direction = direction,
        criterion = criterion, coefficient = counts[[direction]] / S,
        event_count = counts[[direction]], n_draws = S,
        n_chains = ncol(map), iterations_per_chain = nrow(map),
        numeric_tie_count = ties,
        mcse = vapply(diagnostics, `[[`, numeric(1), "mcse"),
        mcse_status = vapply(diagnostics, `[[`, character(1), "mcse_status"),
        ess_mean = vapply(diagnostics, `[[`, numeric(1), "ess_mean"),
        indicator_rhat = vapply(diagnostics, `[[`, numeric(1), "indicator_rhat"),
        constant = vapply(diagnostics, `[[`, logical(1), "constant"),
        warnings = vapply(diagnostics, `[[`, character(1), "warnings"),
        stringsAsFactors = FALSE
      )
      rm(event, diagnostics)
    }
    # The familiar complement identity applies when ties are absent. The
    # generalized identity retains numerical ties in both exact <= counts.
    if (any(counts$left_over_right + counts$right_over_left != S + ties)) {
      stop("Weak-direction event counts failed their exact tie-aware identity.")
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

rep_pair_precision_weighted <- function(criterion_rows, policies, target = .005) {
  a <- criterion_rows[criterion_rows$criterion == "income", , drop = FALSE]
  b <- criterion_rows[criterion_rows$criterion == "incarceration", , drop = FALSE]
  key <- function(x) paste(x$pool, x$pair_id, x$direction, sep = ":")
  if (anyDuplicated(key(a)) || anyDuplicated(key(b)) || !setequal(key(a), key(b))) {
    stop("Criterion pair/direction identities do not match uniquely.")
  }
  b <- b[match(key(a), key(b)), , drop = FALSE]
  if (!nrow(a)) stop("No criterion direction rows to combine.")
  rows <- lapply(seq_len(nrow(policies)), function(i) {
    policy <- policies[i, ]
    w <- c(policy$weight_income, policy$weight_incarceration)
    if (any(!is.finite(w) | w <= 0) || abs(sum(w) - 1) > 1e-14) {
      stop("Published coefficient precision uses two positive weights summing to one.")
    }
    known <- is.finite(a$mcse) & a$mcse >= 0 & is.finite(b$mcse) & b$mcse >= 0
    mcse <- rep(NA_real_, nrow(a))
    # Independent *computational* stream variances add. There is no weighted
    # indicator trace, scientific cross-criterion joint, or common draw index.
    mcse[known] <- sqrt(w[1]^2 * a$mcse[known]^2 + w[2]^2 * b$mcse[known]^2)
    data.frame(
      pool = a$pool, pair_id = a$pair_id, direction = a$direction,
      policy_id = policy$scenario_id, weight_income = w[1],
      weight_incarceration = w[2], tau = policy$tau,
      coefficient = w[1] * a$coefficient + w[2] * b$coefficient,
      mcse = mcse,
      mcse_status = ifelse(known, "independent_numerical_variance_sum",
        "criterion_mcse_unavailable_tail_unidentified"
      ),
      any_criterion_constant = a$constant | b$constant,
      all_criteria_constant = a$constant & b$constant,
      precision_status = ifelse(!known, "precision_limited_mcse_unavailable",
        ifelse(mcse <= target, "estimated_target_met", "estimated_target_missed")
      ),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# ---- Authenticated main-model inputs and compact pool summaries -------------

rep_pair_precision_read <- function(criterion, pool, entity_ids, draws_dir) {
  fit_id <- paste("PV-TM1", pool, criterion, sep = "-")
  z <- rep_read_posterior(fit_id, draws_dir)
  metadata <- z$metadata
  if (!identical(metadata$model_id, "PV-TM1") ||
    !identical(metadata$universe, "tract") ||
    !identical(metadata$pool, pool) || !identical(metadata$criterion, criterion) ||
    length(metadata$entity_ids) != 1173L || any(!entity_ids %in% metadata$entity_ids)) {
    stop("Pair precision needs the authenticated full main-tract fit.")
  }
  # Read and authenticate the full original vectors before taking the named
  # 90-tract diagnostic subset. This does not refit on a reduced population.
  dimensions <- dim(z$draws)
  values <- matrix(z$draws, nrow = prod(dimensions[1:2]), ncol = dimensions[3L])
  values <- values[, match(entity_ids, metadata$entity_ids), drop = FALSE]
  multiplier <- if (criterion == "income") -100 else 100
  values <- values * multiplier
  colnames(values) <- entity_ids
  coordinates <- data.frame(
    chain = z$chain, iteration = z$iteration,
    stringsAsFactors = FALSE
  )
  proof <- list(
    fit_id = fit_id, model_id = metadata$model_id, pool = pool,
    criterion = criterion, seed = metadata$seed,
    record_type = metadata$record_type,
    draws_sha256 = metadata$draws_sha256,
    source_entity_ids_sha256 = digest::digest(metadata$entity_ids, algo = "sha256"),
    source_entity_count = dimensions[3L],
    n_draws = nrow(values), n_chains = dimensions[2L],
    iterations_per_chain = dimensions[1L],
    chain_ids = unique(z$chain), actual_iteration_range = range(z$iteration),
    coordinates_sha256 = digest::digest(coordinates, algo = "sha256"),
    need_multiplier_applied_once = multiplier
  )
  list(values = values, coordinates = coordinates, proof = proof)
}

rep_pair_precision_pool_summary <- function(criterion, weighted, design, pool) {
  finite <- is.finite(weighted$mcse)
  missed <- finite & weighted$mcse > design$target
  S <- vapply(c("income", "incarceration"), function(k) {
    sizes <- unique(criterion$n_draws[criterion$criterion == k])
    if (length(sizes) != 1L) stop("Each criterion needs its own recorded sample size.")
    as.numeric(sizes)
  }, numeric(1))
  data.frame(
    pool = pool, model_id = "PV-TM1", n_entities = length(design$entity_ids),
    n_frozen_pairs = nrow(design$pairs), n_policies = nrow(design$policies),
    weighted_rows = nrow(weighted), finite_weighted_rows = sum(finite),
    unavailable_weighted_rows = sum(!finite), finite_weighted_target_misses = sum(missed),
    any_criterion_constant_rows = sum(weighted$any_criterion_constant),
    all_criteria_constant_rows = sum(weighted$all_criteria_constant),
    max_finite_weighted_mcse = if (any(finite)) max(weighted$mcse[finite]) else NA_real_,
    # Criterion-specific .005 misses remain diagnostics. The prespecified
    # selection-coefficient target concerns the weighted policy rows.
    criterion_diagnostic_target_misses = sum(is.finite(criterion$mcse) &
      criterion$mcse > design$target),
    target_mcse = design$target, income_draws = S["income"],
    incarceration_draws = S["incarceration"],
    criterion_direction_rows_with_ties = sum(criterion$numeric_tie_count > 0),
    precision_status = if (any(!finite | missed)) "precision_limited" else "estimated_target_met",
    row.names = NULL, stringsAsFactors = FALSE
  )
}

rep_pair_precision_reference <- function(summary, reference) {
  reference <- reference[match(summary$pool, reference$pool), , drop = FALSE]
  if (!identical(summary$pool, reference$pool) || !setequal(names(summary), names(reference))) {
    stop("Pair-precision reference schema differs from the computed summary.")
  }
  differences <- list()
  for (field in names(summary)) {
    if (is.numeric(summary[[field]])) {
      expected <- suppressWarnings(as.numeric(reference[[field]]))
      tolerance <- if (field == "max_finite_weighted_mcse") 1e-12 else 1e-14
      same <- is.finite(summary[[field]]) & is.finite(expected) &
        abs(summary[[field]] - expected) <= tolerance
    } else {
      same <- as.character(summary[[field]]) == as.character(reference[[field]])
    }
    if (anyNA(same) || any(!same)) differences[[field]] <- summary$pool[is.na(same) | !same]
  }
  list(matches = !length(differences), differing_fields = differences)
}

# ---- Explicit execution: one shared heavy-work slot, external results -------

rep_pair_precision <- function(draws_dir = NULL, pools = c("A", "B"),
                               block_size = 128L,
                               output_dir = rep_output("pair-precision")) {
  design <- rep_pair_precision_design()
  if (!length(pools) || anyNA(pools) || anyDuplicated(pools) ||
    any(!pools %in% c("A", "B")) || length(block_size) != 1L ||
    !is.finite(block_size) || block_size != floor(block_size) ||
    block_size < 1L || block_size > 256L) {
    stop("Choose pools A/B and an integer block size from 1 through 256.")
  }
  if (!is.null(draws_dir)) draws_dir <- rep_absolute_path(draws_dir)
  output_dir <- rep_absolute_path(output_dir)
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
    stop("Pair-precision output already contains files; choose a new output directory.")
  }
  rep_require(c("posterior", "digest", "jsonlite"))
  # Acquire the same lock used by fits and report selection BEFORE opening any
  # native array. A diagnostic must not compete with a fitting packet.
  lock <- rep_acquire_lock("pair-precision")
  on.exit(rep_release_lock(lock), add = TRUE)
  output_dir <- rep_dir(output_dir)
  mode <- if (is.null(draws_dir)) "archived_reference" else "explicit_draws"
  completed <- character()
  tryCatch(
    {
      rep_write_json(list(
        status = "running", mode = mode, requested_pools = pools,
        completed_pools = completed
      ), file.path(output_dir, "run-status.json"))
      all_criterion <- all_weighted <- summaries <- proofs <- list()
      for (pool in pools) {
        criterion_rows <- stream_proof <- list()
        for (criterion in c("income", "incarceration")) {
          input <- rep_pair_precision_read(criterion, pool, design$entity_ids, draws_dir)
          stream_proof[[criterion]] <- input$proof
          criterion_rows[[criterion]] <- rep_pair_precision_criterion(
            input$values, input$coordinates, design$pairs, pool, criterion, block_size
          )
          # Keep the compact diagnostics, releasing native/working arrays before
          # loading the other criterion. No alignment across criteria is needed.
          rm(input)
          invisible(gc(FALSE))
          message("Completed ", pool, " ", criterion, " coefficient diagnostics.")
        }
        seeds <- vapply(stream_proof, function(x) as.numeric(x$seed), numeric(1))
        hashes <- vapply(stream_proof, `[[`, character(1), "source_entity_ids_sha256")
        if (anyDuplicated(seeds) || length(unique(hashes)) != 1L) {
          stop("Criterion numerical streams need distinct seeds and the same source entities.")
        }
        criterion <- do.call(rbind, criterion_rows)
        rownames(criterion) <- NULL
        weighted <- rep_pair_precision_weighted(criterion, design$policies, design$target)
        summary <- rep_pair_precision_pool_summary(criterion, weighted, design, pool)
        if (nrow(criterion) != 4L * nrow(design$pairs) ||
          nrow(weighted) != 2L * nrow(design$pairs) * nrow(design$policies)) {
          stop("Unexpected fixed-domain diagnostic row count.")
        }
        all_criterion[[pool]] <- criterion
        all_weighted[[pool]] <- weighted
        summaries[[pool]] <- summary
        proofs[[pool]] <- stream_proof
        completed <- c(completed, pool)
        rep_write_json(list(
          status = "running", mode = mode, requested_pools = pools,
          completed_pools = completed
        ), file.path(output_dir, "run-status.json"))
        message(
          "Completed pool ", pool, ": ", summary$weighted_rows,
          " weighted rows; ", summary$precision_status, "."
        )
      }
      if (length(pools) == 2L) {
        seeds <- unlist(lapply(proofs, function(pool) vapply(pool, `[[`, numeric(1), "seed")))
        source_hashes <- unlist(lapply(proofs, function(pool) {
          vapply(pool, `[[`, character(1), "source_entity_ids_sha256")
        }))
        if (anyDuplicated(seeds) || length(unique(source_hashes)) != 1L) {
          stop("A/B computations need distinct seeds and the same source population.")
        }
      }
      criterion <- do.call(rbind, all_criterion)
      weighted <- do.call(rbind, all_weighted)
      summary <- do.call(rbind, summaries)
      rownames(criterion) <- rownames(weighted) <- rownames(summary) <- NULL
      comparison <- rep_pair_precision_reference(summary, design$reference)
      rep_write_csv(criterion, file.path(output_dir, "criterion-precision.csv"))
      rep_write_csv(weighted, file.path(output_dir, "weighted-precision.csv"))
      rep_write_csv(summary, file.path(output_dir, "summary.csv"))
      metadata <- list(
        author = "JoonHo Lee (jlee296@ua.edu)", model_id = "PV-TM1", mode = mode,
        domain = "Fixed historical main-tract A-only screen union; not rescreened",
        historical_selection_rule = "Five policies and both directions; abs(weighted_error-tau)<=.02 OR abs(weighted_error-.5)<=.02; union initial and adopted A screens, B never selects the set",
        initial_native_computation_distributed = FALSE,
        n_entities = length(design$entity_ids), n_frozen_pairs = nrow(design$pairs),
        pair_identifiers_sha256 = rep_sha256(design$pair_path), policies = design$policies,
        block_size = block_size, target_mcse = design$target,
        independent_numerical_variance_formula = "sqrt(sum(w_k^2 * MCSE_k^2)); unavailable summand remains NA",
        independent_criterion_numerical_streams = TRUE,
        scientific_cross_criterion_joint = FALSE, weighted_indicator_trace_created = FALSE,
        numeric_tie_rule = "Exact <= events retain ties in both directions; tie counts are recorded for review",
        numeric_ties_seen = any(criterion$numeric_tie_count > 0),
        source_streams = proofs,
        software = list(
          R = as.character(getRversion()),
          posterior = as.character(utils::packageVersion("posterior"))
        ),
        code_sha256 = list(
          pair_precision = rep_sha256(rep_path("R", "pair_precision.R")),
          posterior_reader = rep_sha256(rep_path("R", "posterior.R")),
          chain_coordinates = rep_sha256(rep_path("R", "reports.R"))
        ),
        published_reference_comparison = comparison,
        reference_match_required = mode == "archived_reference",
        precision_scope = "Criterion .005 misses are diagnostics; weighted-policy MCSE target is separate from HMC, finite-A solver bounds, and fixed-action loss MCSE."
      )
      rep_write_json(metadata, file.path(output_dir, "metadata.json"))
      if (mode == "archived_reference" && !comparison$matches) {
        stop("Archived pair precision differs from the published reference; inspect the preserved outputs.")
      }
      rep_write_json(
        list(
          status = "complete", mode = mode, completed_pools = completed,
          criterion_rows = nrow(criterion), weighted_rows = nrow(weighted),
          reference_matches = comparison$matches,
          precision_status = as.list(stats::setNames(summary$precision_status, summary$pool))
        ),
        file.path(output_dir, "run-status.json")
      )
      invisible(list(summary = summary, metadata = metadata, output_dir = output_dir))
    },
    error = function(e) {
      rep_write_json(list(
        status = "failed", mode = mode, completed_pools = completed,
        error = conditionMessage(e)
      ), file.path(output_dir, "run-status.json"))
      stop(e)
    }
  )
}
