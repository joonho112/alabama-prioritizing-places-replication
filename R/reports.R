# JoonHo Lee (jlee296@ua.edu)
# Select and evaluate the paper's ordered review reports.
# Inputs: separate-criterion posterior bundles and fixed policy settings.
# Outputs: grades, finite-draw certificates, criterionwise losses and readouts.
# These definitions neither run a solver nor construct a scientific joint posterior.

# ---- Named coordinates and the independent reporting-loss calculation --------

rep_report_grade <- function(grade, entity_ids, G_max) {
  if (!is.numeric(grade) || is.null(names(grade)) || anyNA(grade) ||
      any(!is.finite(grade) | grade < 1 | grade != floor(grade)) ||
      anyDuplicated(names(grade)) || !setequal(names(grade), entity_ids) ||
      length(grade) != length(entity_ids) || length(G_max) != 1L ||
      !is.finite(G_max) || G_max < 1 || G_max != floor(G_max)) {
    stop("Grades must be positive integers named for every candidate entity.")
  }
  grade <- stats::setNames(as.integer(grade[entity_ids]), entity_ids)
  if (length(unique(grade)) > G_max) stop("Occupied bands exceed the maximum.")
  grade
}

rep_report_weights <- function(criteria, weights) {
  if (!is.numeric(weights) || is.null(names(weights)) || anyDuplicated(names(weights)) ||
      !setequal(names(weights), criteria) || any(!is.finite(weights) | weights < 0) ||
      abs(sum(weights) - 1) > 1e-12) {
    stop("Use criterion-named nonnegative weights summing to one.")
  }
  weights[criteria]
}

rep_report_relations <- function(grade, entity_ids = names(grade), G_max = length(grade)) {
  grade <- rep_report_grade(grade, entity_ids, G_max)
  n <- length(entity_ids)
  if (n < 2L) stop("A report needs at least two entities.")
  index <- which(upper.tri(matrix(FALSE, n, n)), arr.ind = TRUE)
  relation <- sign(grade[index[, 1L]] - grade[index[, 2L]])
  data.frame(pair_left = entity_ids[index[, 1L]], pair_right = entity_ids[index[, 2L]],
             left_index = index[, 1L], right_index = index[, 2L],
             relation_code = unname(relation), disclosed = unname(relation != 0),
             stringsAsFactors = FALSE)
}

rep_report_metrics <- function(grade, pairwise, weights, tau, G_max) {
  ids <- as.character(pairwise$entity_ids)
  criteria <- as.character(pairwise$criterion_ids)
  e <- pairwise$e
  if (!is.array(e) || !identical(as.integer(dim(e)),
                                 as.integer(c(length(ids), length(ids), length(criteria)))) ||
      !identical(dimnames(e)[[1L]], ids) ||
      !identical(dimnames(e)[[2L]], ids) || !identical(dimnames(e)[[3L]], criteria)) {
    stop("Pairwise array coordinates differ from the declared entities/criteria.")
  }
  off <- row(e[, , 1L]) != col(e[, , 1L])
  for (k in seq_along(criteria)) {
    if (any(!is.finite(e[, , k][off])) || any(e[, , k][off] < 0 | e[, , k][off] > 1)) {
      stop("Invalid off-diagonal directional error coefficient.")
    }
  }
  weights <- rep_report_weights(criteria, weights)
  if (length(tau) != 1L || !is.finite(tau) || tau < 0) stop("Invalid withholding cost.")
  grade <- rep_report_grade(grade, ids, G_max)
  relations <- rep_report_relations(grade, ids, G_max)
  M <- nrow(relations)
  N <- sum(relations$disclosed)
  high <- ifelse(relations$relation_code < 0, relations$left_index, relations$right_index)
  low <- ifelse(relations$relation_code < 0, relations$right_index, relations$left_index)
  disclosed <- which(relations$disclosed)
  index <- cbind(high[disclosed], low[disclosed])
  Ek <- stats::setNames(vapply(seq_along(criteria), function(k) {
    if (N) sum(e[, , k][index]) else 0
  }, numeric(1)), criteria)
  Ew <- sum(weights * Ek)
  # Each unordered pair contributes one wrong-direction cost or one withholding
  # cost. Rtau is an expected reporting cost, not a probability of error.
  risk <- (Ew + tau * (M - N)) / M
  profile <- data.frame(criterion = criteria, weight = unname(weights), Ek = unname(Ek),
                        Ek_over_N = if (N) unname(Ek / N) else NA_real_,
                        Ek_over_M = unname(Ek / M),
                        Rk_tau = unname((Ek + tau * (M - N)) / M))
  list(M = M, N = N, Goccupied = length(unique(grade)), Ek = Ek, Ew = Ew,
       disclosure_fraction = N / M, Ew_over_N = if (N) Ew / N else NA_real_,
       Ew_over_M = Ew / M, Rtau = risk, profile = profile, grade = grade,
       relations = relations, weights = weights, tau = tau, G_max = G_max)
}

# ---- Exact finite-draw rank scores and reproducible baseline cuts ------------

rep_expected_rank_scores <- function(pairwise, weights) {
  ids <- pairwise$entity_ids
  criteria <- pairwise$criterion_ids
  weights <- rep_report_weights(criteria, weights)
  sizes <- as.numeric(pairwise$sample_sizes[criteria])
  names(sizes) <- criteria
  if (any(!is.finite(sizes) | sizes < 1 | sizes != floor(sizes))) {
    stop("Exact expected ranks need integer sample sizes for each criterion.")
  }
  weight_numerators <- 4 * weights
  if (any(weight_numerators != floor(weight_numerators))) {
    stop("The published exact-rank calculation uses quarter weights.")
  }
  gcd <- function(a, b) {
    while (b != 0) {
      remainder <- a %% b
      a <- b
      b <- remainder
    }
    a
  }
  common_size <- 1
  for (size in sizes) {
    factor <- common_size / gcd(common_size, size)
    if (factor >= 2^53 / size) stop("Sample-size LCM exceeds exact integer range.")
    common_size <- factor * size
  }
  denominator <- 4 * common_size
  if (denominator >= 2^53 / length(ids)) stop("Expected-rank numerator exceeds exact range.")
  numerator <- stats::setNames(rep(denominator, length(ids)), ids)
  reconstruction_error <- 0
  floating <- stats::setNames(rep(1, length(ids)), ids)
  for (criterion in criteria) {
    e <- pairwise$e[, , criterion]
    diag(e) <- 0
    if (any(!is.finite(e) | e < 0 | e > 1)) stop("Invalid finite-draw coefficient.")
    counts <- round(e * sizes[criterion])
    error <- max(abs(e * sizes[criterion] - counts))
    if (error > 8 * .Machine$double.eps * sizes[criterion] ||
        max(abs(counts / sizes[criterion] - e)) > 8 * .Machine$double.eps) {
      stop("Coefficients do not reconstruct their integer Monte Carlo event counts.")
    }
    numerator <- numerator + weight_numerators[criterion] *
      (common_size / sizes[criterion]) * rowSums(counts)
    floating <- floating + weights[criterion] * rowSums(e)
    reconstruction_error <- max(reconstruction_error, error)
  }
  if (any(numerator != floor(numerator) | numerator >= 2^53)) {
    stop("Expected-rank arithmetic left the exact integer range.")
  }
  expected <- numerator / denominator
  difference <- max(abs(expected - floating))
  if (inherits(pairwise, "postrank_pairwise")) {
    package <- postrank::postrank_expected_rank(pairwise, weights)
    difference <- max(difference, max(abs(expected - package[ids])))
  }
  if (difference > 1e-12) stop("Integer-count and floating expected ranks disagree.")
  # Use exact numerators as the sorting scores. Equal numerators form indivisible
  # blocks; native draws are never rounded and no epsilon tie rule is introduced.
  list(scores = numerator, numerator = numerator, denominator = denominator,
       expected_rank = expected, lcm_sample_size = common_size, sample_sizes = sizes,
       count_reconstruction_error = reconstruction_error, floating_rank_difference = difference)
}

rep_borda_scores <- function(values, weights) {
  if (!is.matrix(values) || any(!is.finite(values)) || is.null(rownames(values))) {
    stop("Borda inputs must be a finite entity-by-criterion matrix.")
  }
  weights <- rep_report_weights(colnames(values), weights)
  ranks <- vapply(seq_len(ncol(values)), function(k) {
    rank(-values[, k], ties.method = "average")
  }, numeric(nrow(values)))
  stats::setNames(as.numeric(ranks %*% weights), rownames(values))
}

rep_build_report_bundle <- function(universe, model_id, pool, draws_dir = NULL) {
  if (!universe %in% c("county", "tract") || !pool %in% c("A", "B")) {
    stop("Choose county/tract and pool A/B.")
  }
  keys <- if (universe == "county") {
    x <- rep_read_csv(rep_path("data", "analysis", "county-universe.csv"))
    x[tolower(x$county_government_candidate) == "true", , drop = FALSE]
  } else rep_read_csv(rep_path("data", "analysis", "pilot-tract-keys.csv"))
  ids <- sort(keys$entity_id)
  if (length(ids) != if (universe == "county") 64L else 90L) {
    stop("The publication candidate universe changed.")
  }
  input <- rep_posterior_input(model_id, pool, ids, draws_dir)
  matrices <- rep_input_matrices(input)
  coordinates <- rep_input_coordinates(input)
  observations_path <- rep_path("data", "analysis", paste0(universe, "-observations.csv"))
  observations <- rep_read_csv(observations_path)
  index <- match(ids, observations$entity_id)
  if (anyNA(index)) stop("Candidate raw estimates are missing.")
  raw <- vapply(c("income", "incarceration"), function(criterion) {
    as.numeric(observations[[paste0(criterion, "_estimate_fraction")]][index]) *
      if (criterion == "income") -100 else 100
  }, numeric(length(ids)))
  dimnames(raw) <- list(ids, c("income", "incarceration"))
  # Cache by authenticated draw content, named candidates, raw input and package
  # version, not by a temporary directory or the construction timestamp.
  key <- digest::digest(list(universe = universe, model_id = model_id, pool = pool,
                            ids = ids, values = matrices, raw = raw,
                            postrank = as.character(utils::packageVersion("postrank"))),
                        algo = "sha256")
  cache_path <- rep_cache("report-bundles", paste0(key, ".rds"))
  if (file.exists(cache_path)) {
    bundle <- readRDS(cache_path)
    if (!identical(bundle$cache_key, key)) stop("Stale report bundle cache.")
    return(bundle)
  }
  pairwise <- postrank::postrank_pairwise(input)
  bundle <- list(universe = universe, model_id = model_id, pool = pool,
                 entity_ids = ids, input = input, pairwise = pairwise, raw = raw,
                 matrices = matrices, coordinates = coordinates, cache_key = key)
  rep_dir(dirname(cache_path))
  saveRDS(bundle, cache_path, compress = "gzip")
  bundle
}

rep_make_baselines <- function(bundle, weights, tau, G_max) {
  weights <- rep_report_weights(bundle$pairwise$criterion_ids, weights)
  ids <- bundle$pairwise$entity_ids
  raw_scores <- rep_borda_scores(bundle$raw, weights)
  means <- postrank::postrank_posterior_means(bundle$input)[ids, names(weights), drop = FALSE]
  mean_scores <- rep_borda_scores(means, weights)
  expected <- rep_expected_rank_scores(bundle$pairwise, weights)
  settings <- postrank::postrank_settings(weights = weights, tau = tau, G_max = G_max,
                                          objective = "weighted", q = NULL, exact_G = NULL)
  cuts <- list(
    B0 = NULL,
    B1 = postrank::postrank_cuts(raw_scores, bundle$pairwise, settings,
                                 direction = "ascending", rule = "quantile", G_b = G_max, method = "B1"),
    B2 = postrank::postrank_cuts(mean_scores, bundle$pairwise, settings,
                                 direction = "ascending", rule = "quantile", G_b = G_max, method = "B2"),
    B3 = postrank::postrank_cuts(expected$scores, bundle$pairwise, settings,
                                 direction = "ascending", rule = "optimal", method = "B3"))
  reports <- lapply(names(cuts), function(method) {
    grade <- if (method == "B0") stats::setNames(rep(1L, length(ids)), ids) else cuts[[method]]$grade
    list(method = method, grade = grade,
         metrics = rep_report_metrics(grade, bundle$pairwise, weights, tau, G_max),
         certificate = list(status = if (method == "B3") "fixed_order_optimal" else "not_applicable",
                            lower_bound = NULL, upper_bound = NULL, gap = NULL,
                            scope = if (method == "B3") "Optimal cuts within the fixed exact expected-rank order." else "Prespecified report."),
         construction = if (method == "B3") expected else list(rule = method))
  })
  names(reports) <- names(cuts)
  reports
}

rep_select_reports <- function(bundle, weights, tau = .2, G_max = 5L, solver_control = NULL) {
  if (!identical(bundle$pool, "A")) stop("Reports are selected on A only.")
  reports <- rep_make_baselines(bundle, weights, tau, G_max)
  config <- rep_config()$solver
  control <- list(time_limit = config$time_limit, threads = 1L, seed = config$seed,
                  absolute_risk_gap = config$absolute_risk_gap, relative_gap = config$relative_gap,
                  strategy = "staged", output = FALSE)
  if (!is.null(solver_control)) control[names(solver_control)] <- solver_control
  if (control$threads != 1L) stop("The recorded solver uses one thread.")
  problem <- postrank::postrank_problem(bundle$pairwise, weights = weights, tau = tau,
                                        G_max = G_max, objective = "weighted")
  fit <- postrank::postrank_solve(problem, backend = "highs",
                                 control = do.call(postrank::postrank_control, control),
                                 strategy = "staged")
  if (!isTRUE(fit$incumbent_validated)) stop("HiGHS did not return a validated report.")
  grade <- postrank::get_grades(fit)
  risk <- postrank::get_risk(fit)
  metrics <- rep_report_metrics(grade, bundle$pairwise, weights, tau, G_max)
  if (abs(metrics$Rtau - risk$risk) > 1e-10) stop("Solver and independent risk disagree.")
  reports$P <- list(method = "P", grade = grade, metrics = metrics,
                    certificate = list(status = fit$status$normalized,
                                       lower_bound = risk$lower_bound, upper_bound = risk$risk,
                                       gap = risk$gap, validated = TRUE,
                                       gap_target_met = is.finite(risk$gap) &&
                                         risk$gap <= control$absolute_risk_gap + 64 * .Machine$double.eps,
                                       termination_reason = fit$status$termination_reason,
                                       scope = "Finite selection-draw objective only."),
                    construction = list(backend = "highs", control = control))
  reports
}

# ---- Fixed-action Monte Carlo uncertainty within the original chains ---------

rep_chain_matrix <- function(values, coordinates) {
  if (!is.numeric(values) || !length(values) || any(!is.finite(values)) ||
      !all(c("chain", "iteration") %in% names(coordinates)) ||
      nrow(coordinates) != length(values)) stop("Invalid chain trace coordinates.")
  chain <- as.character(coordinates$chain)
  iteration_text <- as.character(coordinates$iteration)
  if (anyNA(iteration_text) || any(!grepl("^[0-9]+$", iteration_text))) {
    stop("Actual Stan iterations must be positive integers.")
  }
  iteration <- as.integer(iteration_text)
  if (anyNA(chain) || any(!nzchar(chain)) || any(iteration < 1L) || anyNA(iteration) || anyDuplicated(paste(chain, iteration))) {
    stop("Missing or duplicated chain/iteration coordinates.")
  }
  levels <- unique(chain)
  index <- lapply(levels, function(id) which(chain == id))
  grids <- lapply(index, function(i) iteration[i])
  if (length(unique(lengths(index))) != 1L ||
      any(vapply(grids, function(x) any(diff(x) != 1L), logical(1))) ||
      !all(vapply(grids, identical, logical(1), grids[[1L]]))) {
    stop("Chains need balanced, contiguous, chronological iteration grids.")
  }
  matrix <- do.call(cbind, lapply(index, function(i) values[i]))
  dimnames(matrix) <- list(as.character(grids[[1L]]), levels)
  matrix
}

rep_chain_mcse <- function(values, coordinates, deterministic = FALSE) {
  rep_require("posterior")
  matrix <- rep_chain_matrix(as.numeric(values), coordinates)
  constant <- all(matrix == matrix[1L])
  if (deterministic && !constant) stop("A nonconstant trace cannot be analytically constant.")
  messages <- character()
  estimate <- function(fun) {
    tryCatch(withCallingHandlers(as.numeric(fun(matrix)), warning = function(warning) {
      messages <<- c(messages, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }), error = function(error) {
      messages <<- c(messages, conditionMessage(error))
      NA_real_
    })
  }
  if (constant) {
    mcse <- if (deterministic) 0 else NA_real_
    ess <- rhat <- NA_real_
    status <- if (deterministic) "analytically_deterministic" else "constant_trace_mcse_unidentified"
  } else if (nrow(matrix) < 4L) {
    mcse <- ess <- rhat <- NA_real_
    status <- "insufficient_retained_iterations"
  } else {
    mcse <- estimate(posterior::mcse_mean)
    ess <- estimate(posterior::ess_mean)
    rhat <- estimate(posterior::rhat)
    status <- if (is.finite(mcse) && mcse >= 0) "estimated_chain_preserving" else "nonconstant_mcse_unavailable"
  }
  list(mean = mean(matrix), mcse = mcse, ess_mean = ess, rhat = rhat,
       constant = constant, mcse_status = status, n_draws = length(matrix),
       n_chains = ncol(matrix), iterations_per_chain = nrow(matrix),
       warnings = unique(messages))
}

rep_criterion_loss <- function(grade, values, tau, G_max) {
  relation <- rep_report_relations(grade, colnames(values), G_max)
  M <- nrow(relation)
  disclosed <- which(relation$disclosed)
  loss <- rep(tau * (M - length(disclosed)) / M, nrow(values))
  high <- ifelse(relation$relation_code < 0, relation$left_index, relation$right_index)
  low <- ifelse(relation$relation_code < 0, relation$right_index, relation$left_index)
  # Small pair blocks avoid allocating a draw-by-all-pairs temporary matrix.
  if (length(disclosed)) {
    blocks <- split(disclosed, ceiling(seq_along(disclosed) / 64L))
    for (index in blocks) {
      errors <- values[, high[index], drop = FALSE] <= values[, low[index], drop = FALSE]
      loss <- loss + rowSums(errors) / M
    }
  }
  list(values = loss, analytically_constant = length(disclosed) == 0L,
       M = M, N = length(disclosed), relations = relation$relation_code)
}

rep_independent_mcse <- function(estimates, weights) {
  se <- vapply(estimates[names(weights)], function(x) x$mcse, numeric(1))
  active <- weights > 0
  if (any(!is.finite(se[active]))) return(NA_real_)
  # This combines independent computational RNG stream variances only. It does
  # not create a weighted risk draw or assert a scientific joint posterior.
  sqrt(sum((weights[active] * se[active])^2))
}

rep_precision_status <- function(mcse, target) {
  if (!is.finite(mcse)) "MCSE_unavailable" else if (mcse <= target) "estimated_target_met" else "precision_limited"
}

rep_relation_comparison <- function(current, reference, G_max = max(length(current), length(reference))) {
  ids <- names(reference)
  a <- rep_report_relations(current, ids, G_max)$relation_code
  b <- rep_report_relations(reference, ids, G_max)$relation_code
  list(relation_changes = sum(a != b), relation_agreement_fraction = mean(a == b),
       tie_to_disclosure = sum(b == 0 & a != 0),
       disclosure_to_tie = sum(b != 0 & a == 0),
       direction_reversals = sum(a * b == -1), identical_relations = all(a == b))
}

rep_evaluate_fixed <- function(reports, bundle, weights, tau = .2, G_max = 5L,
                               selection_bundle = NULL, reference = "P") {
  if (!identical(bundle$pool, "B") ||
      (!is.null(selection_bundle) && !identical(selection_bundle$pool, "A"))) {
    stop("Fixed evaluation requires pool B and, if supplied, an A selection bundle.")
  }
  if (!identical(reference, "P") || !reference %in% names(reports)) {
    stop("This compendium reports fixed-method comparisons against P.")
  }
  criteria <- bundle$pairwise$criterion_ids
  weights <- rep_report_weights(criteria, weights)
  metrics <- diagnostics <- list()
  traces <- lapply(bundle$coordinates, function(x) x[, c("chain", "iteration"), drop = FALSE])
  estimates <- relations <- list()
  for (method in names(reports)) {
    grade <- reports[[method]]$grade
    metric <- rep_report_metrics(grade, bundle$pairwise, weights, tau, G_max)
    relations[[method]] <- metric$relations$relation_code
    estimates[[method]] <- vector("list", length(criteria))
    names(estimates[[method]]) <- criteria
    for (criterion in criteria) {
      loss <- rep_criterion_loss(grade, bundle$matrices[[criterion]], tau, G_max)
      estimate <- rep_chain_mcse(loss$values, bundle$coordinates[[criterion]], loss$analytically_constant)
      expected <- metric$profile$Rk_tau[metric$profile$criterion == criterion]
      if (abs(estimate$mean - expected) > 1e-12) stop("Direct trace and coefficient risk differ.")
      estimates[[method]][[criterion]] <- estimate
      traces[[criterion]][[method]] <- loss$values
      diagnostics[[paste(method, criterion)]] <- data.frame(
        method = method, criterion = criterion, mean = estimate$mean, mcse = estimate$mcse,
        ess_mean = estimate$ess_mean, rhat = estimate$rhat, constant_trace = estimate$constant,
        analytically_deterministic = loss$analytically_constant,
        mcse_status = estimate$mcse_status, n_draws = estimate$n_draws,
        n_chains = estimate$n_chains, iterations_per_chain = estimate$iterations_per_chain)
    }
    A_risk <- if (is.null(selection_bundle)) NA_real_ else
      rep_report_metrics(grade, selection_bundle$pairwise, weights, tau, G_max)$Rtau
    certificate <- reports[[method]]$certificate
    number <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
    se <- rep_independent_mcse(estimates[[method]], weights)
    metrics[[method]] <- data.frame(
      universe = bundle$universe, method = method, n_entities = length(bundle$entity_ids),
      M = metric$M, N = metric$N, Goccupied = metric$Goccupied,
      disclosure_fraction = metric$disclosure_fraction, tau = tau, G_max = G_max,
      weight_income = unname(weights["income"]), weight_incarceration = unname(weights["incarceration"]),
      A_saved_own_policy_Rtau = A_risk, B_Ew = metric$Ew,
      B_Ew_over_N = metric$Ew_over_N, B_Ew_over_M = metric$Ew_over_M,
      B_Rtau = metric$Rtau, B_Rtau_mcse = se,
      fixed_B_mcse_target = rep_config()$precision$fixed_risk_mcse_target,
      fixed_B_precision_status = rep_precision_status(se, rep_config()$precision$fixed_risk_mcse_target),
      A_solver_gap = number(certificate$gap),
      A_solver_gap_target_met = if (is.null(certificate$gap)) NA else
        is.finite(number(certificate$gap)) && number(certificate$gap) <=
          rep_config()$solver$absolute_risk_gap + 64 * .Machine$double.eps,
      A_solver_primal_validated = if (is.null(certificate$validated)) NA else
        isTRUE(certificate$validated),
      A_solver_upper_bound = number(certificate$upper_bound),
      A_solver_lower_bound = number(certificate$lower_bound),
      A_solver_status = if (is.null(certificate$status)) NA_character_ else certificate$status,
      A_optimality_scope = if (is.null(certificate$scope)) NA_character_ else certificate$scope,
      stringsAsFactors = FALSE)
  }
  paired <- list()
  for (method in names(reports)) {
    same <- identical(relations[[method]], relations[[reference]])
    delta_estimates <- vector("list", length(criteria))
    names(delta_estimates) <- criteria
    for (criterion in criteria) {
      delta <- traces[[criterion]][[method]] - traces[[criterion]][[reference]]
      if (same) {
        if (max(abs(delta)) > 1e-12) stop("Identical report relations have different losses.")
        delta[] <- 0
      }
      delta_estimates[[criterion]] <- rep_chain_mcse(delta, bundle$coordinates[[criterion]], same)
    }
    difference <- sum(weights * vapply(delta_estimates, function(x) x$mean, numeric(1)))
    se <- rep_independent_mcse(delta_estimates, weights)
    metrics[[method]]$B_difference_vs_P <- difference
    metrics[[method]]$B_paired_mcse_vs_P <- se
    metrics[[method]]$paired_mcse_target <- rep_config()$precision$paired_mcse_target
    metrics[[method]]$paired_precision_status <- rep_precision_status(se, rep_config()$precision$paired_mcse_target)
    metrics[[method]]$identical_relations_to_P <- same
    metrics[[method]]$relation_changes_vs_P <- sum(relations[[method]] != relations[[reference]])
    paired[[method]] <- list(reference = reference, difference = difference, mcse = se,
                             identical_relations = same, criterion_estimates = delta_estimates)
  }
  out <- do.call(rbind, metrics)
  rownames(out) <- NULL
  list(metrics = out, criterion_traces = traces,
       criterion_diagnostics = do.call(rbind, diagnostics), paired_differences = paired,
       reference_method = reference, scientific_cross_criterion_joint = FALSE,
       no_total_risk_joint_draw_trace = TRUE)
}

# ---- Whole-band capacity and the eight prespecified tract comparisons --------

rep_capacity_readout <- function(grade, keys, K, universe, method = "P", case_id = "center") {
  if (!all(c("entity_id", "county_id") %in% names(keys)) || anyDuplicated(keys$entity_id)) {
    stop("Capacity context keys must identify entities/counties uniquely.")
  }
  grade <- rep_report_grade(grade, names(grade), length(grade))
  if (any(!names(grade) %in% keys$entity_id) || any(!is.finite(K) | K < 1 | K != floor(K))) {
    stop("Invalid capacity or report entity absent from context keys.")
  }
  levels <- sort(unique(grade))
  sizes <- vapply(levels, function(g) sum(grade == g), integer(1))
  ends <- cumsum(sizes)
  starts <- ends - sizes + 1L
  matched <- match(keys$entity_id, names(grade))
  original <- as.integer(grade[matched])
  band <- match(original, levels)
  rows <- lapply(K, function(capacity) {
    state <- ifelse(ends <= capacity, "core",
                    ifelse(starts <= capacity & ends > capacity, "boundary", "outside"))
    status <- rep("not_in_decision_universe", nrow(keys))
    inside <- !is.na(matched)
    status[inside] <- state[band[inside]]
    data.frame(universe = universe, case_id = case_id, method = method,
               entity_id = keys$entity_id, county_id = keys$county_id,
               review_capacity_K = capacity, grade_original = original,
               capacity_status = status, band_size = sizes[band],
               band_position_min = starts[band], band_position_max = ends[band],
               boundary_unsplit = TRUE, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

rep_stability_readout <- function(cases, keys) {
  selected <- cases[vapply(cases, function(x) isTRUE(x$include_in_stability), logical(1))]
  if (length(selected) != 8L || any(vapply(selected, function(x) x$kind == "exploratory", logical(1)))) {
    stop("Stability uses exactly eight valid prespecified tract cases, excluding direction050.")
  }
  central <- cases[["tract-center"]]$reports$P$grade
  ids <- names(central)
  reference <- sign(outer(central, central, "-"))
  rows <- lapply(selected, function(case) {
    grade <- rep_report_grade(case$reports$P$grade, ids, case$G_max)
    relation <- sign(outer(grade, grade, "-"))
    counts <- lapply(seq_along(ids), function(i) {
      other <- setdiff(seq_along(ids), i)
      a <- relation[i, other]
      b <- reference[i, other]
      data.frame(entity_id = ids[i], tract_id = ids[i],
                 county_id = keys$county_id[match(ids[i], keys$entity_id)],
                 case_id = case$scenario_id,
                 case_kind = if (case$kind == "posterior") "model" else case$kind,
                 scientifically_ready = TRUE, grade_original = grade[i],
                 relation_profile_agreement = mean(a == b),
                 exact_relation_profile_preserved = all(a == b),
                 same_disclosed_direction = sum(a == b & b != 0),
                 same_withheld = sum(a == 0 & b == 0),
                 direction_reversals = sum(a * b == -1),
                 disclosure_to_tie = sum(b != 0 & a == 0),
                 tie_to_disclosure = sum(b == 0 & a != 0), row.names = NULL)
    })
    out <- do.call(rbind, counts)
    capacity <- rep_capacity_readout(grade, keys, c(5L, 10L), "tract", "P", case$scenario_id)
    for (K in c(5L, 10L)) {
      z <- capacity[capacity$review_capacity_K == K, , drop = FALSE]
      out[[paste0("capacity_K", K)]] <- z$capacity_status[match(ids, z$entity_id)]
    }
    out
  })
  per_case <- do.call(rbind, rows)
  rownames(per_case) <- NULL
  summary <- do.call(rbind, lapply(ids, function(id) {
    x <- per_case[per_case$entity_id == id, , drop = FALSE]
    data.frame(entity_id = id, valid_cases = nrow(x),
               mean_relation_profile_agreement = mean(x$relation_profile_agreement),
               exact_profile_preserved_cases = sum(x$exact_relation_profile_preserved),
               core_cases_K5 = sum(x$capacity_K5 == "core"),
               boundary_cases_K5 = sum(x$capacity_K5 == "boundary"),
               core_cases_K10 = sum(x$capacity_K10 == "core"),
               boundary_cases_K10 = sum(x$capacity_K10 == "boundary"))
  }))
  list(per_case = per_case, summary = summary,
       interpretation = "Empirical agreement across eight settings, not posterior probabilities.")
}

# ---- Published scenarios and explicit analysis execution ---------------------

rep_report_scenarios <- function() {
  cases <- rep_read_csv(rep_path("config", "scenarios.csv"))
  integer_columns <- c("order", "G_max", "n_entities", "solver_seed")
  numeric_columns <- c("weight_income", "weight_incarceration", "tau", "solver_time_limit")
  for (column in c(integer_columns, numeric_columns)) {
    if (!column %in% names(cases)) stop("Scenario settings lack ", column)
    cases[[column]] <- as.numeric(cases[[column]])
    if (any(!is.finite(cases[[column]]))) stop("Invalid scenario setting: ", column)
  }
  for (column in integer_columns) {
    if (any(cases[[column]] != floor(cases[[column]]))) stop("Noninteger scenario setting.")
    cases[[column]] <- as.integer(cases[[column]])
  }
  cases$include_in_tract_8case_stability <-
    tolower(cases$include_in_tract_8case_stability) == "true"
  if (anyDuplicated(cases$case_id) || nrow(cases) != 14L ||
      sum(cases$include_in_tract_8case_stability) != 8L) {
    stop("The publication scenario inventory must contain fourteen cases and eight stability cases.")
  }
  cases[order(cases$order), , drop = FALSE]
}

rep_saved_report_cases <- function() {
  cases <- rep_read_json(rep_path("data", "saved", "review-reports.json"),
                         simplifyVector = FALSE)$cases
  for (id in names(cases)) {
    case <- cases[[id]]
    case$weights <- unlist(case$weights, use.names = TRUE)
    case$G_max <- as.integer(case$G_max)
    for (method in names(case$reports)) {
      grade <- unlist(case$reports[[method]]$grade, use.names = TRUE)
      case$reports[[method]]$grade <- stats::setNames(as.integer(grade), names(grade))
    }
    cases[[id]] <- case
  }
  cases
}

rep_export_report_cases <- function(cases) {
  # JSON arrays do not preserve names on R atomic vectors. Use named lists for
  # grades and weights so every geographic identifier remains explicit.
  lapply(cases, function(case) {
    case$weights <- as.list(case$weights)
    case$reports <- lapply(case$reports, function(report) {
      list(method = report$method, grade = as.list(report$grade),
           certificate = report$certificate)
    })
    case
  })
}

rep_run_review_reports <- function(mode = c("saved-only", "reselect"), scenarios = NULL,
                                    draws_dir = NULL, output_dir = NULL) {
  mode <- match.arg(mode)
  if (is.null(output_dir)) output_dir <- rep_output("review-reports", mode)
  config <- rep_report_scenarios()
  if (!is.null(scenarios) &&
      any(!scenarios %in% unique(c(config$case_id, config$scenario_id)))) {
    stop("Unknown scenario selector: ", paste(setdiff(scenarios,
         unique(c(config$case_id, config$scenario_id))), collapse = ", "))
  }
  selected <- if (is.null(scenarios)) rep(TRUE, nrow(config)) else
    config$case_id %in% scenarios | config$scenario_id %in% scenarios
  if (!any(selected)) stop("No requested scenario matches the published cases.")
  # The center report is needed for same-reference model comparisons. A restricted
  # run therefore includes the corresponding center explicitly in its output.
  universes <- unique(config$universe[selected])
  selected <- selected | config$case_id %in% paste0(universes, "-center")
  config <- config[selected, , drop = FALSE]
  saved <- rep_saved_report_cases()
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
    stop("Report output already contains files; choose a new run directory: ", output_dir)
  }
  output_dir <- rep_dir(output_dir)
  completed <- evaluations <- list()
  finished <- FALSE
  lock <- rep_acquire_lock(paste("review-reports", mode))
  on.exit({
    if (!finished) rep_write_json(list(status = "failed", mode = mode,
      completed_cases = names(completed),
      note = "The run did not complete; inspect captured stderr for the error or interruption."), file.path(output_dir, "run-status.json"))
    rep_release_lock(lock)
  }, add = TRUE)
  metric_rows <- capacity_rows <- sensitivity_rows <- list()
  for (i in seq_len(nrow(config))) {
    row <- config[i, , drop = FALSE]
    id <- row$case_id
    weights <- c(income = row$weight_income, incarceration = row$weight_incarceration)
    A <- rep_build_report_bundle(row$universe, row$model_id, row$selection_pool, draws_dir)
    B <- rep_build_report_bundle(row$universe, row$evaluation_model, row$evaluation_pool, draws_dir)
    if (!identical(A$entity_ids, B$entity_ids)) stop("Selection/evaluation candidates differ.")
    if (mode == "saved-only") {
      original <- saved[[id]]
      if (is.null(original) || abs(original$tau - row$tau) > 1e-12 ||
          max(abs(original$weights[names(weights)] - weights)) > 1e-12 ||
          original$G_max != row$G_max || original$selection_model != row$model_id ||
          original$evaluation_model != row$evaluation_model) {
        stop("Saved reports and scenario configuration differ for ", id)
      }
      reports <- original$reports
      for (method in names(reports)) {
        reports[[method]]$method <- method
        if (is.null(reports[[method]]$certificate)) {
          reports[[method]]$certificate <- list(
            status = if (method == "B3") "fixed_order_optimal" else "not_applicable",
            lower_bound = NULL, upper_bound = NULL, gap = NULL,
            scope = if (method == "B3")
              "Optimal cuts within the fixed expected-rank order and this case admissibility constraints." else
              "Prespecified fixed report.")
        }
        reports[[method]]$grade <- rep_report_grade(reports[[method]]$grade, A$entity_ids, row$G_max)
      }
    } else {
      solver_control <- list(seed = row$solver_seed, time_limit = row$solver_time_limit)
      if (row$kind == "exploratory") {
        if (!exists("rep_direction_reports", mode = "function")) {
          stop("Load R/direction.R for the direction050 case.")
        }
        reports <- rep_direction_reports(A, weights, row$tau, row$G_max, solver_control)
      } else reports <- rep_select_reports(A, weights, row$tau, row$G_max, solver_control)
    }
    evaluated <- rep_evaluate_fixed(reports, B, weights, row$tau, row$G_max, A)
    evaluated$metrics$case_id <- id
    evaluated$metrics$scenario_id <- row$scenario_id
    evaluated$metrics$scenario_kind <- row$kind
    evaluated$metrics$B3_P_same_relations <- rep_relation_comparison(
      reports$B3$grade, reports$P$grade, row$G_max)$identical_relations
    evaluated$metrics$selection_model <- row$model_id
    evaluated$metrics$evaluation_model <- row$evaluation_model
    evaluated$metrics$comparison_scope <- if (row$kind == "posterior")
      "Stress-A fixed action evaluated in the common main-model B." else
      "Own-policy fixed-action evaluation in B; different policies have different objectives."
    metric_rows[[id]] <- evaluated$metrics
    completed[[id]] <- list(case_id = id, scenario_id = row$scenario_id, kind = row$kind,
                             universe = row$universe, selection_model = row$model_id,
                             selection_pool = "A", evaluation_model = row$evaluation_model,
                             evaluation_pool = "B", weights = weights, tau = row$tau,
                             solver_seed = row$solver_seed, solver_time_limit = row$solver_time_limit,
                             solver_configuration_role = if (mode == "reselect")
                               "Actual selection controls" else "Recorded original selection controls",
                             selection_bundle_key = A$cache_key, evaluation_bundle_key = B$cache_key,
                             G_max = row$G_max,
                             include_in_stability = row$include_in_tract_8case_stability,
                             reports = reports)
    evaluations[[id]] <- evaluated
    keys <- if (row$universe == "county")
      rep_read_csv(rep_path("data", "analysis", "county-universe.csv")) else
      rep_read_csv(rep_path("data", "analysis", "pilot-tract-keys.csv"))
    capacities <- if (row$universe == "county") c(3L, 5L) else c(5L, 10L)
    for (method in names(reports)) {
      capacity_rows[[paste(id, method)]] <- rep_capacity_readout(
        reports[[method]]$grade, keys, capacities, row$universe, method, row$scenario_id)
    }
    center_id <- paste0(row$universe, "-center")
    center <- completed[[center_id]]
    comparison <- rep_relation_comparison(reports$P$grade, center$reports$P$grade, row$G_max)
    P <- evaluated$metrics[evaluated$metrics$method == "P", , drop = FALSE]
    P$P_vs_centerP_tie_to_disclosure <- comparison$tie_to_disclosure
    P$P_vs_centerP_disclosure_to_tie <- comparison$disclosure_to_tie
    P$P_vs_centerP_direction_reversals <- comparison$direction_reversals
    P$P_vs_centerP_relation_changes <- comparison$relation_changes
    P$P_vs_centerP_relation_agreement_fraction <- comparison$relation_agreement_fraction
    # Both fixed actions are evaluated under this row's current weights and
    # withholding cost. This is a common-objective contrast even when the two
    # reports were selected under different policies; native-policy risk levels
    # from different rows must never be subtracted instead.
    contrast <- list(alternative = reports$P, P = center$reports$P)
    paired <- rep_evaluate_fixed(contrast, B, weights, row$tau, row$G_max)$paired_differences$alternative
    P$B_delta_vs_center_same_objective <- paired$difference
    P$B_delta_paired_mcse <- paired$mcse
    P$B_delta_MC_scope <- "Both fixed actions on identical main B streams, evaluated with this row's weights/tau."
    sensitivity_rows[[id]] <- P
    folder <- rep_dir(file.path(output_dir, id))
    rep_write_json(rep_export_report_cases(completed[id])[[1L]],
                   file.path(folder, "review-report.json"))
    rep_write_csv(evaluated$metrics, file.path(folder, "metrics.csv"))
    rep_write_csv(evaluated$criterion_diagnostics, file.path(folder, "criterion-diagnostics.csv"))
    saveRDS(evaluated$criterion_traces, file.path(folder, "criterion-traces.rds"), compress = "gzip")
    rep_write_json(list(status = "running", mode = mode, completed_cases = names(completed),
                         requested_cases = config$case_id), file.path(output_dir, "run-status.json"))
    cat("Completed", id, "[", mode, "]\n")
    rm(A, B)
    invisible(gc())
  }
  metrics <- do.call(rbind, metric_rows)
  capacity <- do.call(rbind, capacity_rows)
  sensitivity <- do.call(rbind, sensitivity_rows)
  rownames(metrics) <- rownames(capacity) <- rownames(sensitivity) <- NULL
  rep_write_csv(metrics, file.path(output_dir, "report-metrics.csv"))
  rep_write_csv(capacity, file.path(output_dir, "capacity-status.csv"))
  rep_write_csv(sensitivity, file.path(output_dir, "sensitivity.csv"))
  stability <- NULL
  if (sum(vapply(completed, function(x) isTRUE(x$include_in_stability), logical(1))) == 8L) {
    keys <- rep_read_csv(rep_path("data", "analysis", "pilot-tract-keys.csv"))
    stability <- rep_stability_readout(completed, keys)
    rep_write_csv(stability$per_case, file.path(output_dir, "tract-stability-cases.csv"))
    rep_write_csv(stability$summary, file.path(output_dir, "tract-stability-summary.csv"))
  }
  rep_write_json(list(schema_version = 1L, author = "JoonHo Lee (jlee296@ua.edu)",
                       mode = mode, cases = rep_export_report_cases(completed)),
                   file.path(output_dir, "review-reports.json"))
  rep_write_json(list(status = "complete", mode = mode, completed_cases = names(completed),
                       case_count = length(completed), method_count = nrow(metrics),
                       no_scientific_joint_or_total_risk_trace = TRUE,
                       tract_stability_complete = !is.null(stability)),
                   file.path(output_dir, "run-status.json"))
  finished <- TRUE
  invisible(list(cases = completed, evaluations = evaluations, metrics = metrics,
                  capacity = capacity, sensitivity = sensitivity, stability = stability))
}
