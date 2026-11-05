# JoonHo Lee (jlee296@ua.edu)
# Exploratory direction constraint: do not disclose a direction whose weighted
# posterior error probability exceeds .5. The original reporting loss is retained.
# A dynamic program handles the fixed order; an explicit integer model uses
# HiGHS for the free partition. Neither calls private postrank engine functions.

rep_direction_errors <- function(pairwise, weights) {
  weights <- weights[pairwise$criterion_ids]
  n <- length(pairwise$entity_ids)
  error <- matrix(0, n, n, dimnames = list(pairwise$entity_ids, pairwise$entity_ids))
  for (k in pairwise$criterion_ids) {
    e <- pairwise$e[, , k]
    diag(e) <- 0
    error <- error + weights[k] * e
  }
  error
}

rep_direction_feasible <- function(grade, error) {
  g <- grade[rownames(error)]
  !any(outer(g, g, "<") & (error > .5 + 1e-12))
}

rep_direction_cuts <- function(scores, error, tau, G_max = 5L) {
  # Equal scores are indivisible blocks. Each transition appends one tied band:
  # its within-band cost is tau and all previous bands disclose earlier priority.
  ids <- rownames(error)
  scores <- scores[ids]
  levels <- sort(unique(scores))
  blocks <- lapply(levels, function(s) ids[scores == s])
  B <- length(blocks)
  dp <- matrix(Inf, G_max + 1L, B + 1L)
  previous <- matrix(NA_integer_, G_max + 1L, B + 1L)
  dp[1L, 1L] <- 0
  for (g in seq_len(G_max)) {
    for (r in seq_len(B)) {
      for (left in seq_len(r)) {
        if (!is.finite(dp[g, left])) next
        current <- unlist(blocks[left:r], use.names = FALSE)
        before <- if (left == 1L) character() else unlist(blocks[seq_len(left - 1L)], use.names = FALSE)
        if (length(before) && any(error[before, current, drop = FALSE] > .5 + 1e-12)) next
        within <- tau * length(current) * (length(current) - 1L) / 2
        between <- if (length(before)) sum(error[before, current, drop = FALSE]) else 0
        cost <- dp[g, left] + within + between
        if (cost < dp[g + 1L, r + 1L] - 1e-12) {
          dp[g + 1L, r + 1L] <- cost
          previous[g + 1L, r + 1L] <- left
        }
      }
    }
  }
  g <- which.min(dp[-1L, B + 1L])
  optimum <- dp[g + 1L, B + 1L]
  grade <- stats::setNames(integer(length(ids)), ids)
  right <- B
  while (g > 0L) {
    left <- previous[g + 1L, right + 1L]
    grade[unlist(blocks[left:right], use.names = FALSE)] <- g
    right <- left - 1L
    g <- g - 1L
  }
  stopifnot(all(grade > 0L), rep_direction_feasible(grade, error))
  list(grade = grade, objective = optimum / choose(length(ids), 2L),
       scope = "Optimal admissible cuts of the fixed exact expected-rank order.")
}

rep_direction_mip <- function(error, tau, G_max = 5L, control = NULL,
                              feasible_grade = NULL) {
  rep_require(c("Matrix", "highs"))
  n <- nrow(error)
  ids <- rownames(error)
  pairs <- which(upper.tri(error), arr.ind = TRUE)
  M <- nrow(pairs)
  # g_i are integer bands; d_ij and d_ji encode disclosed directions.
  # If neither direction is disclosed, the band difference is zero. The two
  # inequalities describe the convex hull of each pair's three possible states:
  #   d_ij=1: 1 <= g_j-g_i <= G-1; d_ji=1: the reverse; both zero: a tie.
  # This stronger relaxation retains exactly the same integer reports.
  forward <- n + seq_len(M)
  reverse <- n + M + seq_len(M)
  variable_count <- n + 2L * M
  row_index <- column_index <- integer(10L * M)
  value <- numeric(10L * M)
  position <- 0L
  put <- function(r, cols, coefficients) {
    index <- position + seq_along(cols)
    row_index[index] <<- r
    column_index[index] <<- cols
    value[index] <<- coefficients
    position <<- position + length(cols)
  }
  rhs <- numeric(3L * M)
  for (p in seq_len(M)) {
    i <- pairs[p, 1L]; j <- pairs[p, 2L]
    d <- forward[p]; r <- reverse[p]; row <- 3L * (p - 1L)
    put(row + 1L, c(i, j, d, r), c(1, -1, 1, 1 - G_max))
    put(row + 2L, c(j, i, d, r), c(1, -1, 1 - G_max, 1))
    put(row + 3L, c(d, r), c(1, 1))
    rhs[row + 1:3] <- c(0, 0, 1)
  }
  A <- Matrix::sparseMatrix(i = row_index, j = column_index, x = value,
                            dims = c(3L * M, variable_count))
  objective <- c(rep(0, n), (error[pairs] - tau) / M,
                 (error[cbind(pairs[, 2L], pairs[, 1L])] - tau) / M)
  lower <- c(rep(1, n), rep(0, 2L * M))
  upper <- c(rep(G_max, n), as.numeric(error[pairs] <= .5 + 1e-12),
             as.numeric(error[cbind(pairs[, 2L], pairs[, 1L])] <= .5 + 1e-12))
  # The all-tied report is always feasible. A valid fixed-order report may give
  # a tighter objective upper bound without removing any global optimum.
  if (!is.null(feasible_grade)) {
    stopifnot(rep_direction_feasible(feasible_grade, error))
    g <- feasible_grade[ids]
    direction <- c(as.numeric(g[pairs[, 1L]] < g[pairs[, 2L]]),
                   as.numeric(g[pairs[, 2L]] < g[pairs[, 1L]]))
    incumbent_cost <- sum(objective * c(g, direction))
    A <- rbind(A, Matrix::Matrix(objective, nrow = 1L, sparse = TRUE))
    rhs <- c(rhs, incumbent_cost + 1e-12)
  }
  defaults <- rep_config()$solver
  control <- if (is.null(control)) defaults else utils::modifyList(defaults, control)
  options <- highs::highs_control(threads = 1L, time_limit = as.numeric(control$time_limit),
    random_seed = as.integer(control$seed), mip_abs_gap = as.numeric(control$absolute_risk_gap),
    mip_rel_gap = 0, output_flag = FALSE, log_to_console = FALSE)
  started <- proc.time()[["elapsed"]]
  fit <- highs::highs_solve(L = objective, lower = lower, upper = upper,
    A = A, lhs = rep(-Inf, nrow(A)), rhs = rhs,
    types = rep("I", variable_count), offset = tau, control = options)
  elapsed <- proc.time()[["elapsed"]] - started
  feasible <- isTRUE(fit$info$valid) && identical(fit$info$primal_solution_status, "Feasible")
  if (!feasible || is.null(fit$primal_solution)) stop("No feasible constrained-report incumbent.")
  raw_grade <- fit$primal_solution[seq_len(n)]
  if (max(abs(raw_grade - round(raw_grade))) > 1e-6) stop("Noninteger report incumbent.")
  grade <- stats::setNames(as.integer(round(raw_grade)), ids)
  grade <- stats::setNames(match(grade, sort(unique(grade))), ids)
  if (!rep_direction_feasible(grade, error) || length(unique(grade)) > G_max) {
    stop("Constrained-report incumbent failed independent validation.")
  }
  # Independently evaluate the reporting loss, including withholding.
  relation <- outer(grade, grade, "<")
  disclosed <- sum(relation)
  risk <- (sum(error[relation]) + tau * (M - disclosed)) / M
  if (abs(risk - fit$objective_value) > 1e-8) stop("MIP objective differs from report loss.")
  bound <- as.numeric(fit$info$mip_dual_bound)
  if (length(bound) != 1L || !is.finite(bound) || bound > risk + 1e-8) {
    stop("Constrained-report dual bound is unavailable or inconsistent.")
  }
  list(grade = grade,
       certificate = list(status = fit$status_message, lower_bound = bound,
         upper_bound = risk, gap = max(0, risk - bound), validated = TRUE,
         gap_target_met = max(0, risk - bound) <= control$absolute_risk_gap + 1e-12,
         termination_code = fit$status,
         seconds = elapsed, scope = "Finite selection-draw constrained objective only."))
}

rep_direction_reports <- function(bundle, weights, tau = .2, G_max = 5L,
                                   solver_control = NULL) {
  if (!identical(bundle$pool, "A")) stop("Select direction-constrained reports using pool A only.")
  error <- rep_direction_errors(bundle$pairwise, weights)
  scores <- rep_expected_rank_scores(bundle$pairwise, weights)$scores
  fixed <- rep_direction_cuts(scores, error, tau, G_max)
  free <- rep_direction_mip(error, tau, G_max, solver_control, fixed$grade)
  reports <- list(
    B3 = list(method = "B3", grade = fixed$grade,
      certificate = list(status = "fixed_order_optimal", lower_bound = NULL,
        upper_bound = fixed$objective, gap = NULL, validated = TRUE, scope = fixed$scope),
      construction = list(rule = "Admissible optimal cuts of exact expected rank")),
    P = list(method = "P", grade = free$grade, certificate = free$certificate,
      construction = list(rule = "Direction-constrained common report")))
  for (method in names(reports)) {
    reports[[method]]$metrics <- rep_report_metrics(reports[[method]]$grade,
      bundle$pairwise, weights, tau, G_max)
  }
  reports
}
