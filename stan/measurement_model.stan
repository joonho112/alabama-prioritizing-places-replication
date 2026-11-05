// JoonHo Lee (jlee296@ua.edu)
// Measurement models for the published Alabama Opportunity Atlas estimates.
//
// Fit income and incarceration separately. Each observation is a published
// estimate y with a known standard error se, both on the native fraction scale.
// Negative privacy-noisy observations are valid Normal-likelihood inputs.
// The latent probability/mean target is bounded; the noisy observation is not.
//
// Three data-selected latent representations retain the models and numerical
// parameterizations used in the paper:
//   1: single-level non-centered (county M1 and tract TS1);
//   2: tract logits with the county intercept integrated analytically;
//   3: explicit non-centered county and tract effects (TS2/TS3 incarceration).
// Inactive parameter vectors have length zero, rather than unidentified values.
//
// R applies the TS2 standard-error multiplier once before calling Stan.
// The need orientation (-100 income, +100 incarceration) belongs to the later
// reporting calculation and is deliberately absent from this model.

functions {
  // Integrating u_c ~ Normal(0, sigma_county) in
  // eta_i = alpha + u_c + sigma_tract * z_i gives covariance
  // sigma_tract^2 I + sigma_county^2 11'. Its two eigenvalues provide
  // this exact O(n) density without constructing a dense covariance matrix.
  real county_latent_lpdf(vector logits, real alpha,
                         real sigma_county, real sigma_tract) {
    int n = rows(logits);
    real independent_variance = square(sigma_tract);
    real shared_eigenvalue = independent_variance + n * square(sigma_county);
    vector[n] residual = logits - rep_vector(alpha, n);
    real residual_mean = mean(residual);
    vector[n] centered_residual = residual - rep_vector(residual_mean, n);
    real log_det = (n - 1) * log(independent_variance)
                   + log(shared_eigenvalue);
    real quadratic = dot_self(centered_residual) / independent_variance
                     + n * square(residual_mean) / shared_eigenvalue;
    return -0.5 * (n * log(2 * pi()) + log_det + quadratic);
  }

  // TS3 changes measurement-error dependence, not the latent county prior.
  // diag(se) [(1-rho) I + rho 11'] diag(se) preserves each published marginal
  // standard error. The fixed positive rho is shared only within a county.
  real county_error_lpdf(vector observed, vector latent,
                        vector observed_se, real rho) {
    int n = rows(observed);
    real independent_fraction = 1 - rho;
    real shared_eigenvalue = independent_fraction + n * rho;
    vector[n] standardized = (observed - latent) ./ observed_se;
    real log_det = 2 * sum(log(observed_se))
                   + (n - 1) * log1m(rho) + log(shared_eigenvalue);
    real quadratic = dot_self(standardized) / independent_fraction
                     - rho * square(sum(standardized))
                       / (independent_fraction * shared_eigenvalue);
    return -0.5 * (n * log(2 * pi()) + log_det + quadratic);
  }
}

data {
  int<lower=1> N;                        // Number of modeled source entities.
  int<lower=1, upper=N> C;               // Number of source county blocks.
  array[N] int<lower=1, upper=C> county;
  array[C] int<lower=1, upper=N> group_start;
  array[C] int<lower=1, upper=N> group_size;
  vector[N] y;
  vector<lower=0>[N] se;
  int<lower=1, upper=3> latent_structure;
  real alpha_loc;                        // logit(.40) or logit(.03).
  real<lower=0> alpha_scale;              // 1.5 for both criteria.
  real<lower=0> sigma_county_scale;       // .5; inactive in branch 1.
  real<lower=0> sigma_tract_scale;        // 1; also original county M1 sigma.
  real<lower=0, upper=1> rho;             // 0 ordinarily; .25 for TS3.
}

transformed data {
  int has_county_scale = latent_structure == 1 ? 0 : 1;
  int n_eta = latent_structure == 2 ? N : 0;
  int n_county_z = latent_structure == 3 ? C : 0;
  int n_tract_z = latent_structure == 2 ? 0 : N;
  int next_start = 1;

  if (alpha_scale <= 0 || sigma_county_scale <= 0 || sigma_tract_scale <= 0)
    reject("Prior scales must be strictly positive.");
  if (!(rho == 0 || abs(rho - 0.25) <= 1e-12))
    reject("The published error correlations are rho=0 and rho=.25.");
  if (latent_structure == 1 && rho != 0)
    reject("The single-level models use independent measurement errors.");
  for (i in 1:N)
    if (se[i] <= 0) reject("Published/effective standard errors must be positive.");

  // All entities appear once in ordered, contiguous county blocks. County M1
  // has singleton blocks; those blocks do not enter its single-level prior.
  for (c in 1:C) {
    if (group_start[c] != next_start)
      reject("County blocks must be contiguous and ordered.");
    if (group_start[c] + group_size[c] - 1 > N)
      reject("A county block exceeds N.");
    for (j in 1:group_size[c])
      if (county[group_start[c] + j - 1] != c)
        reject("County labels disagree with their block.");
    next_start += group_size[c];
  }
  if (next_start != N + 1) reject("County blocks must cover all observations.");
}

parameters {
  real alpha;
  vector<lower=0>[has_county_scale] sigma_county;
  real<lower=0> sigma_tract;
  vector[n_eta] eta;
  vector[n_county_z] z_county;
  vector[n_tract_z] z_tract;
}

transformed parameters {
  vector[N] latent_logit;
  vector<lower=0, upper=1>[N] target_native;

  if (latent_structure == 1) {
    latent_logit = alpha + sigma_tract * z_tract;
  } else if (latent_structure == 2) {
    latent_logit = eta;
  } else {
    for (i in 1:N)
      latent_logit[i] = alpha + sigma_county[1] * z_county[county[i]]
                        + sigma_tract * z_tract[i];
  }
  target_native = inv_logit(latent_logit);
}

model {
  // Positive scale parameters with Normal(0, s) densities give the specified
  // half-Normal priors, as in the original criterion-specific programs.
  alpha ~ normal(alpha_loc, alpha_scale);
  sigma_tract ~ normal(0, sigma_tract_scale);
  if (has_county_scale == 1)
    sigma_county[1] ~ normal(0, sigma_county_scale);

  if (latent_structure == 2) {
    for (c in 1:C)
      target += county_latent_lpdf(segment(eta, group_start[c], group_size[c]) |
                                  alpha, sigma_county[1], sigma_tract);
  } else {
    z_tract ~ std_normal();
    if (latent_structure == 3) z_county ~ std_normal();
  }

  if (rho == 0) {
    y ~ normal(target_native, se);
  } else {
    for (c in 1:C) {
      int first = group_start[c];
      int size = group_size[c];
      target += county_error_lpdf(segment(y, first, size) |
                                 segment(target_native, first, size),
                                 segment(se, first, size), rho);
    }
  }
  // target_native is derived from latent logits; it is not a parameter
  // transformation in the density, so no logistic Jacobian is added here.
}

generated quantities {
  // Optional replicated noisy estimates under the fitted measurement model.
  // They are neither new historical outcomes nor a validation of policy effects.
  vector[N] y_rep;
  if (rho == 0) {
    for (i in 1:N) y_rep[i] = normal_rng(target_native[i], se[i]);
  } else {
    for (c in 1:C) {
      real shared_error = normal_rng(0, 1);
      for (j in 1:group_size[c]) {
        int i = group_start[c] + j - 1;
        y_rep[i] = target_native[i] + se[i]
                   * (sqrt(rho) * shared_error
                      + sqrt(1 - rho) * normal_rng(0, 1));
      }
    }
  }
}
