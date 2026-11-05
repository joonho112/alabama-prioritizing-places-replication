# Measurement models and review reports

JoonHo Lee ([jlee296@ua.edu](mailto:jlee296@ua.edu))

The annotated [Stan program](../stan/measurement_model.stan) contains the measurement models used in the article. It is fitted separately to income and incarceration. It does not estimate a joint cross-criterion posterior or a causal effect of a proposed project. [config/fit-settings.csv](../config/fit-settings.csv) specifies every model, criterion and computation pool.

## Observation and latent scales

For a published estimate `y_i` and its supplied standard error `s_i`, the ordinary observation law is

```text
y_i | eta_i ~ Normal(inv_logit(eta_i), s_i)
```

Both quantities use native fractions. Negative privacy-noisy incarceration estimates remain valid observations under this Normal model. Their latent targets stay between zero and one. Neither the source estimate nor its standard error is clipped, replaced by a binomial estimate, or transformed to a logit before entering the likelihood.

The alpha prior is Normal(logit(.40), 1.5) for income and Normal(logit(.03), 1.5) for incarceration. The county M1 and tract TS1 models use `eta_i = alpha + sigma_tract * z_i`, with independent standard-Normal `z_i` and a Half-Normal(0,1) spread.

The other tract models use a county plus tract latent law:

```text
eta_i = alpha + u_county[i] + epsilon_i
u_c ~ Normal(0, sigma_county)
epsilon_i ~ Normal(0, sigma_tract)
sigma_county ~ Half-Normal(0, .5)
sigma_tract ~ Half-Normal(0, 1)
```

County observations are not added to the tract likelihood. All 1,173 usable tracts contribute to fitting; the subsequent 90-tract review universe does not restrict the measurement model.

## One program, three numerical branches

| Branch | Fits | Representation |
|---|---|---|
| 1 | County M1; tract TS1 | Single-level non-centered logits |
| 2 | Main tract; TS2/TS3 income | County intercept integrated analytically; sampled tract logits have covariance `sigma_tract^2 I + sigma_county^2 11'` within county |
| 3 | TS2/TS3 incarceration | Explicit non-centered county and tract effects |

Branch 3 retains the adopted numerical repair for those incarceration fits. It has the same hierarchical latent law as branch 2. Parameters inactive in a branch have zero-length vectors, so they are not sampled as unidentified nuisance quantities. `target_native = inv_logit(latent_logit)` is derived; no inverse-logit Jacobian is added to its density.

TS2 multiplies the published standard errors by 1.25 once in the R input builder. TS3 preserves the marginal standard errors and changes within-county observation-error covariance to

```text
diag(s) * [(1 - rho) I + rho 11'] * diag(s), rho = .25
```

The custom Stan densities use the eigenvalues of these rank-one covariance forms. This is an exact evaluation of the stated Normal laws, not a diagonal approximation. Generated `y_rep` values are replicated noisy estimates under the model. They are not subsequent outcomes or evidence that a policy would work.

## From estimates to ordered bands

The adapter orients posterior targets toward greater historical need once: income × −100, incarceration × +100. Natural outcome tables retain income percentile points and incarceration percentage points. Separate-criterion intervals describe each measurement model; they are not a joint confidence region.

The county report uses 64 candidates, while retaining all 67 county posteriors. The tract report uses the fixed 90 pilot tracts. Smaller band numbers mean earlier review. Places in one band have their mutual comparisons withheld; whole bands remain intact at an assumed capacity.

For `M` unordered pairs, `N` disclosed directions, criterion weights `w_k`, expected wrong disclosed directions `E_k`, and withholding cost `tau`, the reporting objective is

```text
R_tau = [sum_k(w_k * E_k) + tau * (M - N)] / M
```

It is an expected reporting cost, not a probability of error. The central settings are equal criterion weights, `tau=.20`, and at most five occupied bands. The optimization determines the number occupied.

B0 withholds all comparisons. B1 cuts published-estimate rank scores into fixed quintile bands; B2 does the same with posterior-mean rank scores. B3 optimizes cuts of posterior expected ranks. P optimizes the unrestricted common ordered report with HiGHS. Score ties are not split. B3's finite-draw rank calculations preserve exact ties even when the two criteria have different sample sizes.

## Selection, numerical evaluation and diagnostics

Pool A selects each action; pool B evaluates that action without reselecting it. Both computations condition on the same observed data, so B is not a held-out empirical sample. Tract model alternatives select from their own A computations and are all evaluated on the same main-model B. Criterion streams remain separate, with their own chain and original iteration coordinates.

Fixed-risk MCSE uses each criterion's whole within-draw entity vector, preserving dependence among pair indicators. Paired differences evaluate two reports on the same criterion draws; independent-criterion numerical contributions are combined as `sqrt(sum_k(w_k^2 * MCSE_k^2))` when the required components are identified. A constant indicator trace has unidentified MCSE and remains missing. Algebraically constant all-withheld loss, or a difference between identical pair relations, has exact zero numerical variation.

A solver certificate bounds the finite-A objective. It does not measure HMC convergence, posterior uncertainty, or policy effectiveness. Hard HMC requirements are R-hat <1.01, bulk and tail ESS ≥400, zero divergences, zero maximum-depth hits, and minimum BFMI >.30. Preferred ESS ≥1,000 and the MCSE targets are recorded separately. Risk and paired-difference MCSE targets are .001; the pair-direction target is .005. Unidentified or precision-limited estimates retain that status.

The exploratory direction screen forbids disclosed directions whose finite-A weighted wrong-direction probability exceeds .5. It is reported separately from the eight policy/model stability cases. Policy contrasts compare the alternative and central actions under the same row's weights and withholding cost; losses under different objectives are not subtracted as though they were common-reference risks.

The [postrank package website](https://joonho112.github.io/postrank/) documents the software. The [postrank explorer](https://joonho112.github.io/postrank-explorer/) offers a visual introduction to common ordered bands. The article and its appendices give the study's substantive interpretation and limits.
