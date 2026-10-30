# Changelog

## metadid (development version)

### Sensitivity analysis and assumption diagnostics

The model’s distinctive claim is that designs observing different parts
of the latent process can be pooled. The assumptions that license it are
not identified by the data, so posterior predictive checks cannot test
them. Three diagnostics now address them directly.

- [`sensitivity_trend()`](https://ben18785.github.io/metadid/reference/sensitivity_trend.md)
  refits across a grid of `pp_trend_shift`, the systematic difference
  between the pre-post and DiD trend distributions, and reports where
  the pooled effect goes.
  [`plot_sensitivity_trend()`](https://ben18785.github.io/metadid/reference/plot_sensitivity_trend.md)
  draws the tipping-point curve.
  [`delta_star()`](https://ben18785.github.io/metadid/reference/delta_star.md)
  gives the breakdown value — the shift that would null the effect —
  analytically from a single fit, and
  [`omega_pp()`](https://ben18785.github.io/metadid/reference/omega_pp.md)
  the pre-post share of the information behind it, with the saturation
  ceiling that share cannot exceed however many pre-post studies are
  added.
- [`design_agreement()`](https://ben18785.github.io/metadid/reference/design_agreement.md)
  refits with `design_effects = TRUE` and returns the per-design offsets
  from the DiD anchor, with
  [`plot_design_agreement()`](https://ben18785.github.io/metadid/reference/plot_design_agreement.md).
  Preferred to fitting each design separately: a post-only subset must
  pin its time trend at zero to be identified, which moves it onto the
  control follow-up scale, a difference larger than the disagreement
  being looked for.
- [`information_share()`](https://ben18785.github.io/metadid/reference/information_share.md)
  and
  [`plot_information_share()`](https://ben18785.github.io/metadid/reference/plot_information_share.md)
  report which studies the result is actually resting on.

[`meta_did_general()`](https://ben18785.github.io/metadid/reference/meta_did_general.md)
gains `pp_trend_shift` (default `0`, which reproduces the unshifted fit
exactly). Fitted objects now carry the arguments they were created with,
so a sweep refits reproducibly rather than guessing at the original
settings; objects from earlier versions have `fit_args = NULL` and
refuse to be swept.

Two notes on the implementation. A DiD study’s effect and trend
estimates share its control arm, so they are correlated with covariance
`-var(control change)`; treating them as independent, as the stylised
derivation does, overstates the information share by several percent,
and
[`omega_pp()`](https://ben18785.github.io/metadid/reference/omega_pp.md)
sums the 2x2 information per study instead. And the sweep’s slope is
`-omega_pp`: raising the trend the model credits to pre-post studies
leaves less of their observed change for treatment.

See
[`vignette("mixed-designs")`](https://ben18785.github.io/metadid/articles/mixed-designs.md)
for worked examples.

#### Prior sensitivity and cross-validation

Fits now emit two generated quantities, so external tooling can work on
a fit without refitting it:

- `lprior`, the log density of the priors the user chose, which
  [priorsense](https://github.com/n-kall/priorsense) power-scales to
  report how far each posterior depends on those choices — from a single
  fit, by importance sampling. Power-scaling by `alpha` is equivalent to
  refitting with every prior scale multiplied by `1/sqrt(alpha)`, so
  widening all priors by a factor of two is `alpha = 0.25`. The
  sensitivity is to the hyperpriors; the standard-normal terms
  implementing the hierarchy are deliberately excluded, since scaling
  those would rescale the shrinkage rather than a prior anyone chose.
- `log_lik`, per-study log-likelihood, for `loo()` —
  leave-one-STUDY-out, the right unit for a meta-analysis — and for the
  likelihood half of a power-scaling check. Summary-level designs only:
  an individual-level study’s contribution is not yet computed, and
  rather than hand `loo()` a silently incomplete likelihood the vector
  is emitted at length zero when individual-level studies are present.
  `lprior` is unaffected.

`loo` and `priorsense` are Suggests, used only by the vignette.

#### Per-study influence

- [`study_influence()`](https://ben18785.github.io/metadid/reference/study_influence.md)
  does leave-one-study-out by PSIS, reporting each study’s `weight`,
  `elpd_i` and Pareto `khat`, with exact refits for the flagged studies
  on request. `refit = "none"` (the default) fits nothing; `"flagged"`
  fits once per study above the k threshold; `"all"` fits K times.
  Refits report the change in the pooled mean, the between-study SD
  **and** the interval width, because a discordant study can leave the
  mean almost untouched and still double the heterogeneity.
  [`plot_study_influence()`](https://ben18785.github.io/metadid/reference/plot_study_influence.md)
  draws k against weight.
- `log_lik_marginal` integrates out the study-level parameters. The
  conditional `log_lik` is unusable for this diagnostic: a study’s own
  theta is informed almost entirely by that study, so leaving it out
  makes the importance ratios heavy-tailed for every study regardless of
  influence – measured at 18 of 20 studies above k = 0.7 on a
  well-behaved fit, against 1 of 13 for the marginal version on a fit
  with a planted outlier. The vignette’s earlier advice to read Pareto k
  from `log_lik` was wrong and has been corrected.
- The integral is exact wherever the cell means are linear in the
  study-level parameters, which covers covariates, multiplicative
  covariates, correlated effects and either gamma mode. Post-only
  studies with an estimated trend divide by (1 + beta_tilde), so they
  use a delta-method marginal instead: `marginal_exact` reports 0 and
  [`study_influence()`](https://ben18785.github.io/metadid/reference/study_influence.md)
  warns rather than refusing, since the neglected terms go as the fourth
  power of tau_beta / (1 + mu_beta). Individual-level data, Student-t
  heterogeneity, unnormalised fits and studies with imputed rho are
  still refused, with a message naming the condition.

#### Prior families are now honoured

[`set_priors()`](https://ben18785.github.io/metadid/reference/set_priors.md)
accepted and validated
[`normal()`](https://ben18785.github.io/metadid/reference/normal.md) for
`treatment_effect_sd`, `time_trend_sd` and `baseline_difference_sd`, but
`shared_priors.stan` hard-coded `cauchy`, so the family was silently
ignored and the user got a half-Cauchy with the normal’s SD as its
scale. Three family flags now carry the choice through, and `lprior`
branches identically.

This also unlocks an exact check on power-scaling. Raising a Cauchy
prior to the power alpha gives a Student-t with nu = 2\*alpha - 1, not a
Cauchy, so there is no refit equivalent; for a NORMAL prior, scaling by
alpha is exactly a scale of 1/sqrt(alpha). `lprior` is therefore
partitioned (`lprior_effect`, `lprior_trend`, `lprior_imbalance`,
`lprior_rho`, `lprior_other`) so priorsense can power-scale one group at
a time via `selection =`, and a partition holding only normal priors can
be validated against a full refit. test-sensitivity.R does exactly that:
power-scaling `lprior_rho` at alpha = 0.5 and refitting with those two
SDs widened by 1/sqrt(0.5) agree to 0.0007 against a Monte Carlo SE of
0.0008, with Pareto k of 0.36.

Also fixed: `treatment_effect_sim` was declared inside an `if` branch of
the generated quantities block, which makes it local in Stan, so the
predictive draw for a new study was never written to the output. Nothing
read it, so no result changes.

Also fixed: `treatment_effect_sim` was declared inside an `if` branch of
the generated quantities block, which makes it local in Stan, so the
predictive draw for a new study was never written to the output at all.
It is now declared outside the branch and actually emitted. Nothing in
the package read it, so no result changes.

### Normalisation by an estimated baseline

Baseline normalisation divides every cell by an observed mean, which is
itself an estimate. The normalised likelihoods treated that denominator
as known, so its sampling error never reached the model. The
summary-level DiD and post-only likelihoods now carry it via a
delta-method variance.

#### Why

Pinning the normalising cell at exactly 1 makes its residual identically
zero, so it contributes nothing to the likelihood. For a **post-only**
study the denominator is also one of the two cells being contrasted, so
the loss is first-order: the statistic is the ratio `x_t / x_c`, whose
variance is `sigma_t^2/n_t + (1 + phi)^2 sigma_c^2/n_c`, and only the
first term was used. With equal arms the SD was 0.78 of its correct
value, and with a control arm a fifth the size of the treatment arm,
0.48. For a **DiD** study the pinned pre-control cell left the control
arm’s change variance as the conditional `sigma_ca^2 (1 - rho^2)`
instead of the paired
`sigma_cb^2 + sigma_ca^2 - 2 rho sigma_cb sigma_ca`, a factor of
`(1 + rho)/2`, and dropped the covariance the shared denominator induces
between the arms.

In simulation the pooled effect stayed calibrated, because the
random-effects term absorbed the missing variance. The damage fell
elsewhere: study-level 90% intervals for post-only studies covered 61%
of the time and came out narrower than those of DiD studies carrying
more information, between-study heterogeneity was inflated by 64-130%,
DiD trend heterogeneity by about 30%, and post-only studies were
over-weighted relative to DiD studies.

#### New

- `rct_summary_study_normalised_lpdf_from_data()` takes the control
  arm’s SD and sample size and uses the delta-method SD of the ratio.
  The branch for normalised data with the time trend fixed at zero uses
  the same form, with `gamma + theta` as the contrast.
- `did_summary_study_normalised_lpdf_from_data()` models the three
  non-pinned cells jointly, with the delta-method covariance of ratios
  sharing one denominator: a rank-one `v_cb * r r'` component plus the
  correlation terms for cells measured on the same people.
- Delta-method coefficients are evaluated at the observed ratios, so the
  covariance carries no parameters.
- Study-level coverage for post-only studies recovers to 0.89 against a
  nominal 0.90, the design-weighting distortion disappears, and
  heterogeneity inflation drops to roughly 20%.

#### Not covered

- **Individual-level data.** There the control observations are modelled
  with the mean pinned at 1 while their SD is a parameter estimated from
  the same observations, so a per-observation correction would distort
  it. A free baseline parameter is the route for that path.
- **The estimand under large baseline dispersion.**
  `treatment_effect_mean` is a precision-weighted average, and
  normalisation correlates each study’s weight with its own normalised
  effect, since both scale as `1/b_i`. With the variances now stated
  correctly, the pooled estimate sits between `E[theta_i/b_i]` and
  `E[theta]/E[b]` instead of on the former. At the dispersion typical of
  applications the two differ by well under a percentage point; it
  becomes material only when baselines vary widely.

### Randomisation-aware baseline imbalance

The baseline difference `gamma_i` is now modelled by a study’s
**assignment mechanism** rather than by its design label. This changes
default behaviour and will move published numbers where post-only
studies are present.

#### Why

`gamma_i` is unidentified for a post-only study: it subtracts directly
from that study’s estimated effect, so whatever the model assumes about
imbalance propagates straight into the pooled result. Previously every
DiD and RCT study shared one `N(mu_gamma, tau_gamma)` population with
`mu_gamma` estimated. Since DiD is typically used *because* assignment
was not random, that transported a selection-driven imbalance —
direction included — from unrandomised studies onto randomised trials.
The resulting bias does not shrink as evidence accumulates; it sharpens,
because `mu_gamma` becomes more precisely estimated.

#### New

- `summary_data` and `individual_data` accept an optional
  `randomisation` column (`"individual"`, `"cluster"`, `"none"`, or
  `NA`). It is **never** inferred from `design`, and `NA` reads as
  `"none"`, so randomisation is always an explicit claim. Optional
  `cluster_size` and `icc` columns supply a per-study design effect.
- [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
  and
  [`meta_did_general()`](https://ben18785.github.io/metadid/reference/meta_did_general.md)
  gain `mu_gamma`, `kappa` and `cluster_deff_default`.
- Randomised studies use `gamma_i ~ N(0, (kappa * s_i)^2)`, where `s_i`
  is the sampling SD of that study’s baseline contrast. Because `s_i`
  scales as `1/sqrt(n_i)`, small randomised studies are down-weighted
  more than large ones. For a post-only randomised study this is
  equivalent to inflating its standard error by `sqrt(1 + kappa^2)`.
- `kappa = "estimate"` is permitted only when a randomised study carries
  pre-treatment data. Post-only randomised studies cannot identify it,
  so
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
  errors rather than sampling a prior-driven parameter — mirroring the
  existing refusal to impute `rho` with nothing to anchor it.
- `allow_unidentified_kappa = TRUE` overrides that refusal, following
  the same idiom as `allow_no_did`. Sampling an unanchored `kappa` is a
  modelling choice, not an estimate: it makes the marginal prior on
  `gamma_i` a scale mixture of normals rather than a normal — both more
  peaked at zero and much heavier-tailed than any fixed `kappa` (at
  `kappa ~ half-normal(0, 0.5)` and `s_i = 0.038`, `P(|gamma| > 0.05)`
  is 0.029 against 0.0008 for a fixed `kappa` at the same mean scale).
  That expresses “most randomised trials achieved balance, occasionally
  one badly did not”, which no single scale can. Verified free of
  sampling pathology in the worst case for it — 30 post-only randomised
  studies against 10 unrandomised DiD, nothing anchoring `kappa` — at 0
  divergences and min EBFMI 0.87, because `gamma` is reconstructed from
  a non-centred `raw ~ std_normal()` rather than sampled directly.
- [`print()`](https://rdrr.io/r/base/print.html) reports the
  randomisation breakdown and the `kappa` in force.
- `kappa` is reported as a parameter, and `baseline_difference_mean` is
  now a transformed parameter that exists whether or not it is sampled.

#### Changed defaults

- `baseline_imbalance` defaults to `"by_randomisation"` (was
  `"estimated"`). The old behaviour remains available as `"estimated"`.
- `mu_gamma` defaults to `"zero"`: the *magnitude* of imbalance is
  pooled across non-randomised studies, its *direction* is not.
- `set_priors(baseline_difference_mean = )` defaults to
  `normal(0, 0.05)`, was `normal(0, 0.5)`. The old default was
  inconsistent with the `cauchy(0.1)` prior on the between-study SD — it
  asserted the average imbalance could be far larger than the spread
  around it — and was materially informative about the pooled effect.
- [`set_priors()`](https://ben18785.github.io/metadid/reference/set_priors.md)
  gains a `kappa` prior, default `normal(0, 0.5)`, read as half-normal.

#### Known limitations

- **Pre-post studies sit on a different scale from the other designs
  when the treated group is selected.** A PP study has no control arm,
  so it can only normalise by its own treatment-arm pre mean, while DiD
  and RCT normalise by the control baseline. With a baseline imbalance
  `gamma`:

  ``` R
  DiD / RCT effect  =  theta / b
  PP effect         =  theta / (b + gamma)  =  (theta / b) / (1 + gamma/b)
  ```

  yet all three are pooled into a single `treatment_effect_mean`.

  Measured, at half the studies pre-post (3 replications, X24/X26/X27):

  | gamma | predicted | measured, normalised | measured, raw |
  |------:|----------:|---------------------:|--------------:|
  |  0.00 |     1.000 |                0.999 |         1.016 |
  |  0.08 |     0.925 |                0.916 |         1.004 |
  |  0.12 |     0.895 |                0.882 |         1.008 |

  So `treatment_effect_mean` is attenuated by roughly 8-12% at a
  plausible imbalance — the same order as the transport bias this
  release is about. The attenuation tracks `0.5 + 0.5/(1 + gamma/b)` to
  within about 1%, vanishes at `gamma = 0`, and is absent from the
  unnormalised arm, which has no divisor and so cannot exhibit it.

  This is pre-existing, not introduced by the randomisation work — but
  that work makes `gamma` explicit, which is what made the mismatch
  visible and measurable.

  Correcting it means rescaling PP effects by `(1 + gamma_i/b_i)`. PP
  studies carry no `gamma` parameter today, but one could be added
  exactly as post-only RCTs have one — being unidentified is not the
  obstacle, or the RCT branch could not work either.

  The obstacle is that the correction is MULTIPLICATIVE, so it depends
  on `E[gamma]` — the direction, not the spread. Under the default
  `mu_gamma = "zero"` that expectation is 1, so a hierarchical draw
  would add variance and remove no bias. Measured, at the same gamma
  spread:

  | gamma     | pooled effect / truth |
  |-----------|----------------------:|
  | one-sided |                 0.926 |
  | mean-zero |                 1.029 |

  Mean-zero gamma produces no attenuation at all — only a small
  second-order Jensen amplification, `E[1/(1+g)] ~ 1 + Var(g)`. So the
  PP scale mismatch and the RCT transport bias are the same question
  from two sides: estimating `mu_gamma` would correct PP but re-import
  DiD selection onto randomised trials, while pinning it at zero
  protects the trials and leaves PP attenuated. One scalar cannot serve
  both.

  **Deliberately not corrected, and the attenuation is a bound rather
  than an expectation.** The one-sided row above is the WORST case: it
  requires every study’s treated group to be selected in the same
  direction. The mean-zero row is what a literature with no shared
  targeting direction looks like, and it shows no attenuation at all. So
  if you doubt that direction transports between studies — the premise
  of this whole release — you should also expect the PP mismatch to be
  small in practice. The two beliefs are the same belief.

  Applying `mu_gamma` to PP would be a further step out than applying it
  to post-only RCTs, not a safer one. `gamma` is a contrast BETWEEN TWO
  ARMS; a PP study has one. Transporting it there asserts where a
  control group would have sat for a control group that was never
  measured.

  The narrow case where estimating `mu_gamma` is defensible: several
  evaluations of the SAME programme under the SAME targeting rule, where
  a shared selection direction is a fact about the design rather than a
  hope. The principled general alternative — indexing `mu_gamma` on
  study covariates describing the targeting rule — needs metadata that
  is not usually available.

- The cluster design effect corrects the **baseline contrast only**; the
  post-treatment likelihood still uses `sigma^2/n`, so
  cluster-randomised studies remain over-precise about their own effect.

- Under `normalise_by_baseline = TRUE` the data are divided by the
  *observed* `mean_pre_control`, which the likelihood then treats as
  exactly 1 with no uncertainty. This drops the divisor’s own sampling
  variance from the baseline contrast (roughly a factor of two with
  balanced arms) and is expected to inflate `tau_gamma` — the very
  quantity the two-population model transports to post-only studies.
  Scenario X20–X22 in `metadid-sims` measures it.

  **Deliberately not corrected here.** Modelling the denominator was
  attempted once before, as the “latent baseline” work in
  [\#32](https://github.com/ben18785/metadid/issues/32)/#33, and
  reverted in c8e2607 for a suite-wide HMC sampling regression. That
  approach gave every study a free positive baseline latent under a
  uniform prior with hard bounds, and sampled `baseline_difference` in a
  *centred* parameterisation — a combination that breaks HMC geometry.
  Anyone revisiting this should read that revert first, and should note
  that a variance-only correction (rebuilding the likelihood covariance
  over the ratios that share a divisor, from data alone, adding no
  parameters) is a different and lower-risk proposition than
  re-introducing baseline latents. Measure with X20–X22 before deciding
  it is worth any risk at all.
