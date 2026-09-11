# metadid (development version)

## Randomisation-aware baseline imbalance

The baseline difference `gamma_i` is now modelled by a study's **assignment
mechanism** rather than by its design label. This changes default behaviour and
will move published numbers where post-only studies are present.

### Why

`gamma_i` is unidentified for a post-only study: it subtracts directly from that
study's estimated effect, so whatever the model assumes about imbalance
propagates straight into the pooled result. Previously every DiD and RCT study
shared one `N(mu_gamma, tau_gamma)` population with `mu_gamma` estimated. Since
DiD is typically used *because* assignment was not random, that transported a
selection-driven imbalance — direction included — from unrandomised studies onto
randomised trials. The resulting bias does not shrink as evidence accumulates;
it sharpens, because `mu_gamma` becomes more precisely estimated.

### New

* `summary_data` and `individual_data` accept an optional `randomisation`
  column (`"individual"`, `"cluster"`, `"none"`, or `NA`). It is **never**
  inferred from `design`, and `NA` reads as `"none"`, so randomisation is always
  an explicit claim. Optional `cluster_size` and `icc` columns supply a
  per-study design effect.
* `meta_did()` and `meta_did_general()` gain `mu_gamma`, `kappa` and
  `cluster_deff_default`.
* Randomised studies use `gamma_i ~ N(0, (kappa * s_i)^2)`, where `s_i` is the
  sampling SD of that study's baseline contrast. Because `s_i` scales as
  `1/sqrt(n_i)`, small randomised studies are down-weighted more than large
  ones. For a post-only randomised study this is equivalent to inflating its
  standard error by `sqrt(1 + kappa^2)`.
* `kappa = "estimate"` is permitted only when a randomised study carries
  pre-treatment data. Post-only randomised studies cannot identify it, so
  `meta_did()` errors rather than sampling a prior-driven parameter — mirroring
  the existing refusal to impute `rho` with nothing to anchor it.
* `allow_unidentified_kappa = TRUE` overrides that refusal, following the same
  idiom as `allow_no_did`. Sampling an unanchored `kappa` is a modelling choice,
  not an estimate: it makes the marginal prior on `gamma_i` a scale mixture of
  normals rather than a normal — both more peaked at zero and much
  heavier-tailed than any fixed `kappa` (at `kappa ~ half-normal(0, 0.5)` and
  `s_i = 0.038`, `P(|gamma| > 0.05)` is 0.029 against 0.0008 for a fixed `kappa`
  at the same mean scale). That expresses "most randomised trials achieved
  balance, occasionally one badly did not", which no single scale can. Verified
  free of sampling pathology in the worst case for it — 30 post-only randomised
  studies against 10 unrandomised DiD, nothing anchoring `kappa` — at 0
  divergences and min EBFMI 0.87, because `gamma` is reconstructed from a
  non-centred `raw ~ std_normal()` rather than sampled directly.
* `print()` reports the randomisation breakdown and the `kappa` in force.
* `kappa` is reported as a parameter, and `baseline_difference_mean` is now a
  transformed parameter that exists whether or not it is sampled.

### Changed defaults

* `baseline_imbalance` defaults to `"by_randomisation"` (was `"estimated"`).
  The old behaviour remains available as `"estimated"`.
* `mu_gamma` defaults to `"zero"`: the *magnitude* of imbalance is pooled
  across non-randomised studies, its *direction* is not.
* `set_priors(baseline_difference_mean = )` defaults to `normal(0, 0.05)`, was
  `normal(0, 0.5)`. The old default was inconsistent with the `cauchy(0.1)`
  prior on the between-study SD — it asserted the average imbalance could be far
  larger than the spread around it — and was materially informative about the
  pooled effect.
* `set_priors()` gains a `kappa` prior, default `normal(0, 0.5)`, read as
  half-normal.

### Known limitations

* **Pre-post studies sit on a different scale from the other designs when the
  treated group is selected.** A PP study has no control arm, so it can only
  normalise by its own treatment-arm pre mean, while DiD and RCT normalise by
  the control baseline. With a baseline imbalance `gamma`:

      DiD / RCT effect  =  theta / b
      PP effect         =  theta / (b + gamma)  =  (theta / b) / (1 + gamma/b)

  yet all three are pooled into a single `treatment_effect_mean`.

  Measured, at half the studies pre-post (3 replications, X24/X26/X27):

  | gamma | predicted | measured, normalised | measured, raw |
  |------:|----------:|---------------------:|--------------:|
  | 0.00  |     1.000 |                0.999 |         1.016 |
  | 0.08  |     0.925 |                0.916 |         1.004 |
  | 0.12  |     0.895 |                0.882 |         1.008 |

  So `treatment_effect_mean` is attenuated by roughly 8-12% at a plausible
  imbalance — the same order as the transport bias this release is about. The
  attenuation tracks `0.5 + 0.5/(1 + gamma/b)` to within about 1%, vanishes at
  `gamma = 0`, and is absent from the unnormalised arm, which has no divisor and
  so cannot exhibit it.

  This is pre-existing, not introduced by the randomisation work — but that work
  makes `gamma` explicit, which is what made the mismatch visible and
  measurable.

  Correcting it means rescaling PP effects by `(1 + gamma_i/b_i)`. PP studies
  carry no `gamma` parameter today, but one could be added exactly as post-only
  RCTs have one — being unidentified is not the obstacle, or the RCT branch
  could not work either.

  The obstacle is that the correction is MULTIPLICATIVE, so it depends on
  `E[gamma]` — the direction, not the spread. Under the default
  `mu_gamma = "zero"` that expectation is 1, so a hierarchical draw would add
  variance and remove no bias. Measured, at the same gamma spread:

  | gamma        | pooled effect / truth |
  |--------------|----------------------:|
  | one-sided    |                 0.926 |
  | mean-zero    |                 1.029 |

  Mean-zero gamma produces no attenuation at all — only a small second-order
  Jensen amplification, `E[1/(1+g)] ~ 1 + Var(g)`. So the PP scale mismatch and
  the RCT transport bias are the same question from two sides: estimating
  `mu_gamma` would correct PP but re-import DiD selection onto randomised
  trials, while pinning it at zero protects the trials and leaves PP attenuated.
  One scalar cannot serve both.

  **Deliberately not corrected, and the attenuation is a bound rather than an
  expectation.** The one-sided row above is the WORST case: it requires every
  study's treated group to be selected in the same direction. The mean-zero row
  is what a literature with no shared targeting direction looks like, and it
  shows no attenuation at all. So if you doubt that direction transports between
  studies — the premise of this whole release — you should also expect the PP
  mismatch to be small in practice. The two beliefs are the same belief.

  Applying `mu_gamma` to PP would be a further step out than applying it to
  post-only RCTs, not a safer one. `gamma` is a contrast BETWEEN TWO ARMS; a PP
  study has one. Transporting it there asserts where a control group would have
  sat for a control group that was never measured.

  The narrow case where estimating `mu_gamma` is defensible: several evaluations
  of the SAME programme under the SAME targeting rule, where a shared selection
  direction is a fact about the design rather than a hope. The principled
  general alternative — indexing `mu_gamma` on study covariates describing the
  targeting rule — needs metadata that is not usually available.
* The cluster design effect corrects the **baseline contrast only**; the
  post-treatment likelihood still uses `sigma^2/n`, so cluster-randomised
  studies remain over-precise about their own effect.
* Under `normalise_by_baseline = TRUE` the data are divided by the *observed*
  `mean_pre_control`, which the likelihood then treats as exactly 1 with no
  uncertainty. This drops the divisor's own sampling variance from the baseline
  contrast (roughly a factor of two with balanced arms) and is expected to
  inflate `tau_gamma` — the very quantity the two-population model transports to
  post-only studies. Scenario X20–X22 in `metadid-sims` measures it.

  **Deliberately not corrected here.** Modelling the denominator was attempted
  once before, as the "latent baseline" work in #32/#33, and reverted in
  c8e2607 for a suite-wide HMC sampling regression. That approach gave every
  study a free positive baseline latent under a uniform prior with hard bounds,
  and sampled `baseline_difference` in a *centred* parameterisation — a
  combination that breaks HMC geometry. Anyone revisiting this should read that
  revert first, and should note that a variance-only correction (rebuilding the
  likelihood covariance over the ratios that share a divisor, from data alone,
  adding no parameters) is a different and lower-risk proposition than
  re-introducing baseline latents. Measure with X20–X22 before deciding it is
  worth any risk at all.
