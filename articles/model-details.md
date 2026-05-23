# Model details and identification

This vignette describes the statistical model underlying
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md),
how baseline normalisation is performed (statistically inside Stan
rather than by plug-in division in R), and what each study design can
and cannot identify.

## The latent DiD model

metadid assumes that every study — regardless of design — arises from a
common latent difference-in-differences (DiD) data-generating process.
For study $`i`$, outcomes in the **control group** follow

``` math
\begin{pmatrix}
Y_{i,c,\mathrm{pre}} \\
Y_{i,c,\mathrm{post}}
\end{pmatrix}
\sim
\mathcal{N}
\left[
\begin{pmatrix}
\alpha_i \\
\alpha_i + \beta_i
\end{pmatrix}
,
\begin{pmatrix}
\sigma^2_{i,c,\mathrm{pre}} &
\rho_{i,c}\,\sigma_{i,c,\mathrm{pre}}\,\sigma_{i,c,\mathrm{post}} \\
\rho_{i,c}\,\sigma_{i,c,\mathrm{pre}}\,\sigma_{i,c,\mathrm{post}} &
\sigma^2_{i,c,\mathrm{post}}
\end{pmatrix}
\right],
```

and outcomes in the **treatment group** follow

``` math
\begin{pmatrix}
Y_{i,t,\mathrm{pre}} \\
Y_{i,t,\mathrm{post}}
\end{pmatrix}
\sim
\mathcal{N}
\left[
\begin{pmatrix}
\alpha_i + \gamma_i \\
\alpha_i + \gamma_i + \beta_i + \theta_i
\end{pmatrix}
,
\begin{pmatrix}
\sigma^2_{i,t,\mathrm{pre}} &
\rho_{i,t}\,\sigma_{i,t,\mathrm{pre}}\,\sigma_{i,t,\mathrm{post}} \\
\rho_{i,t}\,\sigma_{i,t,\mathrm{pre}}\,\sigma_{i,t,\mathrm{post}} &
\sigma^2_{i,t,\mathrm{post}}
\end{pmatrix}
\right].
```

The parameters are:

- $`\alpha_i`$: baseline mean in the control group
- $`\beta_i`$: time trend shared across groups
- $`\gamma_i`$: baseline difference between treatment and control groups
- $`\theta_i`$: study-specific treatment effect (the DiD estimand)
- $`\rho_{i,c}`$, $`\rho_{i,t}`$: pre/post correlations within each
  group
- $`\sigma_{i,g,t}`$: marginal standard deviations

The key identifying assumption is that, absent treatment, the treatment
group would have followed the same time trend $`\beta_i`$ as the control
group (i.e., the parallel trends assumption).

## What each design observes

Different study designs observe different subsets of this latent
bivariate structure:

| Design | Groups observed | Time points observed | Cells of the 2×2 table |
|----|----|----|----|
| **DiD** | Control + Treatment | Pre + Post | All 4 |
| **DiD (change only)** | Control + Treatment | Change scores only | Differences of 2 pairs |
| **RCT** | Control + Treatment | Post only | 2 |
| **Pre-post** | Treatment only | Pre + Post | 2 |

Because each design sees fewer cells, it has less ability to separate
the parameters $`\alpha`$, $`\beta`$, $`\gamma`$, and $`\theta`$.

## Identification

### DiD studies: fully identified

DiD studies observe all four cells of the 2×2 table. From the mean
structure above, the four expected cell means are:

|  | Pre | Post |
|----|----|----|
| **Control** | $`\alpha_i`$ | $`\alpha_i + \beta_i`$ |
| **Treatment** | $`\alpha_i + \gamma_i`$ | $`\alpha_i + \gamma_i + \beta_i + \theta_i`$ |

Taking the double difference of these expected values gives

``` math
(\mu_{i,t,\mathrm{post}} - \mu_{i,t,\mathrm{pre}}) -
(\mu_{i,c,\mathrm{post}} - \mu_{i,c,\mathrm{pre}}) =
(\beta_i + \theta_i) - \beta_i = \theta_i.
```

The time trend $`\beta_i`$ cancels, and the baseline difference
$`\gamma_i`$ cancels, leaving $`\theta_i`$ cleanly identified. DiD
studies are the anchor for the entire model.

### DiD (change only) studies: identified but without levels

Some studies report only the mean change over time (post minus pre) in
each arm, without reporting the level means separately. The expected
change scores are $`\beta_i`$ (control) and $`\beta_i + \theta_i`$
(treatment), so the difference in change scores still identifies
$`\theta_i`$. However, because the pre-treatment levels are not
observed, these studies cannot anchor their own normalisation baseline
(see below).

### RCT studies: treatment effect confounded with baseline differences

RCT studies observe only post-treatment outcomes for both arms. The
expected difference between arms is

``` math
\mu_{i,t,\mathrm{post}} - \mu_{i,c,\mathrm{post}} =
(\alpha_i + \gamma_i + \beta_i + \theta_i) -
(\alpha_i + \beta_i) = \gamma_i + \theta_i.
```

Without pre-treatment data, $`\theta_i`$ is confounded with the baseline
difference $`\gamma_i`$. Randomisation makes $`\gamma_i`$ small in
expectation, but the model cannot separate the two from the data of a
single RCT alone.

### Pre-post studies: treatment effect confounded with time trends

Pre-post studies observe only the treatment arm at both time points. The
expected change over time is

``` math
\mu_{i,t,\mathrm{post}} - \mu_{i,t,\mathrm{pre}} =
(\alpha_i + \gamma_i + \beta_i + \theta_i) -
(\alpha_i + \gamma_i) = \beta_i + \theta_i.
```

Without a control arm, $`\theta_i`$ is confounded with the time trend
$`\beta_i`$.

### Summary

| Design | Identifies $`\theta_i`$? | Confound |
|----|----|----|
| **DiD** | Yes | — |
| **DiD (change only)** | Yes | — |
| **RCT** | No | Baseline group difference $`\gamma_i`$ |
| **Pre-post** | No | Time trend $`\beta_i`$ |

**Without DiD studies, the treatment effect is not identified from the
data.**
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
will stop with an error if no DiD studies are present. This check can be
overridden with `allow_no_did = TRUE`, but the resulting posterior will
be driven primarily by the priors rather than the data.

When DiD studies are present alongside RCT or pre-post studies, the
hierarchical model propagates information: the time trend distribution
estimated from DiD studies informs the pre-post decomposition, and the
baseline structure from DiD studies informs the RCT decomposition. This
cross-design borrowing is the core value of the metadid approach.

### Differenced-form likelihoods

For RCT and pre-post designs, the model uses differenced-form
likelihoods that eliminate nuisance parameters algebraically:

- **RCT**: the likelihood is based on the treatment–control difference
  in post-treatment means, so the time trend $`\beta_i`$ cancels. The
  remaining parameters are the treatment effect $`\theta_i`$ and the
  baseline difference $`\gamma_i`$ (borrowed from DiD studies).
- **Pre-post** (default): the likelihood is based on the within-subject
  post–pre difference, so the baseline $`\alpha_i + \gamma_i`$ cancels.
  The remaining parameters are the treatment effect $`\theta_i`$ and the
  time trend $`\beta_i`$ (borrowed from DiD studies).

For pre-post studies, a non-differenced (bivariate normal) form is
available via `meta_did_general(pp_likelihood = "bivariate")`. This
retains the pre/post correlation $`\rho_i`$ as an estimable parameter,
contributing to the hierarchical $`\rho`$ model, at the cost of
estimating additional nuisance parameters.

## Baseline normalisation

By default (`normalise = TRUE`),
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
expresses treatment effects as fractions of each study’s treatment-arm
pre-treatment baseline. This places studies on a common relative scale
(useful when raw outcomes are on incommensurable units across studies)
and makes the population-level parameters interpretable as proportional
effects.

The normalisation is performed **statistically inside the Stan model**,
not by dividing the observed data in R. Each study’s pre-treatment
baseline enters the model as a latent parameter with a wide data-vague
uniform prior; the likelihood operates on the raw observations; and the
hierarchical pooling layer over treatment effects, time trends, and
baseline imbalances lives on the canonical fractional scale (fraction of
the treatment-arm pre-treatment baseline). Bridging from the canonical
scale to the absolute scale of the observed data happens at the
likelihood call site through a multiplication by the per-study baseline.

Setting `normalise = FALSE` switches the model to absolute-scale pooling
instead: there is no per-study baseline latent, and the population-level
parameters are interpreted in the user’s original outcome units. This is
appropriate for meta-analyses where all studies share a common
measurement scale and a meaningful absolute effect size is the quantity
of interest.

### Why model the baseline rather than divide by the sample mean

A naive plug-in approach — dividing observed means and SDs by the sample
control-pre mean before fitting — treats the baseline as if it were
known exactly, ignoring its sampling uncertainty. By Jensen’s inequality
the ratio of sample means is not equal to the ratio of population means;
the plug-in estimator picks up a finite-sample bias that scales with the
variance of the baseline estimator. Modelling the baseline as a latent
parameter inside the joint posterior correctly propagates that
uncertainty, eliminating the Jensen contribution to bias and producing
credible intervals on the fractional effect that have the right shape.

The cost is computational: the model carries one additional latent
parameter per study. The wide uniform prior on this parameter has an
upper bound auto-computed in R as 100× the maximum observed baseline
mean (overridable via `set_priors(baseline_per_study = uniform(0, X))`),
which is effectively flat over the data-supported region.

### Choice of which baseline is the latent: `baseline_latent_arm`

The default `baseline_latent_arm = "treatment"` makes the treatment-arm
pre-baseline the per-study latent with the wide prior. The control-arm
pre-baseline is then a derived quantity, related to the latent via the
hierarchical baseline-imbalance parameter $`\gamma_i = (b_{T,i} -
b_{C,i}) / b_{C,i}`$ — defined relative to the control-pre baseline
under the control-pre reference convention:

``` math
b_{C,i} = b_{T,i} / (1 + \gamma_i).
```

Setting `baseline_latent_arm = "control"` reverses the choice: the
control-arm pre-baseline becomes the latent and the treatment-arm
pre-baseline is derived as $`b_{T,i} = b_{C,i} \cdot (1 + \gamma_i)`$.
The lower bound $`\gamma_i > -1`$ keeps $`(1 + \gamma_i)`$ strictly
positive in both parameterisations. The **reported canonical scale is
the same** under both options — both pool on the treatment-arm
pre-baseline reference — so the choice does not change the estimand.
What it does change is which arm’s pre-period observations most directly
anchor the per-study baseline’s posterior, which matters when the two
arms have substantially different sample sizes or precision. In
well-identified problems the two parameterisations give numerically
equivalent posteriors on every population-level parameter, modulo MCMC
noise.

### Per-design contribution to baseline identification

For DiD studies, both arms’ pre-period means are observed, so whichever
baseline is chosen as the latent is directly informed by the
corresponding pre-period observations; the other arm’s baseline is
identified via $`\gamma_i`$ from the cross-arm pre-period comparison.

For RCT studies, no pre-period observations exist. The per-study latent
baseline is identified jointly with the per-study time trend
$`\beta_i`$, imbalance $`\gamma_i`$, and treatment effect $`\theta_i`$
via the post-period observations and the hierarchical priors pooled
across the rest of the meta-analysis (notably the DiD studies, which
directly identify $`\beta_i`$ and $`\gamma_i`$).

For pre-post studies, the treatment arm’s pre-period mean is observed
but no control arm exists. Under `baseline_latent_arm = "treatment"`
(the natural choice) the latent is directly informed by that
observation. The imbalance $`\gamma_i`$ is not identified per-study from
pre-post data alone and is drawn from the hierarchical prior populated
by DiD studies in the same fit.

For DiD change-only studies (which report only the change scores, not
the pre/post split), no per-study baseline observation exists. The model
does not attempt to infer a per-study baseline in this case — the
double-differenced likelihood used for these studies cancels the
baseline out, so the treatment effect on the canonical fractional scale
is identified from the difference between treatment-arm and control-arm
change scores. **Users supplying change-only data are responsible for
ensuring those change scores are already on a comparable scale to the
rest of the meta-analysis** (typically by pre-dividing each study’s
change scores by its own pre-period level before submission); the
package does not pre-normalise change-only data.

### Interpreting the population treatment effect

Under `normalise = TRUE` the population treatment effect $`\mu_\theta`$
is the expected fractional shift in outcomes caused by the intervention,
expressed as a fraction of the treatment-arm pre-treatment baseline. For
example, $`\mu_\theta = -0.33`$ means a 33% reduction relative to the
baseline level. Two studies on incommensurable raw-outcome scales (e.g.
cholesterol in mg/dL vs in mmol/L) can both contribute to this estimand,
because dividing each study’s effect by its own arm-internal baseline
removes the unit dependency. The cost of this scale-removal is the
exchangeability assumption it implies on the per-study fractional
effects — see the discussion in the package’s
`vignette("incommensurable-scales")` (forthcoming) for when this is and
isn’t appropriate.

Under `normalise = FALSE` the population treatment effect $`\mu_\theta`$
is on the user’s original outcome units and represents a meaningful
absolute effect size; this requires all studies to share a measurement
scale.

## Hierarchical structure

Study-specific treatment effects are drawn from a population
distribution:

``` math
\theta_i \sim \mathcal{N}(\mu_\theta, \tau_\theta^2),
```

where $`\mu_\theta`$ is the overall treatment effect (the primary
quantity of interest) and $`\tau_\theta`$ captures between-study
heterogeneity.

Other study-level parameters (time trends $`\beta_i`$, baseline
differences $`\gamma_i`$) similarly share population-level priors.
Pre-post correlations $`\rho`$ can be modelled hierarchically via a
Fisher-$`z`$ transform when `hierarchical_rho = TRUE`.

### Design effects

When `design_effects = TRUE`, the model allows the population treatment
effect mean to differ systematically by design:

``` math
\mu_{\theta,\text{RCT}} = \mu_\theta + \delta_{\text{RCT}}, \quad
\mu_{\theta,\text{PP}} = \mu_\theta + \delta_{\text{PP}}
```

where $`\delta_{\text{RCT}}`$ and $`\delta_{\text{PP}}`$ are estimated
offsets. This relaxes the assumption that all designs estimate exactly
the same estimand, which may be appropriate when selection effects or
time trends differ systematically across designs.

### Robust heterogeneity

When `robust_heterogeneity = TRUE`, the treatment effect distribution
uses a Student-$`t`$ instead of a normal:

``` math
\theta_i \sim t_\nu(\mu_\theta, \tau_\theta^2)
```

where $`\nu`$ (the degrees of freedom) is estimated. This accommodates
outlier studies that would otherwise inflate $`\tau_\theta`$.

### Controlling assumptions with `meta_did_general()`

The
[`meta_did_general()`](https://ben18785.github.io/metadid/reference/meta_did_general.md)
function provides explicit control over how nuisance parameters are
handled for non-DiD designs, via three arguments:

- **`time_trend`**: Controls the time trend $`\beta_i`$ for RCT and
  pre-post studies.
  - `"pooled"` (default): hierarchical prior shared across designs,
    informed by DiD studies.
  - `"fixed_zero"`: $`\beta_i = 0`$ for RCT and pre-post studies. For
    pre-post studies, this attributes all pre-post change to treatment.
    For RCTs, it bypasses the time trend reparameterisation described
    above.
- **`baseline_imbalance`**: Controls the baseline difference
  $`\gamma_i`$ for RCT studies.
  - `"estimated"` (default): $`\gamma_i`$ is estimated, with information
    borrowed from DiD studies when normalised.
  - `"fixed_zero"`: $`\gamma_i = 0`$, assuming randomisation eliminates
    baseline imbalances.
- **`pp_likelihood`**: Controls the likelihood form for pre-post
  studies.
  - `"differenced"` (default): uses the post-minus-pre difference,
    eliminating the baseline algebraically. The pre/post correlation
    $`\rho_i`$ is not separately estimable.
  - `"bivariate"`: uses the full bivariate normal likelihood for the
    (pre, post) pair. This retains $`\rho_i`$ as an estimable parameter,
    contributing to the hierarchical $`\rho`$ model, at the cost of
    estimating additional nuisance parameters.

These settings can be combined independently. For example, one might
trust the randomisation assumption (`baseline_imbalance = "fixed_zero"`)
while still borrowing time trend information from DiD studies
(`time_trend = "pooled"`).

The default settings of
[`meta_did_general()`](https://ben18785.github.io/metadid/reference/meta_did_general.md)
are identical to
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md).
Setting `time_trend` and `baseline_imbalance` to `"fixed_zero"`
reproduces the behaviour of the deprecated
[`meta_did_naive()`](https://ben18785.github.io/metadid/reference/meta_did_naive.md).

## Meta-regression with covariates

When study-level covariates are available (e.g., intervention dose, year
of publication), the treatment effect mean can be modelled as a linear
function of those covariates. If $`\mathbf{x}_i`$ is a $`K`$-vector of
covariate values for study $`i`$, the hierarchical prior becomes

``` math
\theta_i \sim \mathcal{N}\!\left(
  \mu_\theta + \mathbf{x}_i^\top \boldsymbol{\beta},\;
  \tau_\theta^2
\right),
```

where $`\boldsymbol{\beta}`$ is a vector of meta-regression coefficients
estimated jointly with all other parameters. When
`robust_heterogeneity = TRUE`, the normal is replaced by a Student-$`t`$
as before. The same covariate adjustment applies across all study
designs (DiD, RCT, and pre-post), with design-specific offsets
($`\delta_{\text{RCT}}`$, $`\delta_{\text{PP}}`$) added when
`design_effects = TRUE`.

### Covariate centering

By default (`center_covariates = TRUE`), covariates are mean-centered
across all studies in the meta-analysis before fitting. This has a
useful interpretive consequence: $`\mu_\theta`$ represents the
population treatment effect **at the average covariate values**, rather
than at $`\mathbf{x} = 0`$ (which may not be a meaningful reference
point).

The centering values are stored in the fitted object (`fit$cov_centers`)
and are needed to reconstruct predictions on the original covariate
scale.

### Specifying covariates in `meta_did()`

Covariates are passed as a one-sided formula:

``` r

fit <- meta_did(
  summary_data = my_data,
  covariates   = ~ dose + year
)
```

The covariate columns must be present in `summary_data` (and/or
`individual_data`), must be numeric, and must be constant within each
study. The prior on $`\boldsymbol{\beta}`$ defaults to
$`\mathcal{N}(0, 10)`$ per coefficient and can be adjusted via
`set_priors(beta_cov = normal(0, sd))`.

For a worked example including simulation and recovery, see
[`vignette("covariates")`](https://ben18785.github.io/metadid/articles/covariates.md).

## Prior specification

Priors can be customised via
[`set_priors()`](https://ben18785.github.io/metadid/reference/set_priors.md).
See
[`?set_priors`](https://ben18785.github.io/metadid/reference/set_priors.md)
for the full list of parameters and their defaults.
