# Diagnostics for the assumptions the data cannot check.
#
# The model's distinctive claim is that designs observing different parts of the
# latent process can be pooled. These three ask whether that holds: whether the
# designs agree once allowed to differ, which studies are actually driving the
# result, and how far a trend-exchangeability failure would move it.

# ---------------------------------------------------------------------------
# plot_sensitivity_trend(): the tipping-point curve
# ---------------------------------------------------------------------------

#' Plot a trend-exchangeability sweep
#'
#' @description
#' The tipping-point curve: the pooled effect against \eqn{\Delta}, the
#' systematic difference between the pre-post and DiD trend distributions, with
#' the interval that the sweep refitted at each point. The dashed horizontal
#' line is no effect; where the ribbon first touches it is the shift at which
#' the conclusion stops holding.
#'
#' @param sweep The object returned by [sensitivity_trend()].
#' @return A ggplot2 object.
#' @seealso [sensitivity_trend()]
#' @export
plot_sensitivity_trend <- function(sweep) {
  g <- sweep$grid
  p <- ggplot2::ggplot(g, ggplot2::aes(x = .data$delta)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo, ymax = .data$hi),
                         fill = "skyblue", alpha = 0.4) +
    ggplot2::geom_line(ggplot2::aes(y = .data$estimate), linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(y = .data$estimate), size = 2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, colour = "grey60") +
    ggplot2::labs(
      x = expression(Delta ~ "(pre-post trend mean, relative to DiD)"),
      y = "Population treatment effect"
    ) +
    ggplot2::theme_minimal()
  # Mark the breakdown value when the sweep actually brackets it; a sweep that
  # never reaches zero is the robust case and gets no line rather than a
  # misleading extrapolation.
  if (!is.na(sweep$delta_ci)) {
    p <- p + ggplot2::geom_vline(xintercept = sweep$delta_ci,
                                 linetype = "dotted", colour = "firebrick")
  }
  p
}

# ---------------------------------------------------------------------------
# design_agreement(): do the designs agree once allowed to differ?
# ---------------------------------------------------------------------------

#' Test whether the designs agree
#'
#' @description
#' Refits the model with `design_effects = TRUE`, which adds an offset to the
#' population effect mean for RCT and pre-post studies relative to DiD, and
#' returns those offsets. They are the model's own estimate of how far each
#' design disagrees with the DiD anchor --- the assumption that all designs
#' estimate one common effect, made checkable.
#'
#' @details
#' Offsets near zero support pooling. An offset that excludes zero says the
#' designs are not exchangeable, which may reflect a real difference (selection,
#' different follow-up, a different estimand) or a fault in how that design is
#' being handled. It will not tell you which.
#'
#' One refit, so it costs what the original fit cost. Note that
#' `design_effects = TRUE` re-references `treatment_effect_mean` to the DiD
#' studies, so the pooled number from the refit is not the one to report ---
#' only the offsets.
#'
#' Prefer this to fitting each design subset separately. Subsets of post-only
#' studies have to pin the time trend at zero to be identified at all, which
#' puts their effect on the control follow-up scale rather than the baseline
#' scale --- a difference of a factor \eqn{(1+\tilde\beta)}, around 9% at a
#' typical trend, which is larger than the disagreement you would be looking for.
#'
#' @param fit A [meta_did()] fit carrying more than one design.
#' @param probs Interval probabilities.
#' @param quiet Suppress the refit message.
#' @return A list with `offsets` (a data frame of `design`, `estimate`, `lo`,
#'   `hi`, `n_studies`) and the refitted object `fit`.
#' @seealso [plot_design_agreement()]
#' @export
design_agreement <- function(fit, probs = c(0.05, 0.95), quiet = FALSE) {
  stopifnot(inherits(fit, "meta_did_fit"))
  if (is.null(fit$fit_args)) {
    stop("This fit does not carry the arguments it was created with, so it ",
         "cannot be refitted reproducibly. Refit the model with the current ",
         "version of the package first.", call. = FALSE)
  }
  counts <- .design_counts(fit)
  present <- names(counts)[counts > 0]
  if (length(intersect(present, c("rct", "pp"))) == 0) {
    stop("Only DiD studies are present, so there are no design offsets to ",
         "estimate.", call. = FALSE)
  }
  if (!"did" %in% present) {
    stop("Design offsets are measured relative to the DiD studies, and this ",
         "fit has none.", call. = FALSE)
  }

  if (!quiet) message("design_agreement: refitting with design_effects = TRUE")
  args <- utils::modifyList(fit$fit_args, list(design_effects = TRUE))
  f <- do.call(.meta_did_core, args)

  rows <- list()
  for (d in c("rct", "pp")) {
    if (counts[[d]] == 0) next
    v <- if (d == "rct") "delta_rct_raw" else "delta_pp_raw"
    draws <- as.numeric(f$fit$draws(v, format = "draws_matrix"))
    rows[[d]] <- data.frame(
      design    = d,
      estimate  = mean(draws),
      lo        = unname(stats::quantile(draws, probs[1])),
      hi        = unname(stats::quantile(draws, probs[2])),
      n_studies = counts[[d]]
    )
  }
  list(offsets = do.call(rbind, rows), fit = f)
}

#' Plot design offsets
#'
#' @param agreement The object returned by [design_agreement()].
#' @return A ggplot2 object.
#' @seealso [design_agreement()]
#' @export
plot_design_agreement <- function(agreement) {
  d <- agreement$offsets
  d$label <- sprintf("%s (n = %d)", toupper(d$design), d$n_studies)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$estimate, y = .data$label)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$lo, xmax = .data$hi),
                            height = 0.15) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::labs(x = "Offset relative to DiD studies", y = NULL) +
    ggplot2::theme_minimal()
}

# ---------------------------------------------------------------------------
# information_share(): which studies drive the result?
# ---------------------------------------------------------------------------

# Study counts by design, across both data formats.
.design_counts <- function(fit) {
  out <- c(did = 0L, did_change = 0L, rct = 0L, pp = 0L)
  if (!is.null(fit$summary_data) && nrow(fit$summary_data) > 0) {
    tb <- table(fit$summary_data$design)
    for (nm in names(tb)) if (nm %in% names(out)) out[[nm]] <- out[[nm]] + tb[[nm]]
  }
  if (!is.null(fit$individual_data) && nrow(fit$individual_data) > 0) {
    u <- unique(fit$individual_data[, c("study_id", "design")])
    tb <- table(u$design)
    for (nm in names(tb)) if (nm %in% names(out)) out[[nm]] <- out[[nm]] + tb[[nm]]
  }
  out
}

# ---------------------------------------------------------------------------
# Exact per-study information about the pooled effect
# ---------------------------------------------------------------------------

# Each summary study is, after integrating out its own theta/beta/gamma, a
# Gaussian observation whose mean is linear in the POPULATION parameters
# (mu_beta, mu_gamma, mu_theta) and whose covariance includes both its sampling
# error and the latent variances it had to borrow. Writing A_i for that design
# matrix and V_i for the covariance, total information is
#
#   I = sum_i A_i' V_i^-1 A_i
#
# and the precision about mu_theta is 1 / (I^-1)[theta, theta]. A study's
# contribution is the drop in that precision when it is removed, which is the
# per-study analogue of omega_pp() and cannot be negative: removing a study
# cannot sharpen the posterior.
#
# This is the weighting the fitted hierarchy actually implies. A naive
# inverse-variance weight ignores that a pre-post study must borrow the trend
# and a post-only study the imbalance -- but note that simply ADDING those
# variances over-corrects, because those designs also help to estimate the
# nuisance means rather than only consuming them. Only the information
# calculation gets it right.
#
# Returns NULL when the fit is outside the linear-Gaussian case (unnormalised
# baselines, individual-level studies), so the caller can fall back.
.precision_loss_per_study <- function(fit) {
  sdata <- fit$summary_data
  flags <- fit$model_flags
  if (is.null(sdata) || nrow(sdata) == 0) return(NULL)
  if (!is.null(fit$individual_data) && nrow(fit$individual_data) > 0) return(NULL)
  if (!isTRUE(flags$is_baseline_normalised == 1)) return(NULL)

  md <- function(v, default = NA_real_) {
    d <- try(fit$fit$draws(v, format = "draws_matrix"), silent = TRUE)
    if (inherits(d, "try-error") || length(d) == 0) return(default)
    stats::median(as.numeric(d))
  }
  tau_th <- md("treatment_effect_sd")
  tau_b  <- md("time_trend_sd")
  tau_g  <- md("baseline_difference_sd")
  mu_th  <- md("treatment_effect_mean")
  mu_b   <- md("time_trend_mean")
  mu_g   <- md("baseline_difference_mean", 0)
  kappa  <- md("kappa", 0.5)
  if (anyNA(c(tau_th, tau_b, tau_g, mu_th, mu_b))) return(NULL)

  # gamma's population for each study, mirroring gamma_from_raw(): a randomised
  # study is zero-centred at kappa * s_i, an unrandomised one has the fitted
  # population, and a pinned one contributes nothing.
  rand <- if ("randomisation" %in% names(sdata)) {
    r <- as.character(sdata$randomisation); r[is.na(r)] <- "none"; r
  } else rep("none", nrow(sdata))
  randomised <- rand %in% c("individual", "cluster")

  pieces <- vector("list", nrow(sdata))
  for (i in seq_len(nrow(sdata))) {
    r <- sdata[i, ]
    rho <- .rho_for_studies(fit, r)
    design <- as.character(r$design)
    # s_gamma on the normalised scale, as .gamma_scale_summary() builds it.
    s_g <- switch(design,
      did = sqrt(r$sd_pre_treatment^2 / r$n_treatment +
                 r$sd_pre_control^2 / r$n_control),
      rct = sqrt(r$sd_post_treatment^2 / r$n_treatment +
                 r$sd_post_control^2 / r$n_control),
      NA_real_)
    var_g <- if (is.na(s_g)) 0 else if (randomised[i]) (kappa * s_g)^2 else tau_g^2
    mu_gi <- if (!is.na(s_g) && randomised[i]) 0 else mu_g

    if (design == "did") {
      m <- c(r$mean_post_control, r$mean_pre_treatment, r$mean_post_treatment)
      v_cb <- r$sd_pre_control^2 / r$n_control
      v_ca <- r$sd_post_control^2 / r$n_control
      c_c  <- rho * r$sd_pre_control * r$sd_post_control / r$n_control
      v_tb <- r$sd_pre_treatment^2 / r$n_treatment
      v_ta <- r$sd_post_treatment^2 / r$n_treatment
      c_t  <- rho * r$sd_pre_treatment * r$sd_post_treatment / r$n_treatment
      S <- matrix(0, 3, 3)
      S[1, 1] <- v_ca - 2 * m[1] * c_c + m[1]^2 * v_cb
      S[2, 2] <- v_tb + m[2]^2 * v_cb
      S[3, 3] <- v_ta + m[3]^2 * v_cb
      S[1, 2] <- S[2, 1] <- -m[2] * c_c + m[1] * m[2] * v_cb
      S[1, 3] <- S[3, 1] <- -m[3] * c_c + m[1] * m[3] * v_cb
      S[2, 3] <- S[3, 2] <- c_t + m[2] * m[3] * v_cb
      # Order of the latent and population parameters: (beta, gamma, theta).
      A <- matrix(c(1, 0, 0, 0, 1, 0, 1, 1, 1), nrow = 3, byrow = TRUE)
      V <- S + A %*% diag(c(tau_b^2, var_g, tau_th^2)) %*% t(A)
      pieces[[i]] <- list(A = A, V = V)

    } else if (design == "did_change") {
      v <- r$sd_change_control^2 / r$n_control +
           r$sd_change_treatment^2 / r$n_treatment
      pieces[[i]] <- list(A = matrix(c(0, 0, 1), nrow = 1),
                          V = matrix(v + tau_th^2, 1, 1))

    } else if (design == "rct") {
      se2 <- r$sd_post_treatment^2 / r$n_treatment +
             r$mean_post_treatment^2 * r$sd_post_control^2 / r$n_control
      if (isTRUE(flags$is_time_trend_rct_zero == 1) ||
          isTRUE(flags$is_time_trend_rct_summary_zero == 1)) {
        pieces[[i]] <- list(A = matrix(c(0, 1, 1), nrow = 1),
                            V = matrix(se2 + var_g + tau_th^2, 1, 1))
      } else {
        # Apparent effect (theta + gamma)/(1 + beta): linearised, as the
        # marginal likelihood does.
        den <- 1 + mu_b
        num <- mu_th + mu_gi
        A <- matrix(c(-num / den^2, 1 / den, 1 / den), nrow = 1)
        V <- matrix(se2 + (tau_th^2 + var_g) / den^2 +
                      num^2 / den^4 * tau_b^2, 1, 1)
        pieces[[i]] <- list(A = A, V = V)
      }

    } else if (design == "pp") {
      v <- (r$sd_pre_treatment^2 + r$sd_post_treatment^2 -
            2 * rho * r$sd_pre_treatment * r$sd_post_treatment) / r$n_treatment
      zero_trend <- isTRUE(flags$is_time_trend_pp_zero == 1) ||
                    isTRUE(flags$is_time_trend_pp_summary_zero == 1)
      A <- if (zero_trend) matrix(c(0, 0, 1), nrow = 1) else matrix(c(1, 0, 1), nrow = 1)
      V <- matrix(v + tau_th^2 + if (zero_trend) 0 else tau_b^2, 1, 1)
      pieces[[i]] <- list(A = A, V = V)

    } else {
      return(NULL)
    }
  }

  contrib <- function(p) t(p$A) %*% solve(p$V) %*% p$A
  I_all <- Reduce(`+`, lapply(pieces, contrib))
  prec <- function(I) {
    inv <- try(solve(I), silent = TRUE)
    if (inherits(inv, "try-error")) return(NA_real_)
    1 / inv[3, 3]
  }
  p_all <- prec(I_all)
  drops <- vapply(pieces, function(p) p_all - prec(I_all - contrib(p)), numeric(1))
  if (anyNA(drops) || p_all <= 0) return(NULL)
  # The FRACTION of precision lost, deliberately not normalised. These sum to
  # one only when no design enables another -- a DiD-only fit, say. In a mixed
  # analysis they sum to more, because dropping a DiD study also costs the
  # pre-post studies the trend information they borrow, and that excess is
  # worth seeing rather than dividing away.
  drops / p_all
}

#' Share of the evidence each study and design contributes
#'
#' @description
#' Which studies the pooled effect is actually resting on, which a table of
#' estimates never shows. Two columns, answering two different questions:
#'
#' * `info_loss` is the fraction of the precision of \eqn{\mu_\theta} lost by
#'   dropping that study, from the information matrix over
#'   \eqn{(\mu_\beta, \mu_\gamma, \mu_\theta)} under the fitted hierarchy.
#'   **It does not sum to one.**
#' * `weight_naive` is the inverse-variance weight,
#'   \eqn{w_i \propto 1/(s_i^2 + \tau_\theta^2)}, which is what a
#'   conventional meta-analysis of the same point estimates would use. It does
#'   sum to one.
#'
#' @details
#' `info_loss` sums to one only when no design enables another -- a DiD-only
#' fit, where it reduces to a partition of the evidence (verified in
#' `test-sensitivity.R`). In a mixed analysis it sums to **more** than one,
#' because dropping a DiD study also costs the pre-post studies the trend
#' information they borrow from it. That excess is the point: on a 10/10/10
#' mixed fit the losses were 0.73 for the DiD arm, 0.19 for pre-post and 0.09
#' for post-only, summing to 1.01 -- the DiD studies are close to
#' indispensable, and not because they carry most of the weight on the mean.
#'
#' For a design-level *share*, which does partition, use [omega_pp()]. The two
#' are not interchangeable: on that same fit it puts the pre-post share at
#' 0.414 against a naive 0.410. Note also that the obvious fix to the naive
#' weight -- adding \eqn{\tau_\beta^2} or \eqn{\tau_\gamma^2} to those
#' designs' denominators -- **over**-corrects, giving 0.367, because those
#' designs help to estimate the nuisance means rather than only consuming them.
#'
#' Both columns concern the **mean**. Neither says anything about a study's
#' effect on the between-study SD, and a study carrying the smallest weight in
#' an analysis can still double the width of the pooled interval -- see
#' [study_influence()].
#'
#' \eqn{s_i} is the design-appropriate sampling SD: the double-difference SE for
#' DiD, the change-score SE for pre-post, the ratio SE for post-only. Variance
#' components are taken at their posterior medians. `info_loss` needs the
#' linear-Gaussian case (summary data, baseline normalisation) and is `NA`
#' outside it.
#'
#' @param fit A [meta_did()] fit with summary-level studies.
#' @return A data frame with one row per study: `study_id`, `design`, `se`,
#'   `info_loss` (not summing to one; `NA` outside the linear-Gaussian case)
#'   and `weight_naive` (summing to one), ordered by decreasing `info_loss`.
#' @seealso [plot_information_share()], [omega_pp()], [study_influence()]
#' @export
information_share <- function(fit) {
  stopifnot(inherits(fit, "meta_did_fit"))
  sdata <- fit$summary_data
  if (is.null(sdata) || nrow(sdata) == 0) {
    stop("information_share() needs summary-level data; for individual-level ",
         "studies the per-study sampling variances are parameters rather ",
         "than data.", call. = FALSE)
  }
  tau <- stats::median(as.numeric(
    fit$fit$draws("treatment_effect_sd", format = "draws_matrix")
  ))

  se <- rep(NA_real_, nrow(sdata))
  for (i in seq_len(nrow(sdata))) {
    r <- sdata[i, ]
    se[i] <- switch(
      as.character(r$design),
      # Double difference: both arms' changes.
      did = {
        rho <- .rho_for_studies(fit, r)
        sqrt(.change_var(r$sd_pre_control, r$sd_post_control, rho, r$n_control) +
             .change_var(r$sd_pre_treatment, r$sd_post_treatment, rho, r$n_treatment))
      },
      did_change = sqrt(r$sd_change_control^2 / r$n_control +
                        r$sd_change_treatment^2 / r$n_treatment),
      # Arm contrast at follow-up.
      rct = sqrt(r$sd_post_control^2 / r$n_control +
                 r$sd_post_treatment^2 / r$n_treatment),
      # Within-arm change.
      pp = {
        rho <- .rho_for_studies(fit, r)
        sqrt(.change_var(r$sd_pre_treatment, r$sd_post_treatment, rho, r$n_treatment))
      },
      NA_real_
    )
  }
  w <- 1 / (se^2 + tau^2)
  out <- data.frame(
    study_id     = sdata$study_id,
    design       = sdata$design,
    se           = se,
    weight_naive = w / sum(w, na.rm = TRUE)
  )
  out$info_loss <- .precision_loss_per_study(fit)   # NULL -> column dropped
  if (is.null(out$info_loss)) out$info_loss <- NA_real_
  out <- out[, c("study_id", "design", "se", "info_loss", "weight_naive")]
  key <- if (all(is.na(out$info_loss))) out$weight_naive else out$info_loss
  out[order(-key), , drop = FALSE]
}

#' Plot the share of the evidence by study
#'
#' @param fit A [meta_did()] fit, or the data frame from [information_share()].
#' @param top Show only this many studies, pooling the rest. `NULL` shows all.
#' @return A ggplot2 object.
#' @seealso [information_share()]
#' @export
plot_information_share <- function(fit, top = 20L,
                                   column = c("info_loss", "weight_naive")) {
  column <- match.arg(column)
  d <- if (inherits(fit, "meta_did_fit")) information_share(fit) else fit
  if (column == "info_loss" && all(is.na(d$info_loss))) column <- "weight_naive"
  d$value <- d[[column]]
  d <- d[order(-d$value), , drop = FALSE]
  if (!is.null(top) && nrow(d) > top) {
    rest <- d[1, ]
    rest[] <- NA
    rest$study_id <- sprintf("(%d others)", nrow(d) - top)
    rest$value <- sum(d$value[-seq_len(top)])
    d <- rbind(d[seq_len(top), ], rest)
  }
  d$study_id <- factor(d$study_id, levels = rev(d$study_id))
  lab <- if (column == "info_loss") {
    "Precision about the pooled effect lost by dropping the study"
  } else "Inverse-variance weight on the pooled mean"
  ggplot2::ggplot(d, ggplot2::aes(x = .data$value, y = .data$study_id,
                                  fill = .data$design)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = lab, y = NULL, fill = "Design") +
    ggplot2::theme_minimal()
}

# ---------------------------------------------------------------------------
# study_influence(): which studies move the posterior?
# ---------------------------------------------------------------------------

#' Per-study influence on the pooled result
#'
#' @description
#' Leave-one-study-out influence, via Pareto-smoothed importance sampling on the
#' marginal likelihood, with optional exact refits for the studies PSIS cannot
#' approximate. Answers "would this result survive without study *i*?".
#'
#' @details
#' Two things are reported, and they are not the same:
#'
#' * `weight` is that study's inverse-variance share, from
#'   [information_share()]. It governs the pull on the pooled **mean**, and
#'   `info_loss` beside it is how much precision the fit loses without the
#'   study.
#' * `khat` is the Pareto diagnostic from PSIS. It is formally a statement about
#'   whether the importance-sampling approximation is reliable, but it is
#'   elevated precisely when the leave-this-out posterior is far from the full
#'   one -- so a high value means removing the study moves the posterior.
#'
#' A low weight does **not** make a high `khat` ignorable. Weight describes the
#' effect on the mean, while a discordant study acts on the between-study SD,
#' and that widens every interval. In testing, the lowest-weighted study of
#' thirteen (weight 0.069) doubled `treatment_effect_sd` and doubled the width
#' of the pooled interval. That is why the refits report both.
#'
#' PSIS runs on `log_lik_marginal`, which integrates out the study-level
#' parameters. The conditional `log_lik` is unusable for this: a study's own
#' theta is informed almost entirely by that study, so leaving it out makes the
#' importance ratios heavy-tailed for every study regardless of influence
#' (measured at 18 of 20 studies above 0.7 on a well-behaved fit). The marginal
#' version is only available for configurations where the integral is exact ---
#' see `shared_generated_quantities.stan` --- and this function says so rather
#' than falling back silently.
#'
#' @param fit A [meta_did()] fit.
#' @param refit Whether to refit without studies PSIS flags. `"none"` (default)
#'   does no fitting and costs nothing. `"flagged"` refits once per study with
#'   `khat` above `k_threshold`. `"all"` refits once per study. Refits are
#'   expensive, especially with individual-level data, so nothing fits unless
#'   asked.
#' @param k_threshold Pareto k above which a study is flagged.
#' @param quiet Suppress the message naming how many refits will run.
#' @return A data frame with one row per study: `study_id`, `design`, `weight`
#'   (the inverse-variance weight on the mean), `info_loss` (the precision about
#'   the pooled effect lost by dropping the study), `elpd_i`, `khat`, `flagged`,
#'   and --- when refits ran --- `d_effect_mean`, `d_effect_sd` and
#'   `d_ci_width`, the changes from removing that study.
#' @seealso [information_share()], [sensitivity_trend()]
#' @export
study_influence <- function(fit, refit = c("none", "flagged", "all"),
                            k_threshold = 0.7, quiet = FALSE) {
  stopifnot(inherits(fit, "meta_did_fit"))
  refit <- match.arg(refit)
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("study_influence() needs the 'loo' package.", call. = FALSE)
  }

  llm <- try(fit$fit$draws("log_lik_marginal"), silent = TRUE)
  if (inherits(llm, "try-error") || length(llm) == 0) {
    stop("This fit carries no marginal likelihood, so a per-study PSIS ",
         "diagnostic would not be interpretable. It needs summary-level data ",
         "(an individual study's per-arm SDs sit in the variance, so there is ",
         "no closed form), baseline normalisation, a normal heterogeneity ",
         "distribution, rho reported for every study, and the differenced ",
         "pre-post likelihood. Refit within those limits, or use ",
         "study_influence(refit = \"all\"), which needs none of this but ",
         "costs one fit per study.", call. = FALSE)
  }
  # Approximate rather than exact when post-only studies carry an estimated
  # trend: their apparent effect is a ratio of normals and the marginal uses the
  # delta method. Worth saying, not worth refusing over -- the neglected terms
  # go as the fourth power of tau_beta / (1 + mu_beta).
  exact <- try(as.numeric(fit$fit$draws("marginal_exact",
                                        format = "draws_matrix"))[1],
               silent = TRUE)
  if (!inherits(exact, "try-error") && isTRUE(exact == 0) && !quiet) {
    warning("The marginal likelihood is first-order for post-only studies ",
            "with an estimated time trend, because their apparent effect is a ",
            "ratio of normals. Pareto k and elpd are approximate here; the ",
            "ordering is still informative, and a study flagged near the ",
            "threshold is worth confirming with refit = \"flagged\".",
            call. = FALSE)
  }

  l <- loo::loo(llm)
  shares <- information_share(fit)
  sdata <- fit$summary_data
  out <- data.frame(
    study_id = sdata$study_id,
    design   = sdata$design,
    weight   = shares$weight_naive[match(sdata$study_id, shares$study_id)],
    info_loss = shares$info_loss[match(sdata$study_id, shares$study_id)],
    elpd_i   = l$pointwise[, "elpd_loo"],
    khat     = l$diagnostics$pareto_k
  )
  out$flagged <- out$khat > k_threshold

  which_refit <- switch(refit,
    none    = integer(0),
    flagged = which(out$flagged),
    all     = seq_len(nrow(out))
  )
  if (length(which_refit) == 0) {
    return(out[order(-out$khat), , drop = FALSE])
  }
  if (is.null(fit$fit_args)) {
    stop("Refits need the arguments this fit was created with, which it does ",
         "not carry. Refit the model with the current version of the package.",
         call. = FALSE)
  }
  if (!quiet) {
    message(sprintf("study_influence: refitting %d time%s (one per flagged study)",
                    length(which_refit), if (length(which_refit) == 1) "" else "s"))
  }

  base <- .effect_summary(fit)
  out$d_effect_mean <- NA_real_
  out$d_effect_sd   <- NA_real_
  out$d_ci_width    <- NA_real_
  for (j in which_refit) {
    drop_id <- out$study_id[j]
    args <- fit$fit_args
    if (!is.null(args$summary_data)) {
      args$summary_data <- args$summary_data[args$summary_data$study_id != drop_id, ,
                                             drop = FALSE]
    }
    if (!is.null(args$individual_data)) {
      args$individual_data <-
        args$individual_data[args$individual_data$study_id != drop_id, , drop = FALSE]
    }
    f <- try(do.call(.meta_did_core, args), silent = TRUE)
    if (inherits(f, "try-error")) next
    without <- .effect_summary(f)
    # Reported as "what this study contributes": full fit minus the fit without
    # it. Both the mean and the SD, because a discordant study can leave the
    # mean alone and still double the heterogeneity.
    out$d_effect_mean[j] <- base$mean - without$mean
    out$d_effect_sd[j]   <- base$sd   - without$sd
    out$d_ci_width[j]    <- base$width - without$width
  }
  out[order(-out$khat), , drop = FALSE]
}

# Pooled effect mean, between-study SD and interval width for one fit.
.effect_summary <- function(fit) {
  mu  <- as.numeric(fit$fit$draws("treatment_effect_mean", format = "draws_matrix"))
  tau <- as.numeric(fit$fit$draws("treatment_effect_sd",   format = "draws_matrix"))
  list(mean  = mean(mu),
       sd    = mean(tau),
       width = unname(diff(stats::quantile(mu, c(0.05, 0.95)))))
}

#' Plot per-study influence against weight
#'
#' @description
#' Weight on the horizontal axis, Pareto k on the vertical, with the flagging
#' threshold marked. The top-right corner carries the result and disagrees with
#' it; the top-left disagrees without carrying much weight on the mean, which
#' typically means it is acting on the between-study SD instead.
#'
#' @param influence The data frame from [study_influence()].
#' @param k_threshold Where to draw the threshold line.
#' @return A ggplot2 object.
#' @seealso [study_influence()]
#' @export
plot_study_influence <- function(influence, k_threshold = 0.7) {
  ggplot2::ggplot(influence, ggplot2::aes(x = .data$weight, y = .data$khat,
                                          colour = .data$design)) +
    ggplot2::geom_hline(yintercept = k_threshold, linetype = "dashed") +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::labs(x = "Inverse-variance weight on the pooled mean",
                  y = "Pareto k (leave-one-study-out)", colour = "Design") +
    ggplot2::theme_minimal()
}
