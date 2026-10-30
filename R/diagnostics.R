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

#' Share of the evidence each study and design contributes
#'
#' @description
#' Inverse-variance weights on the random-effects scale,
#' \eqn{w_i \propto 1/(s_i^2 + \tau_\theta^2)}, normalised to sum to one. The
#' answer to "which studies is this result actually resting on?", which a table
#' of estimates never shows.
#'
#' @details
#' \eqn{s_i} is the design-appropriate sampling SD of that study's effect
#' estimate: the double-difference SE for DiD, the change-score SE for pre-post,
#' the arm-contrast SE for post-only. \eqn{\tau_\theta} is taken at its
#' posterior median.
#'
#' This is an approximation in one respect worth knowing: it ignores the
#' coupling through the nuisance parameters, so a post-only study's weight is
#' slightly overstated relative to a DiD study's, since part of its information
#' is spent on \eqn{\gamma}. [omega_pp()] computes the exact two-design version.
#' Summary-level data only.
#'
#' @param fit A [meta_did()] fit with summary-level studies.
#' @return A data frame with one row per study: `study_id`, `design`, `se`,
#'   `weight` (summing to one), ordered by decreasing weight.
#' @seealso [plot_information_share()], [omega_pp()]
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
    study_id = sdata$study_id,
    design   = sdata$design,
    se       = se,
    weight   = w / sum(w, na.rm = TRUE)
  )
  out[order(-out$weight), , drop = FALSE]
}

#' Plot the share of the evidence by study
#'
#' @param fit A [meta_did()] fit, or the data frame from [information_share()].
#' @param top Show only this many studies, pooling the rest. `NULL` shows all.
#' @return A ggplot2 object.
#' @seealso [information_share()]
#' @export
plot_information_share <- function(fit, top = 20L) {
  d <- if (inherits(fit, "meta_did_fit")) information_share(fit) else fit
  if (!is.null(top) && nrow(d) > top) {
    rest <- data.frame(study_id = sprintf("(%d others)", nrow(d) - top),
                       design = NA_character_, se = NA_real_,
                       weight = sum(d$weight[-seq_len(top)]))
    d <- rbind(d[seq_len(top), ], rest)
  }
  d$study_id <- factor(d$study_id, levels = rev(d$study_id))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$weight, y = .data$study_id,
                                  fill = .data$design)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Share of the evidence on the pooled effect",
                  y = NULL, fill = "Design") +
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
#'   [information_share()]. It governs the pull on the pooled **mean**.
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
#' @return A data frame with one row per study: `study_id`, `design`, `weight`,
#'   `elpd_i`, `khat`, `flagged`, and --- when refits ran --- `d_effect_mean`,
#'   `d_effect_sd` and `d_ci_width`, the changes from removing that study.
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
    weight   = shares$weight[match(sdata$study_id, shares$study_id)],
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
    ggplot2::labs(x = "Share of the evidence on the pooled mean",
                  y = "Pareto k (leave-one-study-out)", colour = "Design") +
    ggplot2::theme_minimal()
}
