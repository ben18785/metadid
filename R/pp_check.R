# Posterior predictive checks for meta_did_fit objects.

# ============================================================
# pp_check_effects()
# ============================================================

#' Posterior predictive check: study-level treatment effects
#'
#' For each study, compares the observed naive treatment effect to the
#' posterior predictive distribution under the fitted model. Uses full
#' posterior draws for all quantities including standard errors.
#'
#' @param fit A `meta_did_fit` object with `method = "sample"`.
#' @param study_id Character vector of study IDs to include. `NULL` (default)
#'   shows all studies.
#' @param prob Width of the credible shading (not currently used but reserved).
#'   Default `0.9`.
#' @return A ggplot2 object, faceted by study.
#' @export
pp_check_effects <- function(fit, study_id = NULL, prob = 0.9) {
  if (fit$method != "sample") {
    stop("pp_check_effects() requires a fit with method = 'sample'.", call. = FALSE)
  }

  obs <- compute_observed_effects(fit)
  if (!is.null(study_id)) {
    obs <- obs[obs$study_id %in% study_id, , drop = FALSE]
    if (nrow(obs) == 0) stop("No matching studies found.", call. = FALSE)
  }

  draws <- fit$fit$draws(format = "draws_matrix")
  n_draws <- nrow(draws)
  n_studies <- nrow(obs)

  # Population mean and SD per draw
  mu_mat <- build_pop_mean_matrix(obs, draws)
  sigma_draws <- as.numeric(draws[, "treatment_effect_sd"])
  sigma_mat <- matrix(sigma_draws, nrow = n_draws, ncol = n_studies)

  # Study-level SE per draw
  se_mat <- compute_se_matrix(fit, obs, draws)

  # Replicated study effects from population distribution
  if (as.logical(fit$model_flags$is_student_t_heterogeneity)) {
    nu_draws <- as.numeric(draws[, "nu_treatment_vec[1]"])
    nu_vec <- rep(nu_draws, times = n_studies)
    t_raw <- stats::rt(n_draws * n_studies, df = nu_vec)
    theta_rep <- mu_mat + sigma_mat * matrix(t_raw, nrow = n_draws)
  } else {
    theta_rep <- matrix(
      stats::rnorm(n_draws * n_studies, mean = mu_mat, sd = sigma_mat),
      nrow = n_draws
    )
  }

  # Replicated observables (theta + sampling noise)
  y_rep <- matrix(
    stats::rnorm(n_draws * n_studies, mean = theta_rep, sd = se_mat),
    nrow = n_draws
  )

  # Plot data
  rep_df <- data.frame(
    study_id = rep(obs$study_id, each = n_draws),
    y_rep = as.numeric(y_rep)
  )
  obs_df <- data.frame(study_id = obs$study_id, y_obs = obs$y_obs)

  rep_df$study_id <- factor(rep_df$study_id, levels = obs$study_id)
  obs_df$study_id <- factor(obs_df$study_id, levels = obs$study_id)

  ggplot2::ggplot(rep_df, ggplot2::aes(x = .data$y_rep)) +
    ggplot2::geom_density(fill = "skyblue", alpha = 0.5) +
    ggplot2::geom_vline(
      data = obs_df, ggplot2::aes(xintercept = .data$y_obs),
      linewidth = 0.8
    ) +
    ggplot2::facet_wrap(~ study_id, scales = "free") +
    ggplot2::labs(x = "Treatment effect", y = "Density") +
    ggplot2::theme_minimal()
}


# ============================================================
# pp_check_cdf()
# ============================================================

#' Posterior predictive check: CDF comparison
#'
#' Compares observed data to the posterior predictive distribution using
#' cumulative distribution functions.
#'
#' Two modes are available via the `type` argument:
#'
#' \describe{
#'   \item{`"individual"`}{For each individual-level study, compares the
#'     empirical CDF of observed outcomes to the posterior predictive CDF
#'     (with uncertainty ribbon), faceted by study and group/time cell.
#'     Requires individual-level data in the fit.}
#'   \item{`"summary"`}{Compares the empirical CDF of observed study-level
#'     treatment effects to the posterior predictive CDF of replicated
#'     study effects. A single-panel plot that provides an aggregate
#'     calibration check across all studies.}
#' }
#'
#' @param fit A `meta_did_fit` object with `method = "sample"`.
#' @param type Character string: `"individual"` (default) for within-study
#'   outcome distributions, or `"summary"` for the distribution of
#'   study-level treatment effects.
#' @param study_id Character vector of study IDs to include. `NULL` (default)
#'   includes all studies of the relevant type.
#' @param prob Width of the credible ribbon. Default `0.9`.
#' @return A ggplot2 object.
#' @export
pp_check_cdf <- function(fit, type = c("individual", "summary"),
                         study_id = NULL, prob = 0.9) {
  type <- match.arg(type)
  if (fit$method != "sample") {
    stop("pp_check_cdf() requires a fit with method = 'sample'.", call. = FALSE)
  }

  switch(type,
    individual = pp_check_cdf_individual(fit, study_id, prob),
    summary    = pp_check_cdf_summary(fit, study_id, prob)
  )
}


# ---- Individual-level CDF (existing logic) ----

#' @noRd
pp_check_cdf_individual <- function(fit, study_id, prob) {
  idata <- fit$individual_data
  if (is.null(idata) || nrow(idata) == 0) {
    stop(
      'pp_check_cdf(type = "individual") requires individual-level data in the fit.',
      call. = FALSE
    )
  }

  available <- unique(idata$study_id)
  selected <- if (!is.null(study_id)) intersect(study_id, available) else available
  if (length(selected) == 0) {
    stop("No matching individual-level studies found.", call. = FALSE)
  }

  draws <- fit$fit$draws(format = "draws_matrix")
  n_draws <- nrow(draws)
  is_normalised <- as.logical(fit$model_flags$is_baseline_normalised)
  n_grid <- 200
  lo_q <- (1 - prob) / 2
  hi_q <- 1 - lo_q

  result_list <- list()

  for (s in selected) {
    s_data <- idata[idata$study_id == s, ]
    design <- s_data$design[1]

    # Design index: position among studies of same design in individual data
    design_studies <- unique(idata$study_id[idata$design == design])
    design_idx <- match(s, design_studies)

    cells <- unique(s_data[, c("group", "time")])

    for (r in seq_len(nrow(cells))) {
      grp <- cells$group[r]
      tm <- cells$time[r]

      obs_vals <- s_data$value[s_data$group == grp & s_data$time == tm]
      obs_sd <- stats::sd(obs_vals)
      x_grid <- seq(
        min(obs_vals) - 0.5 * obs_sd,
        max(obs_vals) + 0.5 * obs_sd,
        length.out = n_grid
      )

      params <- get_cell_draws(draws, design, design_idx, grp, tm, is_normalised)

      # Theoretical CDF at each grid point for each draw
      cdf_matrix <- vapply(seq_len(n_draws), function(d) {
        stats::pnorm(x_grid, params$mu[d], params$sigma[d])
      }, numeric(n_grid))
      # cdf_matrix is n_grid x n_draws; transpose to n_draws x n_grid
      cdf_matrix <- t(cdf_matrix)

      cdf_lo  <- apply(cdf_matrix, 2, stats::quantile, probs = lo_q)
      cdf_hi  <- apply(cdf_matrix, 2, stats::quantile, probs = hi_q)
      cdf_med <- apply(cdf_matrix, 2, stats::median)

      ecdf_fn <- stats::ecdf(obs_vals)

      result_list[[length(result_list) + 1]] <- data.frame(
        study_id = s,
        group = grp,
        time = tm,
        x = x_grid,
        cdf_lo = cdf_lo,
        cdf_hi = cdf_hi,
        cdf_med = cdf_med,
        ecdf = ecdf_fn(x_grid),
        stringsAsFactors = FALSE
      )
    }
  }

  result_df <- do.call(rbind, result_list)
  result_df$cell <- paste(result_df$group, result_df$time, sep = " / ")
  result_df$study_id <- factor(result_df$study_id, levels = selected)

  ggplot2::ggplot(result_df) +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = .data$x, ymin = .data$cdf_lo, ymax = .data$cdf_hi),
      alpha = 0.3, fill = "skyblue"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = .data$x, y = .data$cdf_med),
      linetype = "dashed", color = "steelblue"
    ) +
    ggplot2::geom_step(ggplot2::aes(x = .data$x, y = .data$ecdf)) +
    ggplot2::facet_grid(study_id ~ cell, scales = "free_x") +
    ggplot2::labs(x = "Value", y = "Cumulative probability") +
    ggplot2::theme_minimal()
}


# ---- Summary-level CDF ----

#' @noRd
pp_check_cdf_summary <- function(fit, study_id, prob) {
  obs <- compute_observed_effects(fit)
  if (!is.null(study_id)) {
    obs <- obs[obs$study_id %in% study_id, , drop = FALSE]
  }
  if (nrow(obs) == 0) {
    stop("No studies found for pp_check_cdf(type = \"summary\").", call. = FALSE)
  }

  draws <- fit$fit$draws(format = "draws_matrix")
  n_draws <- nrow(draws)
  n_studies <- nrow(obs)

  # Population mean and SD per draw
  mu_mat <- build_pop_mean_matrix(obs, draws)
  sigma_draws <- as.numeric(draws[, "treatment_effect_sd"])
  sigma_mat <- matrix(sigma_draws, nrow = n_draws, ncol = n_studies)

  # Study-level SE per draw
  se_mat <- compute_se_matrix(fit, obs, draws)

  # Replicated study effects from population distribution
  if (as.logical(fit$model_flags$is_student_t_heterogeneity)) {
    nu_draws <- as.numeric(draws[, "nu_treatment_vec[1]"])
    nu_vec <- rep(nu_draws, times = n_studies)
    t_raw <- stats::rt(n_draws * n_studies, df = nu_vec)
    theta_rep <- mu_mat + sigma_mat * matrix(t_raw, nrow = n_draws)
  } else {
    theta_rep <- matrix(
      stats::rnorm(n_draws * n_studies, mean = mu_mat, sd = sigma_mat),
      nrow = n_draws
    )
  }

  # Replicated observables (theta + sampling noise)
  y_rep <- matrix(
    stats::rnorm(n_draws * n_studies, mean = theta_rep, sd = se_mat),
    nrow = n_draws
  )

  # Evaluate per-draw ECDFs on a common grid
  y_obs <- obs$y_obs
  all_vals <- c(y_obs, as.numeric(y_rep))
  n_grid <- 200
  x_grid <- seq(min(all_vals), max(all_vals), length.out = n_grid)

  lo_q <- (1 - prob) / 2
  hi_q <- 1 - lo_q

  # Each row of cdf_mat is one draw's ECDF evaluated at x_grid
  cdf_mat <- t(apply(y_rep, 1, function(row) stats::ecdf(row)(x_grid)))

  cdf_lo  <- apply(cdf_mat, 2, stats::quantile, probs = lo_q)
  cdf_hi  <- apply(cdf_mat, 2, stats::quantile, probs = hi_q)
  cdf_med <- apply(cdf_mat, 2, stats::median)

  pred_df <- data.frame(x = x_grid, cdf_med = cdf_med,
                        cdf_lo = cdf_lo, cdf_hi = cdf_hi)

  ecdf_fn <- stats::ecdf(y_obs)
  obs_df <- data.frame(x = x_grid, ecdf = ecdf_fn(x_grid))

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = pred_df,
      ggplot2::aes(x = .data$x, ymin = .data$cdf_lo, ymax = .data$cdf_hi),
      fill = "skyblue", alpha = 0.4
    ) +
    ggplot2::geom_line(
      data = pred_df,
      ggplot2::aes(x = .data$x, y = .data$cdf_med),
      colour = "steelblue", linetype = "dashed"
    ) +
    ggplot2::geom_step(
      data = obs_df,
      ggplot2::aes(x = .data$x, y = .data$ecdf)
    ) +
    ggplot2::labs(x = "Study-level treatment effect",
                  y = "Cumulative probability") +
    ggplot2::theme_minimal()
}


# ============================================================
# Internal helpers
# ============================================================

#' Compute observed naive treatment effects for each study
#' @noRd
compute_observed_effects <- function(fit) {
  results <- list()
  sdata <- fit$summary_data
  idata <- fit$individual_data

  # ---- Summary data ----
  if (!is.null(sdata) && nrow(sdata) > 0) {

    did <- sdata[sdata$design == "did", , drop = FALSE]
    if (nrow(did) > 0) {
      results$did_summary <- data.frame(
        study_id = did$study_id,
        design_family = "did",
        design_type = "did_summary",
        design_index = seq_len(nrow(did)),
        y_obs = (did$mean_post_treatment - did$mean_pre_treatment) -
                (did$mean_post_control - did$mean_pre_control),
        stringsAsFactors = FALSE
      )
    }

    chg <- sdata[sdata$design == "did_change", , drop = FALSE]
    if (nrow(chg) > 0) {
      results$did_change <- data.frame(
        study_id = chg$study_id,
        design_family = "did",
        design_type = "did_change",
        design_index = seq_len(nrow(chg)),
        y_obs = chg$mean_change_treatment - chg$mean_change_control,
        stringsAsFactors = FALSE
      )
    }

    rct <- sdata[sdata$design == "rct", , drop = FALSE]
    if (nrow(rct) > 0) {
      results$rct_summary <- data.frame(
        study_id = rct$study_id,
        design_family = "rct",
        design_type = "rct_summary",
        design_index = seq_len(nrow(rct)),
        y_obs = rct$mean_post_treatment - rct$mean_post_control,
        stringsAsFactors = FALSE
      )
    }

    pp <- sdata[sdata$design == "pp", , drop = FALSE]
    if (nrow(pp) > 0) {
      results$pp_summary <- data.frame(
        study_id = pp$study_id,
        design_family = "pp",
        design_type = "pp_summary",
        design_index = seq_len(nrow(pp)),
        y_obs = pp$mean_post_treatment - pp$mean_pre_treatment,
        stringsAsFactors = FALSE
      )
    }
  }

  # ---- Individual data ----
  if (!is.null(idata) && nrow(idata) > 0) {
    for (design in c("did", "rct", "pp")) {
      ind_d <- idata[idata$design == design, , drop = FALSE]
      if (nrow(ind_d) == 0) next

      study_ids <- unique(ind_d$study_id)
      y_obs <- numeric(length(study_ids))

      for (j in seq_along(study_ids)) {
        s <- ind_d[ind_d$study_id == study_ids[j], ]
        if (design == "did") {
          y_obs[j] <- (mean(s$value[s$group == "treatment" & s$time == "post"]) -
                       mean(s$value[s$group == "treatment" & s$time == "pre"])) -
                      (mean(s$value[s$group == "control" & s$time == "post"]) -
                       mean(s$value[s$group == "control" & s$time == "pre"]))
        } else if (design == "rct") {
          y_obs[j] <- mean(s$value[s$group == "treatment" & s$time == "post"]) -
                      mean(s$value[s$group == "control" & s$time == "post"])
        } else {
          y_obs[j] <- mean(s$value[s$group == "treatment" & s$time == "post"]) -
                      mean(s$value[s$group == "treatment" & s$time == "pre"])
        }
      }

      results[[paste0(design, "_individual")]] <- data.frame(
        study_id = study_ids,
        design_family = design,
        design_type = paste0(design, "_individual"),
        design_index = seq_along(study_ids),
        y_obs = y_obs,
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, results)
}


#' Build matrix of population-mean draws (n_draws x n_studies)
#' @noRd
build_pop_mean_matrix <- function(obs, draws) {
  n_draws <- nrow(draws)
  n_studies <- nrow(obs)
  mu_mat <- matrix(NA_real_, nrow = n_draws, ncol = n_studies)

  for (i in seq_len(n_studies)) {
    param <- switch(obs$design_family[i],
      did = "treatment_effect_mean",
      rct = "treatment_effect_mean_rct",
      pp  = "treatment_effect_mean_pp"
    )
    mu_mat[, i] <- as.numeric(draws[, param])
  }

  mu_mat
}


#' Compute SE matrix (n_draws x n_studies) using full posterior draws
#' @noRd
compute_se_matrix <- function(fit, obs, draws) {
  n_draws <- nrow(draws)
  n_studies <- nrow(obs)
  se_mat <- matrix(NA_real_, nrow = n_draws, ncol = n_studies)
  sdata <- fit$summary_data
  idata <- fit$individual_data

  for (i in seq_len(n_studies)) {
    dt <- obs$design_type[i]
    di <- obs$design_index[i]

    se_mat[, i] <- switch(dt,
      did_summary    = se_did_summary(sdata, di, draws),
      did_change     = se_did_change(sdata, di),
      rct_summary    = se_rct_summary(sdata, di),
      pp_summary     = se_pp_summary(sdata, di, draws),
      did_individual = se_did_individual(idata, di, draws),
      rct_individual = se_rct_individual(idata, di, draws),
      pp_individual  = se_pp_individual(idata, di, draws)
    )
  }

  se_mat
}


# ---- SE helpers per design type ----

#' @noRd
se_did_summary <- function(sdata, di, draws) {
  did <- sdata[sdata$design == "did", , drop = FALSE]
  row <- did[di, ]
  sd_cb <- row$sd_pre_control
  sd_ca <- row$sd_post_control
  sd_tb <- row$sd_pre_treatment
  sd_ta <- row$sd_post_treatment
  n_c <- row$n_control
  n_t <- row$n_treatment

  rho_val <- if ("rho" %in% names(row)) row$rho else NA_real_
  if (!is.na(rho_val)) {
    rho <- rho_val
  } else {
    rho <- get_missing_rho_draws(draws, did, di, "z_rho_missing_did_summary")
  }

  sqrt(
    (sd_cb^2 + sd_ca^2 - 2 * rho * sd_cb * sd_ca) / n_c +
    (sd_tb^2 + sd_ta^2 - 2 * rho * sd_tb * sd_ta) / n_t
  )
}

#' @noRd
se_did_change <- function(sdata, di) {
  chg <- sdata[sdata$design == "did_change", , drop = FALSE]
  row <- chg[di, ]
  rep(
    sqrt(row$sd_change_control^2 / row$n_control +
         row$sd_change_treatment^2 / row$n_treatment),
    length.out = 1
  )
}

#' @noRd
se_rct_summary <- function(sdata, di) {
  rct <- sdata[sdata$design == "rct", , drop = FALSE]
  row <- rct[di, ]
  sqrt(row$sd_post_control^2 / row$n_control +
       row$sd_post_treatment^2 / row$n_treatment)
}

#' @noRd
se_pp_summary <- function(sdata, di, draws) {
  pp <- sdata[sdata$design == "pp", , drop = FALSE]
  row <- pp[di, ]
  sd_tb <- row$sd_pre_treatment
  sd_ta <- row$sd_post_treatment
  n_t <- row$n_treatment

  rho_val <- if ("rho" %in% names(row)) row$rho else NA_real_
  if (!is.na(rho_val)) {
    rho <- rho_val
  } else {
    rho <- get_missing_rho_draws(draws, pp, di, "z_rho_missing_pp_summary")
  }

  sqrt((sd_tb^2 + sd_ta^2 - 2 * rho * sd_tb * sd_ta) / n_t)
}

#' @noRd
se_did_individual <- function(idata, di, draws) {
  scb <- as.numeric(draws[, paste0("sigma_control_before_did[", di, "]")])
  sca <- as.numeric(draws[, paste0("sigma_control_after_did[", di, "]")])
  stb <- as.numeric(draws[, paste0("sigma_treatment_before_did[", di, "]")])
  sta <- as.numeric(draws[, paste0("sigma_treatment_after_did[", di, "]")])
  rho <- as.numeric(draws[, paste0("rho_did[", di, "]")])

  ns <- individual_sample_sizes(idata, "did", di)
  sqrt(
    (scb^2 + sca^2 - 2 * rho * scb * sca) / ns$n_c +
    (stb^2 + sta^2 - 2 * rho * stb * sta) / ns$n_t
  )
}

#' @noRd
se_rct_individual <- function(idata, di, draws) {
  sca <- as.numeric(draws[, paste0("sigma_control_after_rct[", di, "]")])
  sta <- as.numeric(draws[, paste0("sigma_treatment_after_rct[", di, "]")])

  ns <- individual_sample_sizes(idata, "rct", di)
  sqrt(sca^2 / ns$n_c + sta^2 / ns$n_t)
}

#' @noRd
se_pp_individual <- function(idata, di, draws) {
  stb <- as.numeric(draws[, paste0("sigma_treatment_before_pp[", di, "]")])
  sta <- as.numeric(draws[, paste0("sigma_treatment_after_pp[", di, "]")])
  rho <- as.numeric(draws[, paste0("rho_pp[", di, "]")])

  ns <- individual_sample_sizes(idata, "pp", di)
  sqrt((stb^2 + sta^2 - 2 * rho * stb * sta) / ns$n_t)
}


# ---- Shared helpers ----

#' Get sample sizes for an individual-level study
#' @noRd
individual_sample_sizes <- function(idata, design, design_idx) {
  ind_d <- idata[idata$design == design, ]
  study_ids <- unique(ind_d$study_id)
  s_id <- study_ids[design_idx]
  s_data <- ind_d[ind_d$study_id == s_id, ]

  n_c <- sum(s_data$group == "control" & s_data$time == "pre")
  n_t <- sum(s_data$group == "treatment" & s_data$time == "pre")
  # PP has no control group; for RCT use post counts
  if (n_c == 0) n_c <- sum(s_data$group == "control" & s_data$time == "post")
  if (n_t == 0) n_t <- sum(s_data$group == "treatment" & s_data$time == "post")

  list(n_c = n_c, n_t = n_t)
}


#' Extract draws for studies with missing rho via Fisher z-transform
#' @noRd
get_missing_rho_draws <- function(draws, design_data, study_idx, param_prefix) {
  rho <- if ("rho" %in% names(design_data)) design_data$rho
         else rep(NA_real_, nrow(design_data))
  missing_position <- cumsum(is.na(rho))[study_idx]

  z_draws <- as.numeric(draws[, paste0(param_prefix, "[", missing_position, "]")])
  tanh(z_draws)
}


#' Get mu and sigma draws for a single cell (group x time) of an individual study
#' @noRd
get_cell_draws <- function(draws, design, design_idx, group, time, is_normalised) {
  idx <- design_idx

  if (design == "did") {
    baseline_c <- if (is_normalised) rep(1, nrow(draws)) else
      as.numeric(draws[, paste0("baseline_control_did[", idx, "]")])
    baseline_t <- if (is_normalised) rep(1, nrow(draws)) else
      as.numeric(draws[, paste0("baseline_treatment_did[", idx, "]")])
    tt <- as.numeric(draws[, paste0("time_trend_did[", idx, "]")])
    te <- as.numeric(draws[, paste0("treatment_effect_did[", idx, "]")])

    if (group == "control" && time == "pre") {
      list(mu = baseline_c,
           sigma = as.numeric(draws[, paste0("sigma_control_before_did[", idx, "]")]))
    } else if (group == "control" && time == "post") {
      list(mu = baseline_c + tt,
           sigma = as.numeric(draws[, paste0("sigma_control_after_did[", idx, "]")]))
    } else if (group == "treatment" && time == "pre") {
      list(mu = baseline_t,
           sigma = as.numeric(draws[, paste0("sigma_treatment_before_did[", idx, "]")]))
    } else {
      list(mu = baseline_t + tt + te,
           sigma = as.numeric(draws[, paste0("sigma_treatment_after_did[", idx, "]")]))
    }

  } else if (design == "rct") {
    baseline_c <- as.numeric(draws[, paste0("baseline_control_rct[", idx, "]")])
    # When normalised, baseline_treatment = baseline_control (randomisation)
    bt_param <- paste0("baseline_treatment_rct[", idx, "]")
    baseline_t <- if (bt_param %in% colnames(draws))
      as.numeric(draws[, bt_param]) else baseline_c

    tt_param <- paste0("time_trend_rct[", idx, "]")
    tt <- if (tt_param %in% colnames(draws))
      as.numeric(draws[, tt_param]) else rep(0, nrow(draws))

    te <- as.numeric(draws[, paste0("treatment_effect_rct[", idx, "]")])

    if (group == "control") {
      list(mu = baseline_c + tt,
           sigma = as.numeric(draws[, paste0("sigma_control_after_rct[", idx, "]")]))
    } else {
      list(mu = baseline_t + tt + te,
           sigma = as.numeric(draws[, paste0("sigma_treatment_after_rct[", idx, "]")]))
    }

  } else { # pp
    baseline_t <- if (is_normalised) rep(1, nrow(draws)) else
      as.numeric(draws[, paste0("baseline_treatment_pp[", idx, "]")])

    tt_param <- paste0("time_trend_pp[", idx, "]")
    tt <- if (tt_param %in% colnames(draws))
      as.numeric(draws[, tt_param]) else rep(0, nrow(draws))

    te <- as.numeric(draws[, paste0("treatment_effect_pp[", idx, "]")])

    if (time == "pre") {
      list(mu = baseline_t,
           sigma = as.numeric(draws[, paste0("sigma_treatment_before_pp[", idx, "]")]))
    } else {
      list(mu = baseline_t + tt + te,
           sigma = as.numeric(draws[, paste0("sigma_treatment_after_pp[", idx, "]")]))
    }
  }
}
