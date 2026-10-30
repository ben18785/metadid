# Regenerate the printed outputs for vignette("mixed-designs").
#
# The vignette's chunks are eval = FALSE, because fitting takes minutes and
# CmdStan is not available everywhere, so its printed results are pasted in by
# hand. That is a standing hazard: there is nothing to stop a pasted block
# drifting from what the code actually produces, or being written from memory.
# This script produces every block, so the pasted text has a provenance.
#
# Run it, then copy each fenced block below the corresponding chunk. It prints
# in the order the sections appear.
#
#   Rscript vignettes/_regenerate-outputs.R > vignettes/_outputs.txt
#
# Requires cmdstanr with a working CmdStan, plus loo and priorsense for the
# last two sections.

library(metadid)
library(dplyr)

SEED <- 4718

cat("## Setup -------------------------------------------------------------\n\n")

sim <- simulate_meta_did(
  n_studies = 30, n_control = 80, n_treatment = 80,
  true_effect = -0.15, sigma_effect = 0.03,
  true_trend = -0.02, sigma_trend = 0.01,
  baseline_mean = 0.45, baseline_sd = 0.02,
  rho = 0.5, seed = SEED
)

study_ids   <- unique(sim$study_id)
true_params <- attr(sim, "true_params")
part <- function(ids) {
  s <- sim |> filter(study_id %in% ids)
  attr(s, "true_params") <- true_params |> filter(study_id %in% ids)
  s
}
mixed <- bind_rows(
  as_summary_did(part(study_ids[1:10])),
  as_summary_rct(part(study_ids[11:20])),
  as_summary_pp(part(study_ids[21:30]))
)

fit_mixed <- meta_did(summary_data = mixed, seed = SEED)

FIGDIR <- "man/figures"
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)

# The vignette's chunks are eval = FALSE, so its figures have to be written to
# man/figures and referenced, exactly as the older sections already do.
fig <- function(name, plot, width = 7, height = 4.5) {
  path <- file.path(FIGDIR, paste0("vignette-", name, ".png"))
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = 150)
  cat("\n[figure written: ", path, "]\n", sep = "")
}

block <- function(title, expr) {
  cat("\n### ", title, "\n\n```\n", sep = "")
  out <- utils::capture.output(print(expr))
  cat(paste0("#> ", out, collapse = "\n"), "\n```\n", sep = "")
}

# --- The two scenario prints, which predate the delta-method correction -----
all_did <- as_summary_did(sim)
fit_all_did <- meta_did(summary_data = all_did, seed = SEED)
block("print(fit_all_did)", fit_all_did)
block("print(fit_mixed)", fit_mixed)

# --- Checking the assumptions the data cannot -------------------------------

block("information_share(fit_mixed) |> head(5)",
      information_share(fit_mixed) |> head(5))

fig("info-share", plot_information_share(fit_mixed))

agree <- design_agreement(fit_mixed, quiet = TRUE)
block("agree$offsets", agree$offsets)
fig("design-agreement", plot_design_agreement(agree), height = 2.6)

sweep <- sensitivity_trend(fit_mixed, delta = seq(-0.06, 0.06, by = 0.03),
                           quiet = TRUE)
block("sweep$grid", sweep$grid)

fig("sensitivity-trend", plot_sensitivity_trend(sweep))

block("the three breakdown summaries", data.frame(
  omega_empirical = sweep$omega_empirical,
  delta_ci        = sweep$delta_ci,
  delta_star      = delta_star(fit_mixed)$median
))

# --- How much of this is the priors? ---------------------------------------

if (requireNamespace("priorsense", quietly = TRUE)) {
  block("powerscale_sensitivity(fit_mixed$fit, variable = ...)",
        priorsense::powerscale_sensitivity(
          fit_mixed$fit,
          variable = c("treatment_effect_mean", "treatment_effect_sd",
                       "time_trend_mean", "time_trend_sd",
                       "baseline_difference_sd")))
} else {
  cat("\n[priorsense not installed; sensitivity table not regenerated]\n")
}

wide <- set_priors(
  treatment_effect_mean  = normal(0, 20),
  treatment_effect_sd    = cauchy(10),
  time_trend_mean        = normal(0, 20),
  time_trend_sd          = cauchy(10),
  baseline_difference_sd = cauchy(0.2),
  sigma                  = cauchy(10)
)
fit_wide <- meta_did(summary_data = mixed, priors = wide, seed = SEED)
block("default vs widened priors", rbind(
  default = summary(fit_mixed)[1, c("mean", "lo", "hi")],
  wide    = summary(fit_wide)[1, c("mean", "lo", "hi")]
))

# --- Which studies would change the answer? -------------------------------
# The planted-outlier illustration. Same construction as the vignette chunk.
sim12 <- simulate_meta_did(n_studies = 12, true_effect = -0.15,
                           sigma_effect = 0.03, true_trend = -0.04,
                           sigma_trend = 0.02, seed = 21)
base12 <- as_summary_did(sim12)
base12$rho <- 0.5
odd <- base12[1, ]
odd$study_id <- "odd_one"
odd$n_control <- 20L
odd$n_treatment <- 20L
odd$mean_post_treatment <- odd$mean_pre_treatment * 1.25
fit_odd <- meta_did(summary_data = bind_rows(base12, odd), seed = 5)

if (requireNamespace("loo", quietly = TRUE)) {
  inf <- study_influence(fit_odd, refit = "flagged")
  block("study_influence(fit_odd, refit = 'flagged') -- planted study",
        inf[inf$study_id == "odd_one", ])
  fig("study-influence", plot_study_influence(inf))
  clean <- inf[inf$study_id != "odd_one", ]
  cat(sprintf("\n#>  (the twelve others: weight %.4f to %.4f, khat %.2f to %.2f)\n",
              min(clean$weight), max(clean$weight),
              min(clean$khat), max(clean$khat)))

  # The masking claim the vignette makes, regenerated rather than remembered.
  big <- odd; big$study_id <- "odd_two"
  big$n_control <- 2000L; big$n_treatment <- 2000L
  fit_two <- meta_did(summary_data = bind_rows(base12, odd, big), seed = 5)
  inf2 <- study_influence(fit_two)
  cat(sprintf("\n#>  masking: any flagged = %s, khat = %s, weight ratio %.2f\n",
              any(inf2$flagged),
              paste(round(sort(inf2$khat, decreasing = TRUE)[1:2], 2), collapse = "/"),
              max(inf2$weight) / min(inf2$weight)))
} else {
  cat("\n[loo not installed; influence output not regenerated]\n")
}

cat("\n## Done. Paste each block under its chunk in mixed-designs.Rmd.\n")
