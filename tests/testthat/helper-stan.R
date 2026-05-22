# Shared helpers for Stan-dependent tests.
# testthat automatically sources helper-*.R files before running any tests.

# ---------------------------------------------------------------------------
# Skip guard
# ---------------------------------------------------------------------------

skip_if_no_stan <- function() {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
}

# ---------------------------------------------------------------------------
# Model compilation
# ---------------------------------------------------------------------------

# Compile the Stan model directly from src/stan/. During devtools::load_all(),
# find.package() returns the source root, so src/stan/ is reachable. Under
# R CMD check, the Stan source is preserved in the install tree at the same
# relative path.
#
# We pass force_recompile = TRUE to bypass cmdstanr's binary-config
# validation. Newer cmdstanr versions (>= 0.9) require a sidecar JSON
# config alongside any cached binary; binaries produced by older paths
# (notably instantiate's install-time compile) lack this and would
# otherwise raise "No CmdStan config files found. Set
# 'save_cmdstan_config=TRUE' when fitting the model.", surfaced in CI as
# the entire Stan-dependent test set being skipped. The cost is a fresh
# compile per CI run (~30-60s); the benefit is robustness across
# cmdstanr version changes.
get_compiled_model <- function() {
  stan_file <- file.path(
    find.package("metadid"),
    "src/stan/meta_analysis_master.stan"
  )
  if (!file.exists(stan_file)) return(NULL)
  tryCatch(
    cmdstanr::cmdstan_model(
      stan_file,
      include_paths   = dirname(stan_file),
      force_recompile = TRUE
    ),
    error = function(e) NULL
  )
}
