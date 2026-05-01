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

# Compile (or retrieve from cache) the Stan model directly from src/stan/.
# During devtools::load_all(), find.package() returns the source root, so
# src/stan/ is reachable. cmdstanr caches compiled binaries, so subsequent
# calls within a session are fast.
get_compiled_model <- function() {
  stan_file <- file.path(
    find.package("metadid"),
    "src/stan/meta_analysis_master.stan"
  )
  if (!file.exists(stan_file)) return(NULL)
  tryCatch(
    cmdstanr::cmdstan_model(stan_file, include_paths = dirname(stan_file)),
    error = function(e) NULL
  )
}
