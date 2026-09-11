# Shared helpers for Stan-dependent tests.
# testthat automatically sources helper-*.R files before running any tests.

# ---------------------------------------------------------------------------
# Skip guard
# ---------------------------------------------------------------------------

skip_if_no_stan <- function() {
  skip_on_cran()
  skip_on_covr()
  skip_if_not_installed("cmdstanr")
  skip_if_not(instantiate::stan_cmdstan_exists(), "CmdStan not available")
}

# ---------------------------------------------------------------------------
# Model compilation
# ---------------------------------------------------------------------------

# Locate and compile (or retrieve from cache) the Stan model, from EITHER an
# installed package or a source checkout.
#
# This used to look only in src/stan/, which exists solely in a source tree.
# R CMD check -- what CI runs -- tests the INSTALLED package, where src/ is not
# shipped and the model lives in bin/stan/ (put there by src/install.libs.R).
# So get_compiled_model() returned NULL under check, skip_if() fired, and every
# Stan-backed test in test-stan.R and test-recovery.R silently skipped. CI went
# green while exercising none of the model, which is how a fixture that trips
# the hierarchical-rho guard survived unnoticed.
#
# Returning NULL still skips, but now only when the model genuinely cannot be
# found -- not merely because the package happens to be installed.
get_compiled_model <- function() {
  # Installed package: sources and a prebuilt binary both sit in bin/stan.
  bin_stan <- system.file("bin/stan", package = "metadid")
  if (nzchar(bin_stan)) {
    stan_file <- file.path(bin_stan, "meta_analysis_master.stan")
    exe_file  <- file.path(bin_stan, "meta_analysis_master")
    if (file.exists(stan_file)) {
      args <- list(stan_file = stan_file, include_paths = bin_stan)
      # Reuse the binary compiled at install time when it is there; otherwise
      # let cmdstanr build it.
      if (file.exists(exe_file)) args$exe_file <- exe_file
      return(tryCatch(do.call(cmdstanr::cmdstan_model, args),
                      error = function(e) NULL))
    }
  }

  # Source checkout (devtools::load_all()): find.package() returns the source
  # root, so src/stan/ is reachable and cmdstanr compiles from it.
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
