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

# Resolve and load the compiled Stan model. We support two layouts so that
# the tests can run both under devtools::load_all() (development) and under
# R CMD check (CI):
#
#   * Installed package (CI, R CMD check): src/install.libs.R copies the
#     Stan source from src/stan/ to <install_dir>/bin/stan/ and compiles
#     the master model there via instantiate::stan_package_compile(). At
#     test time we point cmdstanr at that pre-compiled binary via the
#     exe_file argument, avoiding a recompile.
#
#   * Source tree (devtools::load_all): the installed bin/stan/ doesn't
#     exist, so we fall back to compiling from src/stan/ directly.
#     cmdstanr caches the resulting binary, so subsequent calls within
#     the session are fast.
#
# Mirrors the runtime resolution in R/meta_did.R so that test-time and
# production paths are consistent.
get_compiled_model <- function() {
  bin_dir <- system.file("bin/stan", package = "metadid")
  if (nzchar(bin_dir)) {
    stan_file <- file.path(bin_dir, "meta_analysis_master.stan")
    exe_file  <- file.path(bin_dir, "meta_analysis_master")
    if (file.exists(stan_file)) {
      return(tryCatch(
        cmdstanr::cmdstan_model(
          stan_file     = stan_file,
          exe_file      = if (file.exists(exe_file)) exe_file else NULL,
          include_paths = bin_dir
        ),
        error = function(e) NULL
      ))
    }
  }
  # Dev fallback: compile from src/stan/.
  src_file <- file.path(
    find.package("metadid"),
    "src/stan/meta_analysis_master.stan"
  )
  if (!file.exists(src_file)) return(NULL)
  tryCatch(
    cmdstanr::cmdstan_model(src_file, include_paths = dirname(src_file)),
    error = function(e) NULL
  )
}
