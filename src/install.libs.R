libs <- file.path(R_PACKAGE_DIR, "libs", R_ARCH)
dir.create(libs, recursive = TRUE, showWarnings = FALSE)
for (file in c("symbols.rds", Sys.glob(paste0("*", SHLIB_EXT)))) {
  if (file.exists(file)) {
    file.copy(file, file.path(libs, file))
  }
}
bin <- file.path(R_PACKAGE_DIR, "bin")
if (!file.exists(bin)) {
  dir.create(bin, recursive = TRUE, showWarnings = FALSE)
}
bin_stan <- file.path(bin, "stan")
fs::dir_copy(path = "stan", new_path = bin_stan)
callr::r(
  func = function(bin_stan) {
    # Compile only the master model; fragments are resolved via include_paths
    instantiate::stan_package_compile(
      models        = file.path(bin_stan, "meta_analysis_master.stan"),
      include_paths = bin_stan
    )
  },
  args = list(bin_stan = bin_stan),
  show = TRUE,
  stderr = "2>&1"
)
