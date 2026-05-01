// Only needed for non-normalised case; when normalised, baselines are fixed at 1.
vector[n_studies_rct_summary * (1 - is_baseline_normalised)] baseline_treatment_rct_summary_effective;

if (!is_baseline_normalised) {
  if (is_baseline_control_equal_treatment_rct_summary)
    baseline_treatment_rct_summary_effective = baseline_control_rct_summary;
  else
    baseline_treatment_rct_summary_effective = baseline_treatment_rct_summary;
}
