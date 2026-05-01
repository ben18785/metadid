
vector[n_studies_rct_summary] baseline_treatment_rct_summary_effective;

if (is_baseline_normalised)
  // Normalised: baseline_treatment = baseline_control (randomisation)
  baseline_treatment_rct_summary_effective = baseline_control_rct_summary;
else if(is_baseline_control_equal_treatment_rct_summary)
  baseline_treatment_rct_summary_effective = baseline_control_rct_summary;
else
  baseline_treatment_rct_summary_effective = baseline_treatment_rct_summary;
