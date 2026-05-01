# Input validation for summary-level and individual-level study data.

# ---------------------------------------------------------------------------
# Required columns per design
# ---------------------------------------------------------------------------

.summary_required <- list(
  did = c(
    "n_control", "mean_pre_control", "mean_post_control",
    "sd_pre_control", "sd_post_control",
    "n_treatment", "mean_pre_treatment", "mean_post_treatment",
    "sd_pre_treatment", "sd_post_treatment"
  ),
  # did_change: studies that report only change means/SDs (no pre/post split)
  did_change = c(
    "n_control", "mean_change_control", "sd_change_control",
    "n_treatment", "mean_change_treatment", "sd_change_treatment"
  ),
  rct = c(
    "n_control", "mean_post_control", "sd_post_control",
    "n_treatment", "mean_post_treatment", "sd_post_treatment"
  ),
  pp = c(
    "n_treatment", "mean_pre_treatment", "mean_post_treatment",
    "sd_pre_treatment", "sd_post_treatment"
  )
)

.valid_designs <- names(.summary_required)

# ---------------------------------------------------------------------------
# validate_summary_data()
# ---------------------------------------------------------------------------

#' Validate summary-level study data
#'
#' Checks that `data` meets the structural requirements for use in
#' [meta_did()]. Called automatically inside [meta_did()]; also useful for
#' diagnosing problems before fitting.
#'
#' @param data A data frame with one row per study. Must contain `study_id`
#'   and `design` columns. Valid designs:
#'   - `"did"`: DiD studies with separate pre/post summaries for both groups.
#'   - `"did_change"`: DiD studies reporting only change means and SDs.
#'   - `"rct"`: RCT studies with post-treatment summaries only.
#'   - `"pp"`: Pre-post studies with no control group.
#'
#'   The `rho` column (pre-post correlation) is optional for `"did"` and
#'   `"pp"` designs; missing values trigger hierarchical imputation when
#'   `hierarchical_rho = TRUE`.
#'
#' @return `data` invisibly if valid; otherwise stops with an informative error.
#' @export
validate_summary_data <- function(data) {
  if (is.null(data)) return(invisible(NULL))

  # Core columns
  .check_cols(data, c("study_id", "design"), context = "summary_data")

  # design values
  bad_design <- setdiff(unique(data$design), .valid_designs)
  if (length(bad_design) > 0) {
    stop(
      "summary_data$design contains unrecognised values: ",
      paste(bad_design, collapse = ", "),
      ". Must be one of: ", paste(.valid_designs, collapse = ", "), ".",
      call. = FALSE
    )
  }

  # No duplicate study_ids
  dupes <- data$study_id[duplicated(data$study_id)]
  if (length(dupes) > 0) {
    stop(
      "Duplicate study_id values in summary_data: ",
      paste(unique(dupes), collapse = ", "), ".",
      call. = FALSE
    )
  }

  # Per-design column requirements
  for (design in intersect(.valid_designs, unique(data$design))) {
    sub      <- data[data$design == design, , drop = FALSE]
    required <- .summary_required[[design]]
    missing_cols <- setdiff(required, names(data))
    if (length(missing_cols) > 0) {
      stop(
        "summary_data is missing required columns for design '", design, "': ",
        paste(missing_cols, collapse = ", "), ".",
        call. = FALSE
      )
    }
    # Positivity checks on SDs and sample sizes
    sd_cols <- grep("^sd_", required, value = TRUE)
    n_cols  <- grep("^n_", required, value = TRUE)
    for (col in c(sd_cols, n_cols)) {
      bad <- sub$study_id[!is.na(sub[[col]]) & sub[[col]] <= 0]
      if (length(bad) > 0) {
        stop(
          "Column '", col, "' must be strictly positive. ",
          "Non-positive values in studies: ",
          paste(bad, collapse = ", "), ".",
          call. = FALSE
        )
      }
    }
  }

  invisible(data)
}

# ---------------------------------------------------------------------------
# validate_individual_data()
# ---------------------------------------------------------------------------

#' Validate individual-level study data
#'
#' Checks that `data` meets the structural requirements for use in
#' [meta_did()]. Called automatically inside [meta_did()]; also useful for
#' diagnosing problems before fitting.
#'
#' @param data A data frame in long format with one row per observation.
#'   Required columns: `study_id`, `design`, `group`, `time`, `value`.
#'   Valid designs: `"did"`, `"rct"`, `"pp"`.
#'
#' @return `data` invisibly if valid; otherwise stops with an informative error.
#' @export
validate_individual_data <- function(data) {
  if (is.null(data)) return(invisible(NULL))

  .check_cols(data, c("study_id", "design", "group", "time", "value"),
              context = "individual_data")

  # design values (individual data only supports full pre/post designs)
  bad_design <- setdiff(unique(data$design), c("did", "rct", "pp"))
  if (length(bad_design) > 0) {
    stop(
      "individual_data$design contains unrecognised values: ",
      paste(bad_design, collapse = ", "),
      ". Must be one of: 'did', 'rct', 'pp'.",
      call. = FALSE
    )
  }

  # group values
  bad_group <- setdiff(unique(data$group), c("control", "treatment"))
  if (length(bad_group) > 0) {
    stop(
      "individual_data$group must be 'control' or 'treatment'. Got: ",
      paste(bad_group, collapse = ", "), ".",
      call. = FALSE
    )
  }

  # time values
  bad_time <- setdiff(unique(data$time), c("pre", "post"))
  if (length(bad_time) > 0) {
    stop(
      "individual_data$time must be 'pre' or 'post'. Got: ",
      paste(bad_time, collapse = ", "), ".",
      call. = FALSE
    )
  }

  # Per-study checks
  for (study in unique(data$study_id)) {
    sub    <- data[data$study_id == study, , drop = FALSE]
    design <- unique(sub$design)
    if (length(design) > 1) {
      stop("Study '", study, "' has multiple design values in individual_data.",
           call. = FALSE)
    }
    if (design == "pp" && any(sub$group == "control")) {
      stop("Study '", study, "' has design 'pp' but contains control-group rows.",
           call. = FALSE)
    }
    if (design == "rct" && any(sub$time == "pre")) {
      stop("Study '", study, "' has design 'rct' but contains pre-treatment rows.",
           call. = FALSE)
    }
    # Each participant should have exactly one pre and one post value (DiD/PP)
    if (design %in% c("did", "pp")) {
      counts <- table(sub$group, sub$time)
      if (any(counts[, "pre"] != counts[, "post"])) {
        stop(
          "Study '", study, "': unequal number of 'pre' and 'post' observations ",
          "per group. Each participant should have exactly one pre and one post value.",
          call. = FALSE
        )
      }
    }
  }

  invisible(data)
}

# ---------------------------------------------------------------------------
# Internal helper
# ---------------------------------------------------------------------------

.check_cols <- function(data, required, context) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      context, " is missing required columns: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
}
