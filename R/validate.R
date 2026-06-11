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
#'   For repeated-measures designs (`"did"` and `"pp"`), a `subject_id`
#'   column is also required to correctly pair pre and post observations.
#'   Valid designs: `"did"`, `"rct"`, `"pp"`.
#'
#' @return `data` invisibly if valid; otherwise stops with an informative error.
#' @export
validate_individual_data <- function(data) {
  if (is.null(data)) return(invisible(NULL))

  .check_cols(data, c("study_id", "design", "group", "time", "value"),
              context = "individual_data")

  # DiD and PP designs have repeated measures and require subject_id for

  # correct pre/post pairing in the bivariate normal likelihood.
  needs_subject_id <- unique(data$design) %in% c("did", "pp")
  if (any(needs_subject_id) && !"subject_id" %in% names(data)) {
    designs <- unique(data$design[data$design %in% c("did", "pp")])
    stop(
      "individual_data must include a 'subject_id' column for repeated-measures ",
      "designs (", paste(designs, collapse = ", "), "). Each subject must have ",
      "exactly one 'pre' and one 'post' observation.",
      call. = FALSE
    )
  }

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
    # Require complete group/time structure per design
    groups <- unique(sub$group)
    times  <- unique(sub$time)
    if (design == "did") {
      if (!all(c("control", "treatment") %in% groups))
        stop("Study '", study, "' (did): must have both 'control' and 'treatment' groups.",
             call. = FALSE)
      if (!all(c("pre", "post") %in% times))
        stop("Study '", study, "' (did): must have both 'pre' and 'post' time points.",
             call. = FALSE)
    }
    if (design == "rct") {
      if (!all(c("control", "treatment") %in% groups))
        stop("Study '", study, "' (rct): must have both 'control' and 'treatment' groups.",
             call. = FALSE)
    }
    if (design == "pp") {
      if (!"treatment" %in% groups)
        stop("Study '", study, "' (pp): must have a 'treatment' group.",
             call. = FALSE)
      if (!all(c("pre", "post") %in% times))
        stop("Study '", study, "' (pp): must have both 'pre' and 'post' time points.",
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
      # Check subject_id is unique within (study, group, time)
      dupes <- sub[duplicated(sub[, c("group", "time", "subject_id")]), ]
      if (nrow(dupes) > 0) {
        stop(
          "Study '", study, "': duplicate subject_id values within the same ",
          "group/time combination. Each subject must have exactly one 'pre' ",
          "and one 'post' observation per group.",
          call. = FALSE
        )
      }
      # Check every subject has both pre and post within each group
      for (grp in unique(sub$group)) {
        grp_sub <- sub[sub$group == grp, ]
        subj_times <- table(grp_sub$subject_id, grp_sub$time)
        incomplete <- rownames(subj_times)[apply(subj_times, 1, function(r) any(r == 0))]
        if (length(incomplete) > 0) {
          stop(
            "Study '", study, "', group '", grp, "': subject_id(s) ",
            paste(head(incomplete, 3), collapse = ", "),
            if (length(incomplete) > 3) ", ..." else "",
            " are missing a 'pre' or 'post' observation.",
            call. = FALSE
          )
        }
      }
    }
  }

  invisible(data)
}

# ---------------------------------------------------------------------------
# validate_covariates()
# ---------------------------------------------------------------------------

#' Validate study-level covariates for meta-regression
#'
#' Checks that covariate columns exist, are numeric, contain no `NA` values,
#' and (for individual-level data) are constant within each study.
#'
#' @param covariate_names Character vector of column names to use as covariates.
#' @param summary_data Summary-level data frame (or NULL).
#' @param individual_data Individual-level data frame (or NULL).
#'
#' @return Invisible NULL. Stops with an error if validation fails.
#' @keywords internal
validate_covariates <- function(covariate_names, summary_data, individual_data) {
  if (is.null(covariate_names) || length(covariate_names) == 0) {
    return(invisible(NULL))
  }

  # Check columns exist in summary_data

if (!is.null(summary_data) && nrow(summary_data) > 0) {
    missing <- setdiff(covariate_names, names(summary_data))
    if (length(missing) > 0) {
      stop(
        "Covariate columns not found in summary_data: ",
        paste(missing, collapse = ", "), ".",
        call. = FALSE
      )
    }
    for (col in covariate_names) {
      if (!is.numeric(summary_data[[col]])) {
        stop("Covariate '", col, "' in summary_data must be numeric.", call. = FALSE)
      }
      if (any(is.na(summary_data[[col]]))) {
        stop("Covariate '", col, "' in summary_data contains NA values.", call. = FALSE)
      }
    }
  }

  # Check columns exist in individual_data and are constant within study
  if (!is.null(individual_data) && nrow(individual_data) > 0) {
    missing <- setdiff(covariate_names, names(individual_data))
    if (length(missing) > 0) {
      stop(
        "Covariate columns not found in individual_data: ",
        paste(missing, collapse = ", "), ".",
        call. = FALSE
      )
    }
    for (col in covariate_names) {
      if (!is.numeric(individual_data[[col]])) {
        stop("Covariate '", col, "' in individual_data must be numeric.", call. = FALSE)
      }
      if (any(is.na(individual_data[[col]]))) {
        stop("Covariate '", col, "' in individual_data contains NA values.", call. = FALSE)
      }
      # Check constant within study
      n_unique <- tapply(individual_data[[col]], individual_data$study_id,
                         function(x) length(unique(x)))
      bad_studies <- names(n_unique)[n_unique > 1]
      if (length(bad_studies) > 0) {
        stop(
          "Covariate '", col, "' varies within study in individual_data ",
          "(must be constant within study). Problem studies: ",
          paste(head(bad_studies, 3), collapse = ", "),
          if (length(bad_studies) > 3) ", ..." else "", ".",
          call. = FALSE
        )
      }
    }
  }

  invisible(NULL)
}

# ---------------------------------------------------------------------------
# validate_multiplicative_covariate()
# ---------------------------------------------------------------------------

#' Validate a study-level multiplicative covariate
#'
#' Checks that the multiplicative-covariate column exists, is binary
#' (values in `{0, 1}`, no `NA`s), and is constant within each study for
#' individual-level data. Also performs two identifiability checks:
#'
#' 1. The column must vary across studies. If every study has the same
#'    value (all 0 or all 1), the multiplier and the population mean
#'    \eqn{\mu_\theta} are jointly unidentified — any pair
#'    (mean, multiplier) with the same product gives identical
#'    likelihood. This is a hard error.
#' 2. The column must not be perfectly collinear with any additive
#'    covariate. When `cor(x_mult, x_cov_k) > 0.95` for some `k`, the
#'    multiplier and that covariate's coefficient become weakly identified:
#'    the model will still run but `multiplier` and `beta_cov[k]` will be
#'    highly correlated in the posterior. This is a soft warning.
#'
#' @param multiplicative_covariate_name Single column name (character of length 1).
#' @param covariate_names Character vector of additive covariate names (or NULL).
#' @param summary_data Summary-level data frame (or NULL).
#' @param individual_data Individual-level data frame (or NULL).
#'
#' @return Invisible NULL. Stops with an error or emits a warning if a
#'   validation or identifiability check fails.
#' @keywords internal
validate_multiplicative_covariate <- function(multiplicative_covariate_name,
                                              covariate_names,
                                              summary_data, individual_data) {
  if (is.null(multiplicative_covariate_name)) {
    return(invisible(NULL))
  }

  if (!is.character(multiplicative_covariate_name) ||
      length(multiplicative_covariate_name) != 1) {
    stop(
      "'multiplicative_covariate' must be a single column name (character of length 1).",
      call. = FALSE
    )
  }

  col <- multiplicative_covariate_name

  # Cannot overlap with the additive covariate list
  if (col %in% covariate_names) {
    stop(
      "Column '", col, "' is listed in both 'covariates' and ",
      "'multiplicative_covariate'. A covariate must be one or the other.",
      call. = FALSE
    )
  }

  .check_binary <- function(values, context) {
    if (any(is.na(values))) {
      stop("Multiplicative covariate '", col,
           "' in ", context, " contains NA values.", call. = FALSE)
    }
    if (!is.numeric(values)) {
      stop("Multiplicative covariate '", col,
           "' in ", context, " must be numeric (binary 0/1).", call. = FALSE)
    }
    bad <- setdiff(unique(values), c(0, 1))
    if (length(bad) > 0) {
      stop(
        "Multiplicative covariate '", col, "' in ", context,
        " must be binary (values in {0, 1}). Found other values: ",
        paste(utils::head(bad, 5), collapse = ", "),
        if (length(bad) > 5) ", ..." else "", ".",
        call. = FALSE
      )
    }
  }

  # Collect the per-study values across both data sources for identifiability checks
  study_values <- numeric(0)
  study_covs_for_collinearity <- NULL

  if (!is.null(summary_data) && nrow(summary_data) > 0) {
    if (!col %in% names(summary_data)) {
      stop("Multiplicative covariate '", col,
           "' not found in summary_data.", call. = FALSE)
    }
    .check_binary(summary_data[[col]], "summary_data")
    study_values <- c(study_values, summary_data[[col]])
    if (length(covariate_names) > 0) {
      study_covs_for_collinearity <- summary_data[, covariate_names, drop = FALSE]
    }
  }

  if (!is.null(individual_data) && nrow(individual_data) > 0) {
    if (!col %in% names(individual_data)) {
      stop("Multiplicative covariate '", col,
           "' not found in individual_data.", call. = FALSE)
    }
    .check_binary(individual_data[[col]], "individual_data")
    # Constant within study
    n_unique <- tapply(individual_data[[col]], individual_data$study_id,
                       function(x) length(unique(x)))
    bad_studies <- names(n_unique)[n_unique > 1]
    if (length(bad_studies) > 0) {
      stop(
        "Multiplicative covariate '", col, "' varies within study in ",
        "individual_data (must be constant within study). Problem studies: ",
        paste(utils::head(bad_studies, 3), collapse = ", "),
        if (length(bad_studies) > 3) ", ..." else "", ".",
        call. = FALSE
      )
    }
    # Collapse to one row per study for the identifiability checks
    ind_one_per_study <- individual_data[!duplicated(individual_data$study_id), , drop = FALSE]
    study_values <- c(study_values, ind_one_per_study[[col]])
    if (length(covariate_names) > 0) {
      cov_block <- ind_one_per_study[, covariate_names, drop = FALSE]
      study_covs_for_collinearity <- if (is.null(study_covs_for_collinearity)) {
        cov_block
      } else {
        rbind(study_covs_for_collinearity, cov_block)
      }
    }
  }

  # --- Identifiability: x_mult must vary across studies ---
  unique_vals <- unique(study_values)
  if (length(unique_vals) < 2) {
    common <- unique_vals[1]
    stop(
      "Multiplicative covariate '", col, "' is constant across all studies ",
      "(every study has the value ", common, "). With no variation in '",
      col, "', the multiplier multiplier and the population treatment ",
      "effect mu are jointly unidentified — any (mu, multiplier) with the same ",
      "product gives identical likelihood. Either remove the multiplicative ",
      "covariate, or include at least one study with the opposite value.",
      call. = FALSE
    )
  }

  # --- Identifiability: warn on near-perfect collinearity with additive covariates ---
  if (!is.null(study_covs_for_collinearity) && nrow(study_covs_for_collinearity) >= 3) {
    threshold <- 0.95
    for (cv in covariate_names) {
      cv_vals <- study_covs_for_collinearity[[cv]]
      # cor() is undefined when a column is constant; guard.
      if (length(unique(cv_vals)) < 2) next
      r <- suppressWarnings(stats::cor(study_values, cv_vals))
      if (!is.na(r) && abs(r) > threshold) {
        warning(
          "Multiplicative covariate '", col, "' is nearly collinear with ",
          "additive covariate '", cv, "' (|cor| = ", round(abs(r), 3),
          " > ", threshold, "). multiplier and beta_cov[",
          which(covariate_names == cv), "] may be weakly identified; ",
          "expect a strong posterior correlation between them and wider ",
          "credible intervals than usual.",
          call. = FALSE
        )
      }
    }
  }

  invisible(NULL)
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
