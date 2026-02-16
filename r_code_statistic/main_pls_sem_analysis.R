# PLS-SEM ANALYSIS - MAIN WORKFLOW
# Single-model moderated mediation using PLS-SEM with task_complexity as first-stage moderator

# Load utility functions
source("r_scripts/utils_pls_sem.R")
install_and_load_packages()

# General settings
set.seed(12345)

# LOAD DATA
data <- read_excel("data/daten_unipark.xlsx")
N_initial <- nrow(data)
cat("Initial N (raw data):", N_initial, "\n")

# Convert ALL negative values to NA (generic handling)
data <- convert_missing_to_na(data, item_pattern = "^v_")

# Verify conversion was successful
likert_items <- names(data)[grepl("^v_", names(data))]
remaining_neg <- sum(sapply(data[, likert_items], function(x) sum(x < 0, na.rm = TRUE)))
if (remaining_neg == 0) {
  cat("✓ Missing value conversion successful: All negative values replaced with NA\n")
} else {
  warning("⚠ Conversion incomplete: ", remaining_neg, " negative values still remain")
}

# REVERSE CODING

# Define items to reverse code
REVERSE_CONFIG <- create_reverse_config(
  scale_7_items = c("v_57", "v_66", "v_68", "v_369", "v_387", "v_70"),
  scale_5_items = c("v_256", "v_257", "v_259")
)
data <- reverse_code_items(data, REVERSE_CONFIG)

# CREATE EXPERIMENTAL VARIABLES

# Define experimental condition mappings
AI_CONDITION_CONFIG <- create_ai_condition_config(
  condition_var = "c_0001",
  level_mapping = list(
    agent_noval = c(3, 6),
    agent_val = c(2, 5),
    assistiv = c(1, 4)
  ),
  reference_level = "assistiv"
)

TASK_CONFIG <- create_task_config(
  condition_var = "c_0001",
  easy_codes = c(1, 2, 3),
  hard_codes = c(4, 5, 6),
  group_labels = c("easy", "hard")
)

data <- create_experimental_variables(data, AI_CONDITION_CONFIG, TASK_CONFIG)

# MANIPULATION CHECKS & FILTERING

# Perceived AI autonomy items (Likert 1-7)
autonomy_items <- c("v_11", "v_12", "v_75")

# DATA PREPARATION
# Store pre-processed data before filtering to allow multiple runs
data_prepared <- data

# CONFIGURATION: ANALYSIS MODES

# Variable needed for filename and reporting
APPLY_MANIPULATION_EXCLUSION <- TRUE

# FILENAME PREPARATION & PERMISSION CHECK
timestamp <- format(Sys.time(), "%Y%m%d")
excel_file <- paste0("results/PLS_SEM_ModeratedMediation_", timestamp, ".xlsx")

# Check if file is writable BEFORE running analysis
if (file.exists(excel_file)) {
  is_writable <- tryCatch(
    {
      suppressWarnings({
        conn <- file(excel_file, open = "r+")
        close(conn)
      })
      TRUE
    },
    error = function(e) FALSE
  )

  if (!is_writable) {
    stop(
      "\n\n!!! CRITICAL ERROR !!!\n",
      "The output file is open or locked: ", excel_file, "\n",
      "PLEASE CLOSE THE EXCEL FILE AND RUN THE SCRIPT AGAIN.\n",
      "Analysis aborted to prevent data loss.\n\n"
    )
  } else {
    cat("✓ Output file is writable:", excel_file, "\n")
  }
}

# MANIPULATION CHECKS & FILTERING

# Filter participants with extreme misperceptions
N_before_filter <- nrow(data)

# Always apply manipulation checks
data <- apply_manipulation_checks_and_filtering(data, autonomy_items)
cat("Manipulation checks applied (Per Protocol subset - Autonomy)\n")

# Task Complexity Exclusion DISABLED (as requested previously)
cat("Task Complexity Exclusion DISABLED\n")

N_after_filter <- nrow(data)
cat("Initial N:", N_initial, "\n")
cat("After manipulation filter:", N_after_filter, "\n")
cat(
  "Excluded:", N_before_filter - N_after_filter,
  "(", round(100 * (N_before_filter - N_after_filter) / N_before_filter, 1), "% )\n"
)

# CREATE COMPOSITE SCORES

# Define mediators (SDT constructs)
MEDIATOR_CONFIG <- create_mediator_config(list(
  m_autonomy = list(
    items = c("v_56", "v_57_reversed", "v_58"),
    label = "Autonomy"
  ),
  m_competence = list(
    items = c("v_61", "v_62"),
    label = "Competence"
  ),
  m_relatedness = list(
    items = c("v_66_reversed", "v_68_reversed", "v_69"),
    label = "Relatedness"
  )
))

# Define dependent variables
DV_CONFIG <- create_dv_config(list(
  dv_intrinsic_motivation = list(
    items = c("v_368", "v_383", "v_369_reversed"),
    label = "Intrinsic Motivation",
    category = "Motivation"
  ),
  dv_commitment = list(
    items = c("v_385", "v_403", "v_386", "v_387_reversed", "v_390"),
    label = "Organizational Commitment",
    category = "Organizational"
  ),
  dv_jobcrafting = list(
    items = c("v_391", "v_392", "v_393", "v_395"),
    label = "Job Crafting",
    category = "Work Design"
  ),
  dv_engagement = list(
    items = c("v_397", "v_398", "v_399"),
    label = "Work Engagement",
    category = "Engagement"
  ),
  dv_hedonic_wellbeing = list(
    items = c("v_252", "v_254", "v_255", "v_256_reversed", "v_257_reversed", "v_259_reversed"),
    label = "Hedonic Well-being",
    category = "Well-being"
  ),
  dv_eudaimonic_wellbeing = list(
    items = c("v_277", "v_282", "v_283"),
    label = "Eudaimonic Well-being",
    category = "Well-being"
  )
))

# Which DVs to analyze (modify for testing)
DV_VARS_TO_RUN <- names(DV_CONFIG) # All DVs
# DV_VARS_TO_RUN <- c("dv_intrinsic_motivation")  # Subset for testing

composite_results <- create_composites(data, MEDIATOR_CONFIG, DV_CONFIG)
data <- composite_results$data
dv_alpha_tbl <- composite_results$dv_alpha_tbl
mediator_names <- names(MEDIATOR_CONFIG)

cat("\nDVs to analyze:", length(DV_VARS_TO_RUN), "of", length(DV_CONFIG), "\n")

# PRECONDITION CHECKS (PRE-ESTIMATION)

# Sample size check removed (power analysis executed in separate script).
cat("Total N:", nrow(data), "\n")

# HARD NA VALIDATION: Stop if any SEM items have NA
validate_sem_items_complete(
  data = data,
  mediator_config = MEDIATOR_CONFIG,
  dv_config = DV_CONFIG,
  dv_vars = DV_VARS_TO_RUN,
  autonomy_items = autonomy_items
)

# DATA QUALITY CHECKS

# Check missing data patterns
vars_to_check <- c("ai_condition", "task_complexity", mediator_names)
check_missing_data(data, vars_to_check)

# Check preliminary multicollinearity (correlations)
check_multicollinearity(data, mediator_names, threshold = 0.90)

# CREATE DUMMY VARIABLES & INTERACTIONS FOR MODERATED MEDIATION

# Create X_ prefix dummies and X×TC interaction terms
data <- create_dummy_variables_moderated(data, AI_CONDITION_CONFIG)

# FIX 4: Use attributes as SINGLE SOURCE OF TRUTH for predictor/interaction names
predictor_names <- attr(data, "predictor_names")
interaction_names <- attr(data, "interaction_names")
moderator_name <- "task_complexity"

# Validate predictor-interaction alignment
validate_predictor_interaction_alignment(predictor_names, interaction_names)

cat("\nModerated Mediation Design:\n")
cat("  Predictors:", paste(predictor_names, collapse = ", "), "\n")
cat("  Moderator:", moderator_name, "(0 = easy, 1 = hard)\n")
cat("  Interactions:", paste(interaction_names, collapse = ", "), "\n")
cat("  Mediators:", paste(mediator_names, collapse = ", "), "\n")

# DEFINE MODERATED MODEL STRUCTURE

# Structural paths with first-stage moderation (X, TC, X×TC → M → Y)
STRUCTURAL_PATHS <- create_structural_paths_moderated(
  predictor_names, moderator_name, interaction_names, mediator_names
)

# Indirect effects configuration with interaction info
INDIRECT_CONFIG <- create_indirect_config_moderated(
  predictor_names, interaction_names, mediator_names
)

# COMPUTE RELIABILITY BY GROUP

reliability_by_group <- compute_reliability_by_group(
  data, MEDIATOR_CONFIG, DV_CONFIG, DV_VARS_TO_RUN
)
mediator_alpha_by_group <- reliability_by_group$mediator
dv_alpha_by_group <- reliability_by_group$dv


# RUN PLS-SEM MODELS

# Bootstrap settings
BOOTSTRAP_N <- 5000

# PLS algorithm settings (no mean_replacement - uses hard validation instead)
PLS_SETTINGS <- create_pls_settings(
  inner_weights = "path_weighting",
  bootstrap_cores = 1,
  bootstrap_seed = 12345,
  sign_change = "no_sign_change"
)

cat("Single-model approach \n")
cat("Bootstrap resamples:", BOOTSTRAP_N, "\n")
cat("Moderator: task_complexity (first-stage)\n")
cat("Missing data handling: Hard validation (no mean replacement)\n")


# Run moderated mediation models for each DV
all_results <- DV_VARS_TO_RUN |>
  purrr::set_names() |>
  purrr::map(function(dv) {
    dv_label <- DV_CONFIG[[dv]]$label
    cat("\n--- DV:", dv, "(", dv_label, ") ---\n")

    model_results <- tryCatch(
      run_pls_sem_moderated(
        Y_var = dv,
        data = data,
        mediator_names = mediator_names,
        predictor_names = predictor_names,
        moderator_name = moderator_name,
        interaction_names = interaction_names,
        path_config = STRUCTURAL_PATHS,
        indirect_config = INDIRECT_CONFIG,
        pls_settings = PLS_SETTINGS,
        bootstrap_n = BOOTSTRAP_N,
        mediator_config = MEDIATOR_CONFIG,
        dv_config = DV_CONFIG,
        use_raw_items = TRUE
      ),
      error = function(e) {
        cat("✗ Failed for DV:", dv, "\n")
        cat("  Error:", conditionMessage(e), "\n")
        NULL
      }
    )


    return(model_results)
  })

# Filter successful results
ok_dvs <- names(all_results)[!vapply(all_results, is.null, logical(1))]
if (length(ok_dvs) == 0) {
  stop("All DV models failed. Analysis aborted.")
}

# Define mode_label for later use (reporting/errors)
mode_label <- "PER PROTOCOL"

cat("\n✓ Successfully completed:", paste(ok_dvs, collapse = ", "), "\n")

# AGGREGATE RESULTS

# Extract mediator quality ONCE from first successful model (avoids duplication)
first_ok_dv <- ok_dvs[1]
mediator_quality_once <- extract_mediator_quality_once(
  all_results[[first_ok_dv]]$pls_model,
  all_results[[first_ok_dv]]$boot_model,
  mediator_names
)

# Aggregate all result tables (moderated mediation version)
result_tables <- c(
  "paths", "conditional_indirects", "fit_measures", "r2",
  "measurement_quality", "loadings", "htmt",
  "structural_quality", "discrim_status"
)

# Extract and bind all tables
list2env(
  result_tables |>
    purrr::set_names() |>
    purrr::map(~ all_results[ok_dvs] |>
      purrr::map_dfr(`[[`, .x)) |>
    purrr::set_names(paste0(result_tables, "_all")),
  envir = environment()
)

# Combine DV-specific quality with mediator quality (calculated once)
if (nrow(mediator_quality_once) > 0) {
  measurement_quality_all <- bind_rows(measurement_quality_all, mediator_quality_once)
  cat("✓ Mediator quality added (calculated once, not per-DV)\n")
}

# FDR CORRECTION (BENJAMINI-HOCHBERG)
# Two correction families per confirmatory testing:
#   Family A: Interaction → Mediator paths (moderation effects)
#   Family B: Conditional indirect effects
# Q-level: 0.05

FDR_MODE <- "two_families" # Options: "two_families" or "single_family"

fdr_results <- apply_fdr_two_families(
  paths_all = paths_all,
  indirects_all = conditional_indirects_all,
  interaction_names = interaction_names,
  mediator_names = mediator_names,
  alpha = 0.05,
  mode = FDR_MODE
)

# Update tables with FDR columns
paths_all <- fdr_results$paths_all
conditional_indirects_all <- fdr_results$indirects_all
fdr_summary <- fdr_results$fdr_summary

# Validate FDR results (sanity checks)
validate_fdr_results(paths_all, conditional_indirects_all)

# Create FDR summary table for export
fdr_summary_table <- create_fdr_summary_table(fdr_summary)

# AUTOMATED QUALITY ASSESSMENT (OPTIONAL)
# Use modular utilities to test all quality criteria
# Set RUN_QUALITY_TESTS = TRUE to enable automated testing
RUN_QUALITY_TESTS <- TRUE # Set to FALSE to skip automated tests

# PREDICTIVE PERFORMANCE (PLSpredict)
# Run out-of-sample prediction (Q2_predict, RMSE, MAE)
# Time consuming! Default to TRUE for final analysis, FALSE for quick checks.
RUN_PREDICTION_ANALYSIS <- TRUE

prediction_results_all <- data.frame()

if (RUN_PREDICTION_ANALYSIS) {
  print_section_header("PREDICTIVE PERFORMANCE (PLSpredict)")
  cat("Note: Running 10-fold cross-validation with 10 repetitions.\n")
  cat("This may take significant time...\n\n")

  prediction_results_all <- ok_dvs |>
    purrr::map_dfr(function(dv) {
      cat("Evaluating prediction for:", dv, "...\n")
      # Reuse successful model object
      model_obj <- all_results[[dv]]$pls_model

      res <- assess_predictive_performance(model = model_obj, folds = 10, reps = 10)
      if (nrow(res) > 0) {
        res$dv <- dv
        res$group_label <- "Combined" # Moderated model uses one group
      }
      res
    })

  if (nrow(prediction_results_all) > 0) {
    cat("\n✓ Prediction analysis complete. Results added to export.\n")
  } else {
    cat("\n⚠ Prediction analysis produced no results (check errors above).\n")
  }
}


if (RUN_QUALITY_TESTS) {
  print_section_header("AUTOMATED QUALITY ASSESSMENT")

  # Run all tests using tidyverse patterns
  quality_tests <- list(
    measurement = test_measurement_quality(measurement_quality_all),
    discriminant = test_discriminant_validity(htmt_all, threshold = 0.85),
    collinearity = test_structural_collinearity(
      structural_quality_all,
      vif_threshold = 5 # Standard threshold (two-stage approach already orthogonalizes interactions)
    ),
    effect = test_effect_sizes(structural_quality_all)
  )

  quality_summary <- summarize_quality_tests(
    quality_tests$measurement,
    quality_tests$discriminant,
    quality_tests$collinearity,
    quality_tests$effect
  )

  cat("\n", quality_summary$overall_message, "\n\n", sep = "")
  print(quality_summary$summary_table)

  # Print actionable recommendations using modular approach
  if (!quality_summary$overall_passed) {
    print_section_header("RECOMMENDED ACTIONS")

    quality_summary$details |>
      purrr::iwalk(function(test, name) {
        if (name == "effect_sizes") {
          return()
        } # Skip effect sizes

        if (!isTRUE(test$passed) && !is.null(test$issues) &&
          nrow(test$issues) > 0) {
          cat("\n⚠ ",
            tools::toTitleCase(gsub("_", " ", name)),
            " Issues:\n",
            sep = ""
          )

          test$issues |>
            dplyr::select(dplyr::any_of(c(
              "dv", "group_label", "construct", "composite_reliability",
              "ave", "issue_type", "construct_1", "construct_2", "htmt",
              "endogenous", "predictor", "vif", "severity"
            ))) |>
            print()
        }
      })
  }

  cat("\n")
}

# RESULTS SUMMARY (minimal - full results in Excel)

cat("PLS-SEM ANALYSIS COMPLETE\n")
cat("Date:", format(Sys.Date(), "%Y-%m-%d"), "\n")
cat("Bootstrapping:", BOOTSTRAP_N, "resamples\n")
cat("DVs analyzed:", paste(ok_dvs, collapse = ", "), "\n")
cat("Mediators:", paste(mediator_names, collapse = ", "), "\n\n")


# Brief quality summary only
cat("\nQUALITY OVERVIEW\n")
cat("Total paths:", nrow(paths_all), "\n")
cat("Total indirect effects:", nrow(conditional_indirects_all), "\n")
cat("See Excel file for full results.\n")

# EXPORT RESULTS
# Create results directory if it doesn't exist
if (!dir.exists("results")) {
  dir.create("results")
}

# Create publication-ready summary (using conditional indirects)
summary_list <- create_publication_summary(
  paths_all, conditional_indirects_all, r2_all,
  measurement_quality_all, structural_quality_all,
  htmt_all
)

# Create date-stamped filename (overwrites same day)
# Create date-stamped filename (overwrites same day)
timestamp <- format(Sys.time(), "%Y%m%d")
file_suffix <- if (APPLY_MANIPULATION_EXCLUSION) "" else "_ITT"
excel_file <- paste0("results/PLS_SEM_ModeratedMediation_", timestamp, file_suffix, ".xlsx")

# Export to Excel (all results in one workbook)
tryCatch(
  {
    library(openxlsx)
    wb <- createWorkbook()

    # 1. Summary Sheet (First)
    addWorksheet(wb, "Summary_Reporting")
    writeData(wb, "Summary_Reporting", "--- MEASUREMENT QUALITY ---", startRow = 1)
    writeData(wb, "Summary_Reporting", summary_list$measurement, startRow = 2)

    start_row <- nrow(summary_list$measurement) + 5
    writeData(wb, "Summary_Reporting", "--- PATH COEFFICIENTS & SIGNIFIKANZ ---", startRow = start_row)
    writeData(wb, "Summary_Reporting", summary_list$paths, startRow = start_row + 1)

    start_row <- start_row + nrow(summary_list$paths) + 5
    writeData(wb, "Summary_Reporting", "--- CONDITIONAL INDIRECT EFFECTS ---", startRow = start_row)
    writeData(wb, "Summary_Reporting", summary_list$indirects, startRow = start_row + 1)

    start_row <- start_row + nrow(summary_list$indirects) + 5
    writeData(wb, "Summary_Reporting", "--- R-SQUARED ---", startRow = start_row)
    writeData(wb, "Summary_Reporting", summary_list$r2, startRow = start_row + 1)

    # 2. Detailed Sheets (moderated mediation version)

    # Create sample info summary for transparency
    sample_info <- data.frame(
      metric = c(
        "Analysis Mode",
        "Initial N (raw data)", "After manipulation filter",
        "Excluded (manipulation)", "Exclusion %", "Bootstrap resamples",
        "VIF threshold", "FDR correction method"
      ),
      value = c(
        if (APPLY_MANIPULATION_EXCLUSION) "Per Protocol" else "Intention-to-Treat (ITT)",
        N_initial, N_after_filter, N_before_filter - N_after_filter,
        round(100 * (N_before_filter - N_after_filter) / N_before_filter, 1),
        BOOTSTRAP_N, "3.3", "Benjamini-Hochberg"
      ),
      stringsAsFactors = FALSE
    )

    # Prepare export list
    export_list <- list(
      "Summary_Reporting" = summary_list$measurement,
      "Path_Coefficients" = paths_all,
      "Conditional_Indirects" = conditional_indirects_all,
      "R2_Values" = r2_all,
      "Measurement_Quality" = measurement_quality_all,
      "HTMT_Matrix" = htmt_all,
      "Loadings" = loadings_all,
      "Structural_Collinearity" = structural_quality_all,
      "Multiplicity_FDR" = fdr_summary_table,
      "Predictive_Power" = prediction_results_all, # Always include if defined
      "Sensitivity_Analysis" = if (exists("sensitivity_results")) sensitivity_results else NULL,
      "Sample_Info" = sample_info
    )

    cat("\nExporting", length(export_list), "worksheets to Excel...\n")

    for (sheet_name in names(export_list)) {
      if (sheet_name == "Summary_Reporting") next

      df_to_write <- export_list[[sheet_name]]
      # Add worksheet if it's a dataframe (even if 0 rows, so user sees headers)
      if (!is.null(df_to_write) && is.data.frame(df_to_write)) {
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet_name, df_to_write)
        if (nrow(df_to_write) == 0) cat("  ! Sheet '", sheet_name, "' is empty (0 rows).\n")
      }
    }

    saveWorkbook(wb, excel_file, overwrite = TRUE)
    cat(" Excel file saved:", excel_file, "\n")
    cat("Empirical bootstrap p-values for indirect effects (pvalue column)\n")
    cat("FDR correction: Benjamini-Hochberg (p_value_fdr, sig_fdr_05)\n")
    cat("Sample tracking sheet: Sample_Info\n")
    cat("Loadings with bootstrap CIs (ci_lower, ci_upper, t_value)\n")
    if (exists("prediction_results_all") && nrow(prediction_results_all) > 0) {
      cat("Predictive Power (Q2_predict, RMSE, MAE) in 'Predictive_Power' sheet\n")
    }
    cat("Sensitivity analysis for manipulation thresholds (Sensitivity_Analysis sheet)\n")
  },
  error = function(e) {
    cat("Excel export failed for ", mode_label, ": ", conditionMessage(e), "\n")
  }
)

cat("\n ANALYSIS COMPLETE\n")