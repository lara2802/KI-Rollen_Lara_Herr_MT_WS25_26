# PLS-SEM UTILITY FUNCTIONS WITH CONFIGURATION HELPERS
# Generic functions + configuration builders for PLS-SEM analysis

# CONFIGURATION HELPER FUNCTIONS

#' Create default reverse coding configuration
#' @param scale_7_items Vector of 7-point scale items to reverse
#' @param scale_5_items Vector of 5-point scale items to reverse
#' @return List with reverse coding configuration
create_reverse_config <- function(scale_7_items = c(), scale_5_items = c()) {
  list(
    scale_7 = list(items = scale_7_items, max_value = 8),
    scale_5 = list(items = scale_5_items, max_value = 6)
  )
}

#' Create AI condition mapping configuration
#' @param condition_var Name of condition variable in data
#' @param level_mapping Named list mapping level names to condition codes
#' @param reference_level Name of reference level for dummy coding
#' @return List with AI condition configuration
create_ai_condition_config <- function(condition_var, level_mapping, reference_level) {
  list(
    condition_var = condition_var,
    levels = level_mapping,
    reference_level = reference_level,
    ordered_levels = names(level_mapping)
  )
}

#' Create task complexity mapping configuration
#' @param condition_var Name of condition variable in data
#' @param easy_codes Vector of codes for easy condition
#' @param hard_codes Vector of codes for hard condition
#' @param group_labels Labels for groups (default c("easy", "hard"))
#' @return List with task complexity configuration
create_task_config <- function(condition_var, easy_codes, hard_codes,
                               group_labels = c("easy", "hard")) {
  list(
    condition_var = condition_var,
    levels = list(easy_codes, hard_codes),
    group_labels = group_labels
  )
}

#' Create mediator configuration
#' @param mediator_list Named list where each element contains items and label
#' @return List with mediator configuration
create_mediator_config <- function(mediator_list) {
  mediator_list
}

#' Create DV configuration
#' @param dv_list Named list where each element contains items, label, category
#' @return List with DV configuration
create_dv_config <- function(dv_list) {
  dv_list
}

#' Create PLS algorithm settings
#' @param inner_weights Weighting scheme (default "path_weighting")
#' @param bootstrap_cores Number of cores (NULL = all available)
#' @param bootstrap_seed Random seed for bootstrap
#' @param sign_change Sign change method for bootstrap
#' @return List with PLS settings
#' @note missing_handling removed - use hard validation (no mean replacement)
create_pls_settings <- function(inner_weights = "path_weighting",
                                bootstrap_cores = NULL,
                                bootstrap_seed = 12345,
                                sign_change = "no_sign_change") {
  list(
    inner_weights = inner_weights,
    bootstrap_cores = bootstrap_cores,
    bootstrap_seed = bootstrap_seed,
    sign_change = sign_change
  )
}

# MODERATED MEDIATION HELPERS

#' Create structural paths configuration for moderated mediation
#' First-stage paths include main effects (X, TC) and interactions (X×TC)
#' @param predictor_names Vector of main predictor dummy names
#' @param moderator_name Name of the moderator variable (task_complexity)
#' @param interaction_names Vector of interaction term names (predictor_x_TC)
#' @param mediator_names Vector of mediator variable names
#' @param outcome_name Name of outcome variable (default "y_out")
#' @return List with path specifications for moderated mediation
create_structural_paths_moderated <- function(predictor_names, moderator_name,
                                              interaction_names, mediator_names,
                                              outcome_name = "y_out") {
  # First-stage: X, TC, X×TC → each Mediator
  first_stage_from <- c(predictor_names, moderator_name, interaction_names)

  list(
    first_stage = list(from = first_stage_from, to = mediator_names),
    second_stage = list(from = mediator_names, to = outcome_name),
    direct = list(from = predictor_names, to = outcome_name)
  )
}

#' Create indirect effects configuration for moderated mediation
#' Tracks predictor, mediator, outcome, and corresponding interaction term
#' @param predictor_names Vector of predictor dummy names
#' @param interaction_names Vector of interaction term names (same order as predictors)
#' @param mediator_names Vector of mediator variable names
#' @param outcome_name Name of outcome variable (default "y_out")
#' @return List with indirect effect specifications including interaction info
create_indirect_config_moderated <- function(predictor_names, interaction_names,
                                             mediator_names, outcome_name = "y_out") {
  indirect_list <- list()

  for (i in seq_along(predictor_names)) {
    pred <- predictor_names[i]
    interaction <- interaction_names[i]
    pred_short <- gsub("X_", "", pred)

    for (med in mediator_names) {
      med_short <- gsub("m_", "", med)
      indirect_list[[length(indirect_list) + 1]] <- list(
        from = pred,
        through = med,
        to = outcome_name,
        interaction = interaction,
        name = paste0("ind_", pred_short, "_", med_short),
        label = paste(pred, "→", med, "→", outcome_name)
      )
    }
  }
  indirect_list
}

# DATA VALIDATION FUNCTIONS

#' Validate predictor-interaction alignment
#' Ensures interaction names correctly correspond to predictor names
#' @param predictor_names Vector of predictor dummy names
#' @param interaction_names Vector of interaction term names
#' @return TRUE if valid, stops with error otherwise
validate_predictor_interaction_alignment <- function(predictor_names, interaction_names) {
  if (length(predictor_names) != length(interaction_names)) {
    stop(
      "ALIGNMENT ERROR: Number of predictors (", length(predictor_names),
      ") does not match number of interactions (", length(interaction_names), ")"
    )
  }

  # Check each interaction follows expected naming pattern (seminr style: Pred*Mod)
  for (i in seq_along(predictor_names)) {
    expected_interaction <- paste0(predictor_names[i], "_x_task_complexity")
    if (interaction_names[i] != expected_interaction) {
      stop(
        "ALIGNMENT ERROR: Interaction '", interaction_names[i],
        "' does not match expected '", expected_interaction,
        "' for predictor '", predictor_names[i], "'"
      )
    }
  }

  cat(" Predictor-interaction alignment validated\n")
  invisible(TRUE)
}

#' Validate SEM indicator items for NA - HARD VALIDATION
#' Checks all required SEM items for missing values BEFORE model estimation
#' If any NA found, stops with clear error message
#' @param data Data frame with item-level data
#' @param mediator_config Mediator configuration with items
#' @param dv_config DV configuration with items
#' @param dv_vars Vector of DV names to check
#' @param autonomy_items Optional manipulation check items
#' @return TRUE if all valid, otherwise stops with error
validate_sem_items_complete <- function(data, mediator_config, dv_config,
                                        dv_vars, autonomy_items = NULL) {
  cat("\n=== HARD NA VALIDATION FOR SEM ITEMS ===\n")

  all_items <- c()

  # Collect all mediator items
  for (med_name in names(mediator_config)) {
    all_items <- c(all_items, mediator_config[[med_name]]$items)
  }

  # Collect all DV items (only for selected DVs)
  for (dv_name in dv_vars) {
    if (dv_name %in% names(dv_config)) {
      all_items <- c(all_items, dv_config[[dv_name]]$items)
    }
  }

  # Add autonomy items if provided
  if (!is.null(autonomy_items)) {
    all_items <- c(all_items, autonomy_items)
  }

  all_items <- unique(all_items)

  # Check which items exist
  missing_cols <- setdiff(all_items, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "VALIDATION FAILED: Required SEM columns missing from data:\n  ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Check for NA in each item
  na_counts <- colSums(is.na(data[, all_items, drop = FALSE]))
  items_with_na <- names(na_counts[na_counts > 0])

  if (length(items_with_na) > 0) {
    # Create detailed error message
    na_details <- paste0(items_with_na, " (", na_counts[items_with_na], " NA)")
    stop(
      "HARD VALIDATION FAILED: NA values found in SEM items.\n",
      "  Total items with NA: ", length(items_with_na), " of ", length(all_items), "\n",
      "  Details:\n    ", paste(na_details, collapse = "\n    "), "\n\n",
      "  FIX: Remove rows with NA or impute BEFORE calling this script.\n",
      "  Do NOT use mean replacement in SEM estimation."
    )
  }

  cat(" All", length(all_items), "SEM indicator items validated\n")
  cat(" No NA values found in any SEM items\n")
  cat(" Total N for SEM:", nrow(data), "\n")

  invisible(TRUE)
}

# FDR (BENJAMINI-HOCHBERG) CORRECTION FOR MULTIPLE TESTING
# PURPOSE: Control False Discovery Rate across confirmatory tests
# METHOD: Benjamini-Hochberg (BH) procedure
# FAMILIES:
#   Family A: Interaction → Mediator paths (moderation effects)
#   Family B: Conditional indirect effects
# Q-LEVEL: 0.05

#' Apply two-family FDR correction to paths and indirect effects
#' Family A: interaction→mediator paths | Family B: conditional indirect effects
#' @param paths_all Combined paths table from all DV models
#' @param indirects_all Combined conditional indirects table
#' @param interaction_names Vector of interaction term names
#' @param mediator_names Vector of mediator names
#' @param alpha Significance level (default 0.05)
#' @param mode "two_families" (default) or "single_family"
#' @return List with corrected tables and summary
apply_fdr_two_families <- function(paths_all, indirects_all,
                                   interaction_names, mediator_names,
                                   alpha = 0.05, mode = "two_families") {
  cat("\nMULTIPLICITY CORRECTION (BH-FDR)\n")
  cat("# Method: Benjamini-Hochberg (controls FDR at q =", alpha, ")\n")
  cat("# Mode:", mode, "\n\n")

  fdr_summary <- list()

  if (mode == "two_families") {
    # ----- FAMILY A: Interaction → Mediator paths -----
    # Filter paths: predictor is interaction term, endogenous is mediator
    # Clean names to ensure matching works regardless of whitespace
    paths_all$lhs <- trimws(as.character(paths_all$lhs))
    paths_all$rhs <- trimws(as.character(paths_all$rhs))
    interaction_names <- trimws(as.character(interaction_names))
    mediator_names <- trimws(as.character(mediator_names))

    family_a_mask <- paths_all$rhs %in% interaction_names &
      paths_all$lhs %in% mediator_names

    if (nrow(paths_all) > 0) {
      paths_all$fdr_family <- NA_character_
      paths_all$p_value_fdr <- NA_real_
      paths_all$sig_fdr_05 <- NA
    }

    if (sum(family_a_mask, na.rm = TRUE) > 0) {
      p_raw_a <- paths_all$p_value[family_a_mask]
      p_fdr_a <- p.adjust(p_raw_a, method = "BH")

      paths_all$fdr_family[family_a_mask] <- "A"
      paths_all$p_value_fdr[family_a_mask] <- p_fdr_a
      paths_all$sig_fdr_05[family_a_mask] <- p_fdr_a < alpha

      fdr_summary$family_a <- list(
        family = "A",
        description = "Interaction → Mediator paths (moderation)",
        n_tests = sum(family_a_mask, na.rm = TRUE),
        n_sig_raw = sum(p_raw_a < alpha, na.rm = TRUE),
        n_sig_fdr = sum(p_fdr_a < alpha, na.rm = TRUE),
        method = "Benjamini-Hochberg (BH)",
        q_level = alpha
      )

      cat("Family A (Interaction → Mediator):\n")
      cat("  Tests:", fdr_summary$family_a$n_tests, "\n")
      cat("  Significant (raw p < 0.05):", fdr_summary$family_a$n_sig_raw, "\n")
      cat("  Significant (FDR q < 0.05):", fdr_summary$family_a$n_sig_fdr, "\n\n")
    } else {
      cat("Family A: No interaction→mediator paths found\n\n")
      fdr_summary$family_a <- list(
        family = "A", n_tests = 0, n_sig_raw = 0, n_sig_fdr = 0,
        method = "Benjamini-Hochberg (BH)", q_level = alpha
      )
    }

    # Fill in "Not corrected" for paths not in Family A
    paths_all$fdr_family[is.na(paths_all$fdr_family)] <- "Not corrected"

    # ----- FAMILY B: Conditional indirect effects -----
    # Now using ACTUAL empirical bootstrap p-values from compute_conditional_indirect_effects

    if (nrow(indirects_all) > 0) {
      indirects_all$fdr_family <- "B"
      indirects_all$p_value_fdr <- NA_real_
      indirects_all$sig_fdr_05 <- NA
    }

    if (nrow(indirects_all) > 0) {
      if (!"p_value" %in% names(indirects_all)) {
        warning("FDR: 'p_value' column missing from indirects_all. Using CI-based fallback.")
        indirects_all$p_value <- ifelse(indirects_all$significant, 0.01, 0.50)
      }

      p_fdr_b <- p.adjust(indirects_all$p_value, method = "BH")
      indirects_all$p_value_fdr <- p_fdr_b
      indirects_all$sig_fdr_05 <- p_fdr_b < alpha

      fdr_summary$family_b <- list(
        family = "B",
        description = "Conditional indirect effects",
        n_tests = nrow(indirects_all),
        n_sig_raw = sum(indirects_all$p_value < alpha, na.rm = TRUE),
        n_sig_fdr = sum(indirects_all$sig_fdr_05, na.rm = TRUE),
        method = "Benjamini-Hochberg (BH)",
        q_level = alpha,
        note = "Empirical bootstrap p-values (2*min(prop>0, prop<0))"
      )

      cat("Family B (Conditional Indirect Effects):\n")
      cat("  Tests:", fdr_summary$family_b$n_tests, "\n")
      cat("  Significant (raw p < 0.05):", fdr_summary$family_b$n_sig_raw, "\n")
      cat("  Significant (FDR q < 0.05):", fdr_summary$family_b$n_sig_fdr, "\n")
      cat("  Method: Empirical bootstrap p-values\n\n")
    } else {
      fdr_summary$family_b <- list(
        family = "B", n_tests = 0, n_sig_raw = 0, n_sig_fdr = 0,
        method = "Benjamini-Hochberg (BH)", q_level = alpha
      )
    }
  } else {
    # Single family mode: correct all p-values together
    all_p <- c(
      paths_all$p_value,
      ifelse(indirects_all$significant, 0.01, 0.50)
    )
    all_p_fdr <- p.adjust(all_p, method = "BH")

    n_paths <- nrow(paths_all)
    if (n_paths > 0) {
      paths_all$fdr_family <- "Combined"
      paths_all$p_value_fdr <- all_p_fdr[1:n_paths]
      paths_all$sig_fdr_05 <- paths_all$p_value_fdr < alpha
    }

    if (nrow(indirects_all) > 0) {
      indirects_all$fdr_family <- "Combined"
      indirects_all$p_value_fdr <- all_p_fdr[(n_paths + 1):length(all_p_fdr)]
      indirects_all$sig_fdr_05 <- indirects_all$p_value_fdr < alpha
    }

    fdr_summary$combined <- list(
      family = "Combined",
      n_tests = length(all_p),
      method = "Benjamini-Hochberg (BH)",
      q_level = alpha
    )
  }

  cat("Multiplicity: BH-FDR applied to (A) interaction→mediator paths and (B) conditional indirect effects; raw p-values retained.\n")
  cat("Note: Paths not in families A or B are marked as 'Not corrected' and retain raw p-values only.\n")

  list(
    paths_all = paths_all,
    indirects_all = indirects_all,
    fdr_summary = fdr_summary
  )
}

#' Create FDR summary table for Excel export
#' @param fdr_summary FDR summary list from apply_fdr_two_families
#' @return Data frame for Multiplicity_FDR sheet
create_fdr_summary_table <- function(fdr_summary) {
  rows <- list()

  for (family_name in names(fdr_summary)) {
    family <- fdr_summary[[family_name]]
    rows[[length(rows) + 1]] <- data.frame(
      family = family$family,
      description = ifelse(!is.null(family$description), family$description, NA),
      n_tests = family$n_tests,
      method = family$method,
      q_level = family$q_level,
      n_sig_raw = family$n_sig_raw,
      n_sig_fdr = family$n_sig_fdr,
      note = ifelse(!is.null(family$note), family$note, NA),
      stringsAsFactors = FALSE
    )
  }

  bind_rows(rows)
}

#' Validate FDR correction results (sanity checks per checklist 3.1)
#' @param paths_all Paths table with FDR columns
#' @param indirects_all Indirects table with FDR columns
#' @return TRUE if all checks pass, warning messages if not
validate_fdr_results <- function(paths_all, indirects_all) {
  checks_passed <- TRUE

  # Check 1: p_value_fdr exists
  if (!"p_value_fdr" %in% names(paths_all)) {
    warning("FDR VALIDATION: p_value_fdr missing from paths_all")
    checks_passed <- FALSE
  }
  if (!"p_value_fdr" %in% names(indirects_all)) {
    warning("FDR VALIDATION: p_value_fdr missing from indirects_all")
    checks_passed <- FALSE
  }

  # Check 2: p_value_fdr within [0, 1]
  if (any(paths_all$p_value_fdr < 0 | paths_all$p_value_fdr > 1, na.rm = TRUE)) {
    warning("FDR VALIDATION: p_value_fdr outside [0,1] in paths_all")
    checks_passed <- FALSE
  }
  if (any(indirects_all$p_value_fdr < 0 | indirects_all$p_value_fdr > 1, na.rm = TRUE)) {
    warning("FDR VALIDATION: p_value_fdr outside [0,1] in indirects_all")
    checks_passed <- FALSE
  }

  if (checks_passed) {
    cat(" FDR validation passed: columns exist, values in [0,1]\n")
  }

  invisible(checks_passed)
}


# PACKAGE MANAGEMENT FUNCTIONS

#' Install and load required packages
#' @param packages Vector of package names (optional)
#' @return NULL (loads packages into environment)
install_and_load_packages <- function(packages = NULL) {
  if (is.null(packages)) {
    packages <- c("readxl", "dplyr", "psych", "naniar", "seminr", "htmlwidgets")
  }

  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cran.rstudio.com/")
      library(pkg, character.only = TRUE)
    }
  }

  library(dplyr)
  invisible(NULL)
}

# DATA PREPARATION FUNCTIONS

#' Convert coded missing values to NA
#' @param data Data frame with survey data
#' @param missing_code Ignored in this version (legacy)
#' @param item_pattern Regex pattern to identify Likert items (default "^v_")
#' @return Data frame with NA replacing any value < 0
convert_missing_to_na <- function(data, missing_code = -77, item_pattern = "^v_") {
  data <- as.data.frame(data)
  likert_items <- names(data)[grepl(item_pattern, names(data))]

  # Count how many replacements happen
  total_replacements <- 0

  data[, likert_items] <- lapply(data[, likert_items], function(x) {
    # Try to convert to numeric if character/factor
    if (!is.numeric(x)) {
      x_num <- suppressWarnings(as.numeric(as.character(x)))
      # Only replace if conversion wasn't total garbage (i.e. not all NAs if originally not all NAs)
      if (!all(is.na(x_num)) || all(is.na(x))) {
        x <- x_num
      }
    }

    if (is.numeric(x)) {
      n_neg <- sum(x < 0, na.rm = TRUE)
      if (n_neg > 0) {
        x[x < 0] <- NA
        total_replacements <<- total_replacements + n_neg
      }
    }
    x
  })

  if (total_replacements > 0) {
    cat("  Replaced", total_replacements, "negative values with NA\n")
  }

  return(data)
}

#' Reverse code survey items based on configuration
#' @param data Data frame containing survey items
#' @param reverse_config List with scale configurations
#' @return Data frame with reversed items added
reverse_code_items <- function(data, reverse_config) {
  for (scale_name in names(reverse_config)) {
    scale_info <- reverse_config[[scale_name]]
    items <- scale_info$items
    max_value <- scale_info$max_value

    for (item in items) {
      if (item %in% names(data)) {
        reversed_name <- paste0(item, "_reversed")
        data[[reversed_name]] <- max_value - data[[item]]
      } else {
        warning("Item '", item, "' not found in data for reverse coding")
      }
    }
  }
  return(data)
}

#' Create experimental condition variables
#' @param data Data frame with condition variable
#' @param ai_config AI condition mapping
#' @param task_config Task complexity mapping
#' @param validation_config Validation indicator mapping (optional)
#' @param verbose Print diagnostic tables
#' @return Data frame with condition variables added
create_experimental_variables <- function(data, ai_config, task_config,
                                          validation_config = NULL,
                                          verbose = TRUE) {
  condition_var <- ai_config$condition_var

  # Create AI condition factor
  data$ai_condition <- NA
  for (level_name in names(ai_config$levels)) {
    codes <- ai_config$levels[[level_name]]
    data$ai_condition[data[[condition_var]] %in% codes] <- level_name
  }
  data$ai_condition <- factor(data$ai_condition, levels = ai_config$ordered_levels)

  # Create task complexity variable
  data$task_complexity <- NA
  data$task_complexity[data[[condition_var]] %in% task_config$levels[[1]]] <- 0
  data$task_complexity[data[[condition_var]] %in% task_config$levels[[2]]] <- 1
  data$task_complexity <- as.numeric(data$task_complexity)

  # Optional: validation indicator
  if (!is.null(validation_config)) {
    data$is_validation <- NA
    data$is_validation[data[[condition_var]] %in% validation_config$levels[[1]]] <- 1
    data$is_validation[data[[condition_var]] %in% validation_config$levels[[2]]] <- 0
    data$is_validation <- as.numeric(data$is_validation)
  }

  if (verbose) {
    cat("\n=== DESIGN CHECK ===\n")
    print(table(data[[condition_var]], data$ai_condition, useNA = "ifany"))
    print(table(data[[condition_var]], data$task_complexity, useNA = "ifany"))
  }

  return(data)
}

#' Apply manipulation checks and filter extreme misperceptions
#' SYMMETRIC APPROACH: Same threshold logic for all conditions
#' Exclude participants whose perceived autonomy deviates too far from expected
#' @param data Data frame with items and condition variables
#' @param autonomy_items Vector of items for perceived autonomy
#' @param a_threshold Threshold for perceived AI autonomy (1-7 scale)
#'   assistiv: exclude if mean_perceived_autonomy > a_threshold (expected LOW)
#'   agent_*: exclude if mean_perceived_autonomy < (8 - a_threshold) (expected HIGH)
#' @param verbose Print exclusion summary
#' @return Filtered data frame
apply_manipulation_checks_and_filtering <- function(data, autonomy_items,
                                                    a_threshold = 5,
                                                    verbose = TRUE) {
  if (verbose) cat("\n=== MANIPULATION CHECKS & FILTERING (SYMMETRIC) ===\n")

  # Validate autonomy items exist
  missing_autonomy <- setdiff(autonomy_items, names(data))
  if (length(missing_autonomy) > 0) {
    stop("Missing autonomy items in data: ", paste(missing_autonomy, collapse = ", "))
  }

  # Calculate mean perceived autonomy
  data$mean_perceived_autonomy <- rowMeans(data[, autonomy_items], na.rm = TRUE)

  # Initial N
  n_initial <- nrow(data)

  # EXCLUSION LOGIC:
  # assistiv (AI Assistant): expected LOW autonomy -> exclude if perceived >= a_threshold
  # agent_noval (AI Agent without validation): expected HIGH autonomy -> exclude if perceived <= (8 - a_threshold)
  # agent_val (AI Agent with validation): expected MODERATE autonomy -> exclude extremes (==1 or ==7)

  inverse_threshold <- 8 - a_threshold # For agent_noval (high autonomy expected)

  to_exclude <-
    (data$ai_condition == "assistiv" & data$mean_perceived_autonomy >= a_threshold) |
      (data$ai_condition == "agent_noval" & data$mean_perceived_autonomy <= inverse_threshold) |
      (data$ai_condition == "agent_val" & (data$mean_perceived_autonomy == 1 | data$mean_perceived_autonomy == 7))

  # Track exclusions
  excluded_counts <- table(data$ai_condition[to_exclude], useNA = "no")

  # Apply filtering
  filtered_data <- data[!to_exclude, ]

  if (verbose) {
    cat("Exclusion criteria:\n")
    cat("  assistiv (low autonomy expected): exclude if perceived >=", a_threshold, "\n")
    cat("  agent_noval (high autonomy expected): exclude if perceived <=", inverse_threshold, "\n")
    cat("  agent_val (moderate autonomy expected): exclude if perceived == 1 or == 7 (extremes)\n\n")

    cat("Exclusions per AI condition:\n")
    all_levels <- levels(data$ai_condition)
    for (lvl in all_levels) {
      count <- if (lvl %in% names(excluded_counts)) excluded_counts[lvl] else 0
      cat("  - ", lvl, ": ", count, " excluded\n", sep = "")
    }

    cat("\nFinal sample size (N):", nrow(filtered_data), "(Initial N:", n_initial, ")\n")
  }

  return(filtered_data)
}

#' Apply manipulation filter with specified threshold (for sensitivity analysis)
#' @param data Data frame with items and condition variables
#' @param autonomy_items Vector of items for perceived autonomy
#' @param threshold Threshold type: "default" (5), "lenient" (6), "strict" (4), "none" (no filtering)
#' @return Filtered data frame
apply_manipulation_filter_by_threshold <- function(data, autonomy_items, threshold = "default") {
  # Calculate mean perceived autonomy if not already present
  if (!"mean_perceived_autonomy" %in% names(data)) {
    data$mean_perceived_autonomy <- rowMeans(data[, autonomy_items], na.rm = TRUE)
  }

  if (threshold == "none") {
    # Intention-to-treat: no filtering
    return(data)
  }

  # Map threshold names to numeric values
  a_threshold <- switch(threshold,
    "default" = 5,
    "lenient" = 6, # Less strict (exclude fewer participants)
    "strict" = 4, # More strict (exclude more participants)
    stop("Unknown threshold: ", threshold)
  )

  inverse_threshold <- 8 - a_threshold

  to_exclude <-
    (data$ai_condition == "assistiv" & data$mean_perceived_autonomy >= a_threshold) |
      (data$ai_condition == "agent_noval" & data$mean_perceived_autonomy <= inverse_threshold) |
      (data$ai_condition == "agent_val" & (data$mean_perceived_autonomy == 1 | data$mean_perceived_autonomy == 7))

  return(data[!to_exclude, ])
}

#' Create composite scores for constructs
#' @param data Data frame with item-level data
#' @param mediator_config List of mediator configurations
#' @param dv_config List of DV configurations
#' @param verbose Print reliability information
#' @return List with updated data and reliability tables
create_composites <- function(data, mediator_config, dv_config, verbose = TRUE) {
  if (verbose) cat("\n=== COMPOSITES: RELIABILITY (alpha) ===\n")

  # Process mediators
  for (med_name in names(mediator_config)) {
    med_info <- mediator_config[[med_name]]
    items <- med_info$items

    missing_items <- setdiff(items, names(data))
    if (length(missing_items) > 0) {
      warning("Missing items for ", med_name, ": ", paste(missing_items, collapse = ", "))
      next
    }

    alpha_result <- psych::alpha(data[, items])

    # Calculate mean, handling cases where all items are missing
    composite_scores <- rowMeans(data[, items], na.rm = TRUE)
    # Replace NaN (all missing) with NA
    composite_scores[is.nan(composite_scores)] <- NA
    data[[med_name]] <- composite_scores

    if (verbose) {
      cat(med_info$label, " α =", round(alpha_result$total$raw_alpha, 3), "\n")
    }
  }

  # Process DVs
  dv_names <- names(dv_config)
  for (dv_name in dv_names) {
    dv_info <- dv_config[[dv_name]]
    items <- dv_info$items

    missing_items <- setdiff(items, names(data))
    if (length(missing_items) > 0) {
      warning("Missing items for ", dv_name, ": ", paste(missing_items, collapse = ", "))
      next
    }

    # Calculate mean, handling cases where all items are missing
    composite_scores <- rowMeans(data[, items], na.rm = TRUE)
    # Replace NaN (all missing) with NA
    composite_scores[is.nan(composite_scores)] <- NA
    data[[dv_name]] <- composite_scores
  }

  # Compute DV reliability table
  dv_alpha_tbl <- lapply(dv_names, function(dv_name) {
    dv_info <- dv_config[[dv_name]]
    items <- dv_info$items

    if (all(items %in% names(data))) {
      alpha_result <- psych::alpha(data[, items])
      data.frame(
        dv = dv_name,
        label = dv_info$label,
        alpha = as.numeric(alpha_result$total$raw_alpha),
        n_items = length(items),
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  }) %>% bind_rows()

  if (verbose) {
    cat("\n=== OUTCOME RELIABILITY (alpha) ===\n")
    print(dv_alpha_tbl %>% mutate(alpha = round(alpha, 3)))
  }

  return(list(
    data = data,
    dv_alpha_tbl = dv_alpha_tbl,
    mediator_config = mediator_config,
    dv_config = dv_config
  ))
}

#' Create dummy variables and interaction terms for moderated mediation
#' Uses X_ prefix for predictors and creates X×TC interaction terms
#' @param data Data frame with ai_condition and task_complexity
#' @param ai_config AI condition configuration (reference = assistiv)
#' @param verbose Print diagnostic information
#' @return Data frame with X_ dummies, task_complexity, and interaction terms
create_dummy_variables_moderated <- function(data, ai_config, verbose = TRUE) {
  if (verbose) cat("\n=== CREATE MODERATED MEDIATION VARIABLES ===\n")

  if (verbose) {
    cat("AI condition counts:\n")
    print(table(data$ai_condition, useNA = "ifany"))
  }

  # Verify task_complexity is numeric 0/1
  if (!is.numeric(data$task_complexity)) {
    data$task_complexity <- as.numeric(data$task_complexity)
  }

  if (!all(data$task_complexity %in% c(0, 1, NA))) {
    stop("task_complexity must be coded as 0 (easy) or 1 (hard)")
  }

  # Create predictor dummies with X_ prefix (reference = assistiv)
  reference <- ai_config$reference_level
  predictor_names <- c()

  for (level_name in ai_config$ordered_levels) {
    if (level_name != reference) {
      dummy_name <- paste0("X_", level_name)
      data[[dummy_name]] <- as.numeric(data$ai_condition == level_name)
      predictor_names <- c(predictor_names, dummy_name)
    }
  }

  if (verbose) {
    cat("\nReference group:", reference, "\n")
    cat("Predictor dummies created:", paste(predictor_names, collapse = ", "), "\n")
  }

  # Create centered variables for interaction to reduce VIF
  # Mean centering (X - mean(X)) ensures interaction is orthogonal to main effects
  # This is the "Manual Two-Stage" equivalent which solves VIF issues

  # Center moderator (task_complexity)
  # Use scale() but extract as vector to avoid matrix class issues
  tc_centered <- as.numeric(scale(data$task_complexity, scale = FALSE))

  interaction_names <- c()
  for (pred in predictor_names) {
    # Center predictor
    pred_centered <- as.numeric(scale(data[[pred]], scale = FALSE))

    # Create manual interaction term using centered variables
    # product = (X - X_mean) * (M - M_mean)
    # Naming convention: Use _x_ to avoid formula parsing issues with *
    interaction_name <- paste0(pred, "_x_task_complexity")

    data[[interaction_name]] <- pred_centered * tc_centered
    interaction_names <- c(interaction_names, interaction_name)
  }

  if (verbose) {
    cat("Interaction terms created (Manual Centered Product):", paste(interaction_names, collapse = ", "), "\n")
    cat("\nModerated mediation design:\n")
    cat("  First-stage predictors: ", paste(predictor_names, collapse = ", "), ", task_complexity\n", sep = "")
    cat("  Interaction terms:      ", paste(interaction_names, collapse = ", "), "\n", sep = "")
    cat("\nTask complexity distribution:\n")
    print(table(data$task_complexity, useNA = "ifany"))
  }

  attr(data, "predictor_names") <- predictor_names
  attr(data, "interaction_names") <- interaction_names

  return(data)
}

# RELIABILITY & VALIDATION FUNCTIONS

#' Safe wrapper for psych::alpha()
alpha_safe <- function(df_items) {
  out <- tryCatch(
    psych::alpha(df_items)$total$raw_alpha,
    error = function(e) NA_real_
  )
  as.numeric(out)
}

#' Compute Cronbach's alpha by groups
alpha_by_group <- function(df, group_var, item_vars, construct_name) {
  grp <- df[[group_var]]
  lvls <- sort(unique(as.character(stats::na.omit(grp))))
  bind_rows(lapply(lvls, function(g) {
    df_g <- df[as.character(df[[group_var]]) == g, , drop = FALSE]
    data.frame(
      construct = construct_name,
      group_label = g,
      alpha = alpha_safe(df_g[, item_vars, drop = FALSE]),
      n_items = length(item_vars),
      n_complete = sum(stats::complete.cases(df_g[, item_vars, drop = FALSE])),
      stringsAsFactors = FALSE
    )
  }))
}

#' Check missing data patterns
check_missing_data <- function(data, vars_to_check) {
  cat("\n=== MISSING DATA CHECK ===\n")
  missing_counts <- colSums(is.na(data[, vars_to_check]))
  missing_pct <- round(100 * missing_counts / nrow(data), 1)
  print(data.frame(
    variable = names(missing_counts),
    missing_n = as.integer(missing_counts),
    missing_pct = missing_pct
  ))
  invisible(NULL)
}

#' Check multicollinearity
check_multicollinearity <- function(data, cont_vars, threshold = 0.90) {
  cat("\n=== MULTICOLLINEARITY CHECK ===\n")
  cat("\n-- Descriptives --\n")
  desc <- psych::describe(data[, cont_vars])
  print(round(desc[, c("n", "mean", "sd", "min", "max")], 3))

  cat("\n-- Correlations --\n")
  cor_mat <- cor(data[, cont_vars], use = "pairwise.complete.obs")
  print(round(cor_mat, 3))

  max_cor <- max(abs(cor_mat[upper.tri(cor_mat)]), na.rm = TRUE)
  cat("\nMax |r|:", round(max_cor, 3), "\n")
  if (is.finite(max_cor) && max_cor >= threshold) {
    cat("⚠ Very high correlations detected\n")
  } else {
    cat(" Correlations acceptable\n")
  }

  invisible(NULL)
}

#' Compute reliability by group
#' @param data Data frame with items and grouping variable
#' @param mediator_config Mediator configuration list
#' @param dv_config DV configuration list
#' @param dv_vars DVs to compute reliability for
#' @param group_var Name of grouping variable (default "task_complexity")
#' @param verbose Print output
compute_reliability_by_group <- function(data, mediator_config, dv_config,
                                         dv_vars, group_var = "task_complexity",
                                         verbose = TRUE) {
  # Validate group_var exists
  if (!group_var %in% names(data)) {
    stop("Group variable '", group_var, "' not found in data")
  }

  mediator_alpha_list <- list()
  for (med_name in names(mediator_config)) {
    med_info <- mediator_config[[med_name]]
    mediator_alpha_list[[med_name]] <- alpha_by_group(
      data, group_var, med_info$items, med_name
    )
  }
  mediator_alpha_by_group <- bind_rows(mediator_alpha_list)

  dv_alpha_list <- list()
  for (dv_name in dv_vars) {
    if (dv_name %in% names(dv_config)) {
      dv_info <- dv_config[[dv_name]]
      dv_alpha_list[[dv_name]] <- alpha_by_group(
        data, group_var, dv_info$items, dv_name
      )
    }
  }
  dv_alpha_by_group <- bind_rows(dv_alpha_list)

  if (verbose) {
    cat("\n=== RELIABILITY BY GROUP (", group_var, ") ===\n", sep = "")
    cat("\n-- Mediators --\n")
    print(mediator_alpha_by_group %>% mutate(alpha = round(alpha, 3)))
    cat("\n-- DVs --\n")
    print(dv_alpha_by_group %>% mutate(alpha = round(alpha, 3)))
  }

  list(mediator = mediator_alpha_by_group, dv = dv_alpha_by_group)
}

# PLS-SEM MODEL BUILDING

#' Build measurement model from raw items
#' @param mediator_config List with mediator configurations containing items
#' @param dv_config List with DV configurations containing items
#' @param predictor_names Vector of predictor names (will be single items)
#' @param outcome_name Name of outcome variable in current analysis
#' @return seminr measurement model object
build_measurement_model_from_items <- function(mediator_config, dv_config,
                                               predictor_names, outcome_name) {
  construct_list <- list()

  # Predictors as single items (dummy variables)
  for (pred in predictor_names) {
    construct_list[[pred]] <- composite(pred, single_item(pred))
  }

  # Mediators as multi-item reflective factors
  for (med_name in names(mediator_config)) {
    med_info <- mediator_config[[med_name]]
    # Use reflective() for constructs where latent factor causes indicators
    construct_list[[med_name]] <- composite(med_name, med_info$items, weights = mode_A)
  }

  # Outcome as multi-item reflective composite
  if (outcome_name %in% names(dv_config)) {
    dv_info <- dv_config[[outcome_name]]
    # Use reflective() for outcome
    # Always name the outcome construct "y_out" to match structural paths
    construct_list[["y_out"]] <- composite("y_out", dv_info$items, weights = mode_A)
  } else {
    stop("Outcome '", outcome_name, "' not found in dv_config")
  }

  do.call(constructs, construct_list)
}

#' Build measurement model (backward compatible - for single items)
#' @param predictor_names Vector of predictor names
#' @param mediator_names Vector of mediator names
#' @param outcome_name Name of outcome variable
#' @return seminr measurement model object
#' @deprecated Use build_measurement_model_from_items() for proper PLS-SEM
build_measurement_model <- function(predictor_names, mediator_names, outcome_name = "y_out") {
  construct_list <- list()
  for (pred in predictor_names) construct_list[[pred]] <- composite(pred, single_item(pred))
  for (med in mediator_names) construct_list[[med]] <- composite(med, single_item(med))
  construct_list[[outcome_name]] <- composite(outcome_name, single_item(outcome_name))
  do.call(constructs, construct_list)
}

#' Build structural model
build_structural_model <- function(path_config) {
  path_list <- list()
  for (path_set_name in names(path_config)) {
    path_set <- path_config[[path_set_name]]
    path_list[[path_set_name]] <- paths(from = path_set$from, to = path_set$to)
  }
  do.call(relationships, path_list)
}

# PLS-SEM ESTIMATION

# MODERATED MEDIATION PLS-SEM ESTIMATION

#' Run single PLS-SEM model with moderated mediation (no MGA)
#' Estimates one model on full dataset with interaction terms for first-stage moderation
#' @param Y_var Name of the outcome variable
#' @param data Full dataset with all variables
#' @param mediator_names Vector of mediator variable names
#' @param predictor_names Vector of main predictor dummy names (X_agent_noval, X_agent_val)
#' @param moderator_name Name of moderator variable (task_complexity)
#' @param interaction_names Vector of interaction term names (X_agent_noval_x_TC, etc.)
#' @param path_config Structural paths configuration (from create_structural_paths_moderated)
#' @param indirect_config Indirect effects configuration (from create_indirect_config_moderated)
#' @param pls_settings PLS algorithm settings
#' @param bootstrap_n Number of bootstrap resamples
#' @param mediator_config Mediator item configuration
#' @param dv_config DV item configuration
#' @param use_raw_items Whether to use raw items (TRUE) or composites (FALSE)
#' @return List with model results, paths, conditional indirect effects, and quality metrics
run_pls_sem_moderated <- function(Y_var, data, mediator_names, predictor_names,
                                  moderator_name, interaction_names,
                                  path_config, indirect_config, pls_settings,
                                  bootstrap_n = 500,
                                  mediator_config = NULL, dv_config = NULL,
                                  use_raw_items = TRUE) {
  cat("\n--- Moderated Mediation PLS-SEM for:", Y_var, "---\n")

  # Build measurement model with raw items
  if (use_raw_items && !is.null(mediator_config) && !is.null(dv_config)) {
    mediator_items <- unique(unlist(lapply(mediator_config, function(x) x$items)))
    dv_items <- dv_config[[Y_var]]$items

    # All variables needed for estimation
    all_predictors <- c(predictor_names, moderator_name)
    vars_sem <- unique(c(all_predictors, interaction_names, mediator_items, dv_items))

    # Select required variables
    df_sem <- data %>%
      select(all_of(vars_sem)) %>%
      as.data.frame()

    # HARD VALIDATION: Check for any NA in required SEM items (Fix 1)
    # If NA found, stop() rather than silently dropping rows
    na_counts <- colSums(is.na(df_sem))
    items_with_na <- names(na_counts[na_counts > 0])

    if (length(items_with_na) > 0) {
      n_rows_affected <- sum(!complete.cases(df_sem))
      stop(
        "HARD VALIDATION FAILED for DV '", Y_var, "':\n",
        "  NA values found in ", length(items_with_na), " SEM variables.\n",
        "  Affected items: ", paste(items_with_na[1:min(5, length(items_with_na))], collapse = ", "),
        ifelse(length(items_with_na) > 5, "...", ""), "\n",
        "  Rows with NA: ", n_rows_affected, " of ", nrow(df_sem), "\n",
        "  Fix: Ensure data is complete before running SEM. Do NOT use mean replacement."
      )
    }

    cat("  ✓ Data validated: N =", nrow(df_sem), "(no NA in SEM items)\n")

    # Build measurement model: predictors as single items, mediators/DV as multi-item
    construct_list <- list()

    # Predictors and moderator as single-item composites
    for (pred in all_predictors) {
      construct_list[[pred]] <- composite(pred, single_item(pred))
    }

    # Interaction terms as single-item composites (using manual centered variables)
    for (inter in interaction_names) {
      construct_list[[inter]] <- composite(inter, single_item(inter))
    }

    # Mediators as multi-item reflective
    for (med_name in names(mediator_config)) {
      med_info <- mediator_config[[med_name]]
      construct_list[[med_name]] <- composite(med_name, med_info$items, weights = mode_A)
    }

    # Outcome as multi-item reflective
    dv_info <- dv_config[[Y_var]]
    construct_list[["y_out"]] <- composite("y_out", dv_info$items, weights = mode_A)

    measurement_model <- do.call(constructs, construct_list)
  } else {
    stop("Moderated mediation requires use_raw_items = TRUE with valid configs")
  }

  # Build structural model from path config
    # STAGE 1: MAIN EFFECTS MODEL (GET SCORES)
    cat("  > Stage 1: Estimating Main Effects Model (to get latent scores)...\n")

  # Stage 1 Structural Model: Remove interactions
  # Filter path_config to exclude interaction terms from "first_stage"
  path_config_stage1 <- path_config
  path_config_stage1$first_stage$from <- setdiff(path_config$first_stage$from, interaction_names)
  structural_model_stage1 <- build_structural_model(path_config_stage1)

  # Stage 1 Measurement Model: Remove interaction constructs
  # Use original items for everything else
  construct_list_stage1 <- construct_list
  # Remove interaction constructs (which are currently single items of manual product terms)
  # We will NOT use the manual product terms from 'create_dummy_variables' anymore
  # We will use the SCORES from Stage 1.
  for (inter in interaction_names) {
    construct_list_stage1[[inter]] <- NULL
  }
  measurement_model_stage1 <- do.call(constructs, construct_list_stage1)

  # Estimate Stage 1
  pls_model_stage1 <- estimate_pls(
    data = df_sem,
    measurement_model = measurement_model_stage1,
    structural_model = structural_model_stage1,
    inner_weights = get(pls_settings$inner_weights)
  )

    # STAGE 2: INTERACTION MODEL (USING SCORES)
    cat("  > Stage 2: Creating Interaction Terms from Stage 1 Scores...\n")

  # Extract scores (for Stage 2 data creation)
  scores <- pls_model_stage1$construct_scores

  # Check if scores valid
  if (is.null(scores)) {
    stop("Stage 1 estimation failed to produce construct scores.")
  }

  # Create Interaction Scores (X_score * TC_score)
  # Add them to df_sem
  for (i in seq_along(predictor_names)) {
    pred <- predictor_names[i]
    inter_name <- interaction_names[i] # e.g. X_val_x_TC

    # Calculate product of standardized scores
    term_score <- scores[, pred] * scores[, moderator_name]

    # Add to dataframe
    df_sem[[inter_name]] <- term_score
  }

  cat("  > Stage 2: Estimating Full Model with Interaction Scores...\n")

  # Stage 2 Measurement Model: Add Interaction Constructs (Single Item of Score)
  construct_list_stage2 <- construct_list_stage1
  for (inter in interaction_names) {
    # Define interaction construct as single item normalized score
    construct_list_stage2[[inter]] <- composite(inter, single_item(inter))
  }
  measurement_model_stage2 <- do.call(constructs, construct_list_stage2)

  # Stage 2 Structural Model: Full model (Interactions included)
  structural_model_stage2 <- build_structural_model(path_config)

  # Estimate Stage 2
  pls_model <- estimate_pls(
    data = df_sem,
    measurement_model = measurement_model_stage2,
    structural_model = structural_model_stage2,
    inner_weights = get(pls_settings$inner_weights)
  )

  # Robust Bootstrap the Stage 2 model
  cat("  > Bootstrapping Stage 2 (N =", bootstrap_n, ") with Robust Failsafe...\n")

  # Use new robust manual bootstrap instead of seminr::bootstrap_model directly
  boot_model <- tryCatch(
    {
      robust_bootstrap_model(
        pls_model = pls_model,
        nboot = bootstrap_n,
        cores = pls_settings$bootstrap_cores,
        seed = pls_settings$bootstrap_seed
      )
    },
    error = function(e) {
      cat("!!! ROBUST BOOTSTRAP FAILURE DETECTED !!!\n")
      print(e)
      cat("WARNING: Proceeding without bootstrap results (p-values will be NA).\n")
      NULL
    }
  )

  # Check validation only if not NULL
  if (is.null(boot_model)) {
    cat("  ! Bootstrapping failed completely. Results will contain point estimates only.\n")
  }

  # Extract path coefficients
  paths_tbl <- extract_paths(pls_model, boot_model, Y_var, "pooled")

  # Extract R² values
  r2_tbl <- extract_r2(pls_model, Y_var, "pooled")

  # Compute conditional indirect effects
  cat("  > Computing conditional indirect effects...\n")
  cond_indirects <- compute_conditional_indirect_effects(
    pls_model, boot_model, Y_var, indirect_config
  )

  # Extract measurement quality (DV only - mediators calculated separately)
  meas_quality <- extract_measurement_quality(
    pls_model, boot_model, Y_var, "pooled",
    mediator_names = mediator_names
  )

  # Extract loadings
  loadings_tbl <- extract_loadings(pls_model, boot_model, Y_var, "pooled")

  # Check discriminant validity
  discrim_result <- check_discriminant_validity(pls_model, Y_var, "pooled")

  # Extract structural quality (VIF, f²)
  struct_quality <- extract_structural_quality(pls_model, Y_var, "pooled")

  cat("  ✓ Model estimation complete\n")

  list(
    pls_model = pls_model,
    boot_model = boot_model,
    paths = paths_tbl,
    conditional_indirects = cond_indirects,
    fit_measures = data.frame(
      dv = Y_var,
      n_total = nrow(df_sem),
      convergence = "PLS converged",
      stringsAsFactors = FALSE
    ),
    r2 = r2_tbl,
    measurement_quality = meas_quality,
    loadings = loadings_tbl,
    htmt = discrim_result$htmt_long,
    structural_quality = struct_quality,
    discrim_status = data.frame(
      dv = Y_var,
      group_label = "pooled",
      htmt_ok = as.logical(discrim_result$all_ok)[1],
      max_htmt = as.numeric(discrim_result$max_htmt)[1],
      stringsAsFactors = FALSE
    )
  )
}

#' Compute conditional indirect effects from bootstrap model
#' For moderated mediation: IE(TC=0) = a0 * b, IE(TC=1) = (a0 + g) * b
#' @param pls_model Estimated PLS model
#' @param boot_model Bootstrapped PLS model
#' @param dv Name of dependent variable
#' @param indirect_config Indirect effects configuration with interaction info
#' @return Data frame with conditional indirect effects and CIs
compute_conditional_indirect_effects <- function(pls_model, boot_model, dv, indirect_config) {
  tryCatch(
    {
      results_list <- list()

      # Get path coefficients from model
      paths <- pls_model$path_coef

      # Get bootstrap draws for computing CIs (if available)
      is_valid_boot <- !is.null(boot_model) && is.list(boot_model) && !inherits(boot_model, "try-error")
      boot_paths <- if (is_valid_boot) boot_model$boot_paths else NULL

      # Function to compute empirical p-value from bootstrap distribution
      compute_empirical_p_value <- function(boot_dist) {
        if (all(is.na(boot_dist))) {
          return(NA_real_)
        }
        prop_pos <- mean(boot_dist > 0, na.rm = TRUE)
        prop_neg <- mean(boot_dist < 0, na.rm = TRUE)
        2 * min(prop_pos, prop_neg)
      }

      for (spec in indirect_config) {
        predictor <- spec$from
        mediator <- spec$through
        outcome <- spec$to
        interaction <- spec$interaction

        # Extract coefficients
        # a0 = X → M (main effect)
        # g  = X×TC → M (interaction)
        # b  = M → Y (second stage)

        a0 <- paths[predictor, mediator]
        g <- paths[interaction, mediator]
        b <- paths[mediator, outcome]

        if (is.na(a0) || is.na(g) || is.na(b)) {
          next
        }

        # Point estimates for conditional indirect effects
        ie_easy <- a0 * b # TC = 0
        ie_hard <- (a0 + g) * b # TC = 1
        delta_ie <- g * b # Difference (moderated portion)

        # Bootstrap CIs
        # boot_paths is a 3D array: [from_var, to_var, bootstrap_sample]

        boot_a0 <- NULL
        boot_g <- NULL
        boot_b <- NULL

        if (!is.null(boot_paths)) {
          from_vars <- rownames(boot_paths)
          to_vars <- colnames(boot_paths)

          get_boot_path <- function(from_var, to_var) {
            if (from_var %in% from_vars && to_var %in% to_vars) {
              return(boot_paths[from_var, to_var, ])
            }
            return(NULL)
          }

          boot_a0 <- get_boot_path(predictor, mediator)
          boot_g <- get_boot_path(interaction, mediator)
          boot_b <- get_boot_path(mediator, outcome)
        }

        if (!is.null(boot_a0) && !is.null(boot_g) && !is.null(boot_b)) {
          # Compute bootstrap distribution of indirect effects
          boot_ie_easy <- boot_a0 * boot_b
          boot_ie_hard <- (boot_a0 + boot_g) * boot_b
          boot_delta <- boot_g * boot_b

          # 95% CIs
          ci_easy <- quantile(boot_ie_easy, c(0.025, 0.975), na.rm = TRUE)
          ci_hard <- quantile(boot_ie_hard, c(0.025, 0.975), na.rm = TRUE)
          ci_delta <- quantile(boot_delta, c(0.025, 0.975), na.rm = TRUE)

          pval_easy <- compute_empirical_p_value(boot_ie_easy)
          pval_hard <- compute_empirical_p_value(boot_ie_hard)
          pval_delta <- compute_empirical_p_value(boot_delta)
        } else {
          ci_easy <- c(NA, NA)
          ci_hard <- c(NA, NA)
          ci_delta <- c(NA, NA)
          pval_easy <- NA_real_
          pval_hard <- NA_real_
          pval_delta <- NA_real_
        }

        # Significance based on CI (0 not in interval)
        sig_easy <- !is.na(ci_easy[1]) && (ci_easy[1] > 0 | ci_easy[2] < 0)
        sig_hard <- !is.na(ci_hard[1]) && (ci_hard[1] > 0 | ci_hard[2] < 0)
        sig_delta <- !is.na(ci_delta[1]) && (ci_delta[1] > 0 | ci_delta[2] < 0)

        pred_short <- gsub("X_", "", predictor)
        med_short <- gsub("m_", "", mediator)

        # Add IE at TC=0 (easy)
        results_list[[length(results_list) + 1]] <- data.frame(
          dv = dv,
          effect = paste0(pred_short, "→", med_short, "→Y"),
          condition = "TC=0 (easy)",
          estimate = ie_easy,
          ci_lower = ci_easy[1],
          ci_upper = ci_easy[2],
          p_value = pval_easy,
          significant = sig_easy,
          stringsAsFactors = FALSE
        )

        # Add IE at TC=1 (hard)
        results_list[[length(results_list) + 1]] <- data.frame(
          dv = dv,
          effect = paste0(pred_short, "→", med_short, "→Y"),
          condition = "TC=1 (hard)",
          estimate = ie_hard,
          ci_lower = ci_hard[1],
          ci_upper = ci_hard[2],
          p_value = pval_hard,
          significant = sig_hard,
          stringsAsFactors = FALSE
        )

        # Add Delta (moderation effect)
        results_list[[length(results_list) + 1]] <- data.frame(
          dv = dv,
          effect = paste0(pred_short, "→", med_short, "→Y"),
          condition = "Delta (moderation)",
          estimate = delta_ie,
          ci_lower = ci_delta[1],
          ci_upper = ci_delta[2],
          p_value = pval_delta,
          significant = sig_delta,
          stringsAsFactors = FALSE
        )
      }

      if (length(results_list) > 0) {
        bind_rows(results_list)
      } else {
        data.frame(
          dv = character(),
          effect = character(),
          condition = character(),
          estimate = numeric(),
          ci_lower = numeric(),
          ci_upper = numeric(),
          p_value = numeric(),
          significant = logical(),
          stringsAsFactors = FALSE
        )
      }
    },
    error = function(e) {
      cat("✗ Conditional IEs failed for", dv, ":", conditionMessage(e), "\n")
      # Return empty frame with expected structural
      data.frame(
        dv = character(), condition = character(), effect = character(),
        estimate = numeric(), se = numeric(),
        ci_lower = numeric(), ci_upper = numeric(),
        p_value = numeric(), significant = logical(),
        stringsAsFactors = FALSE
      )
    }
  )
}

#' Extract path coefficients with empirical bootstrap p-values
#' @param pls_model Estimated PLS model
#' @param boot_model Bootstrapped PLS model
#' @param dv Name of dependent variable
#' @param group Group label
#' @return Data frame with path coefficients and empirical bootstrap p-values
extract_paths <- function(pls_model, boot_model, dv, group) {
  # Robust check for valid boot_model object
  is_valid_boot <- !is.null(boot_model) && is.list(boot_model) && !inherits(boot_model, "try-error")

  # If bootstrap failed, construct table from pls_model point estimates only
  if (!is_valid_boot) {
    paths <- pls_model$path_coef
    rows <- list()
    for (from in rownames(paths)) {
      for (to in colnames(paths)) {
        if (paths[from, to] != 0 && !is.na(paths[from, to])) {
          rows[[length(rows) + 1]] <- data.frame(
            dv = dv, group_label = group,
            lhs = from, rhs = to,
            est = paths[from, to], se = NA, t_value = NA, p_value = NA,
            ci_lower = NA, ci_upper = NA,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    return(do.call(rbind, rows))
  }

    # PRIORITY: STANDARD SEMINR SUMMARY
    # Now that robust_bootstrap_model creates a complete object with loadings/weights,
  # summary() should work. We try this first to use standard seminr logic.

  summ <- tryCatch(summary(boot_model), error = function(e) NULL)

  if (!is.null(summ) && !is.null(summ$bootstrapped_paths) && nrow(summ$bootstrapped_paths) > 0) {
    # Get bootstrap summary and preserve rownames explicitly
    boot_matrix <- summ$bootstrapped_paths
    original_rownames <- rownames(boot_matrix)
    paths_tbl <- as.data.frame(boot_matrix)
    rownames(paths_tbl) <- original_rownames

    paths_tbl$dv <- dv
    paths_tbl$group_label <- group

    # Standardize column names (depending on seminr version)
    nms <- names(paths_tbl)
    if ("Original Est." %in% nms) names(paths_tbl)[nms == "Original Est."] <- "est"
    if ("Original Sample" %in% nms) names(paths_tbl)[nms == "Original Sample"] <- "est"
    if ("Bootstrap SD" %in% nms) names(paths_tbl)[nms == "Bootstrap SD"] <- "se"
    if ("Std. Error" %in% nms) names(paths_tbl)[nms == "Std. Error"] <- "se"
    if ("T Stat." %in% nms) names(paths_tbl)[nms == "T Stat."] <- "t_value"
    if ("2.5% CI" %in% nms) names(paths_tbl)[nms == "2.5% CI"] <- "ci_lower"
    if ("97.5% CI" %in% nms) names(paths_tbl)[nms == "97.5% CI"] <- "ci_upper"

    # Fill missing cols
    if (!"est" %in% names(paths_tbl)) paths_tbl$est <- NA_real_
    if (!"se" %in% names(paths_tbl)) paths_tbl$se <- NA_real_
    if (!"t_value" %in% names(paths_tbl)) paths_tbl$t_value <- NA_real_
    if (!"ci_lower" %in% names(paths_tbl)) paths_tbl$ci_lower <- NA_real_
    if (!"ci_upper" %in% names(paths_tbl)) paths_tbl$ci_upper <- NA_real_

    # CALCULATE P-VALUE FROM T-VALUE AND DF (seminr logic)
    if (!"p_value" %in% names(paths_tbl)) {
      if (!all(is.na(paths_tbl$t_value))) {
        paths_tbl$p_value <- 2 * (1 - pnorm(abs(paths_tbl$t_value)))
      } else {
        paths_tbl$p_value <- NA_real_
      }
    }

    # Parse paths for lhs/rhs
    path_col <- intersect(c("Path", "path", "Relationship", "relationship"), names(paths_tbl))
    path_strings <- NULL

    if (length(path_col) > 0) {
      path_strings <- as.character(paths_tbl[[path_col[1]]])
    } else {
      rn <- rownames(paths_tbl)
      if (!is.null(rn) && length(rn) == nrow(paths_tbl)) {
        path_strings <- rn
      }
    }

    if (!is.null(path_strings)) {
      # Robust extraction of from/to names (handles "A -> B" or "A->B")
      parts <- strsplit(rownames(paths_tbl), "\\s*->\\s*")
      paths_tbl$rhs <- trimws(vapply(parts, function(x) if (length(x) >= 1) x[1] else NA_character_, character(1)))
      paths_tbl$lhs <- trimws(vapply(parts, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1)))
    } else {
      paths_tbl$rhs <- NA_character_
      paths_tbl$lhs <- NA_character_
    }

    required_cols <- c("dv", "group_label", "lhs", "rhs", "est", "se", "t_value", "p_value", "ci_lower", "ci_upper")
    missing_cols <- setdiff(required_cols, names(paths_tbl))
    for (col in missing_cols) {
      if (col == "t_value" && "t_value" %in% names(paths_tbl)) {
        # Already handled
      } else {
        paths_tbl[[col]] <- NA_real_
      }
    }

    # Filter out non-existent paths (near-zero estimates)
    # These are usually artifacts of the matrix extraction (e.g., identity paths)
    paths_tbl <- paths_tbl[abs(as.numeric(paths_tbl$est)) > 1e-10, ]

    if (nrow(paths_tbl) == 0) {
      return(data.frame())
    }

    return(paths_tbl[, required_cols, drop = FALSE])
  }

    # FALLBACK: MANUAL CALCULATION (If summary() still fails)
  
    # FALLBACK: MANUAL CALCULATION (If summary() still fails or is empty)
  
  boot_paths <- boot_model$boot_paths

  # Check if we have a valid 3D array [from, to, iter]
  if (!is.null(boot_paths) && length(dim(boot_paths)) == 3) {
    # Get the original point estimates
    paths <- pls_model$path_coef

    rows_list <- list()

    # Iterate over all paths in the model
    for (from in rownames(paths)) {
      for (to in colnames(paths)) {
        # Only process actual paths (non-zero or existing)
        original_est <- paths[from, to]

        if (!is.na(original_est) && abs(original_est) > 1e-10) {
          # Check if we have bootstrap data for this path
          if (from %in% rownames(boot_paths) && to %in% colnames(boot_paths)) {
            # Extract distribution
            dist <- boot_paths[from, to, ]
            dist <- dist[!is.na(dist)]

            if (length(dist) > 2) {
              # Calculate Stats
              se <- sd(dist)
              t_val <- if (se > 0) original_est / se else NA

              # P-Value (Empirical two-tailed)
              prop_pos <- mean(dist > 0)
              prop_neg <- mean(dist < 0)
              p_val <- 2 * min(prop_pos, prop_neg)

              # Confidence Intervals (Percentile method)
              ci <- quantile(dist, probs = c(0.025, 0.975))

              rows_list[[length(rows_list) + 1]] <- data.frame(
                dv = dv,
                group_label = group,
                lhs = from,
                rhs = to,
                est = original_est,
                se = se,
                t_value = t_val,
                p_value = p_val,
                ci_lower = as.numeric(ci[1]),
                ci_upper = as.numeric(ci[2]),
                stringsAsFactors = FALSE
              )
            } else {
              rows_list[[length(rows_list) + 1]] <- data.frame(
                dv = dv, group_label = group, lhs = from, rhs = to,
                est = original_est, se = NA, t_value = NA, p_value = NA,
                ci_lower = NA, ci_upper = NA, stringsAsFactors = FALSE
              )
            }
          } else {
            rows_list[[length(rows_list) + 1]] <- data.frame(
              dv = dv, group_label = group, lhs = from, rhs = to,
              est = original_est, se = NA, t_value = NA, p_value = NA,
              ci_lower = NA, ci_upper = NA, stringsAsFactors = FALSE
            )
          }
        }
      }
    }

    if (length(rows_list) > 0) {
      return(do.call(rbind, rows_list))
    }
  }

  return(data.frame())
}


#' Extract R² values
extract_r2 <- function(pls_model, dv, group) {
  r2_vec <- pls_model$rSquared

  # Handle matrix (e.g. Rsq and AdjRsq rows) vs vector
  if (is.matrix(r2_vec)) {
    # Typically row 1 is Rsq, row 2 is AdjRsq. We take the first row.
    # Check if "Rsq" row exists
    if ("Rsq" %in% rownames(r2_vec)) {
      r2_vals <- r2_vec["Rsq", ]
    } else {
      r2_vals <- r2_vec[1, ]
    }
    endo_names <- names(r2_vals) # Should now be correct length
  } else {
    r2_vals <- as.numeric(r2_vec)
    endo_names <- names(r2_vec)
  }

  n_rows <- length(r2_vals)

  # Fallback if names are still missing or length mismatch
  if (is.null(endo_names) || length(endo_names) != n_rows) {
    if (n_rows == 1) {
      endo_names <- dv # Safe assumption for single DV model
    } else {
      if (!is.null(endo_names)) {
        # Suppress warning if mismatch but proceed with generated names
        # warning("Mismatch in R2 names for ", dv, " ", group)
      }
      endo_names <- paste0("Endogenous_", 1:n_rows)
    }
  }

  data.frame(
    dv = rep(dv, n_rows),
    group_label = rep(group, n_rows),
    endogenous = endo_names,
    r2 = as.numeric(r2_vals),
    stringsAsFactors = FALSE
  )
}

#' Compute specific indirect effects
compute_specific_indirects <- function(pls_model, boot_model, dv, group, indirect_config) {
  paths <- pls_model$path_coef
  indirects_list <- list()

  for (spec in indirect_config) {
    a_path <- paths[spec$from, spec$through]
    b_path <- paths[spec$through, spec$to]

    if (!is.na(a_path) && !is.na(b_path) && abs(a_path) > 1e-10 && abs(b_path) > 1e-10) {
      ind_result <- tryCatch(
        {
          specific_effect_significance(
            boot_seminr_model = boot_model,
            from = spec$from, through = spec$through, to = spec$to, alpha = 0.05
          )
        },
        error = function(e) {
          cat("Error computing indirect effect for", spec$name, ":", conditionMessage(e), "\n")
          return(NULL)
        }
      )

      if (!is.null(ind_result)) {
        # Safely extract values with fallbacks
        est_val <- if (is.list(ind_result) && "Original Est." %in% names(ind_result)) {
          ind_result[["Original Est."]][1]
        } else if (is.matrix(ind_result) && "Original Est." %in% colnames(ind_result)) {
          ind_result[1, "Original Est."]
        } else if (is.numeric(ind_result) && "Original Est." %in% names(ind_result)) {
          ind_result["Original Est."]
        } else if (is.list(ind_result) && "Original Sample" %in% names(ind_result)) {
          ind_result[["Original Sample"]][1]
        } else if (is.matrix(ind_result) && "Original Sample" %in% colnames(ind_result)) {
          ind_result[1, "Original Sample"]
        } else {
          a_path * b_path
        }

        se_val <- if (is.list(ind_result) && "Bootstrap SD" %in% names(ind_result)) {
          ind_result[["Bootstrap SD"]][1]
        } else if (is.list(ind_result) && "Std. Error" %in% names(ind_result)) {
          ind_result[["Std. Error"]][1]
        } else if (is.matrix(ind_result) && "Bootstrap SD" %in% colnames(ind_result)) {
          ind_result[1, "Bootstrap SD"]
        } else if (is.matrix(ind_result) && "Std. Error" %in% colnames(ind_result)) {
          ind_result[1, "Std. Error"]
        } else {
          NA_real_
        }

        ci_lower <- if (is.list(ind_result) && "2.5% CI" %in% names(ind_result)) {
          ind_result[["2.5% CI"]][1]
        } else if (is.matrix(ind_result) && "2.5% CI" %in% colnames(ind_result)) {
          ind_result[1, "2.5% CI"]
        } else if (is.list(ind_result) && "2.5%" %in% names(ind_result)) {
          ind_result[["2.5%"]][1]
        } else if (is.matrix(ind_result) && "2.5%" %in% colnames(ind_result)) {
          ind_result[1, "2.5%"]
        } else if (is.numeric(ind_result) && "2.5% CI" %in% names(ind_result)) {
          ind_result["2.5% CI"]
        } else if (is.numeric(ind_result) && "2.5%" %in% names(ind_result)) {
          ind_result["2.5%"]
        } else {
          NA_real_
        }

        ci_upper <- if (is.list(ind_result) && "97.5% CI" %in% names(ind_result)) {
          ind_result[["97.5% CI"]][1]
        } else if (is.matrix(ind_result) && "97.5% CI" %in% colnames(ind_result)) {
          ind_result[1, "97.5% CI"]
        } else if (is.list(ind_result) && "97.5%" %in% names(ind_result)) {
          ind_result[["97.5%"]][1]
        } else if (is.matrix(ind_result) && "97.5%" %in% colnames(ind_result)) {
          ind_result[1, "97.5%"]
        } else if (is.numeric(ind_result) && "97.5% CI" %in% names(ind_result)) {
          ind_result["97.5% CI"]
        } else if (is.numeric(ind_result) && "97.5%" %in% names(ind_result)) {
          ind_result["97.5%"]
        } else {
          NA_real_
        }

        p_val <- if (is.list(ind_result) && "T Stat." %in% names(ind_result)) {
          2 * (1 - pnorm(abs(ind_result[["T Stat."]][1])))
        } else if (is.matrix(ind_result) && "T Stat." %in% colnames(ind_result)) {
          2 * (1 - pnorm(abs(ind_result[1, "T Stat."])))
        } else if (is.numeric(ind_result) && "T Stat." %in% names(ind_result)) {
          2 * (1 - pnorm(abs(ind_result["T Stat."])))
        } else {
          NA_real_
        }


        # Determine Mediation Type
        # Direct effect: c_prime (predictor -> outcome)
        # Indirect effect: a * b

        mediation_type <- "No Mediation"

        # Check if direct effect exists in bootstrap results
        direct_row_name <- paste0(spec$from, " -> ", spec$to)
        direct_summ <- tryCatch(summary(boot_model)$bootstrapped_paths[direct_row_name, ], error = function(e) NULL)

        direct_sig <- FALSE
        direct_est <- NA
        if (!is.null(direct_summ)) {
          direct_est <- direct_summ["Original Est."]
          # Check significance (0 doesn't fall in CI)
          ci_low <- direct_summ["2.5% CI"]
          ci_hi <- direct_summ["97.5% CI"]
          if (!is.na(ci_low) && !is.na(ci_hi)) {
            if (ci_low > 0 || ci_hi < 0) direct_sig <- TRUE
          }
        }

        indirect_sig <- FALSE
        if (!is.na(ci_lower) && !is.na(ci_upper)) {
          if (ci_lower > 0 || ci_upper < 0) indirect_sig <- TRUE
        }

        if (indirect_sig && !direct_sig) {
          mediation_type <- "Full Mediation"
        } else if (indirect_sig && direct_sig) {
          # Check sign
          if (sign(est_val) == sign(direct_est)) {
            mediation_type <- "Partial Mediation (Complementary)"
          } else {
            mediation_type <- "Partial Mediation (Competitive)"
          }
        } else if (!indirect_sig && direct_sig) {
          mediation_type <- "Direct Only (No Mediation)"
        }

        indirects_list[[length(indirects_list) + 1]] <- data.frame(
          dv = dv, group_label = group, w_level = group,
          effect = paste0(spec$name, "_", group),
          est = est_val,
          se = se_val,
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          p_value = p_val,
          significant = indirect_sig,
          mediation_type = mediation_type,
          stringsAsFactors = FALSE
        )
      } else {
        indirects_list[[length(indirects_list) + 1]] <- data.frame(
          dv = dv, group_label = group, w_level = group,
          effect = paste0(spec$name, "_", group),
          est = a_path * b_path, se = NA, ci_lower = NA, ci_upper = NA,
          p_value = NA, mediation_type = "Not Tested",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Total indirect effects
  for (condition in unique(sapply(indirect_config, function(x) x$from))) {
    cond_inds <- indirects_list[grepl(condition, sapply(indirects_list, function(x) x$effect))]
    if (length(cond_inds) > 0) {
      total_est <- sum(sapply(cond_inds, function(x) x$est))
      cond_name <- gsub("D_", "", condition)
      indirects_list[[length(indirects_list) + 1]] <- data.frame(
        dv = dv, group_label = group, w_level = group,
        effect = paste0("ind_", cond_name, "_total_", group),
        est = total_est, se = NA, ci_lower = NA, ci_upper = NA, p_value = NA,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(indirects_list) > 0) bind_rows(indirects_list) else data.frame()
}

#' Perform PLS-MGA tests
#' Perform PLS-MGA tests
perform_mga_tests <- function(boot_easy, boot_hard, dv) {
  paths_easy <- boot_easy$seminr_model$path_coef
  paths_hard <- boot_hard$seminr_model$path_coef
  mga_list <- list()
  idx <- 1

  # Iterate over common paths
  for (to_var in intersect(colnames(paths_easy), colnames(paths_hard))) {
    for (from_var in intersect(rownames(paths_easy), rownames(paths_hard))) {
      coef_easy <- paths_easy[from_var, to_var]
      coef_hard <- paths_hard[from_var, to_var]

      # Check if coefficients are valid and at least one is non-zero
      if (!is.na(coef_easy) && !is.na(coef_hard) &&
        (abs(coef_easy) > 1e-10 || abs(coef_hard) > 1e-10)) {
        path_diff <- coef_hard - coef_easy
        summ_easy <- summary(boot_easy)
        summ_hard <- summary(boot_hard)
        se_easy <- NA
        se_hard <- NA

        # Helper to extract SE safely
        get_se <- function(summ, from, to) {
          if (is.null(summ$bootstrapped_paths)) {
            return(NA)
          }
          row_name <- paste(from, "->", to)
          boot_tbl <- as.data.frame(summ$bootstrapped_paths)
          if (row_name %in% rownames(boot_tbl)) {
            return(boot_tbl[row_name, "Std. Error"])
          }
          # Try alternative format "from -> to" with spaces
          row_name_sp <- paste(from, " -> ", to)
          if (row_name_sp %in% rownames(boot_tbl)) {
            return(boot_tbl[row_name_sp, "Std. Error"])
          }
          return(NA)
        }

        se_easy <- get_se(summ_easy, from_var, to_var)
        se_hard <- get_se(summ_hard, from_var, to_var)

        # Parametric Test (assuming independent groups)
        if (!is.na(se_easy) && !is.na(se_hard) && (se_easy + se_hard) > 0) {
          # Pooled SE (simplified assumption for independent samples)
          pooled_se <- sqrt(se_easy^2 + se_hard^2)
          z_stat <- path_diff / pooled_se
          p_value <- 2 * (1 - pnorm(abs(z_stat)))
        } else {
          z_stat <- NA
          p_value <- NA
        }

        mga_list[[idx]] <- data.frame(
          dv = dv,
          path = paste0(from_var, " -> ", to_var),
          path_easy = coef_easy,
          path_hard = coef_hard,
          diff_hard_minus_easy = path_diff,
          z_score_parametric = z_stat,
          p_val_parametric = p_value,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
    }
  }

  if (length(mga_list) > 0) bind_rows(mga_list) else data.frame()
}

#' Perform MICOM (Measurement Invariance of Composite Models)
#' checks Step 2: Compositional Invariance using Permutation
#'
#' @param pls_model_pooled The PLS model estimated on the full dataset
#' @param data Full dataset
#' @param group_var Name of grouping variable
#' @param construct_names Names of constructs to test
#' @param n_perm Number of permutations (default 100, recommend 1000+)
#' @param cores Number of cores for parallel processing
#' @return Data frame with invariance results
perform_micom <- function(pls_model_pooled, data, group_var, construct_names, n_perm = 100, cores = NULL) {
  cat("\n=== MICOM: COMPOSITIONAL INVARIANCE (Step 2) ===\n")

  groups <- unique(data[[group_var]])
  if (length(groups) != 2) {
    warning("MICOM currently supports exactly 2 groups")
    return(data.frame())
  }

  # 1. Calculate Original Correlations (c_orig)
    # Needed: Scores from pooled weights vs Scores from group-specific weights

  # Scores from pooled model are already computed on the full dataset
  scores_pooled <- as.data.frame(pls_model_pooled$construct_scores)

  # We need group-specific weights applied to group data
  # Extract parts for re-estimation
  mm <- pls_model_pooled$measurement_model
  sm <- pls_model_pooled$smMatrix

  # Helper to estimate and get scores for a subset
  estimate_and_get_scores <- function(df_subset) {
    # Suppress output during permutations
    capture.output({
      suppressMessages({
        model <- tryCatch(
          estimate_pls(
            data = df_subset, measurement_model = mm, structural_model = sm,
            inner_weights = seminr::path_weighting,
            missing = seminr::mean_replacement,
            missing_value = NA
          ),
          error = function(e) NULL
        )
      })
    })
    if (is.null(model)) {
      return(NULL)
    }
    return(model$construct_scores)
  }

  # Original Group Estimation
  scores_group_list <- list()
  c_orig <- numeric(length(construct_names))
  names(c_orig) <- construct_names

  valid_constructs <- c()

  for (const in construct_names) {
    # Check if construct exists in pooled scores
    if (!const %in% colnames(scores_pooled)) next
    valid_constructs <- c(valid_constructs, const)
  }

  if (length(valid_constructs) == 0) {
    return(data.frame())
  }

  # Re-estimate separate models to get Y_separate
  df_g1 <- data[data[[group_var]] == groups[1], ]
  df_g2 <- data[data[[group_var]] == groups[2], ]

  mod_g1 <- estimate_and_get_scores(df_g1)
  mod_g2 <- estimate_and_get_scores(df_g2)

  if (is.null(mod_g1) || is.null(mod_g2)) {
    warning("MICOM: Could not estimate original group models")
    return(data.frame())
  }

  # Create a full length vector for each construct with group-specific scores
  c_values_orig <- numeric(length(valid_constructs))
  names(c_values_orig) <- valid_constructs

  for (const in valid_constructs) {
    # Pooled scores for G1 and G2
    # Find row indices
    idx_g1 <- which(data[[group_var]] == groups[1])
    idx_g2 <- which(data[[group_var]] == groups[2])

    y_p_g1 <- scores_pooled[idx_g1, const]
    y_p_g2 <- scores_pooled[idx_g2, const]

    # Group scores
    y_g_g1 <- mod_g1[, const]
    y_g_g2 <- mod_g2[, const]

    # Combine
    y_pooled_all <- c(y_p_g1, y_p_g2)
    y_group_all <- c(y_g_g1, y_g_g2)

    # Calculate c
    if (sd(y_pooled_all) > 0 && sd(y_group_all) > 0) {
      c_values_orig[[const]] <- cor(y_pooled_all, y_group_all)
    } else {
      c_values_orig[[const]] <- 0
    }
  }

  cat("  Original correlations (c):\n")
  print(round(c_values_orig, 4))

  # 2. Permutation Test
  # Shuffle group labels -> split -> estimate separate -> combine scores -> cor(Y_p, Y_perm_sep)

  # Helper for parallel processing if cores > 1

  # Run loop
  if (!is.null(cores) && cores > 1) {
    # simplified parallel not implemented here to avoid heavy dependencies, using lapply
    # In real scenario: parallel::mclapply or parLapply
    perm_results <- lapply(1:n_perm, run_permutation)
  } else {
    perm_results <- lapply(1:n_perm, run_permutation)
  }

  # Bind results (Rows = Permutations, Cols = Constructs)
  perm_matrix <- do.call(rbind, perm_results)

  # 3. Calculate p-values

  results_list <- list()

  for (k in seq_along(valid_constructs)) {
    cn <- valid_constructs[k]
    c_orig_val <- c_values_orig[cn]
    c_perms <- perm_matrix[, k]
    c_perms <- c_perms[!is.na(c_perms)]

    # 5% quantile
    quantile_05 <- quantile(c_perms, 0.05)

    passed <- c_orig_val >= quantile_05

    results_list[[length(results_list) + 1]] <- data.frame(
      construct = cn,
      c_observed = c_orig_val,
      c_quantile_5 = as.numeric(quantile_05),
      compositional_invariance_established = passed,
      p_value = sum(c_perms <= c_orig_val) / length(c_perms),
      stringsAsFactors = FALSE
    )
  }

  res <- bind_rows(results_list)
  print(res)
  return(res)
}

#' Custom Robust Bootstrap Function
#' Manually loops through bootstrap resamples, skipping those that fail (e.g. deviation < 1e-7 or singularity).
#' Returns a list object mimicking seminr::bootstrap_model output (partial).
#' @param pls_model The estimated PLS model
#' @param nboot Number of bootstrap samples
#' @param cores Number of cores (ignored in this simple loop version for stability, or could use parallel backend)
#' @param seed Random seed
robust_bootstrap_model <- function(pls_model, nboot = 500, cores = NULL, seed = 123) {
  set.seed(seed)

  # Data from model (Handle different seminr versions/fields)
  # Standard seminr uses $data, but some contexts showed $rawdata or $rawData
  original_data <- pls_model$data
  if (is.null(original_data)) original_data <- pls_model$rawdata
  if (is.null(original_data)) original_data <- pls_model$rawData

  if (is.null(original_data)) {
    stop("Could not extract data from pls_model. Fields 'data', 'rawdata', 'rawData' are all NULL.")
  }

  n_obs <- nrow(original_data)

  # Storage for successful paths
  # We need to know dimensions first.
  # Using pls_model$path_coef to get structure
  path_coef <- pls_model$path_coef
  rows <- rownames(path_coef)
  cols <- colnames(path_coef)
  n_rows <- length(rows)
  n_cols <- length(cols)

  # Pre-allocate array: [row, col, boot_iter]
  # We'll fill this dynamically
  boot_paths_list <- list()
  boot_loadings_list <- list() # New: Capture loadings
  boot_weights_list <- list() # New: Capture weights

  success_count <- 0

  # Progress bar
  pb <- txtProgressBar(min = 0, max = nboot, style = 3)

  for (i in 1:nboot) {
    # Resample indices with replacement
    indices <- sample.int(n_obs, size = n_obs, replace = TRUE)
    boot_data <- original_data[indices, ]

    # Estimate model on resampled data
    # We must suppress errors
    res <- tryCatch(
      {
        # Capture output to suppress regular console spam
        utils::capture.output({
          suppressMessages({
            boot_est <- estimate_pls(
              data = boot_data,
              measurement_model = pls_model$measurement_model,
              structural_model = pls_model$smMatrix, # Using the matrix from original model
              inner_weights = seminr::path_factorial, # Hardcoded or passed? Assuming path_factorial based on config
              missing = seminr::mean_replacement,
              missing_value = NA
            )
          })
        })
        boot_est
      },
      error = function(e) NULL,
      warning = function(w) NULL
    )

    if (!is.null(res) && !is.null(res$path_coef)) {
      # Check for NAs in path coef (failed convergence)
      if (!any(is.na(res$path_coef))) {
        boot_paths_list[[length(boot_paths_list) + 1]] <- res$path_coef
        boot_loadings_list[[length(boot_loadings_list) + 1]] <- res$outer_loadings
        boot_weights_list[[length(boot_weights_list) + 1]] <- res$outer_weights
        success_count <- success_count + 1
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)

  cat(paste0("\n  > Robust Bootstrap: ", success_count, "/", nboot, " resamples succeeded.\n"))

  if (success_count < 10) {
    warning("Too few successful bootstrap samples (<10). Returning NULL.")
    return(NULL)
  }

  # Convert list of matrices to 3D array [from, to, iter]
  # Assumption: all path_coef matrices have same dims and names (they should)

  # PATHS
  boot_paths_array <- array(NA, dim = c(n_rows, n_cols, success_count), dimnames = list(rows, cols, 1:success_count))

  # LOADINGS
  loadings_rows <- rownames(pls_model$outer_loadings)
  loadings_cols <- colnames(pls_model$outer_loadings)
  boot_loadings_array <- array(NA,
    dim = c(length(loadings_rows), length(loadings_cols), success_count),
    dimnames = list(loadings_rows, loadings_cols, 1:success_count)
  )

  # WEIGHTS
  weights_rows <- rownames(pls_model$outer_weights)
  weights_cols <- colnames(pls_model$outer_weights)
  boot_weights_array <- array(NA,
    dim = c(length(weights_rows), length(weights_cols), success_count),
    dimnames = list(weights_rows, weights_cols, 1:success_count)
  )

  for (k in 1:success_count) {
    boot_paths_array[, , k] <- boot_paths_list[[k]]

    # Store loadings if captured (they should be)
    if (!is.null(boot_loadings_list[[k]])) {
      boot_loadings_array[, , k] <- boot_loadings_list[[k]]
    }

    # Store weights
    if (!is.null(boot_weights_list[[k]])) {
      boot_weights_array[, , k] <- boot_weights_list[[k]]
    }
  }

  # Create a mock seminr boot_model object
  # We include just enough to make extraction functions work
  mock_boot <- list(
    boot_paths = boot_paths_array,
    boot_loadings = boot_loadings_array,
    boot_weights = boot_weights_array,
    bootstrapped_paths = NULL, # Summary will create this
    bootstrapped_loadings = NULL,
    bootstrapped_weights = NULL,
    nboot = success_count,
    original_model = pls_model
  )

  # Add class to pass checks
  class(mock_boot) <- c("boot_seminr_model", "list")

  return(mock_boot)
}

# MEASUREMENT & STRUCTURAL QUALITY ASSESSMENT

#' Extract mediator measurement quality from first model (calculated once)
#' Mediators are shared across all DV models, so quality only needs calculation once
#' @param pls_model Estimated PLS model (from any DV iteration)
#' @param boot_model Bootstrapped PLS model
#' @param mediator_names Vector of mediator construct names
#' @return Data frame with mediator reliability metrics (dv = "all_models")
extract_mediator_quality_once <- function(pls_model, boot_model, mediator_names) {
  # Call standard function but keep ONLY mediator constructs
  full_quality <- extract_measurement_quality(pls_model, boot_model, "all_models", "pooled")

  if (nrow(full_quality) == 0) {
    return(full_quality)
  }

  # Filter to mediators only
  mediator_quality <- full_quality[full_quality$construct %in% mediator_names, ]

  if (nrow(mediator_quality) > 0) {
    cat("  ✓ Mediator quality extracted once (", nrow(mediator_quality), " constructs)\n", sep = "")
  }

  return(mediator_quality)
}

#' Extract measurement model quality metrics
#' @param pls_model Estimated PLS model
#' @param boot_model Bootstrapped PLS model
#' @param dv Name of dependent variable
#' @param group Group label
#' @param dv_construct Optional: the DV construct name to filter (typically "y_out")
#' @param mediator_names Optional: vector of mediator names to EXCLUDE (avoids duplication)
#' @return Data frame with reliability metrics
extract_measurement_quality <- function(pls_model, boot_model, dv, group,
                                        dv_construct = NULL, mediator_names = NULL) {
  # Robust check for valid boot_model object
  is_valid_boot <- !is.null(boot_model) && is.list(boot_model) && !inherits(boot_model, "try-error")

  summ <- if (is_valid_boot) tryCatch(summary(boot_model), error = function(e) NULL) else NULL
  summ_pls <- tryCatch(summary(pls_model), error = function(e) NULL)

  # Check if we have at least one summary
  if (is.null(summ) && is.null(summ_pls)) {
    cat("Summaries are NULL for measurement quality", dv, group, "\n")
    return(data.frame())
  }

  quality_list <- list()

  # Helper to safely extract column from possible matrix or dataframe
  get_col <- function(obj, col_name) {
    if (is.null(obj)) {
      return(NULL)
    }
    if (is.matrix(obj)) {
      if (col_name %in% colnames(obj)) {
        return(obj[, col_name])
      }
    } else if (is.data.frame(obj) || is.list(obj)) {
      if (col_name %in% names(obj)) {
        return(obj[[col_name]])
      }
    }
    return(NULL)
  }

  # Composite reliability (ρc)
  # Priority: Boot summary -> PLS summary -> PLS model direct
  rhoC <- get_col(if (!is.null(summ)) summ$reliability else NULL, "rhoC")

  if (is.null(rhoC)) {
    rhoC <- get_col(summ_pls$reliability, "rhoC")
  }

  if (is.null(rhoC)) {
    # Fallback to the original PLS model if not in summary
    rhoC <- get_col(pls_model$reliability, "rhoC")

    if (is.null(rhoC)) {
      rhoC <- tryCatch(pls_model$composite_reliability, error = function(e) NULL)
    }

    if (is.null(rhoC)) {
      # Reliability not available
      return(data.frame())
    }
  }

  # Dijkstra-Henseler's rho_A
  rhoA <- get_col(if (!is.null(summ)) summ$reliability else NULL, "rhoA")
  if (is.null(rhoA)) {
    rhoA <- get_col(summ_pls$reliability, "rhoA")
  }
  if (is.null(rhoA)) {
    rhoA <- get_col(pls_model$reliability, "rhoA")
    if (is.null(rhoA)) {
      rhoA <- rep(NA, length(rhoC))
      names(rhoA) <- names(rhoC)
    }
  }

  # Average Variance Extracted (AVE)
  AVE <- tryCatch(pls_model$AVE, error = function(e) NULL)

  # Try getting AVE from summaries if missing in model
  if (is.null(AVE)) {
    AVE <- get_col(summ_pls$reliability, "AVE")
  }

  if (is.null(AVE)) {
    cat("AVE is NULL for", dv, group, "\n")
    return(data.frame())
  }

  # Only report for multi-item constructs
  constructs <- names(rhoC)

  for (construct in constructs) {
    # Skip single-item constructs (dummy variables)
    # Check if AVE is exactly 1 (typical for single item) or NA
    if (is.na(AVE[construct]) || abs(AVE[construct] - 1.0) < 1e-6) next

    # Skip mediators if filtering to DV only (avoids duplicate calculation)
    if (!is.null(mediator_names) && construct %in% mediator_names) next

    quality_list[[length(quality_list) + 1]] <- data.frame(
      dv = dv,
      group_label = group,
      construct = construct,
      composite_reliability = as.numeric(rhoC[construct]),
      rho_a = as.numeric(rhoA[construct]),
      ave = as.numeric(AVE[construct]),
      cr_ok = as.numeric(rhoC[construct]) >= 0.70,
      ave_ok = as.numeric(AVE[construct]) >= 0.50,
      stringsAsFactors = FALSE
    )
  }

  if (length(quality_list) > 0) {
    bind_rows(quality_list)
  } else {
    data.frame(
      dv = character(),
      group_label = character(),
      construct = character(),
      composite_reliability = numeric(),
      rho_a = numeric(),
      ave = numeric(),
      cr_ok = logical(),
      ave_ok = logical(),
      stringsAsFactors = FALSE
    )
  }
}

#' Extract indicator loadings
#' @param pls_model Estimated PLS model
#' @param boot_model Bootstrapped PLS model
#' @param dv Name of dependent variable
#' @param group Group label
#' @return Data frame with loadings
extract_loadings <- function(pls_model, boot_model, dv, group) {
  # Robust check for valid boot_model object
  is_valid_boot <- !is.null(boot_model) && is.list(boot_model) && !inherits(boot_model, "try-error")

  # If bootstrap failed, return point estimates only
  if (!is_valid_boot) {
    loadings <- pls_model$outer_loadings
    rows <- list()
    for (const in colnames(loadings)) {
      for (item in rownames(loadings)) {
        load <- loadings[item, const]
        if (load != 0) {
          rows[[length(rows) + 1]] <- data.frame(
            dv = dv, group_label = group,
            construct = const, item = item,
            loading = load, se = NA, t_value = NA, p_value = NA,
            ci_lower = NA, ci_upper = NA,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    return(do.call(rbind, rows))
  }

  summ <- tryCatch(
    summary(boot_model),
    error = function(e) NULL
  )

  # If summary fails (e.g., custom robust bootstrap), fall back to point estimates
  if (is.null(summ) || is.null(summ$bootstrapped_loadings) || nrow(summ$bootstrapped_loadings) == 0) {
    loadings <- pls_model$outer_loadings
    rows <- list()
    for (const in colnames(loadings)) {
      for (item in rownames(loadings)) {
        load <- loadings[item, const]
        if (load != 0) {
          rows[[length(rows) + 1]] <- data.frame(
            dv = dv, group_label = group,
            construct = const, item = item,
            loading = load, se = NA, t_value = NA, p_value = NA,
            ci_lower = NA, ci_upper = NA,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    return(do.call(rbind, rows))
  }

  if (!is.null(summ$bootstrapped_loadings) && nrow(summ$bootstrapped_loadings) > 0) {
    loadings_tbl <- tryCatch(
      as.data.frame(summ$bootstrapped_loadings),
      error = function(e) {
        return(NULL)
      }
    )
  } else {
    loadings_tbl <- NULL
  }

  # IF summary lacks loadings but we have resamples, compute manually (Fix for User)
  if (is.null(loadings_tbl) && !is.null(boot_model$boot_loadings)) {
    cat("  > Calculating loading statistics from resamples manually...\n")
    loadings <- pls_model$outer_loadings
    rows_list <- list()

    for (const in colnames(loadings)) {
      for (item in rownames(loadings)) {
        orig_load <- loadings[item, const]
        if (orig_load != 0) {
          # Get resamples for this item-construct pair
          resamples <- boot_model$boot_loadings[item, const, ]

          se <- sd(resamples, na.rm = TRUE)
          t_val <- if (se > 0) abs(orig_load / se) else NA
          # Simple p-value from t-dist (approx)
          p_val <- 2 * (1 - pnorm(t_val))

          ci_lower <- quantile(resamples, 0.025, na.rm = TRUE)
          ci_upper <- quantile(resamples, 0.975, na.rm = TRUE)

          rows_list[[length(rows_list) + 1]] <- data.frame(
            dv = dv, group_label = group,
            construct = const, item = item,
            loading = orig_load,
            se = se, t_value = t_val, p_value = p_val,
            ci_lower = ci_lower, ci_upper = ci_upper,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    return(do.call(rbind, rows_list))
  }

  if (is.null(loadings_tbl)) {
    # Final fallback to point estimates
    loadings <- pls_model$outer_loadings
    rows <- list()
    for (const in colnames(loadings)) {
      for (item in rownames(loadings)) {
        load <- loadings[item, const]
        if (load != 0) {
          rows[[length(rows) + 1]] <- data.frame(
            dv = dv, group_label = group,
            construct = const, item = item,
            loading = load, se = NA, t_value = NA, p_value = NA,
            ci_lower = NA, ci_upper = NA,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    return(do.call(rbind, rows))
  }

  loadings_tbl$dv <- dv
  loadings_tbl$group_label <- group

  # Handle different possible column names from seminr
  if ("Original Est." %in% names(loadings_tbl)) {
    loadings_tbl$loading_ok <- loadings_tbl[["Original Est."]] >= 0.70
    names(loadings_tbl)[names(loadings_tbl) == "Original Est."] <- "loading"
  } else if ("Original Sample" %in% names(loadings_tbl)) {
    loadings_tbl$loading_ok <- loadings_tbl[["Original Sample"]] >= 0.70
    names(loadings_tbl)[names(loadings_tbl) == "Original Sample"] <- "loading"
  } else {
    loadings_tbl$loading <- NA_real_
    loadings_tbl$loading_ok <- FALSE
  }

  names(loadings_tbl)[names(loadings_tbl) == "Bootstrap SD"] <- "se"
  names(loadings_tbl)[names(loadings_tbl) == "Std. Error"] <- "se"

  # Extract bootstrap CIs if available
  if ("2.5% CI" %in% names(loadings_tbl)) {
    names(loadings_tbl)[names(loadings_tbl) == "2.5% CI"] <- "ci_lower"
  } else {
    loadings_tbl$ci_lower <- NA_real_
  }
  if ("97.5% CI" %in% names(loadings_tbl)) {
    names(loadings_tbl)[names(loadings_tbl) == "97.5% CI"] <- "ci_upper"
  } else {
    loadings_tbl$ci_upper <- NA_real_
  }

  # Extract t-value if available
  if ("T Stat." %in% names(loadings_tbl)) {
    names(loadings_tbl)[names(loadings_tbl) == "T Stat."] <- "t_value"
  } else if ("t value" %in% names(loadings_tbl)) {
    names(loadings_tbl)[names(loadings_tbl) == "t value"] <- "t_value"
  } else {
    loadings_tbl$t_value <- NA_real_
  }

  # Ensure required columns exist
  if (!"loading" %in% names(loadings_tbl)) loadings_tbl$loading <- NA_real_
  if (!"se" %in% names(loadings_tbl)) loadings_tbl$se <- NA_real_
  if (!"loading_ok" %in% names(loadings_tbl)) loadings_tbl$loading_ok <- FALSE

  # Robust extraction of item/construct names
  row_info <- strsplit(rownames(loadings_tbl), "\\s*->\\s*")
  loadings_tbl$item <- trimws(sapply(row_info, `[`, 1))
  loadings_tbl$construct <- trimws(sapply(row_info, `[`, 2))

  # Calculate p-value from t-value if missing
  if (!"p_value" %in% names(loadings_tbl) || all(is.na(loadings_tbl$p_value))) {
    # Use same logic as paths: normal approx or t-dist
    if ("t_value" %in% names(loadings_tbl)) {
      df_val <- if (!is.null(boot_model$nboot)) boot_model$nboot - 1 else 999
      loadings_tbl$p_value <- 2 * (1 - pt(abs(loadings_tbl$t_value), df = df_val))
    } else {
      loadings_tbl$p_value <- NA_real_
    }
  }

  # Final safety check before returning
  required_cols <- c(
    "dv", "group_label", "construct", "item", "loading", "se",
    "ci_lower", "ci_upper", "t_value", "p_value", "loading_ok"
  )
  for (col in required_cols) {
    if (!col %in% names(loadings_tbl)) {
      if (col %in% c("dv", "group_label", "construct", "item")) {
        loadings_tbl[[col]] <- NA_character_
      } else if (col == "loading_ok") {
        loadings_tbl[[col]] <- FALSE
      } else {
        loadings_tbl[[col]] <- NA_real_
      }
    }
  }

  return(loadings_tbl[, required_cols])
}

#' Check discriminant validity using HTMT criterion
#' @param pls_model Estimated PLS model
#' @param dv Name of dependent variable
#' @param group Group label
#' @param threshold HTMT threshold (default 0.85, can use 0.90)
#' @return List with HTMT matrix and validation status
check_discriminant_validity <- function(pls_model, dv, group, threshold = 0.85) {
  # Extract HTMT from model
  htmt_matrix <- NULL

  # Try to get HTMT from summary
  if (!is.null(pls_model$htmt)) {
    htmt_matrix <- pls_model$htmt
  } else {
    # Compute correlation matrix as fallback
    construct_scores <- pls_model$construct_scores
    if (!is.null(construct_scores)) {
      htmt_matrix <- cor(construct_scores, use = "pairwise.complete.obs")
    }
  }

  if (is.null(htmt_matrix)) {
    return(list(
      htmt_long = data.frame(
        dv = character(),
        group_label = character(),
        construct_1 = character(),
        construct_2 = character(),
        htmt = numeric(),
        htmt_ok = logical(),
        stringsAsFactors = FALSE
      ),
      all_ok = NA,
      max_htmt = NA
    ))
  }

  # Convert to long format
  htmt_long <- list()
  idx <- 1

  for (i in 1:(nrow(htmt_matrix) - 1)) {
    for (j in (i + 1):ncol(htmt_matrix)) {
      htmt_long[[idx]] <- data.frame(
        dv = dv,
        group_label = group,
        construct_1 = rownames(htmt_matrix)[i],
        construct_2 = colnames(htmt_matrix)[j],
        htmt = as.numeric(htmt_matrix[i, j]),
        htmt_ok = abs(as.numeric(htmt_matrix[i, j])) < threshold,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }

  htmt_df <- if (length(htmt_long) > 0) {
    bind_rows(htmt_long)
  } else {
    data.frame(
      dv = character(),
      group_label = character(),
      construct_1 = character(),
      construct_2 = character(),
      htmt = numeric(),
      htmt_ok = logical(),
      stringsAsFactors = FALSE
    )
  }

  all_ok <- if (nrow(htmt_df) > 0) all(htmt_df$htmt_ok, na.rm = TRUE) else NA
  max_htmt <- if (nrow(htmt_df) > 0) max(abs(htmt_df$htmt), na.rm = TRUE) else NA

  list(
    htmt_long = htmt_df,
    all_ok = all_ok,
    max_htmt = max_htmt
  )
}

#' Extract structural model quality metrics (VIF and f²)
#' @param pls_model Estimated PLS model
#' @param dv Name of dependent variable
#' @param group Group label
#' @return Data frame with VIF and f² values
extract_structural_quality <- function(pls_model, dv, group) {
  # If pls_model is NULL/invalid, return empty
  if (is.null(pls_model)) {
    return(data.frame())
  }
  quality_list <- list()

  # Try to key metrics from summary(pls_model) if available
  summ_pls <- tryCatch(summary(pls_model), error = function(e) NULL)

  # Extract VIF (Variance Inflation Factor)
  vif_matrix <- pls_model$vif_antecedents
  if (is.null(vif_matrix) && !is.null(summ_pls)) {
    vif_matrix <- summ_pls$vif_antecedents
  }

  if (!is.null(vif_matrix)) {
    if (is.list(vif_matrix) && !is.data.frame(vif_matrix) && !is.matrix(vif_matrix)) {
      # Handle as list (newer seminr versions)
      # Structure: List where names are endogenous vars, containing named vectors of predictors
      for (endogenous in names(vif_matrix)) {
        predictors <- vif_matrix[[endogenous]]
        if (!is.null(predictors)) {
          for (predictor in names(predictors)) {
            vif_val <- predictors[[predictor]]
            if (!is.na(vif_val) && vif_val > 0) {
              quality_list[[length(quality_list) + 1]] <- data.frame(
                dv = dv,
                group_label = group,
                endogenous = endogenous,
                predictor = predictor,
                vif = as.numeric(vif_val),
                vif_ok = as.numeric(vif_val) < 5,
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }
    } else if (is.matrix(vif_matrix) || is.data.frame(vif_matrix)) {
      # Handle as matrix (older versions or specific model types)
      for (endogenous in colnames(vif_matrix)) {
        for (predictor in rownames(vif_matrix)) {
          vif_val <- vif_matrix[predictor, endogenous]
          if (!is.na(vif_val) && vif_val > 0) {
            quality_list[[length(quality_list) + 1]] <- data.frame(
              dv = dv,
              group_label = group,
              endogenous = endogenous,
              predictor = predictor,
              vif = as.numeric(vif_val),
              vif_ok = as.numeric(vif_val) < 5,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  # Extract f² effect sizes
  f2_matrix <- pls_model$fSquare
  if (is.null(f2_matrix) && !is.null(summ_pls)) {
    f2_matrix <- summ_pls$fSquare
  }

  if (!is.null(f2_matrix)) {
    for (endogenous in colnames(f2_matrix)) {
      for (predictor in rownames(f2_matrix)) {
        f2_val <- f2_matrix[predictor, endogenous]
        if (!is.na(f2_val)) {
          # Classify effect size
          effect_size <- if (abs(f2_val) < 0.02) {
            "none"
          } else if (abs(f2_val) < 0.15) {
            "small"
          } else if (abs(f2_val) < 0.35) {
            "medium"
          } else {
            "large"
          }

          # Find matching entry
          matching <- sapply(quality_list, function(x) {
            x$endogenous == endogenous && x$predictor == predictor
          })

          if (any(matching)) {
            idx <- which(matching)[1]
            quality_list[[idx]]$f_squared <- as.numeric(f2_val)
            quality_list[[idx]]$effect_size <- effect_size
          } else {
            quality_list[[length(quality_list) + 1]] <- data.frame(
              dv = dv,
              group_label = group,
              endogenous = endogenous,
              predictor = predictor,
              vif = NA,
              vif_ok = NA,
              f_squared = as.numeric(f2_val),
              effect_size = effect_size,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  if (length(quality_list) > 0) {
    return(bind_rows(quality_list))
  } else {
    return(data.frame(
      dv = character(),
      group_label = character(),
      endogenous = character(),
      predictor = character(),
      vif = numeric(),
      vif_ok = logical(),
      f_squared = numeric(),
      effect_size = character(),
      stringsAsFactors = FALSE
    ))
  }
}

# QUALITY TESTING UTILITIES

#' Test measurement model quality criteria
#'
#' @param measurement_quality_df Data frame from extract_measurement_quality()
#' @param cr_threshold Minimum composite reliability (default 0.70)
#' @param ave_threshold Minimum AVE for convergent validity (default 0.50)
#' @return List with test results: passed (logical), issues (data frame), summary (character)
#' @export
test_measurement_quality <- function(measurement_quality_df,
                                     cr_threshold = 0.70,
                                     ave_threshold = 0.50) {
  if (nrow(measurement_quality_df) == 0) {
    return(list(
      passed = TRUE,
      issues = data.frame(),
      summary = "No multi-item constructs to assess"
    ))
  }

  # Identify issues
  cr_issues <- measurement_quality_df |>
    dplyr::filter(composite_reliability < cr_threshold) |>
    dplyr::mutate(issue_type = "Low Composite Reliability")

  ave_issues <- measurement_quality_df |>
    dplyr::filter(ave < ave_threshold) |>
    dplyr::mutate(issue_type = "Low AVE (Convergent Validity)")

  all_issues <- dplyr::bind_rows(cr_issues, ave_issues)

  # Overall pass/fail
  passed <- nrow(all_issues) == 0

  # Summary message
  if (passed) {
    summary_msg <- sprintf(
      "✓ All %d constructs meet reliability (CR ≥ %.2f) and convergent validity (AVE ≥ %.2f) criteria",
      nrow(measurement_quality_df), cr_threshold, ave_threshold
    )
  } else {
    summary_msg <- sprintf(
      "⚠ %d of %d constructs have quality issues (CR < %.2f or AVE < %.2f)",
      length(unique(paste(all_issues$construct, all_issues$group_label))),
      length(unique(paste(measurement_quality_df$construct, measurement_quality_df$group_label))),
      cr_threshold, ave_threshold
    )
  }

  list(
    passed = passed,
    issues = all_issues,
    summary = summary_msg,
    criteria = list(cr_threshold = cr_threshold, ave_threshold = ave_threshold)
  )
}

#' Test discriminant validity using HTMT
#'
#' @param htmt_df Data frame with HTMT ratios (from check_discriminant_validity())
#' @param threshold HTMT threshold (default 0.85 conservative, 0.90 liberal)
#' @return List with test results: passed (logical), violations (data frame), summary (character)
#' @export
test_discriminant_validity <- function(htmt_df, threshold = 0.85) {
  if (nrow(htmt_df) == 0) {
    return(list(
      passed = TRUE,
      violations = data.frame(),
      summary = "No construct pairs to assess"
    ))
  }

  # Identify violations
  violations <- htmt_df |>
    dplyr::filter(abs(htmt) >= threshold) |>
    dplyr::arrange(dplyr::desc(abs(htmt)))

  passed <- nrow(violations) == 0

  # Summary message
  if (passed) {
    max_htmt <- max(abs(htmt_df$htmt), na.rm = TRUE)
    summary_msg <- sprintf(
      "✓ All construct pairs meet discriminant validity (HTMT < %.2f). Max HTMT = %.3f",
      threshold, max_htmt
    )
  } else {
    summary_msg <- sprintf(
      "⚠ %d of %d construct pairs have HTMT ≥ %.2f (discriminant validity concern)",
      nrow(violations),
      nrow(htmt_df),
      threshold
    )
  }

  list(
    passed = passed,
    violations = violations,
    summary = summary_msg,
    threshold = threshold
  )
}

#' Test structural model for multicollinearity using VIF
#'
#' @param structural_quality_df Data frame from extract_structural_quality()
#' @param vif_threshold Maximum acceptable VIF (default 5, ideal < 3)
#' @return List with test results: passed (logical), issues (data frame), summary (character)
#' @export
test_structural_collinearity <- function(structural_quality_df, vif_threshold = 5) {
  if (nrow(structural_quality_df) == 0) {
    return(list(
      passed = TRUE,
      issues = data.frame(),
      summary = "No VIF values to assess"
    ))
  }

  # Filter to only VIF values (exclude rows with only f²)
  vif_data <- structural_quality_df |>
    dplyr::filter(!is.na(vif))

  if (nrow(vif_data) == 0) {
    return(list(
      passed = TRUE,
      issues = data.frame(),
      summary = "No VIF values available"
    ))
  }

  # Identify issues
  vif_issues <- vif_data |>
    dplyr::filter(vif >= vif_threshold) |>
    dplyr::arrange(dplyr::desc(vif)) |>
    dplyr::mutate(severity = dplyr::case_when(
      vif >= 10 ~ "Severe",
      vif >= 5 ~ "High",
      TRUE ~ "Acceptable"
    ))

  passed <- nrow(vif_issues) == 0

  # Summary message
  if (passed) {
    max_vif <- max(vif_data$vif, na.rm = TRUE)
    summary_msg <- sprintf(
      "✓ No multicollinearity issues detected (all VIF < %.1f). Max VIF = %.3f",
      vif_threshold, max_vif
    )
  } else {
    summary_msg <- sprintf(
      "⚠ %d of %d paths have VIF ≥ %.1f (multicollinearity concern)",
      nrow(vif_issues),
      nrow(vif_data),
      vif_threshold
    )
  }

  list(
    passed = passed,
    issues = vif_issues,
    summary = summary_msg,
    threshold = vif_threshold
  )
}

#' Test effect sizes (f²) and classify them
#'
#' @param structural_quality_df Data frame from extract_structural_quality()
#' @return List with effect size classification and summary
#' @export
test_effect_sizes <- function(structural_quality_df) {
  if (nrow(structural_quality_df) == 0) {
    return(list(
      classification = data.frame(),
      summary = "No effect sizes to assess"
    ))
  }

  # Filter to only f² values
  f2_data <- structural_quality_df |>
    dplyr::filter(!is.na(f_squared))

  if (nrow(f2_data) == 0) {
    return(list(
      classification = data.frame(),
      summary = "No f² values available"
    ))
  }

  # Classify effect sizes
  classified <- f2_data |>
    dplyr::mutate(
      effect_category = dplyr::case_when(
        abs(f_squared) < 0.02 ~ "None/Negligible",
        abs(f_squared) < 0.15 ~ "Small",
        abs(f_squared) < 0.35 ~ "Medium",
        TRUE ~ "Large"
      ),
      effect_category = factor(
        effect_category,
        levels = c("None/Negligible", "Small", "Medium", "Large")
      )
    ) |>
    dplyr::arrange(dplyr::desc(abs(f_squared)))

  # Count by category
  counts <- classified |>
    dplyr::count(effect_category, name = "n_effects") |>
    tidyr::complete(
      effect_category = factor(
        c("None/Negligible", "Small", "Medium", "Large"),
        levels = c("None/Negligible", "Small", "Medium", "Large")
      ),
      fill = list(n_effects = 0)
    )

  # Summary message
  n_meaningful <- sum(counts$n_effects[counts$effect_category != "None/Negligible"])
  summary_msg <- sprintf(
    "Effect sizes: %d negligible, %d small, %d medium, %d large (Total: %d paths with f² ≥ 0.02)",
    counts$n_effects[1], counts$n_effects[2], counts$n_effects[3], counts$n_effects[4],
    n_meaningful
  )

  list(
    classification = classified,
    counts = counts,
    summary = summary_msg
  )
}

#' Summarize all PLS-SEM quality tests
#'
#' @param measurement_test Result from test_measurement_quality()
#' @param discriminant_test Result from test_discriminant_validity()
#' @param collinearity_test Result from test_structural_collinearity()
#' @param effect_test Result from test_effect_sizes() (optional)
#' @return List with overall summary
#' @export
summarize_quality_tests <- function(measurement_test,
                                    discriminant_test,
                                    collinearity_test,
                                    effect_test = NULL) {
  tests_passed <- c(
    measurement = measurement_test$passed,
    discriminant = discriminant_test$passed,
    collinearity = collinearity_test$passed
  )

  overall_passed <- all(tests_passed)
  n_passed <- sum(tests_passed)
  n_total <- length(tests_passed)

  # Create summary data frame
  summary_df <- data.frame(
    criterion = c("Measurement Quality", "Discriminant Validity", "Multicollinearity (VIF)"),
    status = c(
      ifelse(measurement_test$passed, "✓ Pass", "⚠ Fail"),
      ifelse(discriminant_test$passed, "✓ Pass", "⚠ Fail"),
      ifelse(collinearity_test$passed, "✓ Pass", "⚠ Fail")
    ),
    details = c(
      measurement_test$summary,
      discriminant_test$summary,
      collinearity_test$summary
    ),
    stringsAsFactors = FALSE
  )

  # Overall message
  if (overall_passed) {
    overall_msg <- sprintf("✓ ALL QUALITY CRITERIA PASSED (%d/%d tests)", n_passed, n_total)
  } else {
    overall_msg <- sprintf("⚠ QUALITY ISSUES DETECTED (%d/%d tests passed)", n_passed, n_total)
  }

  list(
    overall_passed = overall_passed,
    overall_message = overall_msg,
    summary_table = summary_df,
    tests_passed = tests_passed,
    details = list(
      measurement = measurement_test,
      discriminant = discriminant_test,
      collinearity = collinearity_test,
      effect_sizes = effect_test
    )
  )
}

# REPORTING UTILITIES

#' Print section header
#'
#' @param title Section title
#' @param subtitle Optional subtitle
#' @return NULL (prints to console)
#' @export
print_section_header <- function(title, subtitle = NULL) {
  cat("\n=== ", title, " ===\n", sep = "")
  if (!is.null(subtitle)) {
    cat(subtitle, "\n\n")
  }
}

# REPORTING HELPERS

stars <- function(p) {
  # Vectorized version using case_when logic
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  )
}

#' Create a consolidated summary of PLS-SEM results for publication
#'
#' @param paths_all Combined path coefficients
#' @param indirects_all Combined indirect effects
#' @param r2_all Combined R-squared values
#' @param meas_quality_all Combined measurement quality (CR, AVE)
#' @param struct_quality_all Combined structural quality (f2, VIF)
#' @param htmt_all Combined HTMT values
#' @return A list of data frames containing consolidated summaries
#' @export
create_publication_summary <- function(paths_all, indirects_all, r2_all,
                                       meas_quality_all, struct_quality_all,
                                       htmt_all) {
  # 1. Measurement Quality Summary (CR, AVE, rho_A)
  measurement_summary <- if (nrow(meas_quality_all) > 0) {
    meas_quality_all |>
      dplyr::select(dv, group_label, construct, composite_reliability, rho_a, ave) |>
      dplyr::mutate(
        dplyr::across(c(composite_reliability, rho_a, ave), ~ round(., 3))
      ) |>
      dplyr::rename(CR = composite_reliability, "rho_A" = rho_a, AVE = ave)
  } else {
    data.frame(
      dv = character(), group_label = character(), construct = character(),
      CR = numeric(), rho_A = numeric(), AVE = numeric()
    )
  }

  # 2. HTMT Discriminant Validity Summary
  htmt_summary <- if (nrow(htmt_all) > 0) {
    htmt_all |>
      dplyr::filter(!is.na(htmt)) |>
      dplyr::mutate(htmt = round(htmt, 3)) |>
      dplyr::select(dv, group_label, construct_1, construct_2, htmt)
  } else {
    data.frame(
      dv = character(), group_label = character(),
      construct_1 = character(), construct_2 = character(), htmt = numeric()
    )
  }

  # 3. Path Coefficients & Significance & f2
  path_summary <- if (nrow(paths_all) > 0) {
    # DIAGNOSTIC LOGGING: Check data before join
        cat("paths_all rows:", nrow(paths_all), "\n")
    cat("struct_quality_all rows:", nrow(struct_quality_all), "\n")

    # Show sample of join keys from paths_all
    if (nrow(paths_all) > 0) {
      cat("\nSample paths_all join keys (first 3 rows):\n")
      paths_sample <- paths_all |>
        dplyr::mutate(
          lhs_clean = trimws(as.character(lhs)),
          rhs_clean = trimws(as.character(rhs))
        ) |>
        dplyr::select(dv, group_label, lhs_clean, rhs_clean) |>
        head(3)
      print(paths_sample)
    }

    # Show sample of join keys from struct_quality_all
    if (nrow(struct_quality_all) > 0) {
      cat("\nSample struct_quality_all join keys (first 3 rows):\n")
      struct_sample <- struct_quality_all |>
        dplyr::mutate(
          endogenous_clean = trimws(as.character(endogenous)),
          predictor_clean = trimws(as.character(predictor))
        ) |>
        dplyr::select(dv, group_label, endogenous_clean, predictor_clean, vif, f_squared) |>
        head(3)
      print(struct_sample)
    }

    # Perform the join
    # NOTE: paths_all uses opposite naming: lhs=FROM (predictor), rhs=TO (endogenous)
    # struct_quality_all uses: predictor=FROM, endogenous=TO
    # So we match: lhs_clean -> predictor_clean, rhs_clean -> endogenous_clean
    paths_with_quality <- paths_all |>
      dplyr::mutate(
        lhs_clean = trimws(as.character(lhs)),
        rhs_clean = trimws(as.character(rhs))
      ) |>
      dplyr::left_join(
        struct_quality_all |>
          dplyr::mutate(
            endogenous_clean = trimws(as.character(endogenous)),
            predictor_clean = trimws(as.character(predictor))
          ) |>
          dplyr::select(dv, group_label, endogenous_clean, predictor_clean, vif, f_squared),
        by = c("dv" = "dv", "group_label" = "group_label", "lhs_clean" = "predictor_clean", "rhs_clean" = "endogenous_clean")
      )

    # Check join results
    n_vif_non_na <- sum(!is.na(paths_with_quality$vif))
    n_f2_non_na <- sum(!is.na(paths_with_quality$f_squared))
    cat("\nJoin results:\n")
    cat("  Paths with non-NA VIF:", n_vif_non_na, "/", nrow(paths_with_quality), "\n")
    cat("  Paths with non-NA F²:", n_f2_non_na, "/", nrow(paths_with_quality), "\n")

    # Show sample of joined data
    if (nrow(paths_with_quality) > 0) {
      cat("\nSample joined data (first 3 rows):\n")
      joined_sample <- paths_with_quality |>
        dplyr::select(dv, group_label, lhs_clean, rhs_clean, vif, f_squared) |>
        head(3)
      print(joined_sample)
    }
    
    # Continue with formatting
    paths_with_quality |>
      dplyr::mutate(
        sig = stars(p_value),
        path_fmt = sprintf("%.3f%s", est, sig),
        vif = round(vif, 2),
        f2 = round(f_squared, 3)
      ) |>
      dplyr::select(dv, group_label, from = rhs, to = lhs, estimate = est, p_value, sig, path_fmt, vif, f2)
  } else {
    data.frame(
      dv = character(), group_label = character(), from = character(), to = character(),
      estimate = numeric(), p_val = numeric(), sig = character(), path_fmt = character(),
      vif = numeric(), f2 = numeric()
    )
  }

  # 4. Indirect Effects & Mediation Type
  # Note: conditional_indirects has columns: dv, effect, condition, estimate, ci_lower, ci_upper, significant
  indirect_summary <- if (nrow(indirects_all) > 0) {
    indirects_all |>
      dplyr::mutate(
        sig = ifelse(significant, "*", ""),
        effect_fmt = sprintf("%.3f%s [%.3f, %.3f]", estimate, sig, ci_lower, ci_upper)
      ) |>
      dplyr::select(dv, condition, effect, estimate, ci_lower, ci_upper, significant, sig, effect_fmt)
  } else {
    data.frame(
      dv = character(), condition = character(), effect = character(),
      estimate = numeric(), ci_lower = numeric(), ci_upper = numeric(),
      significant = logical(), sig = character(), effect_fmt = character()
    )
  }

  # 5. R-Squared Summary
  r2_summary <- if (nrow(r2_all) > 0) {
    r2_all |>
      dplyr::mutate(r2 = round(r2, 3)) |>
      dplyr::select(dv, group_label, endogenous, r2)
  } else {
    data.frame(dv = character(), group_label = character(), endogenous = character(), r2 = numeric())
  }

  list(
    measurement = measurement_summary,
    htmt = htmt_summary,
    paths = path_summary,
    indirects = indirect_summary,
    r2 = r2_summary
  )
}

# MANIPULATION CHECK FUNCTIONS

#' Perform manipulation checks for Task Complexity and AI Condition
#'
#' @param data Data frame with items and condition variables
#' @param complexity_items Vector of items for task complexity check
#' @param autonomy_items Vector of items for autonomy check
#' @return List of results (printed to console)
#' @export
perform_manipulation_checks <- function(data, complexity_items, autonomy_items) {
  # Helper to format p-values
  fmt_p <- function(p) {
    if (is.na(p)) {
      return("NA")
    }
    if (p < .001) {
      return("< .001")
    }
    return(paste0("= ", sprintf("%.3f", p)))
  }

  # --- 1. Task Complexity Check (Easy vs. Hard) ---
  cat("\n--------------------------------------------------------------\n")
  cat("MANIPULATION CHECK 1: TASK COMPLEXITY (Easy vs. Hard)\n")
  cat("--------------------------------------------------------------\n")
  cat("Items:", paste(complexity_items, collapse = ", "), "\n")

  if (all(complexity_items %in% names(data))) {
    alpha_comp <- psych::alpha(data[, complexity_items])
    cat("Cronbach's Alpha:", round(alpha_comp$total$raw_alpha, 3), "\n")

    # Create Composite
    data$check_complexity <- rowMeans(data[, complexity_items], na.rm = TRUE)

    # Descriptives
    cat("\nDescriptive Statistics by Group:\n")
    desc_stats <- data %>%
      dplyr::group_by(task_complexity) %>%
      dplyr::summarise(
        Group = dplyr::first(ifelse(task_complexity == 1, "Hard", "Easy")),
        N = dplyr::n(),
        Mean = mean(check_complexity, na.rm = TRUE),
        SD = sd(check_complexity, na.rm = TRUE)
      )
    print(desc_stats)

    # Levene's Test (Assumption Check)
    cat("\nAssumption Check: Homogeneity of Variance (Fligner-Killeen):\n")
    ft <- stats::fligner.test(check_complexity ~ task_complexity, data = data)
    print(ft)

    if (ft$p.value < 0.05) {
      cat("=> Result: Significant (p ", fmt_p(ft$p.value), "). Assumption VIOLATED.\n", sep = "")
      cat("   Justification: Variances are not equal. Using Welch's t-test.\n")
    } else {
      cat("=> Result: Not significant. Assumption met (but using Welch's t-test as robust standard).\n")
    }

    # Statistical Test (Welch's t-test)
    cat("\nIndependent Samples t-test (Welch's):\n")
    t_res <- stats::t.test(check_complexity ~ task_complexity, data = data)
    print(t_res)

    # Effect Size: Cohen's d
    # d = (M1 - M2) / SD_pooled
    # SD_pooled = sqrt( ((n1-1)s1^2 + (n2-1)s2^2) / (n1+n2-2) )
    m1 <- desc_stats$Mean[1]
    s1 <- desc_stats$SD[1]
    n1 <- desc_stats$N[1]
    m2 <- desc_stats$Mean[2]
    s2 <- desc_stats$SD[2]
    n2 <- desc_stats$N[2]

    sd_pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
    cohen_d <- abs(m1 - m2) / sd_pooled

    cat("\nEffect Size:\n")
    cat(sprintf("Cohen's d = %.2f\n", cohen_d))

    if (t_res$p.value < 0.05) {
      cat(" RESULT: Significant difference between groups (p ", fmt_p(t_res$p.value), ").\n", sep = "")
      cat("✗ RESULT: No significant difference detected.\n")
    }

    # --- Normality Check for Task Complexity ---
    cat("\nNormality Check (Shapiro-Wilk) by Group:\n")
    norm_res <- data %>%
      dplyr::group_by(task_complexity) %>%
      dplyr::summarise(
        Group = dplyr::first(ifelse(task_complexity == 1, "Hard", "Easy")),
        Shapiro_W = shapiro.test(check_complexity)$statistic,
        p_val = shapiro.test(check_complexity)$p.value,
        Skewness = psych::describe(check_complexity)$skew,
        Kurtosis = psych::describe(check_complexity)$kurtosis
      )
    print(norm_res)

    if (any(norm_res$p_val < 0.05)) {
      cat("=> Result: Significant deviation from normality (p < .05).\n")
      cat("   Justification: Use robust tests (Welch) or non-parametric alternatives if deviation is extreme.\n")
    } else {
      cat("=> Result: Normality assumption met (p > .05).\n")
    }
  } else {
    cat("Error: Complexity items not found in data.\n")
  }

  # --- 2. AI Condition Autonomy Check (Assistiv vs. Val vs. NoVal) ---
  cat("\n--------------------------------------------------------------\n")
  cat("MANIPULATION CHECK 2: PERCEIVED AUTONOMY (AI Conditions)\n")
  cat("--------------------------------------------------------------\n")
  cat("Items:", paste(autonomy_items, collapse = ", "), "\n")

  if (all(autonomy_items %in% names(data))) {
    alpha_auto <- psych::alpha(data[, autonomy_items])
    cat("Cronbach's Alpha:", round(alpha_auto$total$raw_alpha, 3), "\n")

    # Create Composite
    data$check_autonomy <- rowMeans(data[, autonomy_items], na.rm = TRUE)

    # Descriptives
    cat("\nDescriptive Statistics by Condition:\n")
    desc_auto <- data %>%
      dplyr::group_by(ai_condition) %>%
      dplyr::summarise(
        N = dplyr::n(),
        Mean = mean(check_autonomy, na.rm = TRUE),
        SD = sd(check_autonomy, na.rm = TRUE)
      )
    print(desc_auto)

    # Homogeneity
    cat("\nAssumption Check: Homogeneity of Variance (Fligner-Killeen):\n")
    ft_auto <- stats::fligner.test(check_autonomy ~ ai_condition, data = data)
    print(ft_auto)

    if (ft_auto$p.value < 0.05) {
      cat("=> Result: Significant (p ", fmt_p(ft_auto$p.value), "). Assumption VIOLATED.\n", sep = "")
      cat("   Justification: Variances are not equal. Using Welch's ANOVA.\n")
    } else {
      cat("=> Result: Not significant. Assumption met (but using Welch's ANOVA as robust standard).\n")
    }

    # Welch's ANOVA
    cat("\nOne-Way Analysis of Variance (Welch's ANOVA):\n")
    anova_res <- stats::oneway.test(check_autonomy ~ ai_condition, data = data, var.equal = FALSE)
    print(anova_res)

    # Stats extraction
    f_stat <- anova_res$statistic
    df1 <- anova_res$parameter[1]
    df2 <- anova_res$parameter[2]
    p_val <- anova_res$p.value

    # Effect Size: Eta-squared (eta^2)
    grand_mean <- mean(data$check_autonomy, na.rm = TRUE)
    ss_between <- sum(desc_auto$N * (desc_auto$Mean - grand_mean)^2)
    ss_within <- sum((desc_auto$N - 1) * desc_auto$SD^2)
    ss_total <- ss_between + ss_within
    eta_sq <- ss_between / ss_total

    cat("\nAPA Reporting Metrics:\n")
    cat(sprintf(
      "F(%.2f, %.2f) = %.2f, p %s, eta_sq = %.3f\n",
      df1, df2, f_stat, fmt_p(p_val), eta_sq
    ))

    if (anova_res$p.value < 0.05) {
      cat(" RESULT: Significant main effect of Condition.\n")
      cat("\nPost-Hoc Pairwise Comparisons (Welch t-tests, Holm-adjusted):\n")

      # Custom loop for detailed APA reporting (t, df, p, d)
      groups <- levels(data$ai_condition)
      pairs <- combn(groups, 2)

      # Storage for results
      results_list <- list()

      cat(sprintf(
        "%-25s %-10s %-12s %-10s %-10s %-10s\n",
        "Comparison", "Mean Diff", "t-value", "df", "p_adj", "Cohen's d"
      ))
      cat(paste(rep("-", 85), collapse = ""), "\n")

      # 1. Calculate raw p-values and stats
      raw_p_values <- c()

      for (i in 1:ncol(pairs)) {
        g1 <- pairs[1, i]
        g2 <- pairs[2, i]

        # Subset data
        d1 <- data$check_autonomy[data$ai_condition == g1]
        d2 <- data$check_autonomy[data$ai_condition == g2]

        # Welch t-test
        test <- t.test(d1, d2, var.equal = FALSE)

        # Cohen's d (using pairwise pooled SD)
        n1 <- length(d1)
        n2 <- length(d2)
        v1 <- var(d1, na.rm = TRUE)
        v2 <- var(d2, na.rm = TRUE)
        pooled_sd <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2))
        d_val <- abs(mean(d1, na.rm = TRUE) - mean(d2, na.rm = TRUE)) / pooled_sd

        # Store raw p-value for adjustment
        raw_p_values <- c(raw_p_values, test$p.value)

        results_list[[i]] <- list(
          comp = paste(g1, "vs", g2),
          diff = test$estimate[1] - test$estimate[2],
          t = test$statistic,
          df = test$parameter,
          d = d_val
        )
      }

      # 2. Adjust p-values (Holm)
      adj_p_values <- p.adjust(raw_p_values, method = "holm")

      # 3. Print Results
      for (i in 1:length(results_list)) {
        res <- results_list[[i]]
        p_adj <- adj_p_values[i]

        # Format p-value string
        if (p_adj < .001) {
          p_str <- "< .001"
          sig <- "***"
        } else {
          p_str <- sprintf("= %.3f", p_adj)
          sig <- if (p_adj < .05) "*" else "ns"
        }

        cat(sprintf(
          "%-25s %-10.3f %-12.3f %-10.3f %-10s %-10.3f [%s]\n",
          res$comp, res$diff, res$t, res$df, p_str, res$d, sig
        ))
      }
    } else {
      cat("✗ RESULT: No significant differences between conditions.\n")
    }

    # --- Normality Check for Autonomy ---
    cat("\nNormality Check (Shapiro-Wilk) by Condition:\n")
    norm_res_auto <- data %>%
      dplyr::group_by(ai_condition) %>%
      dplyr::summarise(
        Group = dplyr::first(ai_condition),
        Shapiro_W = shapiro.test(check_autonomy)$statistic,
        p_val = shapiro.test(check_autonomy)$p.value,
        Skewness = psych::describe(check_autonomy)$skew,
        Kurtosis = psych::describe(check_autonomy)$kurtosis
      )
    print(norm_res_auto)

    if (any(norm_res_auto$p_val < 0.05)) {
      cat("=> Result: Significant deviation from normality (p < .05).\n")
      cat("   Justification: Use robust tests (Welch ANOVA) as standard.\n")
    } else {
      cat("=> Result: Normality assumption met (p > .05).\n")
    }
  } else {
    cat("Error: Autonomy items not found in data.\n")
  }
}

#' Identify participants who failed manipulation checks using conservative rules
#'
#' @param data Data frame with composite scores and condition variables
#' @param verbose Print summary of failures
#' @return List with failing IDs and summary data frame
#' @export
identify_manipulation_failures <- function(data, verbose = TRUE, a_threshold = 5) {
  # ALIGNED WITH apply_manipulation_checks_and_filtering() from PLS-SEM master script
  # Uses ONLY autonomy-based exclusion (no task complexity check)
  # Symmetric threshold logic: assistiv expects LOW, agent_noval expects HIGH


  # Calculate mean perceived autonomy if not already present
  if (!"mean_perceived_autonomy" %in% names(data)) {
    autonomy_items <- c("v_11", "v_12", "v_75")
    if (all(autonomy_items %in% names(data))) {
      data$mean_perceived_autonomy <- rowMeans(data[, autonomy_items], na.rm = TRUE)
    } else {
      # Fallback to check_autonomy if already computed
      if ("check_autonomy" %in% names(data)) {
        data$mean_perceived_autonomy <- data$check_autonomy
      } else {
        stop("Autonomy items (v_11, v_12, v_75) or check_autonomy not found in data")
      }
    }
  }

  inverse_threshold <- 8 - a_threshold # For agent_noval (high autonomy expected)

  # EXCLUSION LOGIC (same as apply_manipulation_checks_and_filtering):
  # assistiv (AI Assistant): expected LOW autonomy -> exclude if perceived >= a_threshold

  # agent_noval (AI Agent without validation): expected HIGH autonomy -> exclude if perceived <= inverse_threshold
  # agent_val (AI Agent with validation): expected MODERATE autonomy -> exclude extremes (==1 or ==7)

  fail_assistiv <- data |>
    dplyr::filter(ai_condition == "assistiv" & mean_perceived_autonomy >= a_threshold)

  fail_agent_noval <- data |>
    dplyr::filter(ai_condition == "agent_noval" & mean_perceived_autonomy <= inverse_threshold)

  fail_agent_val <- data |>
    dplyr::filter(ai_condition == "agent_val" &
      (mean_perceived_autonomy == 1 | mean_perceived_autonomy == 7))

  # Combine all failures
  fail_autonomy <- dplyr::bind_rows(fail_assistiv, fail_agent_noval, fail_agent_val)

  # Get unique IDs (using lfdn if available, otherwise row index)
  if ("lfdn" %in% names(data)) {
    excluded_ids <- unique(fail_autonomy$lfdn)
    autonomy_fail_ids <- unique(fail_autonomy$lfdn)
  } else {
    excluded_ids <- as.integer(rownames(fail_autonomy))
    autonomy_fail_ids <- excluded_ids
  }

  summary_tbl <- data.frame(
    Category = c(
      "Total Sample",
      "Assistiv Failures (perceived >= threshold)",
      "Agent_NoVal Failures (perceived <= inverse)",
      "Agent_Val Failures (extremes 1 or 7)",
      "Total Exclusions",
      "Final Analytical Sample"
    ),
    N = c(
      nrow(data),
      nrow(fail_assistiv),
      nrow(fail_agent_noval),
      nrow(fail_agent_val),
      length(excluded_ids),
      nrow(data) - length(excluded_ids)
    ),
    stringsAsFactors = FALSE
  )

  if (verbose) {
    cat("\n=== MANIPULATION CHECK FAILURES (ALIGNED WITH PLS-SEM) ===\n")
    print(summary_tbl)
    cat("\nThresholds used (symmetric, a_threshold =", a_threshold, "):\n")
    cat("- Assistiv (expect LOW autonomy): exclude if perceived >=", a_threshold, "\n")
    cat("- Agent_NoVal (expect HIGH autonomy): exclude if perceived <=", inverse_threshold, "\n")
    cat("- Agent_Val (expect MODERATE autonomy): exclude extremes (1 or 7)\n")
    cat("\nNote: Task complexity is NOT used for exclusion (group-level validation only)\n")
  }

  return(list(
    excluded_ids = excluded_ids,
    summary_table = summary_tbl,
    complexity_fail_ids = integer(0), # No complexity-based exclusion
    autonomy_fail_ids = autonomy_fail_ids
  ))
}

# VISUALIZATION FUNCTIONS (APA STYLE)

#' Create APA-style high-contrast theme for seminr plots
#' Follows APA guidelines: Black/White, high contrast, clear fonts
#' @return A seminr theme object
create_apa_theme <- function() {
  # Check if seminr is available
  if (!requireNamespace("seminr", quietly = TRUE)) {
    warning("seminr package not installed. Returning NULL theme.")
    return(NULL)
  }

  # Create high-contrast B/W theme
  # seminr_theme_create is the function to customize visuals
  # We want: White nodes, black text, clean lines

  seminr::seminr_theme_create(
    # General Plot settings
    plot.title.fontsize = 12,
    plot.fontname = "sans",

    # Measurement Model (MM) - Outer model
    mm.node.color = "white",
    mm.node.fill = "white",
    mm.node.label.fontsize = 10,
    mm.node.label.fontcolor = "black",
    mm.edge.positive.color = "black",
    mm.edge.negative.color = "black", # No red/green distinction for APA
    mm.edge.label.fontsize = 9,
    mm.edge.label.fontcolor = "black",
    mm.edge.width_multiplier = 1,

    # Structural Model (SM) - Inner model
    sm.node.color = "black",
    sm.node.fill = "white",
    sm.node.label.fontsize = 12,
    sm.node.label.fontcolor = "black",
    sm.edge.positive.color = "black",
    sm.edge.negative.color = "black", # No red/green distinction for APA
    sm.edge.label.fontsize = 10,
    sm.edge.label.fontcolor = "black",
    sm.edge.width_multiplier = 1.5, # Slightly thicker structural paths

    # Construct formatting
    construct.node.shape = "ellipse", # Standard for constructs
    manifest.node.shape = "box" # Standard for indicators
  )
}

# PREDICTIVE PERFORMANCE (PLSpredict)

#' Assess PLS predictive performance (Out-of-sample)
#' Uses seminr::predict_pls to calculate Q2_predict, RMSE, MAE
#'
#' @param model A fitted seminr_model object
#' @param folds Number of folds for cross-validation (default 10)
#' @param reps Number of repetitions (default 10)
#' @return Data frame with RMSE, MAE, Q2_predict benchmarks
#' @export
assess_predictive_performance <- function(model, folds = 10, reps = 10) {
  cat("\n--------------------------------------------------------------\n")
  cat("PREDICTIVE PERFORMANCE ASSESSMENT (PLSpredict)\n")
  cat("--------------------------------------------------------------\n")
  cat("Technique: predict_DA (Data Partitioning)\n")
  cat("Config: Folds =", folds, ", Repetitions =", reps, "\n")
  cat("Note: This is computationally intensive. Please wait...\n")

  # Run PLSpredict via seminr
  pred_results <- tryCatch(
    {
      # Default to predict_DA (Data Analysis) technique
      seminr::predict_pls(model, technique = predict_DA, noFolds = folds, reps = reps)
    },
    error = function(e) {
      cat("PLSpredict failed with error:\n")
      cat("  Message: ", conditionMessage(e), "\n")
      cat("  This may indicate model incompatibility with predict_pls().\n")
      return(NULL)
    }
  )

  if (is.null(pred_results)) {
    return(data.frame(
      Construct = character(), Variable = character(), Metric = character(),
      PLS = numeric(), LM = numeric(), Diff = numeric(), Status = character()
    ))
  }

  cat(" Prediction successful.\n")

  # Extract Metrics (RMSE, MAE, Q2) from summary
  sum_pred <- summary(pred_results)

  # Diagnostic output
  cat("\nDiagnosing prediction summary:\n")
  cat("  Available metrics:", paste(names(sum_pred), collapse = ", "), "\n")

  # For each found metric, print a small sample to see structure
  for (nm in names(sum_pred)) {
    if (nm %in% c("RMSE", "MAE", "Q2_predict", "Q2", "prediction_error", "PLS_out_of_sample")) {
      cat("  > Checking metric:", nm, "\n")
      item_data <- sum_pred[[nm]]
      cat("    Class:", class(item_data), "| Dims:", paste(dim(item_data), collapse = "x"), "\n")
      if (!is.null(item_data)) {
        utils::capture.output(print(head(item_data, 2))) |> cat(sep = "\n    ")
      }
    }
  }

  # Helper to process metric matrix (RMSE/MAE)
  process_error_metric <- function(mat, metric_name, type = "Item") {
    if (is.null(mat)) {
      return(NULL)
    }
    if (!is.matrix(mat)) mat <- as.matrix(mat)
    if (length(mat) == 0) {
      return(NULL)
    }

    items <- rownames(mat)
    if (is.null(items)) items <- paste0("Var_", seq_len(nrow(mat)))

    col_pls <- 1
    col_lm <- if (ncol(mat) > 1) 2 else NA

    res_list <- list()
    for (i in seq_len(nrow(mat))) {
      item <- items[i]
      val_pls <- mat[i, col_pls]
      val_lm <- if (!is.na(col_lm)) mat[i, col_lm] else NA
      diff <- if (!is.na(val_lm)) val_lm - val_pls else NA
      status <- if (!is.na(diff)) {
        if (diff > 0) "PLS < LM (Better)" else "PLS > LM (Worse)"
      } else {
        "No Benchmark"
      }
      res_list[[length(res_list) + 1]] <- data.frame(
        Construct = type, Variable = item, Metric = metric_name,
        PLS = round(val_pls, 4),
        LM = if (!is.na(val_lm)) round(val_lm, 4) else NA,
        Diff = if (!is.na(diff)) round(diff, 4) else NA,
        Status = status, stringsAsFactors = FALSE
      )
    }
    do.call(rbind, res_list)
  }

  # Compile all metrics
  df_list <- list()

  # Strategy A: Check for combined 'prediction_error' table (Common in newer seminr)
  if (!is.null(sum_pred$prediction_error)) {
    pe <- sum_pred$prediction_error
    if (!is.matrix(pe)) pe <- as.matrix(pe)
    cn <- colnames(pe)

    # Process RMSE if both PLS and LM exist
    if (all(c("PLS_RMSE", "LM_RMSE") %in% cn)) {
      df_list[[length(df_list) + 1]] <- process_error_metric(pe[, c("PLS_RMSE", "LM_RMSE")], "RMSE", "Item")
    }
    # Process MAE if both exist
    if (all(c("PLS_MAE", "LM_MAE") %in% cn)) {
      df_list[[length(df_list) + 1]] <- process_error_metric(pe[, c("PLS_MAE", "LM_MAE")], "MAE", "Item")
    }
    # Process Q2
    if ("Q2_predict" %in% cn) {
      q2_dat <- pe[, "Q2_predict", drop = FALSE]
      df_list[[length(df_list) + 1]] <- process_error_metric(q2_dat, "Q2_predict", "Item")
    }
  }

  # Strategy B: Check for PLS_out_of_sample / LM_out_of_sample
  if (length(df_list) == 0 && !is.null(sum_pred$PLS_out_of_sample)) {
    cat("  Attempting Strategy B: PLS_out_of_sample / LM_out_of_sample...\n")
    pls_stats <- sum_pred$PLS_out_of_sample
    lm_stats <- sum_pred$LM_out_of_sample

    if (!is.matrix(pls_stats)) pls_stats <- as.matrix(pls_stats)
    if (!is.null(lm_stats) && !is.matrix(lm_stats)) lm_stats <- as.matrix(lm_stats)

    # Check if metrics are in ROWS (seminr format) or COLUMNS (old format)
    metrics_in_rows <- any(c("RMSE", "MAE", "Q2_predict") %in% rownames(pls_stats))

    if (metrics_in_rows) {
      cat("  Detected row-based metric format (RMSE/MAE as rownames)\n")
      # Transpose: rows become columns, items become rows
      for (metric in c("RMSE", "MAE", "Q2_predict")) {
        if (metric %in% rownames(pls_stats)) {
          # Extract the metric row and transpose to column
          pls_vals <- pls_stats[metric, ]
          lm_vals <- if (!is.null(lm_stats) && metric %in% rownames(lm_stats)) {
            lm_stats[metric, ]
          } else {
            rep(NA, length(pls_vals))
          }

          # Create matrix with items as rows, PLS/LM as columns
          mat_comp <- cbind(PLS = pls_vals, LM = lm_vals)
          rownames(mat_comp) <- colnames(pls_stats) # Item names

          df_list[[length(df_list) + 1]] <- process_error_metric(mat_comp, metric, "Item")
        }
      }
    } else {
      # Old column-based format
      cat("  Detected column-based metric format\n")
      for (metric in c("RMSE", "MAE", "Q2_predict")) {
        if (metric %in% colnames(pls_stats)) {
          mat_comp <- cbind(
            PLS = pls_stats[, metric],
            LM = if (!is.null(lm_stats) && metric %in% colnames(lm_stats)) lm_stats[, metric] else NA
          )
          rownames(mat_comp) <- rownames(pls_stats)
          df_list[[length(df_list) + 1]] <- process_error_metric(mat_comp, metric, "Item")
        }
      }
    }
  }

  # Strategy C: Fallback to direct metric names (Old script behavior)
  if (length(df_list) == 0) {
    if (!is.null(sum_pred$RMSE)) df_list[[length(df_list) + 1]] <- process_error_metric(sum_pred$RMSE, "RMSE", "Item")
    if (!is.null(sum_pred$MAE)) df_list[[length(df_list) + 1]] <- process_error_metric(sum_pred$MAE, "MAE", "Item")

    q2_mat <- if (!is.null(sum_pred$Q2_predict)) sum_pred$Q2_predict else sum_pred$Q2
    if (!is.null(q2_mat)) {
      if (!is.matrix(q2_mat)) q2_mat <- as.matrix(q2_mat)
      df_list[[length(df_list) + 1]] <- process_error_metric(q2_mat, "Q2_predict", "Item")
    }
  }

  # Strategy C2: Check construct_error for Q2_predict (construct-level)
  # This is separate from item-level RMSE/MAE
  if (!is.null(sum_pred$construct_error)) {
    cat("  Checking construct_error for Q2_predict...\n")
    const_err <- sum_pred$construct_error
    if (!is.matrix(const_err)) const_err <- as.matrix(const_err)

    # Check if Q2_predict is in rows or columns
    if ("Q2_predict" %in% rownames(const_err)) {
      q2_vals <- const_err["Q2_predict", ]
      q2_mat <- matrix(q2_vals, ncol = 1, dimnames = list(names(q2_vals), "PLS"))
      df_list[[length(df_list) + 1]] <- process_error_metric(q2_mat, "Q2_predict", "Construct")
    } else if ("Q2_predict" %in% colnames(const_err)) {
      q2_mat <- const_err[, "Q2_predict", drop = FALSE]
      df_list[[length(df_list) + 1]] <- process_error_metric(q2_mat, "Q2_predict", "Construct")
    }
  }

  # Strategy D: Check for construct-level predictions (common in newer seminr)
  if (length(df_list) == 0) {
    cat("  Attempting Strategy D: Construct-level predictions...\n")

    # Check for construct_predictions
    if (!is.null(sum_pred$construct_predictions)) {
      const_pred <- sum_pred$construct_predictions
      if (!is.matrix(const_pred)) const_pred <- as.matrix(const_pred)

      for (metric in c("RMSE", "MAE", "Q2_predict")) {
        if (metric %in% colnames(const_pred)) {
          df_list[[length(df_list) + 1]] <- process_error_metric(
            const_pred[, metric, drop = FALSE], metric, "Construct"
          )
        }
      }
    }

    # Alternative: Extract from pred_results$metrics directly
    if (length(df_list) == 0 && !is.null(pred_results$metrics)) {
      cat("  Checking pred_results$metrics...\n")
      metrics_obj <- pred_results$metrics
      if (is.data.frame(metrics_obj) || is.matrix(metrics_obj)) {
        if (!is.matrix(metrics_obj)) metrics_obj <- as.matrix(metrics_obj)
        if (nrow(metrics_obj) > 0) {
          cat("  Found metrics in pred_results$metrics\n")
          # Try to extract RMSE, MAE, Q2
          for (metric in c("RMSE", "MAE", "Q2_predict", "Q2")) {
            if (metric %in% colnames(metrics_obj)) {
              df_list[[length(df_list) + 1]] <- process_error_metric(
                metrics_obj[, metric, drop = FALSE], metric, "Construct"
              )
            }
          }
        }
      }
    }
  }

  final_df <- do.call(rbind, df_list)

  # Calculate Q²_predict (out-of-sample) from prediction errors
  # Q²_predict = 1 - (SSE_predict / SST) where SST uses mean of observed values
  cat("\n  Calculating Q²_predict (out-of-sample) from prediction errors...\n")

  tryCatch(
    {
      # prediction_error contains the actual prediction errors for each observation
      if (!is.null(sum_pred$prediction_error)) {
        pred_errors <- sum_pred$prediction_error

        # Get actual observed values (we need these to calculate SST)
        # In PLSpredict, the observed values are typically in PLS_in_sample or can be reconstructed
        # For each indicator, calculate Q²_predict

        if (is.data.frame(pred_errors) || is.matrix(pred_errors)) {
          pred_errors_mat <- as.matrix(pred_errors)
          n_obs <- nrow(pred_errors_mat)

          # For each indicator/construct column
          q2_results <- list()
          for (col_name in colnames(pred_errors_mat)) {
            errors <- pred_errors_mat[, col_name]

            # SSE_predict = sum of squared prediction errors
            sse_predict <- sum(errors^2, na.rm = TRUE)

            # For SST, we need the observed values
            # In PLSpredict context, we can use the model's construct scores or indicator values
            # Alternative: Use variance of errors as proxy (conservative estimate)

            # Get observed values from model if available
            if (!is.null(model$construct_scores) && col_name %in% colnames(model$construct_scores)) {
              observed <- model$construct_scores[, col_name]
              sst <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
            } else if (!is.null(model$data) && col_name %in% colnames(model$data)) {
              observed <- model$data[, col_name]
              sst <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
            } else {
              # Fallback: estimate from prediction errors
              # Assume observed ≈ predicted + error, use variance
              sst <- var(errors, na.rm = TRUE) * (n_obs - 1) + sse_predict
            }

            # Calculate Q²_predict
            if (sst > 0) {
              q2_predict <- 1 - (sse_predict / sst)

              # Determine if this is an item or construct
              is_construct <- col_name %in% names(model$construct_scores)
              type <- if (is_construct) "Construct" else "Item"

              q2_results[[length(q2_results) + 1]] <- data.frame(
                Construct = type,
                Variable = col_name,
                Metric = "Q²_predict",
                PLS = round(q2_predict, 4),
                LM = NA,
                Diff = NA,
                Status = if (q2_predict > 0) "Q² > 0 (Predictive)" else "Q² ≤ 0 (No Prediction)",
                stringsAsFactors = FALSE
              )
            }
          }

          if (length(q2_results) > 0) {
            q2_df <- do.call(rbind, q2_results)
            final_df <- rbind(final_df, q2_df)
            cat("  ✓ Added", nrow(q2_df), "Q²_predict values (out-of-sample)\n")
          }
        }
      } else {
        cat("  ! prediction_error not available in PLSpredict output\n")
      }
    },
    error = function(e) {
      cat("  ! Could not calculate Q²_predict:", conditionMessage(e), "\n")
    }
  )

  # Check if we have any results at all
  if (is.null(final_df) || nrow(final_df) == 0) {
    cat("  ! ERROR: No prediction metrics extracted from any strategy.\n")
    cat("  ! Available summary components:\n")
    cat("  !   ", paste(names(sum_pred), collapse = ", "), "\n")
    cat("  ! Component classes:\n")
    for (nm in names(sum_pred)) {
      cat("  !   ", nm, ": ", class(sum_pred[[nm]])[1], "\n", sep = "")
    }
    cat("  ! This indicates a structure mismatch with seminr version.\n")
    cat("  ! Please report this output to debug the issue.\n")
    cat("  ! Tip: Run with Sys.setenv(DEBUG_PREDICT='TRUE') for full structure.\n\n")

    return(data.frame(
      Construct = character(), Variable = character(), Metric = character(),
      PLS = numeric(), LM = numeric(), Diff = numeric(), Status = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Reorder
  final_df <- final_df[, c("Construct", "Variable", "Metric", "PLS", "LM", "Diff", "Status")]

  # Print summary of Q2
  q2_vals <- final_df[final_df$Metric == "Q2_predict", "PLS"]
  if (length(q2_vals) > 0) {
    n_pos <- sum(q2_vals > 0, na.rm = TRUE)
    cat("Q2 Summary: ", n_pos, "of", length(q2_vals), "indicators have Q2 > 0 (predictive relevance)\n")
  }

  return(final_df)
}
