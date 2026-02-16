# DESCRIPTIVE STATISTICS
# Comprehensive analysis of sample and constructs

# Load utility functions
source("r_scripts/utils_pls_sem.R")
install_and_load_packages()

# General settings
set.seed(12345)

# LOAD DATA
data <- read_excel("data/daten_unipark.xlsx")

# Convert ALL negative values to NA (generic handling)
data <- convert_missing_to_na(data, item_pattern = "^v_")

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
data <- create_experimental_variables(data, AI_CONDITION_CONFIG, TASK_CONFIG, verbose = FALSE)

# MANIPULATION CHECKS & FILTERING
print_section_header("MANIPULATION CHECKS & FILTERING")

# Calculate validation scores for the check
data$check_complexity <- rowMeans(data[, c("v_20", "v_70_reversed")], na.rm = TRUE)
data$check_autonomy <- rowMeans(data[, c("v_11", "v_12", "v_75")], na.rm = TRUE)

# Autonomy-based exclusion criteria:
# assistiv: >= 5, agent_noval: <= 3, agent_val: extremes (1 or 7)
fail_autonomy_ids <- which(
    (data$ai_condition == "assistiv" & data$check_autonomy >= 5) |
        (data$ai_condition == "agent_noval" & data$check_autonomy <= 3) |
        (data$ai_condition == "agent_val" & (data$check_autonomy == 1 | data$check_autonomy == 7))
)

excluded_indices <- unique(fail_autonomy_ids)
man_check_results <- list(
    excluded_ids = if ("lfdn" %in% names(data)) data$lfdn[excluded_indices] else excluded_indices,
    autonomy_fail_ids = if ("lfdn" %in% names(data)) data$lfdn[fail_autonomy_ids] else fail_autonomy_ids
)

n_before <- nrow(data)
cat("Initial N:", n_before, "\n")
cat("Anzahl ausgeschlossener Teilnehmender:", length(excluded_indices), "\n")
cat("- Wegen Manipulation Check (KI-Autonomie):", length(man_check_results$autonomy_fail_ids), "\n")

if (length(excluded_indices) > 0) {
    cat("--- Details zu ausgeschlossenen Teilnehmenden ---\n")
    if ("lfdn" %in% names(data)) {
        cat("Liste der IDs (lfdn):", paste(man_check_results$excluded_ids, collapse = ", "), "\n\n")
        excluded_data <- data[excluded_indices, ]
    } else {
        cat("Liste der Indices:", paste(man_check_results$excluded_ids, collapse = ", "), "\n\n")
        excluded_data <- data[excluded_indices, ]
    }

    cat("Ausschluss-Matrix (AI Condition):\n")
    if (nrow(excluded_data) > 0) {
        exclusion_matrix <- table(excluded_data$ai_condition, dnn = "AI Condition")
        print(exclusion_matrix)
    }

    # APPLY FILTERING
    data <- data[-excluded_indices, ]

    cat("\nFILTER APPLIED: Participants removed.\n")
} else {
    cat("Keine Teilnehmenden ausgeschlossen.\n")
}

cat("Final Analytical Sample N:", nrow(data), "\n\n")

# CREATE COMPOSITE SCORES
# Define mediators (SDT constructs)
MEDIATOR_CONFIG <- create_mediator_config(list(
    m_autonomy = list(items = c("v_56", "v_57_reversed", "v_58"), label = "Autonomy"),
    m_competence = list(items = c("v_61", "v_62"), label = "Competence"),
    m_relatedness = list(items = c("v_66_reversed", "v_68_reversed", "v_69"), label = "Relatedness")
))

# Define dependent variables
DV_CONFIG <- create_dv_config(list(
    dv_intrinsic_motivation = list(items = c("v_368", "v_383", "v_369_reversed"), label = "Intrinsic Motivation"),
    dv_commitment = list(items = c("v_385", "v_403", "v_386", "v_387_reversed", "v_390"), label = "Organizational Commitment"),
    dv_jobcrafting = list(items = c("v_391", "v_392", "v_393", "v_395"), label = "Job Crafting"),
    dv_engagement = list(items = c("v_397", "v_398", "v_399"), label = "Work Engagement"),
    dv_hedonic_wellbeing = list(items = c("v_252", "v_254", "v_255", "v_256_reversed", "v_257_reversed", "v_259_reversed"), label = "Hedonic Well-being"),
    dv_eudaimonic_wellbeing = list(items = c("v_277", "v_282", "v_283"), label = "Eudaimonic Well-being")
))

composite_results <- create_composites(data, MEDIATOR_CONFIG, DV_CONFIG, verbose = FALSE)
data <- composite_results$data

# 1. SAMPLE DESCRIPTION
print_section_header("SAMPLE DESCRIPTION")

# Overall N
n_total <- nrow(data)
cat("Gesamtstichprobengröße (N):", n_total, "\n\n")

# Age (v_25)
age_stats <- data.frame(
    Mean = mean(data$v_25, na.rm = TRUE),
    SD = sd(data$v_25, na.rm = TRUE),
    Min = min(data$v_25, na.rm = TRUE),
    Max = max(data$v_25, na.rm = TRUE)
)
cat("Alter:\n")
print(round(age_stats, 2))
cat("Range:", age_stats$Max - age_stats$Min, "\n\n")

# Gender (v_23)
# 1: Weiblich, 2: Männlich
gender_counts <- table(data$v_23, useNA = "ifany")
gender_pct <- prop.table(gender_counts) * 100
gender_df <- data.frame(
    Gender = c("Female", "Male"),
    Count = as.numeric(gender_counts),
    Percent = round(as.numeric(gender_pct), 2)
)
cat("Geschlecht:\n")
print(gender_df)
cat("\n")

# Occupation (v_28)
# 1 = Angestellt, 2 = Ohne Beschäftigung, 3 = Selbstständig, 4 = Pensioniert, 5 = Studierend, 6 = Beliebig
# Map numeric codes to labels for descriptive output
data$v_28_labeled <- factor(data$v_28,
    levels = c(1, 2, 3, 4, 5, 6),
    labels = c("Angestellt", "Ohne Beschäftigung", "Selbstständig", "Pensioniert", "Studierend", "Beliebig")
)

occ_counts <- table(data$v_28_labeled, useNA = "ifany")
occ_pct <- prop.table(occ_counts) * 100
cat("Beruf/Studium (v_28 Häufigkeiten):\n")
print(occ_counts)
cat("Beruf/Studium (v_28 Prozent):\n")
print(round(occ_pct, 2))
cat("\n")

# Other features (e.g., Department v_71)
cat("Relevante Merkmale (Berufsfeld v_71 - Top 5):\n")
print(head(sort(table(data$v_71), decreasing = TRUE), 5))
cat("\n")

# AI Usage (v_34)
cat("Nutzungshäufigkeit KI (v_34):\n")
if ("v_34" %in% names(data)) {
    ai_usage_counts <- table(data$v_34, useNA = "ifany")
    ai_usage_pct <- prop.table(ai_usage_counts) * 100
    print(ai_usage_counts)
    print(round(ai_usage_pct, 2))
} else {
    cat("Variable v_34 nicht gefunden.\n")
}
cat("\n")

# Technology Affinity (v_51 - v_54)
tech_affinity_vars <- c("v_51", "v_52", "v_53", "v_54")
cat("Technologieaffinität (v_51 - v_54) - Häufigkeiten:\n")

for (var in tech_affinity_vars) {
    if (var %in% names(data)) {
        cat(paste0("Item ", var, ":\n"))
        ta_counts <- table(data[[var]], useNA = "ifany")
        ta_pct <- prop.table(ta_counts) * 100
        print(ta_counts)
        print(round(ta_pct, 2))
        cat("\n")
    } else {
        cat(paste0("Item ", var, " nicht gefunden.\n"))
    }
}

# 2. GROUP SIZES
print_section_header("GROUP SIZES PER EXPERIMENTAL CONDITION")

group_sizes <- table(data$ai_condition, data$task_complexity)
cat("Gruppengrößen (AI Rolle x Task Complexity):\n")
print(group_sizes)
cat("\n")


# 3. ZENTRALE VARIABLEN (CONSTRUCTS)
print_section_header("ZENTRALE VARIABLEN (CONSTRUCTS)")

construct_names <- c(names(MEDIATOR_CONFIG), names(DV_CONFIG))
construct_labels <- c(
    sapply(MEDIATOR_CONFIG, function(x) x$label),
    sapply(DV_CONFIG, function(x) x$label)
)

construct_desc <- data |>
    dplyr::select(dplyr::all_of(construct_names)) |>
    psych::describe() |>
    as.data.frame() |>
    dplyr::select(mean, sd, min, max) |>
    dplyr::mutate(dplyr::across(everything(), ~ round(., 2))) |>
    dplyr::mutate(label = construct_labels) |>
    dplyr::select(label, everything())

cat("Deskriptive Statistik der Hauptkonstrukte:\n")
print(construct_desc)
cat("\n")

# RELIABILITÄT (CRONBACH'S ALPHA)
print_section_header("RELIABILITÄT (CRONBACH'S ALPHA)")
all_configs <- c(MEDIATOR_CONFIG, DV_CONFIG)
alpha_results <- data.frame(
    Konstrukt = character(),
    Items = integer(),
    Alpha = numeric(),
    stringsAsFactors = FALSE
)

for (name in names(all_configs)) {
    config <- all_configs[[name]]
    items <- config$items
    label <- config$label

    # Check if all items exist in data
    if (all(items %in% names(data))) {
        # Calculate Alpha
        tryCatch(
            {
                if (length(items) > 1) {
                    alpha_obj <- suppressWarnings(psych::alpha(data[, items], check.keys = TRUE))
                    alpha_val <- alpha_obj$total$raw_alpha
                } else {
                    alpha_val <- NA
                }

                alpha_results[nrow(alpha_results) + 1, ] <- list(
                    Konstrukt = label,
                    Items = length(items),
                    Alpha = round(alpha_val, 2)
                )
            },
            error = function(e) {
                cat("Fehler bei Alpha für", label, ":", e$message, "\n")
            }
        )
    } else {
        missing <- items[!items %in% names(data)]
        cat("Warnung: Items fehlen für", label, ":", paste(missing, collapse = ", "), "\n")
    }
}

cat("Cronbach's Alpha für Skalen:\n")
print(alpha_results)
cat("\n")

# VERTEILUNGSFORM (SCHIEFE & KURTOSIS)
print_section_header("VERTEILUNGSFORM (SKEWNESS & KURTOSIS)")
distribution_results <- data.frame(
    Konstrukt = character(),
    Shapiro_W = numeric(),
    Shapiro_p = numeric(),
    Skewness = numeric(),
    Kurtosis = numeric(),
    Normality_Check = character(),
    stringsAsFactors = FALSE
)

# Loop through all constructs (Mediators + DVs)
for (name in names(all_configs)) {
    config <- all_configs[[name]]
    label <- config$label

    # Check if construct exists in data (composite score)
    if (name %in% names(data)) {
        values <- data[[name]]

        # 1. Descriptive measures (Skewness & Kurtosis)
        # Using psych::describe which is robust
        desc <- psych::describe(values)
        skew <- desc$skew
        kurt <- desc$kurtosis

        # 2. Shapiro-Wilk Test (Normality)
        shapiro_w <- NA
        shapiro_p <- NA
        norm_msg <- "N/A"

        tryCatch(
            {
                if (length(na.omit(values)) >= 3 && length(na.omit(values)) <= 5000) {
                    st <- shapiro.test(values)
                    shapiro_w <- st$statistic
                    shapiro_p <- st$p.value

                    if (shapiro_p < 0.05) {
                        norm_msg <- "Sign. Abweichung"
                    } else {
                        norm_msg <- "Normalverteilt"
                    }
                } else {
                    norm_msg <- "N > 5000 (Test skipped)"
                }
            },
            error = function(e) {
                norm_msg <- "Error"
            }
        )

        # Check rule of thumb for Skew/Kurtosis (+/- 1 is ideal, +/- 2 acceptable)
        dist_quality <- "Ideal"
        if (abs(skew) > 1 || abs(kurt) > 1) dist_quality <- "Akzeptabel"
        if (abs(skew) > 2 || abs(kurt) > 2) dist_quality <- "Kritisch"

        distribution_results[nrow(distribution_results) + 1, ] <- list(
            Konstrukt = label,
            Shapiro_W = round(shapiro_w, 3),
            Shapiro_p = round(shapiro_p, 3),
            Skewness = round(skew, 2),
            Kurtosis = round(kurt, 2),
            Normality_Check = paste0(norm_msg, " (", dist_quality, ")")
        )
    }
}

cat("Test auf Normalverteilung und Verteilungsform:\n")
print(distribution_results)

# 4. MANIPULATIONSVARIABLEN
print_section_header("MANIPULATIONSVARIABLEN")

# AI Autonomy by AI Role
autonomy_by_role <- data |>
    dplyr::group_by(ai_condition) |>
    dplyr::summarise(
        Mean_Autonomy = mean(check_autonomy, na.rm = TRUE),
        SD_Autonomy = sd(check_autonomy, na.rm = TRUE),
        N = dplyr::n()
    )
cat("Wahrgenommene KI-Autonomie nach KI-Rolle:\n")
autonomy_by_role_rounded <- autonomy_by_role |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 2)))
print(autonomy_by_role_rounded)
cat("\n")

# Task Complexity by Condition
complexity_by_cond <- data |>
    dplyr::group_by(task_complexity) |>
    dplyr::summarise(
        Mean_Complexity = mean(check_complexity, na.rm = TRUE),
        SD_Complexity = sd(check_complexity, na.rm = TRUE),
        N = dplyr::n()
    )
cat("Wahrgenommene Aufgabenkomplexität nach Bedingung:\n")
complexity_by_cond_rounded <- complexity_by_cond |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 2)))
print(complexity_by_cond_rounded)
cat("\n")


# 5. ZUSAMMENHÄNGE (CORRELATIONS)
print_section_header("ZUSAMMENHÄNGE (CORRELATIONS)")

cor_vars <- data |> dplyr::select(dplyr::all_of(construct_names))
cor_matrix <- cor(cor_vars, use = "pairwise.complete.obs")

# Add Mean and SD to the correlation matrix
cor_summary <- data.frame(
    Variable = construct_labels,
    Mean = colMeans(cor_vars, na.rm = TRUE),
    SD = sapply(cor_vars, sd, na.rm = TRUE)
)

# Combine Matrix and Summary for display
cat("Korrelationsmatrix der Hauptvariablen (inkl. M, SD):\n")
cor_summary_rounded <- cor_summary |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 2)))
print(cor_summary_rounded)
cat("\nMatrix:\n")
print(round(cor_matrix, 2))
cat("\n")

# 6. DATENQUALITÄT
print_section_header("DATENQUALITÄT")

# Missing values
missing_total <- sum(is.na(data))
missing_pct <- (missing_total / (nrow(data) * ncol(data))) * 100
cat("Anteil fehlender Werte (Gesamt):", round(missing_pct, 2), "%\n")

# Missing values per construct
missing_per_var <- colSums(is.na(data |> dplyr::select(dplyr::all_of(construct_names))))
cat("Fehlende Werte pro Konstrukt:\n")
print(missing_per_var)
cat("\n")

