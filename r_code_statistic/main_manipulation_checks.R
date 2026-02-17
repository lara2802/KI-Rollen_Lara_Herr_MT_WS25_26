# MANIPULATION CHECKS
# Statistical verification of experimental conditions

# Load utility functions
source("r_scripts/utils_pls_sem.R")
install_and_load_packages()

# General settings
set.seed(12345)

# LOAD DATA
data <- read_excel("data/daten_unipark.xlsx")

# Convert ALL negative values to NA
data <- convert_missing_to_na(data, item_pattern = "^v_")


# REVERSE CODING
REVERSE_CONFIG <- create_reverse_config(
  scale_7_items = c("v_57", "v_66", "v_68", "v_369", "v_387", "v_70"),
  scale_5_items = c("v_256", "v_257", "v_259")
)

data <- reverse_code_items(data, REVERSE_CONFIG)

# CREATE EXPERIMENTAL VARIABLES
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

# MANIPULATION CHECKS

# Define items for checks
# Complexity: v_20 + v_70_reversed (group-level validation only)
# Autonomy: v_11 + v_12 + v_75 (used for participant exclusion)
check_results <- perform_manipulation_checks(
  data = data,
  complexity_items = c("v_20", "v_70_reversed"),
  autonomy_items = c("v_11", "v_12", "v_75")
)

# Create check_autonomy composite for identify_manipulation_failures
data$check_autonomy <- rowMeans(data[, c("v_11", "v_12", "v_75")], na.rm = TRUE)

# Use PLS-SEM-aligned exclusion logic
exclusion_results <- identify_manipulation_failures(data, verbose = TRUE, a_threshold = 5)

cat("\n Manipulation checks complete.\n")