# G*POWER ANALYSIS - SAMPLE SIZE CALCULATION
# Determines optimal sample size for PLS-SEM
# Based on Inverse Square Root Method & G*Power Logic

if (!requireNamespace("pwr", quietly = TRUE)) {
    install.packages("pwr", repos = "https://cran.rstudio.com/")
}
library(pwr)

# Settings & Parameters
BETA_MIN <- 0.15 # Minimal detectable path coefficient (moderate effect)
BETA_ERROR_PROB <- 0.20
POWER_TARGET <- 1 - BETA_ERROR_PROB # 0.80
SIGNIFICANCE <- 0.05

EFFECT_SIZE_F2 <- 0.15 # Moderate effect (Cohen, 1988: small=0.02, medium=0.15, large=0.35)
MAX_PREDICTORS <- 8 # Max arrows pointing to a single latent variable
ATTRITION_BUFFER <- 0.15
BUFFER_TEXT <- "15%"

# Calculate N using Inverse Square Root Method
# Formula: N > (Constant / |beta_min|)^2
calculate_inverse_sqrt_n <- function(beta_min, power = 0.80) {
    constant <- if (power >= 0.90) 3.242 else 2.486
    n_required <- (constant / abs(beta_min))^2
    return(ceiling(n_required))
}

# Calculate N using G*Power F-Test (Linear Multiple Regression)
calculate_gpower_n <- function(f2, predictors, alpha = 0.05, power = 0.80) {
    result <- pwr.f2.test(u = predictors, f2 = f2, sig.level = alpha, power = power, v = NULL)
    n_required <- result$v + predictors + 1
    return(ceiling(n_required))
}

# Main Execution
cat("\nPLS-SEM POWER ANALYSIS REPORT\n\n")

cat("Reference Model: Single-Model Moderated Mediation\n")
cat("  - Max Predictors (DV Equation):", MAX_PREDICTORS, "\n")
cat("    (3 Mediators + 2 Condition Dummies + 1 Moderator + 2 Interactions)\n\n")

cat("Parameters:\n")
cat("  - Power (1 - beta):", POWER_TARGET, "\n")
cat("  - Significance (alpha):", SIGNIFICANCE, "\n")
cat("  - Min. Path Coeff (beta_min):", BETA_MIN, "(Moderate)\n")
cat("  - Effect Size (f^2):", EFFECT_SIZE_F2, "(Moderate)\n")
cat("  - Attrition Buffer:", BUFFER_TEXT, "\n\n")

n_inverse <- calculate_inverse_sqrt_n(BETA_MIN, POWER_TARGET)
cat("--- Method 1: Inverse Square Root Method ---\n")
cat("Formula: N > (Constant / |beta_min|)^2\n")
cat("Used for detecting minimum path coefficients in PLS-SEM (Kock, 2014).\n")
cat("Result N:", n_inverse, "\n\n")

n_gpower <- calculate_gpower_n(EFFECT_SIZE_F2, MAX_PREDICTORS, SIGNIFICANCE, POWER_TARGET)
cat("--- Method 2: G*Power (Multiple Regression) ---\n")
cat("Settings: F-test, Linear Mult. Regression, R^2 deviation from zero\n")
cat("Used for detecting specific effect sizes (f^2).\n")
cat("Result N:", n_gpower, "\n\n")

defensible_min <- max(n_inverse, n_gpower)
final_n_buffered <- ceiling(defensible_min * (1 + ATTRITION_BUFFER))

cat("RECOMMENDATION\n")
cat("Based on detecting a minimal effect size (Beta =", BETA_MIN, ", f^2 =", EFFECT_SIZE_F2, "):\n")
cat("Defensible Minimum (Larger of two):", defensible_min, "\n")
cat("With", BUFFER_TEXT, "Attrition Buffer:", final_n_buffered, "\n\n")
