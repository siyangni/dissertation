library(pacman)
p_load(stats, car, foreign, svMisc, devtools, roxygen2, lattice, psych, lavaan, semTools, psych, VIM, semPlot)

cat("=== LONGITUDINAL MEASUREMENT INVARIANCE TESTING ===\n")
cat("=== SELF-CONTROL CONSTRUCT: AGES 3, 5, 7, 11, 14, 17 ===\n\n")

# === RUN MASTER RECODE SCRIPT ===
cat("=== RUNNING MASTER RECODE SCRIPT ===\n")
cat("Using scripts/recode_self_control.R to create self-control variables across ages...\n\n")

source("scripts/recode_self_control.R")

cat("Master recode completed. Self-control variables available for longitudinal analysis.\n\n")

# Ensure merged_data exists and apply recoding explicitly for safety
if (!exists("merged_data")) {
  data_path <- "/home/siyang/dissertation_folder/data"
  merged_data <- readRDS(file.path(data_path, "merged1203.rds"))
}
for (age in c(3, 5, 7, 11, 14, 17)) {
  merged_data <- recode_self_control(merged_data, age, verbose = FALSE)
}

# Define self-control variables for each age
sc3_vars <- c("sc3_task_completion", "sc3_distracted", "sc3_fidgeting", "sc3_think_act", 
              "sc3_restless", "sc3_temper", "sc3_obedient", "sc3_lying")

sc5_vars <- c("sc5_task_completion", "sc5_distracted", "sc5_fidgeting", "sc5_think_act", 
              "sc5_restless", "sc5_temper", "sc5_obedient", "sc5_lying")

sc7_vars <- c("sc7_task_completion", "sc7_distracted", "sc7_fidgeting", "sc7_think_act", 
              "sc7_restless", "sc7_temper", "sc7_obedient", "sc7_lying")

sc11_vars <- c("sc11_task_completion", "sc11_distracted", "sc11_fidgeting", "sc11_think_act", 
               "sc11_restless", "sc11_temper", "sc11_obedient", "sc11_lying")

sc14_vars <- c("sc14_task_completion", "sc14_distracted", "sc14_fidgeting", "sc14_think_act", 
               "sc14_restless", "sc14_temper", "sc14_obedient", "sc14_lying")

sc17_vars <- c("sc17_task_completion", "sc17_distracted", "sc17_fidgeting", "sc17_think_act", 
               "sc17_restless", "sc17_temper", "sc17_obedient", "sc17_lying")

all_sc_vars <- c(sc3_vars, sc5_vars, sc7_vars, sc11_vars, sc14_vars, sc17_vars)

cat("Variables for longitudinal analysis:\n")
cat("Age 3:", paste(sc3_vars, collapse = ", "), "\n")
cat("Age 5:", paste(sc5_vars, collapse = ", "), "\n")
cat("Age 7:", paste(sc7_vars, collapse = ", "), "\n")
cat("Age 11:", paste(sc11_vars, collapse = ", "), "\n")
cat("Age 14:", paste(sc14_vars, collapse = ", "), "\n")
cat("Age 17:", paste(sc17_vars, collapse = ", "), "\n\n")

# Check variable availability
available_vars <- all_sc_vars[all_sc_vars %in% names(merged_data)]
missing_vars <- all_sc_vars[!all_sc_vars %in% names(merged_data)]

cat("=== VARIABLE AVAILABILITY CHECK ===\n")
cat("Total variables needed:", length(all_sc_vars), "\n")
cat("Available variables:", length(available_vars), "\n")
cat("Missing variables:", length(missing_vars), "\n")

if(length(missing_vars) > 0) {
  cat("Missing:", paste(missing_vars, collapse = ", "), "\n")
  cat("Note: Ensure the master recode script has been run: scripts/recode_self_control.R\n")
}

# Check availability by age
sc3_available <- sum(sc3_vars %in% names(merged_data))
sc5_available <- sum(sc5_vars %in% names(merged_data))
sc7_available <- sum(sc7_vars %in% names(merged_data))
sc11_available <- sum(sc11_vars %in% names(merged_data))
sc14_available <- sum(sc14_vars %in% names(merged_data))
sc17_available <- sum(sc17_vars %in% names(merged_data))

cat("\nVariables available by age:\n")
cat("Age 3:", sc3_available, "/", length(sc3_vars), "\n")
cat("Age 5:", sc5_available, "/", length(sc5_vars), "\n")
cat("Age 7:", sc7_available, "/", length(sc7_vars), "\n")
cat("Age 11:", sc11_available, "/", length(sc11_vars), "\n")
cat("Age 14:", sc14_available, "/", length(sc14_vars), "\n")
cat("Age 17:", sc17_available, "/", length(sc17_vars), "\n")

# Create datasets for each age with complete cases
cat("\n=== SAMPLE SIZE ANALYSIS ===\n")

# Age 3 complete cases
sc3_data <- merged_data[, sc3_vars[sc3_vars %in% names(merged_data)], drop = FALSE]
sc3_complete <- na.omit(sc3_data)
cat("Age 3 - Complete cases:", nrow(sc3_complete), "/", nrow(merged_data), "\n")

# Age 5 complete cases
sc5_data <- merged_data[, sc5_vars[sc5_vars %in% names(merged_data)], drop = FALSE]
sc5_complete <- na.omit(sc5_data)
cat("Age 5 - Complete cases:", nrow(sc5_complete), "/", nrow(merged_data), "\n")

# Age 7 complete cases
sc7_data <- merged_data[, sc7_vars[sc7_vars %in% names(merged_data)], drop = FALSE]
sc7_complete <- na.omit(sc7_data)
cat("Age 7 - Complete cases:", nrow(sc7_complete), "/", nrow(merged_data), "\n")

# Age 11 complete cases
sc11_data <- merged_data[, sc11_vars[sc11_vars %in% names(merged_data)], drop = FALSE]
sc11_complete <- na.omit(sc11_data)
cat("Age 11 - Complete cases:", nrow(sc11_complete), "/", nrow(merged_data), "\n")

# Age 14 complete cases  
sc14_data <- merged_data[, sc14_vars[sc14_vars %in% names(merged_data)], drop = FALSE]
sc14_complete <- na.omit(sc14_data)
cat("Age 14 - Complete cases:", nrow(sc14_complete), "/", nrow(merged_data), "\n")

# Age 17 complete cases
sc17_data <- merged_data[, sc17_vars[sc17_vars %in% names(merged_data)], drop = FALSE]
sc17_complete <- na.omit(sc17_data)
cat("Age 17 - Complete cases:", nrow(sc17_complete), "/", nrow(merged_data), "\n")

#################
## PREPARE DATA FOR LONGITUDINAL INVARIANCE TESTING
#################

cat("\n=== PREPARING DATA FOR LONGITUDINAL INVARIANCE TESTING ===\n")

# Create long format data for semTools::longInvariance()
# This function expects data in long format with time-varying indicators

# Determine how many variables are available across all ages
n_sc_vars_by_age <- c(
  length(sc3_vars[sc3_vars %in% names(merged_data)]),
  length(sc5_vars[sc5_vars %in% names(merged_data)]),
  length(sc7_vars[sc7_vars %in% names(merged_data)]),
  length(sc11_vars[sc11_vars %in% names(merged_data)]),
  length(sc14_vars[sc14_vars %in% names(merged_data)]),
  length(sc17_vars[sc17_vars %in% names(merged_data)])
)

n_sc_vars <- min(n_sc_vars_by_age)  # Use the minimum number available across all ages

cat("Number of variables available across all ages:", n_sc_vars, "\n")

if(n_sc_vars < 3) {
  stop("Insufficient variables for factor analysis. Need at least 3 indicators per factor.")
}

# Common variable names for analysis (time-invariant names)
common_names <- c("task_completion", "distracted", "fidgeting", "think_act", 
                  "restless", "temper", "obedient", "lying")
available_common <- common_names[1:n_sc_vars]

# Create wide format data for longInvariance (participants with complete data across all ages)
all_vars_available <- c(sc3_vars[sc3_vars %in% names(merged_data)][1:n_sc_vars],
                       sc5_vars[sc5_vars %in% names(merged_data)][1:n_sc_vars],
                       sc7_vars[sc7_vars %in% names(merged_data)][1:n_sc_vars],
                       sc11_vars[sc11_vars %in% names(merged_data)][1:n_sc_vars],
                       sc14_vars[sc14_vars %in% names(merged_data)][1:n_sc_vars],
                       sc17_vars[sc17_vars %in% names(merged_data)][1:n_sc_vars])

# Add ID variable for longInvariance
merged_data$id <- rownames(merged_data)

# Select data with all required variables plus ID
dat_wide <- merged_data[, c("id", all_vars_available)]
dat_wide_complete <- na.omit(dat_wide)

cat("Participants with complete data across all ages:", nrow(dat_wide_complete), "/", nrow(merged_data), "\n")

if(nrow(dat_wide_complete) < 100) {
  warning("Small sample size (< 100) may affect reliability of invariance testing")
}

# Reshape to long format for semTools::longInvariance
# The function expects variable names without time prefixes in the model
cat("\n=== RESHAPING DATA TO LONG FORMAT ===\n")

# Create long format manually for better control
dat_long <- data.frame()

ages <- c(3, 5, 7, 11, 14, 17)
for(i in seq_along(ages)) {
  age <- ages[i]
  
  # Get variables for this age
  age_vars <- c(
    sc3_vars[sc3_vars %in% names(merged_data)][1:n_sc_vars],
    sc5_vars[sc5_vars %in% names(merged_data)][1:n_sc_vars],
    sc7_vars[sc7_vars %in% names(merged_data)][1:n_sc_vars],
    sc11_vars[sc11_vars %in% names(merged_data)][1:n_sc_vars],
    sc14_vars[sc14_vars %in% names(merged_data)][1:n_sc_vars],
    sc17_vars[sc17_vars %in% names(merged_data)][1:n_sc_vars]
  )
  
  # Select variables for current age
  start_idx <- (i-1) * n_sc_vars + 1
  end_idx <- i * n_sc_vars
  current_vars <- age_vars[start_idx:end_idx]
  
  # Create data frame for this age
  age_data <- dat_wide_complete[, c("id", current_vars)]
  age_data$age <- age
  
  # Rename variables to common names
  names(age_data)[2:(n_sc_vars+1)] <- available_common
  
  # Append to long format data
  dat_long <- rbind(dat_long, age_data)
}

# Convert age to factor
dat_long$age <- factor(dat_long$age, levels = c(3, 5, 7, 11, 14, 17))

cat("Long format dataset created:\n")
cat("Total observations:", nrow(dat_long), "\n")
cat("Age groups:", table(dat_long$age), "\n")
cat("Variables per age:", length(available_common), "\n\n")

#################
## SPECIFY TIME-INVARIANT CFA MODEL
#################

cat("=== SPECIFYING CFA MODEL ===\n")

# 2·1  Specify the time‑invariant CFA model (one latent factor)
cfa_model <- paste('
SelfControl =~', paste(available_common, collapse = ' + '), '
')

cat("CFA Model:\n")
cat(cfa_model)
cat("\n")

#################
## LONGITUDINAL MEASUREMENT INVARIANCE TESTING
#################

cat("=== TESTING LONGITUDINAL MEASUREMENT INVARIANCE ===\n")

# 2·2  Fit configural, metric, scalar, and strict models using lavaan::cfa()
cat("Fitting invariance models using lavaan::cfa() with group constraints...\n")

# Step 1: Configural Invariance (baseline - same factor structure across groups)
cat("\n1. CONFIGURAL INVARIANCE (Baseline Model)\n")
cat("   - Same factor structure across all six age groups\n")
cat("   - No equality constraints\n")

configural <- lavaan::cfa(model = cfa_model,
                         data = dat_long,
                         group = "age",
                         estimator = "WLSMV",
                         ordered = available_common,
                         std.lv = TRUE)

cat("Configural model fitted successfully!\n")

# Step 2: Metric Invariance (equal factor loadings)
cat("\n2. METRIC INVARIANCE (Weak Invariance)\n")
cat("   - Equal factor loadings across all six age groups\n")
cat("   - Tests whether items relate to the factor similarly across ages\n")

metric <- lavaan::cfa(model = cfa_model,
                     data = dat_long,
                     group = "age",
                     estimator = "WLSMV",
                     ordered = available_common,
                     std.lv = TRUE,
                     group.equal = "loadings")

cat("Metric model fitted successfully!\n")

# Step 3: Scalar Invariance (equal factor loadings + thresholds)
cat("\n3. SCALAR INVARIANCE (Strong Invariance)\n")
cat("   - Equal factor loadings AND thresholds across all six age groups\n")
cat("   - Tests whether item difficulty is similar across ages\n")

scalar <- lavaan::cfa(model = cfa_model,
                     data = dat_long,
                     group = "age",
                     estimator = "WLSMV",
                     ordered = available_common,
                     std.lv = TRUE,
                     group.equal = c("loadings", "thresholds"))

cat("Scalar model fitted successfully!\n")

## Note: Strict invariance is not tested for ordinal WLSMV here, to avoid
## equal-df comparisons and because residual constraints are not comparable.

# 2·3  Summaries
cat("\n=== MODEL FIT SUMMARIES ===\n")

fit_measures <- lapply(list(configural = configural,
                           metric     = metric,
                           scalar     = scalar),
                      lavaan::fitMeasures, c("chisq","df","cfi","tli","rmsea","srmr"))

# Print fit measures
for(model_name in names(fit_measures)) {
  cat("\n", toupper(model_name), "MODEL:\n")
  fit_vals <- fit_measures[[model_name]]
  for(i in seq_along(fit_vals)) {
    cat(sprintf("  %-8s: %7.3f\n", names(fit_vals)[i], fit_vals[i]))
  }
}

# Create comparison table
cat("\n=== MODEL FIT COMPARISON TABLE ===\n")
fit_comparison <- data.frame(
  Model = c("Configural", "Metric", "Scalar"),
  ChiSq = sapply(fit_measures, function(x) x["chisq"]),
  df = sapply(fit_measures, function(x) x["df"]),
  CFI = sapply(fit_measures, function(x) x["cfi"]),
  TLI = sapply(fit_measures, function(x) x["tli"]),
  RMSEA = sapply(fit_measures, function(x) x["rmsea"]),
  SRMR = sapply(fit_measures, function(x) x["srmr"])
)

# Calculate CFI and RMSEA differences
fit_comparison$CFI_diff <- c(NA, 
                            fit_comparison$CFI[1] - fit_comparison$CFI[2],
                            fit_comparison$CFI[2] - fit_comparison$CFI[3])

fit_comparison$RMSEA_diff <- c(NA,
                              fit_comparison$RMSEA[2] - fit_comparison$RMSEA[1],
                              fit_comparison$RMSEA[3] - fit_comparison$RMSEA[2])

# Round for display
fit_comparison_display <- fit_comparison
fit_comparison_display[, c("ChiSq", "CFI", "TLI", "RMSEA", "SRMR", "CFI_diff", "RMSEA_diff")] <- 
  round(fit_comparison_display[, c("ChiSq", "CFI", "TLI", "RMSEA", "SRMR", "CFI_diff", "RMSEA_diff")], 3)

print(fit_comparison_display)

# 2·4  Model comparison tests
cat("\n=== MODEL COMPARISON TESTS ===\n")

cat("Configural vs Metric (test metric invariance):\n")
diff_test_1 <- lavaan::lavTestLRT(configural, metric)
print(diff_test_1)

cat("\nMetric vs Scalar (test scalar invariance):\n")
diff_test_2 <- lavaan::lavTestLRT(metric, scalar)
print(diff_test_2)

## No LRT for strict vs scalar (strict invariance not fitted for WLSMV ordinals)

#################
## INVARIANCE INTERPRETATION
#################

cat("\n=== MEASUREMENT INVARIANCE INTERPRETATION ===\n")

cat("Interpretation Guidelines:\n")
cat("- CFI decrease > 0.01 suggests lack of invariance\n")
cat("- RMSEA increase > 0.015 suggests lack of invariance\n")
cat("- Significant chi-square difference test suggests lack of invariance\n\n")

# Check metric invariance
metric_cfi_ok <- is.na(fit_comparison$CFI_diff[2]) || fit_comparison$CFI_diff[2] <= 0.01
metric_rmsea_ok <- is.na(fit_comparison$RMSEA_diff[2]) || fit_comparison$RMSEA_diff[2] <= 0.015
metric_chisq_ok <- nrow(diff_test_1) > 1 && diff_test_1$`Pr(>Chisq)`[2] > 0.05

if(metric_cfi_ok && metric_rmsea_ok) {
  cat("✓ METRIC INVARIANCE: SUPPORTED (fit indices)\n")
  cat("  Factor loadings are equivalent across ages 3, 5, 7, 11, 14, and 17\n")
  metric_supported <- TRUE
} else {
  cat("✗ METRIC INVARIANCE: NOT SUPPORTED (fit indices)\n")
  cat("  Factor loadings differ significantly across ages\n")
  metric_supported <- FALSE
}

if(!metric_chisq_ok) {
  cat("  NOTE: Chi-square difference test is significant (p < 0.05)\n")
}

# Check scalar invariance
scalar_cfi_ok <- is.na(fit_comparison$CFI_diff[3]) || fit_comparison$CFI_diff[3] <= 0.01
scalar_rmsea_ok <- is.na(fit_comparison$RMSEA_diff[3]) || fit_comparison$RMSEA_diff[3] <= 0.015
scalar_chisq_ok <- nrow(diff_test_2) > 1 && diff_test_2$`Pr(>Chisq)`[2] > 0.05

if(scalar_cfi_ok && scalar_rmsea_ok) {
  cat("✓ SCALAR INVARIANCE: SUPPORTED (fit indices)\n")
  cat("  Item thresholds are equivalent across ages 3, 5, 7, 11, 14, and 17\n")
  scalar_supported <- TRUE
} else {
  cat("✗ SCALAR INVARIANCE: NOT SUPPORTED (fit indices)\n")
  cat("  Item thresholds differ significantly across ages\n")
  scalar_supported <- FALSE
}

if(!scalar_chisq_ok) {
  cat("  NOTE: Chi-square difference test is significant (p < 0.05)\n")
}

## Strict invariance not evaluated for ordinal WLSMV; focus on metric and scalar only.

#################
## OVERALL CONCLUSION
#################

cat("\n=== OVERALL CONCLUSION ===\n")

if(metric_supported && scalar_supported) {
  cat("GOOD: Strong measurement invariance achieved (metric + scalar).\n")
  cat("The self-control construct can be meaningfully compared across ages 3, 5, 7, 11, 14, and 17.\n")
  cat("Longitudinal mean comparisons are justified.\n")
} else if(metric_supported) {
  cat("PARTIAL: Weak measurement invariance achieved (metric only).\n")
  cat("Factor structure is consistent, but item difficulties vary across ages.\n")
  cat("Relationships with other variables can be compared, but not means.\n")
} else {
  cat("POOR: Measurement invariance not achieved.\n")
  cat("The self-control construct may be measured differently across ages.\n")
  cat("Longitudinal comparisons should be interpreted with caution.\n")
  cat("Consider partial invariance testing or age-specific analyses.\n")
}

#################
## FACTOR LOADINGS BY AGE GROUP
#################

cat("\n=== FACTOR LOADINGS BY AGE GROUP ===\n")

# Extract standardized loadings for each group from configural model
configural_loadings <- standardizedSolution(configural)
loadings_by_group <- configural_loadings[configural_loadings$op == "=~", c("lhs", "rhs", "group", "est.std")]

cat("Standardized factor loadings by age group:\n")
for(grp in 1:6) {
  age_label <- c("Age 3", "Age 5", "Age 7", "Age 11", "Age 14", "Age 17")[grp]
  cat("\n", age_label, ":\n")
  group_loadings <- loadings_by_group[loadings_by_group$group == grp, c("rhs", "est.std")]
  group_loadings$est.std <- round(group_loadings$est.std, 3)
  print(group_loadings)
}

#################
## SAVE RESULTS
#################

cat("\n=== SAVING RESULTS ===\n")
cat("Key objects saved to workspace:\n")
cat("- configural: Baseline model (no constraints)\n")
cat("- metric: Equal loadings model\n") 
cat("- scalar: Equal loadings + thresholds model\n")
## strict model not fitted (ordinal WLSMV)
cat("- fit_comparison: Summary table of all models\n")
cat("- dat_long: Long format dataset used for analysis\n")
cat("- diff_test_1, diff_test_2, diff_test_3: Chi-square difference tests\n")

# Print final sample information
cat("\nFinal sample used for invariance testing:\n")
cat("- Participants:", nrow(dat_wide_complete), "\n")
cat("- Observations:", nrow(dat_long), "\n")
cat("- Age groups: 3, 5, 7, 11, 14, 17\n")
cat("- Variables per age:", length(available_common), "\n")

# Additional diagnostic information
cat("\n=== DIAGNOSTIC INFORMATION ===\n")
cat("Sample sizes by age group:\n")
print(table(dat_long$age))

if(nrow(dat_wide_complete) < 200) {
  cat("\nWARNING: Relatively small sample size may affect model stability.\n")
  cat("Consider using robust estimation methods or larger samples if possible.\n")
}

cat("\nNote: Analysis includes early childhood (ages 3, 5, 7) and adolescent (ages 11, 14, 17) periods.\n")
cat("Developmental changes may be more pronounced across this extended age range.\n")
cat("Analysis completed using lavaan::cfa() with progressive group constraints for robust invariance testing.\n") 





# Score tests for equality constraints (preferable to MIs under constraints)
cat("\n=== SCORE TESTS (SCALAR MODEL) ===\n")
score <- lavTestScore(scalar)
uni <- score$uni
if (!is.null(uni)) {
  uni_df <- as.data.frame(uni)
  if (nrow(uni_df) > 0) {
    stat_candidates <- c("mi", "X2", "stat", "LM.Stat", "LMStat", "score")
    stat_col <- intersect(stat_candidates, names(uni_df))
    if (length(stat_col) == 0) {
      cat("Score test results found, but no MI/stat column detected. Showing first rows.\n")
      print(head(uni_df, 20))
    } else {
      stat_col <- stat_col[1]
      ord <- order(uni_df[[stat_col]], decreasing = TRUE)
      uni_df <- uni_df[ord, , drop = FALSE]
      cols <- intersect(c("lhs","op","rhs","group", stat_col, "epc","epc.lv"), names(uni_df))
      cat("Top parameters to consider freeing (by ", stat_col, "):\n", sep = "")
      print(head(uni_df[, cols, drop = FALSE], 60))
    }
  } else {
    cat("No score test results available (empty table).\n")
  }
} else {
  cat("No score test results available.\n")
}

# Partial Scalar Invariance (leaves small MIs task_completion and think_act)
scalar_partial <- lavaan::cfa(model = cfa_model,
                     data = dat_long,
                     group = "age",
                     estimator = "WLSMV",
                     ordered = available_common,
                     std.lv = TRUE,
                     group.equal = c("loadings", "thresholds"),
                     group.partial = c(
                                   "lying|t1",
                                   "obedient|t1",
                                   "distracted|t1",
                                   "restless|t1",
                                   "temper|t1",
                                   "fidgeting|t1"
                                 ))

summary(scalar_partial, fit.measures = TRUE, standardized = TRUE)

summary(metric, fit.measures = TRUE, standardized = TRUE)


#################
## DATA EXPORT TO CSV
#################

cat("\n=== EXPORTING MEASUREMENT INVARIANCE DATA TO CSV ===\n")

# Create data directory if it doesn't exist
data_dir <- "../data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created data directory:", data_dir, "\n")
}

# Export 1: Wide format complete dataset
cat("Exporting wide format dataset...\n")
write.csv(dat_wide_complete, file.path(data_dir, "measurement_invariance_wide_complete.csv"), 
          row.names = FALSE)
cat("✓ Exported wide format complete dataset (n =", nrow(dat_wide_complete), "participants)\n")

# Export 2: Long format dataset used for invariance testing
cat("Exporting long format dataset...\n")
write.csv(dat_long, file.path(data_dir, "measurement_invariance_long_format.csv"), 
          row.names = FALSE)
cat("✓ Exported long format dataset (n =", nrow(dat_long), "observations)\n")






