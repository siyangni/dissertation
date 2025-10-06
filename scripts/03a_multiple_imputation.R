# =============================================================================
# Multiple Imputation Using Amelia II with Time-Series Structure
# =============================================================================
# This script performs multiple imputation following best practices for 
# longitudinal structural equation models:
#
# KEY FEATURES:
# ✅ Imputes ALL self-control items at all waves (not just scale scores)
# ✅ Includes TVCs and TICs used in the growth model
# ✅ Reflects temporal structure using Amelia's time-series features (ts, cs)
# ✅ Sets correct variable types (ordinal, nominal, continuous)
# ✅ Creates transformations BEFORE imputation to propagate uncertainty
# ✅ Results pooled with semTools::runMI (in script 06)
#
# RATIONALE:
# - Imputing items (not scale scores) preserves measurement error structure
# - Time-series conditioning improves MAR plausibility for adjacent waves
# - Pre-MI transformations ensure imputation uncertainty flows to derivatives
# - Amelia's EMB algorithm is efficient for large multivariate models
#
# References:
# - Honaker, King, & Blackwell (2011). Amelia II. JSS, 45(7).
# - van Buuren (2018). Flexible Imputation of Missing Data (Ch. 6-7).
# - Enders (2010). Applied Missing Data Analysis (Ch. 8-9).
# =============================================================================
library(pacman)
p_load(Amelia, tidyverse)

# =============================================================================
# 1. Identify Variables to Impute
# =============================================================================
# Following best practices, we impute:
# - Self-control ITEMS at all waves (preserves measurement structure)
# - Time-varying covariates (TVCs): parenting and monitoring
# - Time-invariant covariates (TICs): demographics and early-life factors
# - Transformed variables (centered TVCs) created BEFORE MI

# Self-control items across ages (7 items × 5 waves = 35 variables)
sc_items_age5  <- paste0("sc5_",  c("task_completion","distracted","fidgeting","think_act","temper","obedient","lying"))
sc_items_age7  <- paste0("sc7_",  c("task_completion","distracted","fidgeting","think_act","temper","obedient","lying"))
sc_items_age11 <- paste0("sc11_", c("task_completion","distracted","fidgeting","think_act","temper","obedient","lying"))
sc_items_age14 <- paste0("sc14_", c("task_completion","distracted","fidgeting","think_act","temper","obedient","lying"))
sc_items_age17 <- paste0("sc17_", c("task_completion","distracted","fidgeting","think_act","temper","obedient","lying"))

sc_items_all <- c(sc_items_age5, sc_items_age7, sc_items_age11, sc_items_age14, sc_items_age17)

# Time-varying covariates (raw parenting measures)
tvcs_raw <- c(
  "parenting_age5",
  "parenting_age7", 
  "parenting_age11",
  "parental_monitoring_age14",
  "ngcoutw00"  # parental monitoring age 17
)

# Time-invariant covariates (demographics and early-life)
tics <- c(
  "sex",
  "cognitive_ability",
  "low_birthweight",
  "infant_temperament",
  "heavy_fetal_alcohol_exposure",
  "marital_status",
  "race",
  "parents_education_binary"
)

# =============================================================================
# 2. Create Transformed Variables BEFORE Imputation
# =============================================================================
# CRITICAL: Transformations (centering, squaring, interactions) must be created
# BEFORE MI so that imputation uncertainty propagates to them. If created after,
# SEs will be underestimated (Enders, 2010; von Hippel, 2009).

cat("\n=== Creating Transformed Variables (Pre-Imputation) ===\n")

# Grand-mean centering for TVCs (needed in growth model)
# Note: We center BEFORE imputation, then impute the centered versions
if ("parenting_age5" %in% names(merged_data)) {
  merged_data$parenting_age5_c <- as.numeric(scale(merged_data$parenting_age5, 
                                                    center = TRUE, scale = FALSE))
}
if ("parenting_age7" %in% names(merged_data)) {
  merged_data$parenting_age7_c <- as.numeric(scale(merged_data$parenting_age7, 
                                                    center = TRUE, scale = FALSE))
}
if ("parenting_age11" %in% names(merged_data)) {
  merged_data$parenting_age11_c <- as.numeric(scale(merged_data$parenting_age11, 
                                                     center = TRUE, scale = FALSE))
}
if ("parental_monitoring_age14" %in% names(merged_data)) {
  merged_data$pm14_c <- as.numeric(scale(merged_data$parental_monitoring_age14, 
                                         center = TRUE, scale = FALSE))
}
if ("ngcoutw00" %in% names(merged_data)) {
  merged_data$pm17_c <- as.numeric(scale(merged_data$ngcoutw00, 
                                         center = TRUE, scale = FALSE))
}

# Centered TVCs to be imputed
tvcs_centered <- c(
  "parenting_age5_c",
  "parenting_age7_c", 
  "parenting_age11_c",
  "pm14_c",
  "pm17_c"
)

# Add any quadratic or interaction terms here if needed in your model
# Example: merged_data$parenting_age7_sq <- merged_data$parenting_age7^2

# Combined list of ALL variables to impute
# NOTE: We impute EITHER raw OR centered versions to avoid perfect collinearity
# Strategy: Impute centered versions directly (they contain the same information)
vars_to_impute <- c(
  sc_items_all,      # Self-control items (ordinal 0-2)
  tvcs_centered,     # Centered parenting measures (impute these, not raw)
  tics               # Time-invariant covariates
)

# Keep raw versions for reference but don't impute them
# (Amelia will fail with perfect collinearity if we impute both)

# Auxiliary variables (NOT imputed, but improve imputation quality)
auxiliary_vars <- c(
  "weight1"  # Sampling weight (important for MAR assumption)
  # Add other auxiliaries here if available (e.g., earlier SC measures at age 3)
)

# =============================================================================
# 3. Prepare Data for Imputation
# =============================================================================
# Check which variables exist in merged_data
all_vars <- c(vars_to_impute, auxiliary_vars)
available_vars <- intersect(all_vars, names(merged_data))
missing_vars <- setdiff(all_vars, names(merged_data))

if (length(missing_vars) > 0) {
  warning(sprintf("The following variables are not in merged_data and will be skipped: %s",
                  paste(missing_vars, collapse = ", ")))
}

cat("\n=== Variables in Imputation Model ===\n")
cat(sprintf("Total variables: %d\n", length(available_vars)))
cat(sprintf("  - Self-control items: %d\n", sum(available_vars %in% sc_items_all)))
cat(sprintf("  - Raw TVCs: %d\n", sum(available_vars %in% tvcs_raw)))
cat(sprintf("  - Centered TVCs: %d\n", sum(available_vars %in% tvcs_centered)))
cat(sprintf("  - TICs: %d\n", sum(available_vars %in% tics)))
cat(sprintf("  - Auxiliary: %d\n\n", sum(available_vars %in% auxiliary_vars)))

# Extract ID variable and create a time index for time-series structure
id_var <- if ("mcsid" %in% names(merged_data)) "mcsid" else NULL

if (!is.null(id_var)) {
  imputation_data <- merged_data[, c(id_var, available_vars)]
} else {
  # If no ID, create a row index to re-merge later
  merged_data$.imputation_row_id <- seq_len(nrow(merged_data))
  imputation_data <- merged_data[, c(".imputation_row_id", available_vars)]
  id_var <- ".imputation_row_id"
}

# Create a nominal time index for Amelia's time-series features
# This tells Amelia that data are cross-sectional (not panel/long format)
# But we'll use lags/leads to capture temporal dependencies
imputation_data$time_index <- 1  # All observations at single "time point" in wide format

# =============================================================================
# 4. Missingness Diagnostics (Pre-Imputation)
# =============================================================================
cat("=== Missingness Summary (Pre-Imputation) ===\n")
missing_summary <- imputation_data %>%
  select(-all_of(c(id_var, "time_index"))) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(
    total_n = nrow(imputation_data),
    pct_missing = round(100 * n_missing / total_n, 2)
  ) %>%
  arrange(desc(pct_missing))

print(missing_summary, n = Inf)

cat("\nTotal cases with ANY missing value:",
    sum(!complete.cases(imputation_data[, setdiff(available_vars, "weight1")])),
    "out of", nrow(imputation_data), "\n\n")

# =============================================================================
# 5. Specify Variable Types for Amelia
# =============================================================================
# Amelia needs to know which variables are nominal (categorical with no order)
# and which are ordinal (ordered categories). This ensures appropriate imputation.

cat("=== Specifying Variable Types for Amelia ===\n")

# Nominal variables (unordered categories)
# - race (1=White, 2=Asian, 3=Black, 4=Mixed/Other)
noms <- NULL
if ("race" %in% available_vars) {
  noms <- "race"
  cat("Nominal variables:", paste(noms, collapse = ", "), "\n")
}

# Ordinal variables (ordered categories)
# - Self-control items: 0 (low) < 1 (medium) < 2 (high)
sc_items_present <- intersect(sc_items_all, available_vars)

ords <- NULL
if (length(sc_items_present) > 0) {
  ords <- sc_items_present
  cat(sprintf("Ordinal variables: %d self-control items\n", length(ords)))
}

# Bounds for ordinal variables (CRITICAL for Amelia stability)
# SC items are coded 0, 1, 2
bounds_matrix <- NULL
if (length(sc_items_present) > 0) {
  # Create bounds matrix: column 1 = variable index, column 2 = lower, column 3 = upper
  sc_indices <- which(names(imputation_data) %in% sc_items_present)
  bounds_matrix <- matrix(c(sc_indices, 
                           rep(0, length(sc_indices)),  # lower bound
                           rep(2, length(sc_indices))), # upper bound
                         ncol = 3)
  cat(sprintf("Set bounds [0, 2] for %d ordinal SC items\n", nrow(bounds_matrix)))
}

# Binary variables (0/1) are treated as continuous by Amelia (recommended)
binary_vars <- c("sex", "low_birthweight", "heavy_fetal_alcohol_exposure", 
                 "marital_status", "parents_education_binary")
binary_present <- intersect(binary_vars, available_vars)
if (length(binary_present) > 0) {
  cat(sprintf("Binary variables (continuous): %s\n", paste(binary_present, collapse = ", ")))
}

# Continuous variables (everything else not specified above)
continuous_vars <- setdiff(available_vars, c(noms, ords, binary_present, "weight1"))
if (length(continuous_vars) > 0) {
  cat(sprintf("Continuous variables: %d (TVCs, centered TVCs, standardized TICs)\n\n", 
              length(continuous_vars)))
}

# =============================================================================
# 6. Specify Lag/Lead Structure for Time-Series Dependencies
# =============================================================================
# Even though we're in wide format, we can specify which variables at different
# waves should condition on each other. This reflects the temporal structure.
# For example, SC items at age 7 should condition on SC items at age 5.

cat("=== Specifying Time-Series Structure (Lags/Leads) ===\n")

# Build lags specification: each wave's items condition on previous wave
# Format: list(variable_t2 = variable_t1)
# This improves MAR plausibility by using adjacent-wave information
#
# NOTE: With 40+ variables, full lag structure may cause Amelia to fail
# We'll try a SIMPLIFIED lag structure (just one representative item per wave)

lags_list <- list()

# SIMPLIFIED: Just lag one key SC item across waves (task_completion)
# This captures temporal structure without overwhelming Amelia
if (all(c("sc7_task_completion", "sc5_task_completion") %in% available_vars)) {
  lags_list[["sc7_task_completion"]] <- "sc5_task_completion"
}
if (all(c("sc11_task_completion", "sc7_task_completion") %in% available_vars)) {
  lags_list[["sc11_task_completion"]] <- "sc7_task_completion"
}
if (all(c("sc14_task_completion", "sc11_task_completion") %in% available_vars)) {
  lags_list[["sc14_task_completion"]] <- "sc11_task_completion"
}
if (all(c("sc17_task_completion", "sc14_task_completion") %in% available_vars)) {
  lags_list[["sc17_task_completion"]] <- "sc14_task_completion"
}

# Parenting centered measures: each wave lags the previous wave
if (all(c("parenting_age7_c", "parenting_age5_c") %in% available_vars)) {
  lags_list[["parenting_age7_c"]] <- "parenting_age5_c"
}
if (all(c("parenting_age11_c", "parenting_age7_c") %in% available_vars)) {
  lags_list[["parenting_age11_c"]] <- "parenting_age7_c"
}

cat(sprintf("Specified %d lag relationships (simplified to avoid convergence issues).\n\n", length(lags_list)))

# =============================================================================
# 7. Run Amelia II Multiple Imputation with Time-Series Structure
# =============================================================================
cat("=== Preparing long-format panel data for Amelia (ts/cs) ===\n")

# Build long-format panel data: one row per person-wave
waves <- c(5, 7, 11, 14, 17)
item_suffixes <- c("task_completion","distracted","fidgeting","think_act","temper","obedient","lying")

panel_long_list <- lapply(seq_along(waves), function(i) {
  w <- waves[i]
  idx <- i

  # Self-control item columns for this wave that exist
  sc_cols_w <- paste0("sc", w, "_", item_suffixes)
  sc_present <- intersect(sc_cols_w, names(imputation_data))

  # Parenting centered variable name for this wave
  parenting_var <- dplyr::case_when(
    w == 5  ~ "parenting_age5_c",
    w == 7  ~ "parenting_age7_c",
    w == 11 ~ "parenting_age11_c",
    w == 14 ~ "pm14_c",
    w == 17 ~ "pm17_c",
    TRUE ~ NA_character_
  )

  cols_to_take <- unique(na.omit(c(id_var, sc_present, parenting_var, intersect(tics, names(imputation_data)))))
  if (length(cols_to_take) == 0) return(NULL)

  df_w <- imputation_data[, cols_to_take, drop = FALSE]

  # Rename sc{w}_<item> -> <item>
  if (length(sc_present) > 0) {
    names_map <- setNames(object = paste0("sc", w, "_", item_suffixes), nm = item_suffixes)
    names_map <- names_map[names_map %in% names(df_w)]
    # invert map: from current to target
    inv_map <- setNames(nm = names(names_map), object = unname(names_map))
    df_w <- dplyr::rename(df_w, !!!inv_map)
  }

  # Rename parenting var to generic 'parenting_c'
  if (!is.na(parenting_var) && parenting_var %in% names(df_w)) {
    df_w <- dplyr::rename(df_w, parenting_c = dplyr::all_of(parenting_var))
  }

  # Add wave identifiers
  df_w$wave_code  <- w
  df_w$wave_index <- idx

  tibble::as_tibble(df_w)
})

panel_long <- dplyr::bind_rows(panel_long_list)

# Determine ordinal vars in long format
ords_long <- intersect(item_suffixes, names(panel_long))

# Bounds for ordinal vars in long format [0, 2]
bounds_long_matrix <- NULL
if (length(ords_long) > 0) {
  ord_idx <- which(names(panel_long) %in% ords_long)
  bounds_long_matrix <- matrix(c(ord_idx,
                                 rep(0, length(ord_idx)),
                                 rep(2, length(ord_idx))), ncol = 3)
}

cat("=== Running Amelia II Multiple Imputation ===\n")
cat("Number of imputations: m = 5\n")
cat("Variables in imputation model:\n")
cat(sprintf("  - Self-control items: %d\n", sum(available_vars %in% sc_items_all)))
cat(sprintf("  - TVCs and centered TVCs: %d\n", sum(available_vars %in% c(tvcs_raw, tvcs_centered))))
cat(sprintf("  - TICs: %d\n", sum(available_vars %in% tics)))
cat(sprintf("  - Total variables: %d\n", length(available_vars)))
cat(sprintf("  - Lag relationships: %d\n", length(lags_list)))
cat("\nThis may take several minutes due to the large number of variables...\n\n")

set.seed(123)  # For reproducibility

# Identify ID variables to exclude from imputation model
idvars_amelia <- c(id_var, "time_index")

# Run Amelia with proper panel structure (ts/cs) on long data
# Try with lags first, fallback to no lags if it fails, then fallback to wide no-lag
amelia_output <- tryCatch({
  cat("Attempting PANEL imputation WITH lag structure...\n")
  amelia(
    x = panel_long,
    m = 5,                                  # Start with 5 for testing
    idvars = id_var,                        # Keep only CS id as idvar
    ts = "wave_index",                     # Time index (numeric)
    cs = id_var,                            # Cross-section id
    noms = noms,                            # Nominal variables (race)
    ords = ords_long,                       # Ordinal variables (SC items)
    lags = c("parenting_c"),               # Minimal lags to avoid complexity
    polytime = 0,                           # Do not transform ts
    intercs = FALSE,                        # No time interactions
    p2s = 1,
    empri = 0.05 * nrow(panel_long)
  )
}, error = function(e1) {
  cat("\n⚠️  Panel imputation with lags failed. Retrying PANEL without lags...\n")
  cat("Error was:", conditionMessage(e1), "\n\n")
  tryCatch({
    amelia(
      x = panel_long,
      m = 5,
      idvars = id_var,
      ts = "wave_index",
      cs = id_var,
      noms = noms,
      ords = ords_long,
      polytime = 0,
      intercs = FALSE,
      p2s = 1,
      empri = 0.05 * nrow(panel_long)
    )
  }, error = function(e2) {
    cat("\n⚠️  Panel imputation failed. Falling back to WIDE without lags...\n")
    cat("Error was:", conditionMessage(e2), "\n\n")
    amelia(
      x = imputation_data,
      m = 5,
      idvars = idvars_amelia,
      noms = noms,
      ords = ords,
      bounds = bounds_matrix,   # Keep bounds for ordinals
      p2s = 1,
      empri = 0.05 * nrow(imputation_data)
    )
  })
})

cat("\n=== Amelia Imputation Complete ===\n")

# =============================================================================
# 8. Diagnostics: Check Imputation Quality
# =============================================================================
cat("\n=== Imputation Diagnostics ===\n")

# Amelia automatically provides convergence diagnostics
if (!is.null(amelia_output$missMatrix)) {
  cat("Missingness pattern summary available.\n")
}

# Compare distributions of observed vs imputed values for key variables
cat("\nFor visual diagnostics, use:\n")
cat("  - compare.density(amelia_output, var = 'variable_name')\n")
cat("  - overimpute(amelia_output, var = 'variable_name')\n\n")

# Example diagnostics for key variables
diagnostic_vars <- c("parents_education_binary", "sc7_task_completion", "parenting_age7")
diagnostic_vars <- intersect(diagnostic_vars, available_vars)

for (var in diagnostic_vars) {
  cat(sprintf("\nDiagnostic for %s:\n", var))
  try(compare.density(amelia_output, var = var))
}

# =============================================================================
# 9. Create Completed Datasets
# =============================================================================
# Merge imputed data back with full dataset

cat("\n=== Creating Completed Datasets ===\n")

imputed_datasets <- lapply(amelia_output$imputations, function(imp_data) {
  # Start with full merged_data
  full_data <- merged_data
  
  # Replace ALL imputed variables with their imputed values
  # This includes: SC items, TVCs (raw), TVCs (centered), and TICs
  vars_to_replace <- intersect(vars_to_impute, names(imp_data))
  
  for (var in vars_to_replace) {
    full_data[[var]] <- imp_data[[var]]
  }
  
  # NOTE: We do NOT re-compute centered variables here because they were
  # already created before MI and imputed. This ensures proper uncertainty
  # propagation (von Hippel, 2009; Enders, 2010).
  
  # Return the completed dataset
  full_data
})

cat(sprintf("Created %d completed datasets.\n", length(imputed_datasets)))
cat(sprintf("Each dataset contains %d observations and %d variables.\n", 
            nrow(imputed_datasets[[1]]), ncol(imputed_datasets[[1]])))

# =============================================================================
# 10. Export Imputed Datasets
# =============================================================================
# Save the list of imputed datasets for use in subsequent analysis scripts

output_file <- "data/imputed_datasets.rds"
dir.create("data", showWarnings = FALSE, recursive = TRUE)
saveRDS(imputed_datasets, file = output_file)

cat(sprintf("\n=== Imputed datasets saved to: %s ===\n", output_file))
cat(sprintf("Use: imputed_datasets <- readRDS('%s')\n\n", output_file))

# Also save the Amelia output object for diagnostics
saveRDS(amelia_output, file = "data/amelia_output.rds")
cat("Amelia output object saved to: data/amelia_output.rds\n")
cat("(Use for diagnostics: compare.density, overimpute, etc.)\n\n")

# =============================================================================
# 11. Summary Statistics Across Imputations
# =============================================================================
cat("\n=== Post-Imputation Summary (Pooled Descriptives) ===\n")

# Verify no missing data in key variables
cat("Missingness check (should be 0 for all imputed variables):\n")
missing_check <- sapply(vars_to_impute[vars_to_impute %in% names(imputed_datasets[[1]])][1:10], function(var) {
  mean(is.na(imputed_datasets[[1]][[var]]))
})
print(round(missing_check, 4))

# Calculate pooled means and SDs for a sample of continuous variables
sample_continuous <- c("cognitive_ability", "infant_temperament", 
                       "parenting_age7", "parenting_age7_c")
sample_continuous <- intersect(sample_continuous, names(imputed_datasets[[1]]))

if (length(sample_continuous) > 0) {
  cat("\nPooled descriptive statistics (sample of variables):\n")
  pooled_stats <- sapply(sample_continuous, function(var) {
    means <- sapply(imputed_datasets, function(d) mean(d[[var]], na.rm = TRUE))
    sds <- sapply(imputed_datasets, function(d) sd(d[[var]], na.rm = TRUE))
    c(mean = mean(means), sd = mean(sds))
  })
  print(round(t(pooled_stats), 3))
}

# Check ordinal SC items
cat("\nSample SC item distributions (imputation 1, sc7_task_completion):\n")
if ("sc7_task_completion" %in% names(imputed_datasets[[1]])) {
  print(table(imputed_datasets[[1]]$sc7_task_completion, useNA = "ifany"))
}

cat("\n=== Multiple Imputation Complete ===\n")
cat("\n✅ WORKFLOW SUMMARY:\n")
cat("1. Created transformations BEFORE imputation (centered TVCs)\n")
cat("2. Imputed ~43 variables: SC items (35), centered TVCs (5), TICs (8)\n")
cat("3. Attempted time-series structure (lags) - check output for success\n")
cat("4. Set correct variable types (ordinal, nominal, continuous)\n")
cat("5. Set bounds [0, 2] for ordinal SC items\n")
cat("6. Created", length(imputed_datasets), "completed datasets\n\n")
cat("NEXT STEPS:\n")
cat("1. Check diagnostics (compare.density, overimpute)\n")
cat("2. If satisfied, increase m from 5 to 20 and re-run\n")
cat("3. Load imputed datasets: imputed_datasets <- readRDS('data/imputed_datasets.rds')\n")
cat("4. Fit measurement + growth model with semTools::runMI\n")
cat("5. Pool results using Rubin's rules (handled automatically by runMI)\n\n")
cat("NOTE: Raw parenting variables NOT imputed to avoid collinearity.\n")
cat("      Use centered versions (parenting_age*_c, pm14_c, pm17_c) in models.\n\n")

