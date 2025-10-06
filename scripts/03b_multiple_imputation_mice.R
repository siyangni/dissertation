# =============================================================================
# Multiple Imputation Using mice (two-fold FCS style, adjacent waves)
# =============================================================================
# This script replaces Amelia-based MI with mice, tailored for:
# - Ordinal self-control items at 5 waves (0/1/2)
# - Time-varying covariates (centered parenting measures)
# - Time-invariant covariates (demographics)
# - Two-fold FCS-style conditioning using adjacent waves in predictorMatrix
# - Transformations created BEFORE MI (centered TVCs)
# - Outputs identical artifact: data/imputed_datasets.rds
#
# References:
# - van Buuren, S. (2018). Flexible Imputation of Missing Data (2nd ed.)
# - mice package vignette: https://cran.r-project.org/package=mice
# =============================================================================

library(pacman)
p_load(mice, tidyverse, future, furrr)

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)
cat(sprintf("\n=== Parallel processing enabled with %d cores ===\n", parallel::detectCores() - 1))

stopifnot(exists("merged_data"))

# =============================================================================
# 1) Identify variables
# =============================================================================

sc_items_age5  <- paste0("sc5_",  c("task_completion","distracted","fidgeting","think_act","temper","obedient","lying"))
sc_items_age7  <- paste0("sc7_",  c("task_completion","distracted","fidgeting","think_act","temper","obedient","lying"))
sc_items_age11 <- paste0("sc11_", c("task_completion","distracted","fidgeting","think_act","temper","obedient","lying"))
sc_items_age14 <- paste0("sc14_", c("task_completion","distracted","fidgeting","think_act","temper","obedient","lying"))
sc_items_age17 <- paste0("sc17_", c("task_completion","distracted","fidgeting","think_act","temper","obedient","lying"))
sc_items_all   <- c(sc_items_age5, sc_items_age7, sc_items_age11, sc_items_age14, sc_items_age17)

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
# 2) Create transformations BEFORE MI (centered TVCs)
# =============================================================================

cat("\n=== Creating centered TVCs (pre-MI) ===\n")
if ("parenting_age5" %in% names(merged_data)) {
  merged_data$parenting_age5_c <- as.numeric(scale(merged_data$parenting_age5, center = TRUE, scale = FALSE))
}
if ("parenting_age7" %in% names(merged_data)) {
  merged_data$parenting_age7_c <- as.numeric(scale(merged_data$parenting_age7, center = TRUE, scale = FALSE))
}
if ("parenting_age11" %in% names(merged_data)) {
  merged_data$parenting_age11_c <- as.numeric(scale(merged_data$parenting_age11, center = TRUE, scale = FALSE))
}
if ("parental_monitoring_age14" %in% names(merged_data)) {
  merged_data$pm14_c <- as.numeric(scale(merged_data$parental_monitoring_age14, center = TRUE, scale = FALSE))
}
if ("ngcoutw00" %in% names(merged_data)) {
  merged_data$pm17_c <- as.numeric(scale(merged_data$ngcoutw00, center = TRUE, scale = FALSE))
}

tvcs_centered <- c("parenting_age5_c","parenting_age7_c","parenting_age11_c","pm14_c","pm17_c")

# We impute centered TVCs (not raw) to avoid collinearity
vars_to_impute <- c(sc_items_all, tvcs_centered, tics)

auxiliary_vars <- c("weight1")

available_vars <- intersect(c(vars_to_impute, auxiliary_vars), names(merged_data))
missing_vars   <- setdiff(c(vars_to_impute, auxiliary_vars), names(merged_data))
if (length(missing_vars)) cat("Missing in data (skipped):", paste(missing_vars, collapse = ", "), "\n")

id_var <- if ("mcsid" %in% names(merged_data)) "mcsid" else NULL
if (!is.null(id_var)) {
  imputation_data <- merged_data[, c(id_var, available_vars)]
} else {
  merged_data$.imputation_row_id <- seq_len(nrow(merged_data))
  imputation_data <- merged_data[, c(".imputation_row_id", available_vars)]
  id_var <- ".imputation_row_id"
}

cat("\n=== Variables in MI model ===\n")
cat(sprintf("Total: %d\n", length(available_vars)))
cat(sprintf("  - SC items: %d\n", sum(available_vars %in% sc_items_all)))
cat(sprintf("  - Centered TVCs: %d\n", sum(available_vars %in% tvcs_centered)))
cat(sprintf("  - TICs: %d\n\n", sum(available_vars %in% tics)))

# =============================================================================
# 3) Prepare types for mice
# =============================================================================

cat("=== Preparing data types for mice ===\n")
sc_items_present <- intersect(sc_items_all, available_vars)
if (length(sc_items_present) > 0) {
  for (v in sc_items_present) {
    imputation_data[[v]] <- factor(imputation_data[[v]], levels = c(0,1,2), ordered = TRUE)
  }
  cat(sprintf("Converted %d SC items to ordered factors (0<1<2).\n", length(sc_items_present)))
}

binary_vars <- c("sex","low_birthweight","heavy_fetal_alcohol_exposure","marital_status","parents_education_binary")
binary_present <- intersect(binary_vars, available_vars)
noms <- intersect("race", available_vars)
continuous_vars <- setdiff(available_vars, c(sc_items_present, binary_present, noms, "weight1"))

# Ensure types match mice methods:
# - Binary as 2-level factors for logreg
# - Nominal as factor for polyreg
# - Continuous strictly numeric for norm
if (length(binary_present) > 0) {
  for (v in binary_present) {
    imputation_data[[v]] <- factor(imputation_data[[v]], levels = c(0, 1))
  }
}
if (length(noms) == 1) {
  imputation_data[[noms]] <- factor(imputation_data[[noms]])
}
if (length(continuous_vars) > 0) {
  for (v in continuous_vars) {
    imputation_data[[v]] <- suppressWarnings(as.numeric(imputation_data[[v]]))
  }
}

# =============================================================================
# 4) Two-fold FCS-style predictor matrix (adjacent waves)
# =============================================================================

cat("=== Building predictor matrix (adjacent waves) ===\n")
pred <- matrix(0, nrow = length(available_vars), ncol = length(available_vars),
               dimnames = list(available_vars, available_vars))

add_adjacent <- function(item_key) {
  v5  <- paste0("sc5_",  item_key)
  v7  <- paste0("sc7_",  item_key)
  v11 <- paste0("sc11_", item_key)
  v14 <- paste0("sc14_", item_key)
  v17 <- paste0("sc17_", item_key)
  vs  <- intersect(c(v5,v7,v11,v14,v17), available_vars)
  for (v in vs) {
    neigh <- character(0)
    if (v == v5)  neigh <- intersect(v7, available_vars)
    if (v == v7)  neigh <- intersect(c(v5,v11), available_vars)
    if (v == v11) neigh <- intersect(c(v7,v14), available_vars)
    if (v == v14) neigh <- intersect(c(v11,v17), available_vars)
    if (v == v17) neigh <- intersect(v14, available_vars)
    pred[v, neigh] <<- 1
  }
}

for (key in c("task_completion","distracted","fidgeting","think_act","temper","obedient","lying")) add_adjacent(key)

# Parenting TVCs: adjacent waves
adj_pairs <- list(
  c("parenting_age5_c","parenting_age7_c"),
  c("parenting_age7_c","parenting_age11_c"),
  c("parenting_age11_c","pm14_c"),
  c("pm14_c","pm17_c")
)
for (p in adj_pairs) {
  a <- p[1]; b <- p[2]
  if (all(c(a,b) %in% available_vars)) {
    pred[a, b] <- 1
    pred[b, a] <- 1
  }
}

# TICs predict everything
for (tic in intersect(tics, available_vars)) pred[, tic] <- 1

# weight1 predicts but is not imputed
if ("weight1" %in% available_vars) pred[, "weight1"] <- 1
diag(pred) <- 0

cat("Predictor matrix ready.\n\n")

# =============================================================================
# 5) Run mice
# =============================================================================

cat("=== Running mice Multiple Imputation (m=3) in PARALLEL ===\n")
set.seed(123)

method <- rep("", length(available_vars))
names(method) <- available_vars
method[sc_items_present] <- "polr"     # ordinal proportional odds
method[binary_present]   <- "logreg"   # binary logistic
if (length(noms) == 1) method[noms] <- "polyreg"   # nominal
method[continuous_vars]  <- "norm"     # Gaussian
if ("weight1" %in% available_vars) method["weight1"] <- ""  # no imputation

data_for_mice <- imputation_data[, available_vars, drop = FALSE]

imp <- futuremice(data_for_mice,
                  m = 3,
                  maxit = 3,
                  method = method,
                  predictorMatrix = pred,
                  print = TRUE,
                  parallelseed = 123,
                  n.core = 11)  # Uses future plan workers

cat("\n=== mice Imputation Complete ===\n")

# =============================================================================
# 6) Build completed datasets back into full data
# =============================================================================

cat("\n=== Creating Completed Datasets ===\n")
imputed_datasets <- lapply(seq_len(imp$m), function(i) {
  imp_data <- complete(imp, action = i)
  # Convert ordered factors for SC items back to integer 0/1/2
  for (v in sc_items_present) if (v %in% names(imp_data)) imp_data[[v]] <- as.integer(as.character(imp_data[[v]]))
  # Convert binary factors back to 0/1
  for (v in binary_present) if (v %in% names(imp_data)) imp_data[[v]] <- as.integer(as.character(imp_data[[v]]))
  # Convert nominal race back to numeric
  if (length(noms) == 1 && noms %in% names(imp_data)) imp_data[[noms]] <- as.integer(as.character(imp_data[[noms]]))
  full_data <- merged_data
  for (var in intersect(vars_to_impute, names(imp_data))) full_data[[var]] <- imp_data[[var]]
  full_data
})

cat(sprintf("Created %d completed datasets.\n", length(imputed_datasets)))
cat(sprintf("Each dataset contains %d observations and %d variables.\n", nrow(imputed_datasets[[1]]), ncol(imputed_datasets[[1]])))

# =============================================================================
# 7) Save outputs
# =============================================================================
saveRDS(imputed_datasets, file = "/home/siyang/dissertation_folder/data/imputed_datasets.rds")
saveRDS(imp,               file = "/home/siyang/dissertation_folder/data/mice_imp_object.rds")
cat("Saved: /home/siyang/dissertation_folder/data/imputed_datasets.rds and /home/siyang/dissertation_folder/data/mice_imp_object.rds\n")

# =============================================================================
# 8) Diagnostics
# =============================================================================

cat("\n=== Diagnostics (mice) ===\n")
cat("Missing data pattern (first 12 vars):\n")
print(md.pattern(data_for_mice[, head(colnames(data_for_mice), 12), drop = FALSE]))
cat("\nLogged events (first 10):\n")
print(head(imp$loggedEvents, 10))

cat("\n=== MI with mice complete. ===\n")

# Close parallel workers
plan(sequential)
cat("Parallel processing workers closed.\n")

