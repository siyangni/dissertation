library(pacman)
p_load(stats, car, foreign, svMisc, devtools, roxygen2, lattice, psych, lavaan, semTools, psych, VIM, semPlot, ggplot2, tidyverse, parallel)

# Set up parallel processing for model fitting
n_cores <- detectCores()
cat("Detected", n_cores, "CPU cores. Setting up parallel processing...\n")

# Set up cluster for parallel processing
cl <- makeCluster(n_cores)
clusterEvalQ(cl, library(lavaan))

cat("=== LATENT GROWTH CURVE MODEL ===\n")
cat("=== SELF-CONTROL DEVELOPMENT: AGES 3, 5, 7, 11, 14, 17 ===\n")
cat("=== COMPREHENSIVE DEVELOPMENTAL MODEL ===\n\n")

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

cat("Variables for growth modeling:\n")
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
  cat("Note: Run the age-specific recode scripts first to create these variables\n")
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

# Create longitudinal dataset with complete cases across all ages
all_vars_available <- c(sc3_vars[sc3_vars %in% names(merged_data)],
                       sc5_vars[sc5_vars %in% names(merged_data)],
                       sc7_vars[sc7_vars %in% names(merged_data)],
                       sc11_vars[sc11_vars %in% names(merged_data)],
                       sc14_vars[sc14_vars %in% names(merged_data)],
                       sc17_vars[sc17_vars %in% names(merged_data)])

longitudinal_data <- merged_data[, all_vars_available, drop = FALSE]
longitudinal_complete <- na.omit(longitudinal_data)

# Check sample size first
cat("\n=== SAMPLE SIZE ANALYSIS ===\n")
cat("Original dataset:", nrow(merged_data), "\n")
cat("Complete longitudinal data:", nrow(longitudinal_complete), "\n")
cat("Retention rate:", round(nrow(longitudinal_complete)/nrow(merged_data)*100, 1), "%\n")

if(nrow(longitudinal_complete) < 100) {
  warning("Small sample size (< 100) may affect reliability of growth modeling results")
}

# Check if we have sufficient sample size for this complex model
n_params_estimate <- 24*6 + 26 + 15  # rough estimate: 6 measurement models + growth + covariances
recommended_n <- n_params_estimate * 10
cat("Estimated parameters:", n_params_estimate, "\n")
cat("Recommended sample size (10:1 ratio):", recommended_n, "\n")
cat("Current sample size adequate:", nrow(longitudinal_complete) >= recommended_n, "\n\n")

# Additional sample size checks for individual ages
cat("=== INDIVIDUAL AGE SAMPLE SIZES ===\n")
for(age in c("3", "5", "7", "11", "14", "17")) {
  age_vars <- get(paste0("sc", age, "_vars"))
  age_data <- merged_data[, age_vars[age_vars %in% names(merged_data)], drop = FALSE]
  age_complete <- na.omit(age_data)
  cat("Age", age, "- Complete cases:", nrow(age_complete), "/", nrow(merged_data), 
      "(", round(nrow(age_complete)/nrow(merged_data)*100, 1), "%)\n")
}

#################
## LATENT GROWTH CURVE MODEL SPECIFICATION
#################

cat("\n=== MODEL SPECIFICATION ===\n")
cat("Time coding (years from age 3):\n")
cat("Age 3: 0, Age 5: 2, Age 7: 4, Age 11: 8, Age 14: 11, Age 17: 14\n\n")

# 1) Measurement part -------------------------------------------------
meas <- '
# Weak+Strong invariance in one step (after testing separately)
SC3 =~ 1*sc3_task_completion + a2*sc3_distracted + a3*sc3_fidgeting +
       a4*sc3_think_act     + a5*sc3_restless   + a6*sc3_temper +
       a7*sc3_obedient      + a8*sc3_lying
SC5 =~ 1*sc5_task_completion + a2*sc5_distracted + a3*sc5_fidgeting +
       a4*sc5_think_act     + a5*sc5_restless   + a6*sc5_temper +
       a7*sc5_obedient      + a8*sc5_lying
SC7 =~ 1*sc7_task_completion + a2*sc7_distracted + a3*sc7_fidgeting +
       a4*sc7_think_act     + a5*sc7_restless   + a6*sc7_temper +
       a7*sc7_obedient      + a8*sc7_lying
SC11 =~ 1*sc11_task_completion + a2*sc11_distracted + a3*sc11_fidgeting +
        a4*sc11_think_act     + a5*sc11_restless   + a6*sc11_temper +
        a7*sc11_obedient      + a8*sc11_lying
SC14 =~ 1*sc14_task_completion + a2*sc14_distracted + a3*sc14_fidgeting +
        a4*sc14_think_act     + a5*sc14_restless   + a6*sc14_temper +
        a7*sc14_obedient      + a8*sc14_lying
SC17 =~ 1*sc17_task_completion + a2*sc17_distracted + a3*sc17_fidgeting +
        a4*sc17_think_act     + a5*sc17_restless   + a6*sc17_temper +
        a7*sc17_obedient      + a8*sc17_lying

# Equality‑constrained intercepts (strong invariance)
sc3_task_completion ~ i1*1
sc5_task_completion ~ i1*1
sc7_task_completion ~ i1*1
sc11_task_completion ~ i1*1
sc14_task_completion ~ i1*1
sc17_task_completion ~ i1*1
sc3_distracted ~ i2*1
sc5_distracted ~ i2*1
sc7_distracted ~ i2*1
sc11_distracted ~ i2*1
sc14_distracted ~ i2*1
sc17_distracted ~ i2*1
sc3_fidgeting ~ i3*1
sc5_fidgeting ~ i3*1
sc7_fidgeting ~ i3*1
sc11_fidgeting ~ i3*1 
sc14_fidgeting ~ i3*1
sc17_fidgeting ~ i3*1
sc3_think_act ~ i4*1
sc5_think_act ~ i4*1
sc7_think_act ~ i4*1
sc11_think_act ~ i4*1
sc14_think_act ~ i4*1
sc17_think_act ~ i4*1
sc3_restless ~ i5*1
sc5_restless ~ i5*1
sc7_restless ~ i5*1
sc11_restless ~ i5*1
sc14_restless ~ i5*1
sc17_restless ~ i5*1
sc3_temper ~ i6*1
sc5_temper ~ i6*1
sc7_temper ~ i6*1
sc11_temper ~ i6*1
sc14_temper ~ i6*1
sc17_temper ~ i6*1
sc3_obedient ~ i7*1
sc5_obedient ~ i7*1
sc7_obedient ~ i7*1
sc11_obedient ~ i7*1
sc14_obedient ~ i7*1
sc17_obedient ~ i7*1
sc3_lying ~ i8*1
sc5_lying ~ i8*1
sc7_lying ~ i8*1
sc11_lying ~ i8*1
sc14_lying ~ i8*1
sc17_lying ~ i8*1

# Lag‑1 correlated uniquenesses (equal across lags)
sc3_task_completion ~~ r_tc*sc5_task_completion
sc5_task_completion ~~ r_tc*sc7_task_completion
sc7_task_completion ~~ r_tc*sc11_task_completion
sc11_task_completion ~~ r_tc*sc14_task_completion
sc14_task_completion ~~ r_tc*sc17_task_completion

sc3_distracted ~~ r_tc*sc5_distracted
sc5_distracted ~~ r_tc*sc7_distracted
sc7_distracted ~~ r_tc*sc11_distracted
sc11_distracted ~~ r_tc*sc14_distracted
sc14_distracted ~~ r_tc*sc17_distracted

sc3_fidgeting ~~ r_tc*sc5_fidgeting
sc5_fidgeting ~~ r_tc*sc7_fidgeting
sc7_fidgeting ~~ r_tc*sc11_fidgeting  
sc11_fidgeting ~~ r_tc*sc14_fidgeting
sc14_fidgeting ~~ r_tc*sc17_fidgeting

sc3_think_act ~~ r_tc*sc5_think_act
sc5_think_act ~~ r_tc*sc7_think_act
sc7_think_act ~~ r_tc*sc11_think_act  
sc11_think_act ~~ r_tc*sc14_think_act
sc14_think_act ~~ r_tc*sc17_think_act

sc3_restless ~~ r_tc*sc5_restless
sc5_restless ~~ r_tc*sc7_restless
sc7_restless ~~ r_tc*sc11_restless  
sc11_restless ~~ r_tc*sc14_restless
sc14_restless ~~ r_tc*sc17_restless

sc3_temper ~~ r_tc*sc5_temper
sc5_temper ~~ r_tc*sc7_temper
sc7_temper ~~ r_tc*sc11_temper  
sc11_temper ~~ r_tc*sc14_temper
sc14_temper ~~ r_tc*sc17_temper

sc3_obedient ~~ r_tc*sc5_obedient
sc5_obedient ~~ r_tc*sc7_obedient
sc7_obedient ~~ r_tc*sc11_obedient  
sc11_obedient ~~ r_tc*sc14_obedient
sc14_obedient ~~ r_tc*sc17_obedient

sc3_lying ~~ r_tc*sc5_lying
sc5_lying ~~ r_tc*sc7_lying
sc7_lying ~~ r_tc*sc11_lying      
sc11_lying ~~ r_tc*sc14_lying
sc14_lying ~~ r_tc*sc17_lying 
'
  
# 2) Growth part (second‑order) ---------------------------------------
growth <- '
# Linear growth model with time coding based on actual ages
INTERCEPT =~ 1*SC3 + 1*SC5 + 1*SC7 + 1*SC11 + 1*SC14 + 1*SC17
SLOPE     =~ 0*SC3 + 2*SC5 + 4*SC7 + 8*SC11 + 11*SC14 + 14*SC17

# Growth factor means and variances
INTERCEPT ~~ INTERCEPT
SLOPE ~~ SLOPE
INTERCEPT ~~ SLOPE

# Residual variances for latent factors (constrained to be positive)
SC3 ~~ v3*SC3
SC5 ~~ v5*SC5
SC7 ~~ v7*SC7
SC11 ~~ v11*SC11
SC14 ~~ v14*SC14
SC17 ~~ v17*SC17

v3 > 0.001
v5 > 0.001
v7 > 0.001
v11 > 0.001
v14 > 0.001
v17 > 0.001
'

model_lgc <- paste(meas, growth, sep = "\n")

cat("=== FITTING LATENT GROWTH CURVE MODEL ===\n")
cat("Model complexity: 6 time points spanning ages 3-17\n")
cat("Measurement model: 8 indicators per time point with metric invariance\n")
cat("Growth model: Linear trajectory with time-varying slopes\n\n")

# Fit the model
fit_lgc <- sem(model_lgc, data = longitudinal_complete, 
               estimator = "WLSMV", 
               ordered = all_vars_available, 
               std.lv = FALSE, 
               meanstructure = TRUE,
               verbose = TRUE,
               parallel = "snow",
               ncpus = n_cores,
               cl = cl,
               control = list(
                 iter.max = 50,
                 rel.tol = 1e-5,      # Much looser (default is 1e-9)
                 abs.tol = 1e-5,
                 trace = 1)
               )

# Check convergence
if(lavInspect(fit_lgc, "converged")) {
  cat("✓ Model converged successfully\n")
} else {
  cat("✗ Model did not converge - consider simplifying\n")
}

# Model summary
cat("\n=== MODEL RESULTS ===\n")
summary(fit_lgc, fit.measures = TRUE, standardized = TRUE)

# Check identification and problematic parameters
cat("\n=== MODEL DIAGNOSTICS ===\n")
cat("Free parameters:", lavInspect(fit_lgc, "free")$total, "\n")
cat("Observations:", lavInspect(fit_lgc, "nobs"), "\n")
cat("Ratio (obs/params):", round(lavInspect(fit_lgc, "nobs") / lavInspect(fit_lgc, "free")$total, 1), "\n")

# Check for problematic parameters
problematic <- parameterEstimates(fit_lgc) %>% 
  filter(se == 0 | is.na(se) | abs(est/se) < 1.96)

if(nrow(problematic) > 0) {
  cat("\nProblematic parameters (non-significant or undefined SE):\n")
  print(problematic[, c("lhs", "op", "rhs", "est", "se", "pvalue")])
} else {
  cat("\n✓ No problematic parameters detected\n")
}

# Extract growth parameters
cat("\n=== GROWTH PARAMETERS ===\n")
growth_params <- parameterEstimates(fit_lgc) %>%
  filter(lhs %in% c("INTERCEPT", "SLOPE") & op %in% c("~~", "~1")) %>%
  mutate(parameter = case_when(
    lhs == "INTERCEPT" & op == "~1" ~ "Intercept Mean",
    lhs == "SLOPE" & op == "~1" ~ "Slope Mean", 
    lhs == "INTERCEPT" & rhs == "INTERCEPT" ~ "Intercept Variance",
    lhs == "SLOPE" & rhs == "SLOPE" ~ "Slope Variance",
    lhs == "INTERCEPT" & rhs == "SLOPE" ~ "Intercept-Slope Covariance",
    TRUE ~ "Other"
  )) %>%
  filter(parameter != "Other") %>%
  select(parameter, est, se, pvalue)

print(growth_params)

# Calculate correlation between intercept and slope
intercept_var <- growth_params$est[growth_params$parameter == "Intercept Variance"]
slope_var <- growth_params$est[growth_params$parameter == "Slope Variance"]
intercept_slope_cov <- growth_params$est[growth_params$parameter == "Intercept-Slope Covariance"]

if(length(intercept_var) > 0 & length(slope_var) > 0 & length(intercept_slope_cov) > 0) {
  intercept_slope_cor <- intercept_slope_cov / sqrt(intercept_var * slope_var)
  cat("\nIntercept-Slope Correlation:", round(intercept_slope_cor, 3), "\n")
}

# Alternative MLR estimation for comparison
cat("\n=== ALTERNATIVE MLR ESTIMATION ===\n")
fit_mlr <- sem(model_lgc, data = longitudinal_complete, 
               estimator = "MLR",
               meanstructure = TRUE,
               parallel = "snow",
               ncpus = n_cores,
               cl = cl,
               verbose = TRUE, 
               missing = "fiml",
               control = list(
                 iter.max = 50,
                 rel.tol = 1e-5,      # Much looser (default is 1e-9)
                 abs.tol = 1e-5)      # Absolute tolerance
               )

if(lavInspect(fit_mlr, "converged")) {
  cat("✓ MLR model converged successfully\n")
  mlr_fit <- fitMeasures(fit_mlr, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
  cat("MLR Fit Indices:\n")
  print(round(mlr_fit, 3))
} else {
  cat("✗ MLR model did not converge\n")
}

summary(fit_mlr, fit.measures = TRUE, standardized = TRUE)

# Save results
cat("\n=== SAVING RESULTS ===\n")
cat("Key objects saved to workspace:\n")
cat("- fit_lgc: Main WLSMV latent growth curve model\n")
cat("- fit_mlr: Alternative MLR estimation\n")
cat("- longitudinal_complete: Complete case dataset\n")
cat("- growth_params: Extracted growth parameters\n")

cat("\n=== DEVELOPMENTAL INTERPRETATION ===\n")
if(lavInspect(fit_lgc, "converged")) {
  slope_mean <- growth_params$est[growth_params$parameter == "Slope Mean"]
  slope_se <- growth_params$se[growth_params$parameter == "Slope Mean"]
  slope_p <- growth_params$pvalue[growth_params$parameter == "Slope Mean"]
  
  if(length(slope_mean) > 0) {
    cat("Linear slope estimate:", round(slope_mean, 4), "\n")
    cat("Standard error:", round(slope_se, 4), "\n")
    cat("P-value:", round(slope_p, 4), "\n")
    
    if(slope_p < 0.05) {
      if(slope_mean > 0) {
        cat("→ Self-control INCREASES significantly from age 3 to 17\n")
      } else {
        cat("→ Self-control DECREASES significantly from age 3 to 17\n")
      }
    } else {
      cat("→ No significant linear change in self-control from age 3 to 17\n")
    }
    
    # Calculate total change over development
    total_change <- slope_mean * 14  # 14 years from age 3 to 17
    cat("Estimated total change from age 3 to 17:", round(total_change, 3), "units\n")
  }
} else {
  cat("Model did not converge - interpretation not possible\n")
}

cat("\nNote: This model spans early childhood through late adolescence (ages 3-17).\n")
cat("Consider non-linear growth models if linear fit is poor.\n")





####### DIAGNOSTICS #######
# 1. First, check what's in your data
str(longitudinal_complete)  # See structure
dim(longitudinal_complete)  # Number of rows and columns

# 2. Check variances more safely
vars_to_check <- as.data.frame(longitudinal_complete)
var_check <- apply(vars_to_check, 2, function(x) var(x, na.rm = TRUE))
sort(var_check)[1:10]

# 3. Alternative variance check
low_var_vars <- names(which(var_check < 0.1))
print(low_var_vars)

# 4. Check for constant columns
n_unique <- apply(vars_to_check, 2, function(x) length(unique(x[!is.na(x)])))
constant_vars <- names(which(n_unique == 1))
print(constant_vars)

# 5. Simple correlation check
# Take a subset first to test
test_vars <- vars_to_check[, 1:10]
cor(test_vars, use = "pairwise.complete.obs")











# Exploratory Model Tuning
#################
## LATENT GROWTH CURVE MODEL SPECIFICATION
#################

cat("\n=== MODEL SPECIFICATION ===\n")
cat("Time coding (years from age 3):\n")
cat("Age 3: 0, Age 5: 2, Age 7: 4, Age 11: 8, Age 14: 11, Age 17: 14\n\n")

# 1) Measurement part -------------------------------------------------
meas_test <- '
# Weak+Strong invariance in one step (after testing separately)
SC3 =~ 1*sc3_task_completion + a2*sc3_distracted + a3*sc3_fidgeting +
       a4*sc3_think_act     + a5*sc3_restless   + a6*sc3_temper +
       a7*sc3_obedient      + a8*sc3_lying
SC5 =~ 1*sc5_task_completion + a2*sc5_distracted + a3*sc5_fidgeting +
       a4*sc5_think_act     + a5*sc5_restless   + a6*sc5_temper +
       a7*sc5_obedient      + a8*sc5_lying
SC7 =~ 1*sc7_task_completion + a2*sc7_distracted + a3*sc7_fidgeting +
       a4*sc7_think_act     + a5*sc7_restless   + a6*sc7_temper +
       a7*sc7_obedient      + a8*sc7_lying
SC11 =~ 1*sc11_task_completion + a2*sc11_distracted + a3*sc11_fidgeting +
        a4*sc11_think_act     + a5*sc11_restless   + a6*sc11_temper +
        a7*sc11_obedient      + a8*sc11_lying
SC14 =~ 1*sc14_task_completion + a2*sc14_distracted + a3*sc14_fidgeting +
        a4*sc14_think_act     + a5*sc14_restless   + a6*sc14_temper +
        a7*sc14_obedient      + a8*sc14_lying
SC17 =~ 1*sc17_task_completion + a2*sc17_distracted + a3*sc17_fidgeting +
        a4*sc17_think_act     + a5*sc17_restless   + a6*sc17_temper +
        a7*sc17_obedient      + a8*sc17_lying

# Equality‑constrained intercepts (partial scalar invariance)
sc3_task_completion ~ 0*1
sc5_task_completion ~ 0*1
sc7_task_completion ~ 0*1
sc11_task_completion ~ 0*1
sc14_task_completion ~ 0*1
sc17_task_completion ~ 0*1
sc3_distracted ~ NA*1
sc5_distracted ~ NA*1
sc7_distracted ~ NA*1
sc11_distracted ~ NA*1
sc14_distracted ~ NA*1
sc17_distracted ~ NA*1
sc3_fidgeting ~ NA*1
sc5_fidgeting ~ NA*1
sc7_fidgeting ~ NA*1
sc11_fidgeting ~ NA*1 
sc14_fidgeting ~ NA*1
sc17_fidgeting ~ NA*1
sc3_think_act ~ i4*1
sc5_think_act ~ i4*1
sc7_think_act ~ i4*1
sc11_think_act ~ i4*1
sc14_think_act ~ i4*1
sc17_think_act ~ i4*1
sc3_restless ~ NA*1
sc5_restless ~ NA*1
sc7_restless ~ NA*1
sc11_restless ~ NA*1
sc14_restless ~ NA*1
sc17_restless ~ NA*1
sc3_temper ~ NA*1
sc5_temper ~ NA*1
sc7_temper ~ NA*1
sc11_temper ~ NA*1
sc14_temper ~ NA*1
sc17_temper ~ NA*1
sc3_obedient ~ NA*1
sc5_obedient ~ NA*1
sc7_obedient ~ NA*1
sc11_obedient ~ NA*1
sc14_obedient ~ NA*1
sc17_obedient ~ NA*1
sc3_lying ~ NA*1
sc5_lying ~ NA*1
sc7_lying ~ NA*1
sc11_lying ~ NA*1
sc14_lying ~ NA*1
sc17_lying ~ NA*1

# Lag‑1 correlated uniquenesses 
sc3_task_completion ~~ sc5_task_completion
sc5_task_completion ~~ sc7_task_completion
sc7_task_completion ~~ sc11_task_completion
sc11_task_completion ~~ sc14_task_completion
sc14_task_completion ~~ sc17_task_completion

sc3_distracted ~~ sc5_distracted
sc5_distracted ~~ sc7_distracted
sc7_distracted ~~ sc11_distracted
sc11_distracted ~~ sc14_distracted
sc14_distracted ~~ sc17_distracted

sc3_fidgeting ~~ sc5_fidgeting
sc5_fidgeting ~~ sc7_fidgeting
sc7_fidgeting ~~ sc11_fidgeting  
sc11_fidgeting ~~ sc14_fidgeting
sc14_fidgeting ~~ sc17_fidgeting  

sc3_think_act ~~ sc5_think_act
sc5_think_act ~~ sc7_think_act
sc7_think_act ~~ sc11_think_act  
sc11_think_act ~~ sc14_think_act
sc14_think_act ~~ sc17_think_act

sc3_restless ~~ sc5_restless
sc5_restless ~~ sc7_restless
sc7_restless ~~ sc11_restless  
sc11_restless ~~ sc14_restless
sc14_restless ~~ sc17_restless

sc3_temper ~~ sc5_temper
sc5_temper ~~ sc7_temper
sc7_temper ~~ sc11_temper  
sc11_temper ~~ sc14_temper
sc14_temper ~~ sc17_temper

sc3_obedient ~~ sc5_obedient
sc5_obedient ~~ sc7_obedient
sc7_obedient ~~ sc11_obedient  
sc11_obedient ~~ sc14_obedient
sc14_obedient ~~ sc17_obedient

sc3_lying ~~ sc5_lying
sc5_lying ~~ sc7_lying
sc7_lying ~~ sc11_lying      
sc11_lying ~~ sc14_lying  
sc14_lying ~~ sc17_lying 

# Same wave correlated uniquenesses
sc3_temper ~~ sc3_lying 
sc5_temper ~~ sc5_lying
sc7_temper ~~ sc7_lying
sc11_temper ~~ sc11_lying
sc14_temper ~~ sc14_lying
sc17_temper ~~ sc17_lying

sc3_task_completion ~~ sc3_distracted
sc5_task_completion ~~ sc5_distracted
sc7_task_completion ~~ sc7_distracted
sc11_task_completion ~~ sc11_distracted
sc14_task_completion ~~ sc14_distracted
sc17_task_completion ~~ sc17_distracted

# 3B  Free the six first-order latent means
SC3  ~ 1
SC5  ~ 1
SC7  ~ 1
SC11 ~ 1
SC14 ~ 1
SC17 ~ 1
'
  
# 2) Growth part (second‑order) ---------------------------------------
growth_test <- '
# Linear growth model with time coding based on actual ages
INTERCEPT =~ 1*SC3 + 1*SC5 + 1*SC7 + 1*SC11 + 1*SC14 + 1*SC17
SLOPE     =~ 0*SC3 + 2*SC5 + 4*SC7 + 8*SC11 + 11*SC14 + 14*SC17
INTERCEPT ~~ INTERCEPT
SLOPE ~~ SLOPE
INTERCEPT ~~ SLOPE 

## ----- FREE latent means -----
INTERCEPT ~ 1
SLOPE     ~ 1

# Residual variances for latent factors
SC3 ~~ v3*SC3
SC5 ~~ v5*SC5
SC7 ~~ v7*SC7
SC11 ~~ v11*SC11
SC14 ~~ v14*SC14
SC17 ~~ v17*SC17
'

model_lgc_test <- paste(meas_test, growth_test, sep = "\n")

# Fit with MLR
fit_mlr_test <- sem(model_lgc_test, data = longitudinal_complete, 
               estimator = "MLR",
               meanstructure = TRUE,
               missing = "fiml",
               parallel = "snow",
               ncpus = n_cores-1,
               cl = cl
               )

summary(fit_mlr_test, fit.measures = TRUE, standardized = TRUE)

# Partial Scalar Invariance Tuning
mi <- modificationIndices(fit_mlr_test, sort.=TRUE)
head(mi, 20)              # Inspect the biggest offenders

# Fit with WLSMV
fit_lgc_test <- sem(model_lgc_test, data = longitudinal_complete, 
               estimator = "WLSMV",
               ordered = c("sc3_task_completion", "sc3_distracted", "sc3_fidgeting", "sc3_think_act", "sc3_restless", "sc3_temper", "sc3_obedient", "sc3_lying",
                           "sc5_task_completion", "sc5_distracted", "sc5_fidgeting", "sc5_think_act", "sc5_restless", "sc5_temper", "sc5_obedient", "sc5_lying",
                           "sc7_task_completion", "sc7_distracted", "sc7_fidgeting", "sc7_think_act", "sc7_restless", "sc7_temper", "sc7_obedient", "sc7_lying",
                           "sc11_task_completion", "sc11_distracted", "sc11_fidgeting", "sc11_think_act", "sc11_restless", "sc11_temper", "sc11_obedient", "sc11_lying",
                           "sc14_task_completion", "sc14_distracted", "sc14_fidgeting", "sc14_think_act", "sc14_restless", "sc14_temper", "sc14_obedient", "sc14_lying",
                           "sc17_task_completion", "sc17_distracted", "sc17_fidgeting", "sc17_think_act", "sc17_restless", "sc17_temper", "sc17_obedient", "sc17_lying"),
               meanstructure = TRUE,
               parallel = "snow",
               ncpus = n_cores-1,
               cl = cl,
               parameterization = "theta"
               )

summary(fit_lgc_test, fit.measures = TRUE, standardized = TRUE)

lavInspect(fit_lgc_test)



# Clean up parallel processing
stopCluster(cl)
cat("\nParallel processing cluster stopped.\n")




### Testing with Quadratic Growth Model
# 1) Measurement part -------------------------------------------------
meas_test_quad <- '
# Weak+Strong invariance in one step (after testing separately)
SC3 =~ 1*sc3_task_completion + a2*sc3_distracted + a3*sc3_fidgeting +
       a4*sc3_think_act     + a5*sc3_restless   + a6*sc3_temper +
       a7*sc3_obedient      + a8*sc3_lying
SC5 =~ 1*sc5_task_completion + a2*sc5_distracted + a3*sc5_fidgeting +
       a4*sc5_think_act     + a5*sc5_restless   + a6*sc5_temper +
       a7*sc5_obedient      + a8*sc5_lying
SC7 =~ 1*sc7_task_completion + a2*sc7_distracted + a3*sc7_fidgeting +
       a4*sc7_think_act     + a5*sc7_restless   + a6*sc7_temper +
       a7*sc7_obedient      + a8*sc7_lying
SC11 =~ 1*sc11_task_completion + a2*sc11_distracted + a3*sc11_fidgeting +
        a4*sc11_think_act     + a5*sc11_restless   + a6*sc11_temper +
        a7*sc11_obedient      + a8*sc11_lying
SC14 =~ 1*sc14_task_completion + a2*sc14_distracted + a3*sc14_fidgeting +
        a4*sc14_think_act     + a5*sc14_restless   + a6*sc14_temper +
        a7*sc14_obedient      + a8*sc14_lying
SC17 =~ 1*sc17_task_completion + a2*sc17_distracted + a3*sc17_fidgeting +
        a4*sc17_think_act     + a5*sc17_restless   + a6*sc17_temper +
        a7*sc17_obedient      + a8*sc17_lying

# Equality‑constrained intercepts (Partial Scalar invariance)
# Partial Scalar Invariance (intercepts)
# Invariant anchor items (equal intercepts across waves)
sc3_task_completion  ~ i1*1
sc5_task_completion  ~ i1*1
sc7_task_completion  ~ i1*1
sc11_task_completion ~ i1*1
sc14_task_completion ~ i1*1
sc17_task_completion ~ i1*1

sc3_think_act  ~ i2*1
sc5_think_act  ~ i2*1
sc7_think_act  ~ i2*1
sc11_think_act ~ i2*1
sc14_think_act ~ i2*1
sc17_think_act ~ i2*1

# Non-invariant items (intercepts free across waves)
sc3_distracted  ~ 1
sc5_distracted  ~ 1
sc7_distracted  ~ 1
sc11_distracted ~ 1
sc14_distracted ~ 1
sc17_distracted ~ 1

sc3_fidgeting  ~ 1
sc5_fidgeting  ~ 1
sc7_fidgeting  ~ 1
sc11_fidgeting ~ 1
sc14_fidgeting ~ 1
sc17_fidgeting ~ 1

sc3_restless  ~ 1
sc5_restless  ~ 1
sc7_restless  ~ 1
sc11_restless ~ 1
sc14_restless ~ 1
sc17_restless ~ 1

sc3_temper  ~ 1
sc5_temper  ~ 1
sc7_temper  ~ 1
sc11_temper ~ 1
sc14_temper ~ 1
sc17_temper ~ 1

sc3_obedient  ~ 1
sc5_obedient  ~ 1
sc7_obedient  ~ 1
sc11_obedient ~ 1
sc14_obedient ~ 1
sc17_obedient ~ 1

sc3_lying  ~ 1
sc5_lying  ~ 1
sc7_lying  ~ 1
sc11_lying ~ 1
sc14_lying ~ 1
sc17_lying ~ 1

# Fix the six first-order latent means to 0
SC3  ~ 0*1
SC5  ~ 0*1
SC7  ~ 0*1
SC11 ~ 0*1
SC14 ~ 0*1
SC17 ~ 0*1


# Item-specific method factors (CT-C[M-1])
# Reference indicator: task_completion (no method factor)
############################################
M_DISTRACT =~ 1*sc3_distracted  + 1*sc5_distracted  + 1*sc7_distracted  +
              1*sc11_distracted + 1*sc14_distracted + 1*sc17_distracted
M_FIDGET   =~ 1*sc3_fidgeting   + 1*sc5_fidgeting   + 1*sc7_fidgeting   +
              1*sc11_fidgeting  + 1*sc14_fidgeting  + 1*sc17_fidgeting
M_THINK    =~ 1*sc3_think_act   + 1*sc5_think_act   + 1*sc7_think_act   +
              1*sc11_think_act  + 1*sc14_think_act  + 1*sc17_think_act
M_RESTLESS =~ 1*sc3_restless    + 1*sc5_restless    + 1*sc7_restless    +
              1*sc11_restless   + 1*sc14_restless   + 1*sc17_restless
M_TEMPER   =~ 1*sc3_temper      + 1*sc5_temper      + 1*sc7_temper      +
              1*sc11_temper     + 1*sc14_temper     + 1*sc17_temper
M_OBEDIENT =~ 1*sc3_obedient    + 1*sc5_obedient    + 1*sc7_obedient    +
              1*sc11_obedient   + 1*sc14_obedient   + 1*sc17_obedient
M_LYING    =~ 1*sc3_lying       + 1*sc5_lying       + 1*sc7_lying       +
              1*sc11_lying      + 1*sc14_lying      + 1*sc17_lying

# Method factor means fixed to zero
M_DISTRACT ~ 0*1
M_FIDGET   ~ 0*1
M_THINK    ~ 0*1
M_RESTLESS ~ 0*1
M_TEMPER   ~ 0*1
M_OBEDIENT ~ 0*1
M_LYING    ~ 0*1

# Orthogonality constraints (method factors with first-order traits)
M_DISTRACT ~~ 0*SC3 + 0*SC5 + 0*SC7 + 0*SC11 + 0*SC14 + 0*SC17
M_FIDGET   ~~ 0*SC3 + 0*SC5 + 0*SC7 + 0*SC11 + 0*SC14 + 0*SC17
M_THINK    ~~ 0*SC3 + 0*SC5 + 0*SC7 + 0*SC11 + 0*SC14 + 0*SC17
M_RESTLESS ~~ 0*SC3 + 0*SC5 + 0*SC7 + 0*SC11 + 0*SC14 + 0*SC17
M_TEMPER   ~~ 0*SC3 + 0*SC5 + 0*SC7 + 0*SC11 + 0*SC14 + 0*SC17
M_OBEDIENT ~~ 0*SC3 + 0*SC5 + 0*SC7 + 0*SC11 + 0*SC14 + 0*SC17
M_LYING    ~~ 0*SC3 + 0*SC5 + 0*SC7 + 0*SC11 + 0*SC14 + 0*SC17

# Method factors mutually orthogonal
M_DISTRACT ~~ 0*M_FIDGET + 0*M_THINK + 0*M_RESTLESS + 0*M_TEMPER + 0*M_OBEDIENT + 0*M_LYING
M_FIDGET   ~~ 0*M_THINK + 0*M_RESTLESS + 0*M_TEMPER + 0*M_OBEDIENT + 0*M_LYING
M_THINK    ~~ 0*M_RESTLESS + 0*M_TEMPER + 0*M_OBEDIENT + 0*M_LYING
M_RESTLESS ~~ 0*M_TEMPER + 0*M_OBEDIENT + 0*M_LYING
M_TEMPER   ~~ 0*M_OBEDIENT + 0*M_LYING
M_OBEDIENT ~~ 0*M_LYING
'


# 2) Quadratic Growth part (second‑order) ---------------------------------------
growth_test_quad <- '
INTERCEPT  =~ 1*SC3 + 1*SC5 + 1*SC7 + 1*SC11 + 1*SC14 + 1*SC17
SLOPE_LIN  =~ 0*SC3 + 2*SC5 + 4*SC7 + 8*SC11 + 11*SC14 + 14*SC17
SLOPE_QUAD =~ 0*SC3 + 0.4*SC5 + 1.6*SC7 + 6.4*SC11 + 12.1*SC14 + 19.6*SC17


# Covariances
INTERCEPT  ~~ INTERCEPT
SLOPE_LIN  ~~ SLOPE_LIN
SLOPE_QUAD ~~ SLOPE_QUAD
INTERCEPT  ~~ SLOPE_LIN
INTERCEPT  ~~ SLOPE_QUAD
SLOPE_LIN  ~~ SLOPE_QUAD

# Latent means
INTERCEPT  ~ 1
SLOPE_LIN  ~ 1
SLOPE_QUAD ~ 1

# Residual variances of first-order factors
SC3  ~~ SC3
SC5  ~~ SC5
SC7  ~~ SC7
SC11 ~~ SC11
SC14 ~~ SC14
SC17 ~~ SC17
'

model_lgc_test_quad <- paste(meas_test_quad, growth_test_quad, sep = "\n")

# Fit with MLR
fit_mlr_test_quad <- sem(model_lgc_test_quad, data = longitudinal_complete, 
               estimator = "MLR",
               meanstructure = TRUE,
               missing = "fiml",
               check.gradient = TRUE,
               optim.method  = "nlminb"
               )

summary(fit_mlr_test_quad, fit.measures = TRUE, standardized = TRUE)

## Tuning
mi <- modificationIndices(fit_mlr_test_quad, sort.=TRUE)
head(mi, 20)             



# Ordered Version
### Testing with Quadratic Growth Model
# 1) Measurement part -------------------------------------------------
meas_test_quad_ord <- '
# Weak+Strong invariance in one step (after testing separately)
SC3 =~ 1*sc3_task_completion + a2*sc3_distracted + a3*sc3_fidgeting +
       a4*sc3_think_act     + a5*sc3_restless   + a6*sc3_temper +
       a7*sc3_obedient      + a8*sc3_lying
SC5 =~ 1*sc5_task_completion + a2*sc5_distracted + a3*sc5_fidgeting +
       a4*sc5_think_act     + a5*sc5_restless   + a6*sc5_temper +
       a7*sc5_obedient      + a8*sc5_lying
SC7 =~ 1*sc7_task_completion + a2*sc7_distracted + a3*sc7_fidgeting +
       a4*sc7_think_act     + a5*sc7_restless   + a6*sc7_temper +
       a7*sc7_obedient      + a8*sc7_lying
SC11 =~ 1*sc11_task_completion + a2*sc11_distracted + a3*sc11_fidgeting +
        a4*sc11_think_act     + a5*sc11_restless   + a6*sc11_temper +
        a7*sc11_obedient      + a8*sc11_lying
SC14 =~ 1*sc14_task_completion + a2*sc14_distracted + a3*sc14_fidgeting +
        a4*sc14_think_act     + a5*sc14_restless   + a6*sc14_temper +
        a7*sc14_obedient      + a8*sc14_lying
SC17 =~ 1*sc17_task_completion + a2*sc17_distracted + a3*sc17_fidgeting +
        a4*sc17_think_act     + a5*sc17_restless   + a6*sc17_temper +
        a7*sc17_obedient      + a8*sc17_lying

# Equality‑constrained intercepts (Partial Scalar invariance)
# Anchor items: both thresholds equal across time
sc3_task_completion  | tc_t1*t1 + tc_t2*t2
sc5_task_completion  | tc_t1*t1 + tc_t2*t2
sc7_task_completion  | tc_t1*t1 + tc_t2*t2
sc11_task_completion | tc_t1*t1 + tc_t2*t2
sc14_task_completion | tc_t1*t1 + tc_t2*t2
sc17_task_completion | tc_t1*t1 + tc_t2*t2

sc3_think_act  | ta_t1*t1 + ta_t2*t2
sc5_think_act  | ta_t1*t1 + ta_t2*t2
sc7_think_act  | ta_t1*t1 + ta_t2*t2
sc11_think_act | ta_t1*t1 + ta_t2*t2
sc14_think_act | ta_t1*t1 + ta_t2*t2
sc17_think_act | ta_t1*t1 + ta_t2*t2

# Non-invariant items: first threshold free, second threshold equal across time
sc3_distracted  | t1 + d_t2*t2
sc5_distracted  | t1 + d_t2*t2
sc7_distracted  | t1 + d_t2*t2
sc11_distracted | t1 + d_t2*t2
sc14_distracted | t1 + d_t2*t2
sc17_distracted | t1 + d_t2*t2

sc3_fidgeting  | t1 + f_t2*t2
sc5_fidgeting  | t1 + f_t2*t2
sc7_fidgeting  | t1 + f_t2*t2
sc11_fidgeting | t1 + f_t2*t2
sc14_fidgeting | t1 + f_t2*t2
sc17_fidgeting | t1 + f_t2*t2

sc3_restless  | t1 + r_t2*t2
sc5_restless  | t1 + r_t2*t2
sc7_restless  | t1 + r_t2*t2
sc11_restless | t1 + r_t2*t2
sc14_restless | t1 + r_t2*t2
sc17_restless | t1 + r_t2*t2

sc3_temper  | t1 + tm_t2*t2
sc5_temper  | t1 + tm_t2*t2
sc7_temper  | t1 + tm_t2*t2
sc11_temper | t1 + tm_t2*t2
sc14_temper | t1 + tm_t2*t2
sc17_temper | t1 + tm_t2*t2

sc3_obedient  | t1 + o_t2*t2
sc5_obedient  | t1 + o_t2*t2
sc7_obedient  | t1 + o_t2*t2
sc11_obedient | t1 + o_t2*t2
sc14_obedient | t1 + o_t2*t2
sc17_obedient | t1 + o_t2*t2

sc3_lying  | t1 + l_t2*t2
sc5_lying  | t1 + l_t2*t2
sc7_lying  | t1 + l_t2*t2
sc11_lying | t1 + l_t2*t2
sc14_lying | t1 + l_t2*t2
sc17_lying | t1 + l_t2*t2

# Lag‑1 correlated uniquenesses 
sc3_task_completion ~~ sc5_task_completion
sc5_task_completion ~~ sc7_task_completion
sc7_task_completion ~~ sc11_task_completion
sc11_task_completion ~~ sc14_task_completion
sc14_task_completion ~~ sc17_task_completion

sc3_distracted ~~ sc5_distracted
sc5_distracted ~~ sc7_distracted
sc7_distracted ~~ sc11_distracted
sc11_distracted ~~ sc14_distracted
sc14_distracted ~~ sc17_distracted

sc3_fidgeting ~~ sc5_fidgeting
sc5_fidgeting ~~ sc7_fidgeting
sc7_fidgeting ~~ sc11_fidgeting  
sc11_fidgeting ~~ sc14_fidgeting
sc14_fidgeting ~~ sc17_fidgeting  

sc3_think_act ~~ sc5_think_act
sc5_think_act ~~ sc7_think_act
sc7_think_act ~~ sc11_think_act  
sc11_think_act ~~ sc14_think_act
sc14_think_act ~~ sc17_think_act

sc3_restless ~~ sc5_restless
sc5_restless ~~ sc7_restless
sc7_restless ~~ sc11_restless  
sc11_restless ~~ sc14_restless
sc14_restless ~~ sc17_restless

sc3_temper ~~ sc5_temper
sc5_temper ~~ sc7_temper
sc7_temper ~~ sc11_temper  
sc11_temper ~~ sc14_temper
sc14_temper ~~ sc17_temper

sc3_obedient ~~ sc5_obedient
sc5_obedient ~~ sc7_obedient
sc7_obedient ~~ sc11_obedient  
sc11_obedient ~~ sc14_obedient
sc14_obedient ~~ sc17_obedient

sc3_lying ~~ sc5_lying
sc5_lying ~~ sc7_lying
sc7_lying ~~ sc11_lying      
sc11_lying ~~ sc14_lying  
sc14_lying ~~ sc17_lying 

# Fix the six first-order latent means to 0
SC3  ~ 0*1
SC5  ~ 0*1
SC7  ~ 0*1
SC11 ~ 0*1
SC14 ~ 0*1
SC17 ~ 0*1
'


# 2) Quadratic Growth part (second‑order) ---------------------------------------
growth_test_quad <- '
INTERCEPT  =~ 1*SC3 + 1*SC5 + 1*SC7 + 1*SC11 + 1*SC14 + 1*SC17
SLOPE_LIN  =~ 0*SC3 + 2*SC5 + 4*SC7 + 8*SC11 + 11*SC14 + 14*SC17
SLOPE_QUAD =~ 0*SC3 + 0.4*SC5 + 1.6*SC7 + 6.4*SC11 + 12.1*SC14 + 19.6*SC17


# Covariances
INTERCEPT  ~~ INTERCEPT
SLOPE_LIN  ~~ SLOPE_LIN
SLOPE_QUAD ~~ SLOPE_QUAD
INTERCEPT  ~~ SLOPE_LIN
INTERCEPT  ~~ SLOPE_QUAD
SLOPE_LIN  ~~ SLOPE_QUAD

# Latent means
INTERCEPT  ~ 1
SLOPE_LIN  ~ 1
SLOPE_QUAD ~ 1

# Residual variances of first-order factors
SC3  ~~ SC3
SC5  ~~ SC5
SC7  ~~ SC7
SC11 ~~ SC11
SC14 ~~ SC14
SC17 ~~ SC17
'
model_lgc_test_quad_ord <- paste(meas_test_quad_ord, growth_test_quad, sep = "\n")

# Fit with WLSMV
fit_lgc_test_quad_ord <- sem(model_lgc_test_quad_ord, data = longitudinal_complete, 
               estimator = "WLSMV",
               meanstructure = TRUE,
               ordered = c("sc3_task_completion", "sc3_distracted", "sc3_fidgeting", "sc3_think_act", "sc3_restless", "sc3_temper", "sc3_obedient", "sc3_lying",
                           "sc5_task_completion", "sc5_distracted", "sc5_fidgeting", "sc5_think_act", "sc5_restless", "sc5_temper", "sc5_obedient", "sc5_lying",
                           "sc7_task_completion", "sc7_distracted", "sc7_fidgeting", "sc7_think_act", "sc7_restless", "sc7_temper", "sc7_obedient", "sc7_lying",
                           "sc11_task_completion", "sc11_distracted", "sc11_fidgeting", "sc11_think_act", "sc11_restless", "sc11_temper", "sc11_obedient", "sc11_lying",
                           "sc14_task_completion", "sc14_distracted", "sc14_fidgeting", "sc14_think_act", "sc14_restless", "sc14_temper", "sc14_obedient", "sc14_lying",
                           "sc17_task_completion", "sc17_distracted", "sc17_fidgeting", "sc17_think_act", "sc17_restless", "sc17_temper", "sc17_obedient", "sc17_lying"),
               parameterization = "theta"
               )

summary(fit_lgc_test_quad_ord, fit.measures = TRUE, standardized = TRUE)


#################
## DATA EXPORT TO CSV
#################

cat("\n=== EXPORTING DATA TO CSV ===\n")

# Create data directory if it doesn't exist
data_dir <- "../data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created data directory:", data_dir, "\n")
}

# Export 1: Complete longitudinal dataset
longitudinal_export <- longitudinal_complete
longitudinal_export$participant_id <- rownames(longitudinal_complete)
write.csv(longitudinal_export, file.path(data_dir, "longitudinal_complete_self_control.csv"), 
          row.names = FALSE)
cat("✓ Exported longitudinal complete dataset (n =", nrow(longitudinal_export), ")\n")


# Export 2: Complete longitudinal dataset with NAs
longitudinal_export <- longitudinal_data
longitudinal_export$participant_id <- rownames(longitudinal_data)
write.csv(longitudinal_export, file.path(data_dir, "longitudinal_data_self_control_withna.csv"), 
          row.names = FALSE)
cat("✓ Exported longitudinal complete dataset (n =", nrow(longitudinal_export), ")\n")






