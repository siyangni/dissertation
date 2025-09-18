# --- Packages
library(pacman)
p_load(lavaan, semTools)

# --- Check if self-control variables are recoded
# Explicitly list expected self-control variables
expected_sc_variables <- c(
  # Age 3
  "sc3_task_completion", "sc3_distracted", "sc3_fidgeting", "sc3_think_act",
  "sc3_restless", "sc3_temper", "sc3_obedient", "sc3_lying",
  # Age 5  
  "sc5_task_completion", "sc5_distracted", "sc5_fidgeting", "sc5_think_act",
  "sc5_restless", "sc5_temper", "sc5_obedient", "sc5_lying",
  # Age 7
  "sc7_task_completion", "sc7_distracted", "sc7_fidgeting", "sc7_think_act", 
  "sc7_restless", "sc7_temper", "sc7_obedient", "sc7_lying",
  # Age 11
  "sc11_task_completion", "sc11_distracted", "sc11_fidgeting", "sc11_think_act",
  "sc11_restless", "sc11_temper", "sc11_obedient", "sc11_lying",
  # Age 14
  "sc14_task_completion", "sc14_distracted", "sc14_fidgeting", "sc14_think_act",
  "sc14_restless", "sc14_temper", "sc14_obedient", "sc14_lying",
  # Age 17
  "sc17_task_completion", "sc17_distracted", "sc17_fidgeting", "sc17_think_act",
  "sc17_restless", "sc17_temper", "sc17_obedient", "sc17_lying"
)


# --- Check which items are available at all ages
# Define the 8 possible items
task_completion_available <- all(c("sc3_task_completion", "sc5_task_completion", 
                                   "sc7_task_completion", "sc11_task_completion", 
                                   "sc14_task_completion", "sc17_task_completion") %in% names(merged_data))

distracted_available <- all(c("sc3_distracted", "sc5_distracted", 
                              "sc7_distracted", "sc11_distracted", 
                              "sc14_distracted", "sc17_distracted") %in% names(merged_data))

fidgeting_available <- all(c("sc3_fidgeting", "sc5_fidgeting", 
                             "sc7_fidgeting", "sc11_fidgeting", 
                             "sc14_fidgeting", "sc17_fidgeting") %in% names(merged_data))

think_act_available <- all(c("sc3_think_act", "sc5_think_act", 
                             "sc7_think_act", "sc11_think_act", 
                             "sc14_think_act", "sc17_think_act") %in% names(merged_data))

restless_available <- all(c("sc3_restless", "sc5_restless", 
                            "sc7_restless", "sc11_restless", 
                            "sc14_restless", "sc17_restless") %in% names(merged_data))

temper_available <- all(c("sc3_temper", "sc5_temper", 
                          "sc7_temper", "sc11_temper", 
                          "sc14_temper", "sc17_temper") %in% names(merged_data))

obedient_available <- all(c("sc3_obedient", "sc5_obedient", 
                            "sc7_obedient", "sc11_obedient", 
                            "sc14_obedient", "sc17_obedient") %in% names(merged_data))

lying_available <- all(c("sc3_lying", "sc5_lying", 
                         "sc7_lying", "sc11_lying", 
                         "sc14_lying", "sc17_lying") %in% names(merged_data))

# Count available items
available_items_count <- sum(c(task_completion_available, distracted_available, 
                               fidgeting_available, think_act_available,
                               restless_available, temper_available, 
                               obedient_available, lying_available))

if (available_items_count < 3) {
  stop("Need at least 3 common indicators across all ages. Found: ", available_items_count)
}

# --- Build measurement model explicitly based on available items
# Start with empty model
configural_model <- ""

# Add factors based on available items
if (task_completion_available & distracted_available & fidgeting_available) {
  # Use first 3 items if available
  configural_model <- "
SC_t3 =~ sc3_task_completion + sc3_distracted + sc3_fidgeting
SC_t5 =~ sc5_task_completion + sc5_distracted + sc5_fidgeting  
SC_t7 =~ sc7_task_completion + sc7_distracted + sc7_fidgeting
SC_t11 =~ sc11_task_completion + sc11_distracted + sc11_fidgeting
SC_t14 =~ sc14_task_completion + sc14_distracted + sc14_fidgeting
SC_t17 =~ sc17_task_completion + sc17_distracted + sc17_fidgeting
"
  
  # Variables for lavaan ordered specification
  all_sc_variables <- c(
    "sc3_task_completion", "sc3_distracted", "sc3_fidgeting",
    "sc5_task_completion", "sc5_distracted", "sc5_fidgeting",
    "sc7_task_completion", "sc7_distracted", "sc7_fidgeting", 
    "sc11_task_completion", "sc11_distracted", "sc11_fidgeting",
    "sc14_task_completion", "sc14_distracted", "sc14_fidgeting",
    "sc17_task_completion", "sc17_distracted", "sc17_fidgeting"
  )
  
  # Set up for measurement invariance
  longFacNames <- list(SC = c("SC_t3", "SC_t5", "SC_t7", "SC_t11", "SC_t14", "SC_t17"))
  longIndNames <- list(
    task_completion = c("sc3_task_completion", "sc5_task_completion", "sc7_task_completion", 
                        "sc11_task_completion", "sc14_task_completion", "sc17_task_completion"),
    distracted = c("sc3_distracted", "sc5_distracted", "sc7_distracted",
                   "sc11_distracted", "sc14_distracted", "sc17_distracted"),
    fidgeting = c("sc3_fidgeting", "sc5_fidgeting", "sc7_fidgeting",
                  "sc11_fidgeting", "sc14_fidgeting", "sc17_fidgeting")
  )
  
} else if (task_completion_available & distracted_available & think_act_available) {
  # Alternative 3-item set
  configural_model <- "
SC_t3 =~ sc3_task_completion + sc3_distracted + sc3_think_act
SC_t5 =~ sc5_task_completion + sc5_distracted + sc5_think_act
SC_t7 =~ sc7_task_completion + sc7_distracted + sc7_think_act
SC_t11 =~ sc11_task_completion + sc11_distracted + sc11_think_act
SC_t14 =~ sc14_task_completion + sc14_distracted + sc14_think_act
SC_t17 =~ sc17_task_completion + sc17_distracted + sc17_think_act
"
  
  all_sc_variables <- c(
    "sc3_task_completion", "sc3_distracted", "sc3_think_act",
    "sc5_task_completion", "sc5_distracted", "sc5_think_act",
    "sc7_task_completion", "sc7_distracted", "sc7_think_act",
    "sc11_task_completion", "sc11_distracted", "sc11_think_act", 
    "sc14_task_completion", "sc14_distracted", "sc14_think_act",
    "sc17_task_completion", "sc17_distracted", "sc17_think_act"
  )
  
  longFacNames <- list(SC = c("SC_t3", "SC_t5", "SC_t7", "SC_t11", "SC_t14", "SC_t17"))
  longIndNames <- list(
    task_completion = c("sc3_task_completion", "sc5_task_completion", "sc7_task_completion",
                        "sc11_task_completion", "sc14_task_completion", "sc17_task_completion"),
    distracted = c("sc3_distracted", "sc5_distracted", "sc7_distracted",
                   "sc11_distracted", "sc14_distracted", "sc17_distracted"),
    think_act = c("sc3_think_act", "sc5_think_act", "sc7_think_act",
                  "sc11_think_act", "sc14_think_act", "sc17_think_act")
  )
  
} else {
  stop("Cannot find a suitable set of 3+ indicators available at all ages")
}

# --- Strong longitudinal measurement invariance
ME <- measEq.syntax(
  configural.model = configural_model,
  data             = merged_data,
  ordered          = all_sc_variables,
  parameterization = "theta",
  ID.fac           = "UL", 
  ID.cat           = "Wu.Estabrook.2016",
  longFacNames     = longFacNames,
  longIndNames     = longIndNames,
  long.equal       = c("thresholds", "loadings"),
  auto             = 1L,
  return.fit       = FALSE
)

measurement_model <- as.character(ME, single = TRUE)

# --- Autoregressive structure (explicit paths)
autoregressive_structure <- "
# Autoregressive paths with time-gap specific parameters
SC_t5 ~ phi2*SC_t3     # 2-year gap (age 3 to 5)
SC_t7 ~ phi2*SC_t5     # 2-year gap (age 5 to 7)  
SC_t11 ~ phi4*SC_t7    # 4-year gap (age 7 to 11)
SC_t14 ~ phi3*SC_t11   # 3-year gap (age 11 to 14)
SC_t17 ~ phi3*SC_t14   # 3-year gap (age 14 to 17)
"

# --- Linear growth model (time centered at age 11)
linear_growth_syntax <- "
# Linear latent growth model (time centered at age 11)
# Intercept factor (all loadings = 1)
i =~ 1*SC_t3 + 1*SC_t5 + 1*SC_t7 + 1*SC_t11 + 1*SC_t14 + 1*SC_t17

# Linear slope factor (time scores: -8, -6, -4, 0, 3, 6)
s =~ -8*SC_t3 + -6*SC_t5 + -4*SC_t7 + 0*SC_t11 + 3*SC_t14 + 6*SC_t17

# Fix time-specific factor means to zero
SC_t3 ~ 0*1
SC_t5 ~ 0*1
SC_t7 ~ 0*1
SC_t11 ~ 0*1
SC_t14 ~ 0*1
SC_t17 ~ 0*1

# Growth factor means and variances
i ~ 1
s ~ 1
i ~~ i
s ~~ s
i ~~ s
"

# --- Latent basis growth model
latent_basis_growth <- "
# Latent basis growth model
# Intercept factor (all loadings = 1)
i =~ 1*SC_t3 + 1*SC_t5 + 1*SC_t7 + 1*SC_t11 + 1*SC_t14 + 1*SC_t17

# Latent basis slope factor (first=0, last=1, middle freely estimated)
s =~ 0*SC_t3 + lb5*SC_t5 + lb7*SC_t7 + lb11*SC_t11 + lb14*SC_t14 + 1*SC_t17

# Fix time-specific factor means to zero
SC_t3 ~ 0*1
SC_t5 ~ 0*1
SC_t7 ~ 0*1
SC_t11 ~ 0*1
SC_t14 ~ 0*1
SC_t17 ~ 0*1

# Growth factor means and variances
i ~ 1
s ~ 1
i ~~ i
s ~~ s
i ~~ s
"

# --- Zero covariances among time-specific factors (AR handles dependencies)
zero_covariances <- "
# Prevent residual covariances among time-specific factors
SC_t3 ~~ 0*SC_t5
SC_t3 ~~ 0*SC_t7
SC_t3 ~~ 0*SC_t11
SC_t3 ~~ 0*SC_t14
SC_t3 ~~ 0*SC_t17
SC_t5 ~~ 0*SC_t7
SC_t5 ~~ 0*SC_t11
SC_t5 ~~ 0*SC_t14
SC_t5 ~~ 0*SC_t17
SC_t7 ~~ 0*SC_t11
SC_t7 ~~ 0*SC_t14
SC_t7 ~~ 0*SC_t17
SC_t11 ~~ 0*SC_t14
SC_t11 ~~ 0*SC_t17
SC_t14 ~~ 0*SC_t17
"

# -------------------------------------------------------------------
# Parenting at age 7 as a latent construct
# -------------------------------------------------------------------
# Check parenting variables exist
parenting_variables <- c("tell_off", "take_away_treats", "timeout", "reason")
parenting_vars_exist <- all(parenting_variables %in% names(merged_data))

if (!parenting_vars_exist) {
  missing_parenting <- parenting_variables[!parenting_variables %in% names(merged_data)]
  stop("Missing parenting variables: ", paste(missing_parenting, collapse = ", "))
}

# All ordered variables for lavaan
all_ordered_variables <- c(all_sc_variables, parenting_variables)

# Parenting measurement model
parenting_model <- "
# Appropriate parenting at age 7 (higher = more appropriate)
P7 =~ tell_off + take_away_treats + timeout + reason

# Anchor latent mean at 0 for identification
P7 ~ 0*1
P7 ~~ P7
"

# Basic structural relations (parenting effects)
basic_structural_relations <- "
# Parenting effects on growth factors and age-7 self-control
i ~ b_i*P7
s ~ b_s*P7
SC_t7 ~ b_t7*P7
"

# -------------------------------------------------------------------
# Model 1: Linear LCM-SR + Parenting
# -------------------------------------------------------------------
model_1_syntax <- paste(
  measurement_model,
  parenting_model,
  linear_growth_syntax,
  autoregressive_structure,
  zero_covariances,
  basic_structural_relations,
  sep = "\n"
)

fit_model_1 <- lavaan::sem(
  model_1_syntax,
  data             = merged_data,
  ordered          = all_ordered_variables,
  estimator        = "WLSMV",
  parameterization = "theta",
  std.lv           = FALSE,
  meanstructure    = TRUE
)

cat("\n=== Model 1: Linear LCM-SR + Parenting ===\n")
summary(fit_model_1, fit.measures = TRUE, standardized = TRUE)

# Extract fit indices
fit_indices_1 <- fitMeasures(fit_model_1,
  c("chisq.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"))
print(round(fit_indices_1, 3))

# Calculate parenting effects by age (manual calculation)
PE_1 <- parameterEstimates(fit_model_1)
b_i_1 <- PE_1$est[PE_1$lhs == "i" & PE_1$op == "~" & PE_1$rhs == "P7"]
b_s_1 <- PE_1$est[PE_1$lhs == "s" & PE_1$op == "~" & PE_1$rhs == "P7"]

parenting_effect_age3 <- b_i_1 + b_s_1 * (-8)
parenting_effect_age5 <- b_i_1 + b_s_1 * (-6)
parenting_effect_age7 <- b_i_1 + b_s_1 * (-4)
parenting_effect_age11 <- b_i_1 + b_s_1 * (0)
parenting_effect_age14 <- b_i_1 + b_s_1 * (3)
parenting_effect_age17 <- b_i_1 + b_s_1 * (6)

cat("\n--- Parenting effects on latent self-control by age ---\n")
cat("Age 3: ", round(parenting_effect_age3, 3), "\n")
cat("Age 5: ", round(parenting_effect_age5, 3), "\n")
cat("Age 7: ", round(parenting_effect_age7, 3), "\n")
cat("Age 11: ", round(parenting_effect_age11, 3), "\n")
cat("Age 14: ", round(parenting_effect_age14, 3), "\n")
cat("Age 17: ", round(parenting_effect_age17, 3), "\n")

# Standardized effects
STD_1 <- standardizedSolution(fit_model_1)
b_i_std_1 <- STD_1$est.std[STD_1$lhs == "i" & STD_1$op == "~" & STD_1$rhs == "P7"]
b_s_std_1 <- STD_1$est.std[STD_1$lhs == "s" & STD_1$op == "~" & STD_1$rhs == "P7"]
b_t7_std_1 <- STD_1$est.std[STD_1$lhs == "SC_t7" & STD_1$op == "~" & STD_1$rhs == "P7"]

cat("\n--- Standardized effects ---\n")
cat("Intercept: ", round(b_i_std_1, 3), "\n")
cat("Slope: ", round(b_s_std_1, 3), "\n")
cat("Age 7 direct: ", round(b_t7_std_1, 3), "\n")

# -------------------------------------------------------------------
# Model 2: Latent-Basis LCM-SR + Parenting
# -------------------------------------------------------------------
model_2_syntax <- paste(
  measurement_model,
  parenting_model,
  latent_basis_growth,
  autoregressive_structure,
  zero_covariances,
  basic_structural_relations,
  sep = "\n"
)

fit_model_2 <- lavaan::sem(
  model_2_syntax,
  data             = merged_data,
  ordered          = all_ordered_variables,
  estimator        = "WLSMV",
  parameterization = "theta",
  std.lv           = FALSE,
  meanstructure    = TRUE
)

cat("\n=== Model 2: Latent-Basis LCM-SR + Parenting ===\n")
summary(fit_model_2, fit.measures = TRUE, standardized = TRUE)

fit_indices_2 <- fitMeasures(fit_model_2,
  c("chisq.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"))
print(round(fit_indices_2, 3))

# -------------------------------------------------------------------
# Wave-1 covariates processing (explicit variable checking)
# -------------------------------------------------------------------
# Check required covariates
required_covariates <- c("parents_education", "sex_factor", "race_factor", 
                         "marital_status_factor", "parents_income_couple", 
                         "parents_income_lone_parent")

missing_covariates <- required_covariates[!required_covariates %in% names(merged_data)]
if (length(missing_covariates) > 0) {
  stop("Missing covariates: ", paste(missing_covariates, collapse = ", "))
}

# Process each covariate explicitly (assuming typical transformations)
# Note: This section would need to be customized based on actual data structure

# Check if processed covariates already exist, if not create them
processed_covariate_names <- character(0)

# Parents education (assume numeric, standardize)
if ("parents_education" %in% names(merged_data)) {
  if (!"parents_education_z" %in% names(merged_data)) {
    merged_data$parents_education_z <- as.numeric(scale(merged_data$parents_education))
    merged_data$parents_education_z[is.na(merged_data$parents_education_z)] <- 0
  }
  processed_covariate_names <- c(processed_covariate_names, "parents_education_z")
}

# Sex factor (assume binary with levels like Male/Female)
if ("sex_factor" %in% names(merged_data)) {
  if (!"sex_factor_Male" %in% names(merged_data)) {
    merged_data$sex_factor_Male <- as.numeric(merged_data$sex_factor == "Male")
    merged_data$sex_factor_Male[is.na(merged_data$sex_factor_Male)] <- 0
  }
  processed_covariate_names <- c(processed_covariate_names, "sex_factor_Male")
}

# Income variables (assume numeric, standardize)
if ("parents_income_couple" %in% names(merged_data)) {
  if (!"parents_income_couple_z" %in% names(merged_data)) {
    merged_data$parents_income_couple_z <- as.numeric(scale(merged_data$parents_income_couple))
    merged_data$parents_income_couple_z[is.na(merged_data$parents_income_couple_z)] <- 0
  }
  processed_covariate_names <- c(processed_covariate_names, "parents_income_couple_z")
}

if ("parents_income_lone_parent" %in% names(merged_data)) {
  if (!"parents_income_lone_parent_z" %in% names(merged_data)) {
    merged_data$parents_income_lone_parent_z <- as.numeric(scale(merged_data$parents_income_lone_parent))
    merged_data$parents_income_lone_parent_z[is.na(merged_data$parents_income_lone_parent_z)] <- 0
  }
  processed_covariate_names <- c(processed_covariate_names, "parents_income_lone_parent_z")
}

# For race_factor and marital_status_factor, would need to know actual levels
# This is a placeholder - adjust based on actual factor levels in your data
cat("\nNote: Race and marital status factors need manual specification based on actual levels in data\n")

# Create structural relations with explicit covariate terms
# Using the processed covariates that are available
structural_with_controls <- "
# Parenting and covariate effects on growth factors
i ~ b_i*P7 + gi_parents_education_z*parents_education_z + gi_sex_factor_Male*sex_factor_Male + gi_parents_income_couple_z*parents_income_couple_z + gi_parents_income_lone_parent_z*parents_income_lone_parent_z

s ~ b_s*P7 + gs_parents_education_z*parents_education_z + gs_sex_factor_Male*sex_factor_Male + gs_parents_income_couple_z*parents_income_couple_z + gs_parents_income_lone_parent_z*parents_income_lone_parent_z

SC_t7 ~ b_t7*P7
"

# -------------------------------------------------------------------
# Model 3A: Linear LCM-SR + Parenting + Controls
# -------------------------------------------------------------------
model_3a_syntax <- paste(
  measurement_model,
  parenting_model,
  linear_growth_syntax,
  autoregressive_structure,
  zero_covariances,
  structural_with_controls,
  sep = "\n"
)

fit_model_3a <- lavaan::sem(
  model_3a_syntax,
  data             = merged_data,
  ordered          = all_ordered_variables,
  estimator        = "WLSMV",
  parameterization = "theta",
  std.lv           = FALSE,
  meanstructure    = TRUE,
  fixed.x          = TRUE
)

cat("\n=== Model 3A: Linear LCM-SR + Parenting + Controls ===\n")
summary(fit_model_3a, fit.measures = TRUE, standardized = TRUE)

fit_indices_3a <- fitMeasures(fit_model_3a,
  c("chisq.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"))
print(round(fit_indices_3a, 3))

# -------------------------------------------------------------------
# Model 3B: Latent-Basis LCM-SR + Parenting + Controls
# -------------------------------------------------------------------
model_3b_syntax <- paste(
  measurement_model,
  parenting_model,
  latent_basis_growth,
  autoregressive_structure,
  zero_covariances,
  structural_with_controls,
  sep = "\n"
)

fit_model_3b <- lavaan::sem(
  model_3b_syntax,
  data             = merged_data,
  ordered          = all_ordered_variables,
  estimator        = "WLSMV",
  parameterization = "theta",
  std.lv           = FALSE,
  meanstructure    = TRUE,
  fixed.x          = TRUE
)

cat("\n=== Model 3B: Latent-Basis LCM-SR + Parenting + Controls ===\n")
summary(fit_model_3b, fit.measures = TRUE, standardized = TRUE)

fit_indices_3b <- fitMeasures(fit_model_3b,
  c("chisq.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"))
print(round(fit_indices_3b, 3))

# -------------------------------------------------------------------
# Model comparison summary
# -------------------------------------------------------------------
cat("\n=== MODEL COMPARISON SUMMARY ===\n")
cat("Model 1 - Linear LCM-SR:\n")
print(round(fit_indices_1, 3))
cat("\nModel 2 - Latent-Basis LCM-SR:\n") 
print(round(fit_indices_2, 3))
cat("\nModel 3A - Linear + Controls:\n")
print(round(fit_indices_3a, 3))
cat("\nModel 3B - Latent-Basis + Controls:\n")
print(round(fit_indices_3b, 3))