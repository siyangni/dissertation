# --- Packages
library(lavaan)
library(semTools)

# --- Direct variable specification (all self-control indicators across ages)
# Variables per age (wide data)
var_by_age <- list(
  t3  = c("sc3_task_completion", "sc3_distracted", "sc3_fidgeting", "sc3_think_act",
          "sc3_restless", "sc3_temper", "sc3_obedient", "sc3_lying"),
  t5  = c("sc5_task_completion", "sc5_distracted", "sc5_fidgeting", "sc5_think_act",
          "sc5_restless", "sc5_temper", "sc5_obedient", "sc5_lying"),
  t7  = c("sc7_task_completion", "sc7_distracted", "sc7_fidgeting", "sc7_think_act",
          "sc7_restless", "sc7_temper", "sc7_obedient", "sc7_lying"),
  t11 = c("sc11_task_completion", "sc11_distracted", "sc11_fidgeting", "sc11_think_act",
          "sc11_restless", "sc11_temper", "sc11_obedient", "sc11_lying"),
  t14 = c("sc14_task_completion", "sc14_distracted", "sc14_fidgeting", "sc14_think_act",
          "sc14_restless", "sc14_temper", "sc14_obedient", "sc14_lying"),
  t17 = c("sc17_task_completion", "sc17_distracted", "sc17_fidgeting", "sc17_think_act",
          "sc17_restless", "sc17_temper", "sc17_obedient", "sc17_lying")
)

ordered_vars <- unlist(var_by_age, use.names = FALSE)

# --- First-order latent factors per wave
configural_model <- '
SC_t3  =~ sc3_task_completion + sc3_distracted + sc3_fidgeting + sc3_think_act + sc3_restless + sc3_temper + sc3_obedient + sc3_lying
SC_t5  =~ sc5_task_completion + sc5_distracted + sc5_fidgeting + sc5_think_act + sc5_restless + sc5_temper + sc5_obedient + sc5_lying
SC_t7  =~ sc7_task_completion + sc7_distracted + sc7_fidgeting + sc7_think_act + sc7_restless + sc7_temper + sc7_obedient + sc7_lying
SC_t11 =~ sc11_task_completion + sc11_distracted + sc11_fidgeting + sc11_think_act + sc11_restless + sc11_temper + sc11_obedient + sc11_lying
SC_t14 =~ sc14_task_completion + sc14_distracted + sc14_fidgeting + sc14_think_act + sc14_restless + sc14_temper + sc14_obedient + sc14_lying
SC_t17 =~ sc17_task_completion + sc17_distracted + sc17_fidgeting + sc17_think_act + sc17_restless + sc17_temper + sc17_obedient + sc17_lying
'

# --- Tell semTools which factors/indicators repeat over time
longFacNames <- list(SC = c("SC_t3", "SC_t5", "SC_t7", "SC_t11", "SC_t14", "SC_t17"))
longIndNames <- list(
  task_completion = c("sc3_task_completion", "sc5_task_completion", "sc7_task_completion", "sc11_task_completion", "sc14_task_completion", "sc17_task_completion"),
  distracted = c("sc3_distracted", "sc5_distracted", "sc7_distracted", "sc11_distracted", "sc14_distracted", "sc17_distracted"),
  fidgeting = c("sc3_fidgeting", "sc5_fidgeting", "sc7_fidgeting", "sc11_fidgeting", "sc14_fidgeting", "sc17_fidgeting"),
  think_act = c("sc3_think_act", "sc5_think_act", "sc7_think_act", "sc11_think_act", "sc14_think_act", "sc17_think_act"),
  restless = c("sc3_restless", "sc5_restless", "sc7_restless", "sc11_restless", "sc14_restless", "sc17_restless"),
  temper = c("sc3_temper", "sc5_temper", "sc7_temper", "sc11_temper", "sc14_temper", "sc17_temper"),
  obedient = c("sc3_obedient", "sc5_obedient", "sc7_obedient", "sc11_obedient", "sc14_obedient", "sc17_obedient"),
  lying = c("sc3_lying", "sc5_lying", "sc7_lying", "sc11_lying", "sc14_lying", "sc17_lying")
)

# --- Strong longitudinal MI for ordinals: thresholds + loadings, correlated uniqueness across time
ME <- measEq.syntax(
  configural.model = configural_model,
  data             = merged_data,
  ordered          = ordered_vars,
  parameterization = "theta",
  ID.fac           = "UL",
  ID.cat           = "Wu.Estabrook.2016",
  longFacNames     = longFacNames,
  longIndNames     = longIndNames,
  long.equal       = c("thresholds","loadings"),
  auto             = 1L,        # correlated uniqueness for same item across adjacent waves
  return.fit       = FALSE
)

model_mi <- as.character(ME, single = TRUE)  # model string containing the measurement part + constraints

# --- Second-order latent growth on the first-order factors
# Use irregular spacing and center at age 11 for interpretability
ages <- c(3, 5, 7, 11, 14, 17)
time_center <- ages - 11
fac_names <- c("SC_t3", "SC_t5", "SC_t7", "SC_t11", "SC_t14", "SC_t17")

# Autoregressive structured residuals (adjacent waves)
#    Option A: free but equal within same gap length (2y, 3y) + unique 4y
sr_ar <- '
SC_t5  ~ phi2*SC_t3     # 2-year gap
SC_t7  ~ phi2*SC_t5     # 2-year gap
SC_t11 ~ phi4*SC_t7     # 4-year gap
SC_t14 ~ phi3*SC_t11    # 3-year gap
SC_t17 ~ phi3*SC_t14    # 3-year gap
'

# Distribution of growth factors
sr_growth <- '
# Latent growth factors measured by the time-specific latent factors
i =~ 1*SC_t3 + 1*SC_t5 + 1*SC_t7 + 1*SC_t11 + 1*SC_t14 + 1*SC_t17
s =~ -8*SC_t3 + -6*SC_t5 + -4*SC_t7 + 0*SC_t11 + 3*SC_t14 + 6*SC_t17

# Fix means of time-specific factors so i and s carry all means
SC_t3 ~ 0*1
SC_t5 ~ 0*1
SC_t7 ~ 0*1
SC_t11 ~ 0*1
SC_t14 ~ 0*1
SC_t17 ~ 0*1

# Growth-factor means and (co)variances
i ~ 1
s ~ 1
i ~~ i
s ~~ s
i ~~ s
'

# 3) Do NOT allow additional residual covariances among SC_t* (AR handles dependence)
zero_lat_covs <- '
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
'

# --- Fit
model_sr <- paste(model_mi, sr_growth, sr_ar, zero_lat_covs, sep = "\n")


fit_sr <- lavaan::sem(
  model_sr,
  data             = merged_data,
  ordered          = ordered_vars,
  estimator        = "ULSMV",        # fast & robust for many ordinals
  parameterization = "theta",
  std.lv           = FALSE,
  meanstructure    = TRUE
)

summary(fit_sr, fit.measures = TRUE, standardized = TRUE)

# --- Report robust/scaled fit indices only
round(fitMeasures(fit_sr,
      c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","srmr")), 3)

# --- Implied mean trajectory of the latent self-control factor
PE <- parameterEstimates(fit_sr)
i_mean <- PE$est[PE$lhs == "i" & PE$op == "~1"]
s_mean <- PE$est[PE$lhs == "s" & PE$op == "~1"]
implied_SC_means <- i_mean + s_mean * time_center
names(implied_SC_means) <- paste0("Age", ages)
print(round(implied_SC_means, 3))






##### Model 2: LCM-SR with Latent Basis Growth #####
sr_growth_lb <- '
# Intercept (same as yours; time-specific means fixed below)
i =~ 1*SC_t3 + 1*SC_t5 + 1*SC_t7 + 1*SC_t11 + 1*SC_t14 + 1*SC_t17

# Latent-basis slope: first=0, last=1, middle waves free
s =~ 0*SC_t3 + lb5*SC_t5 + lb7*SC_t7 + lb11*SC_t11 + lb14*SC_t14 + 1*SC_t17

# Time-specific factor means fixed to 0 so growth factors carry the mean
SC_t3  ~ 0*1
SC_t5  ~ 0*1
SC_t7  ~ 0*1
SC_t11 ~ 0*1
SC_t14 ~ 0*1
SC_t17 ~ 0*1

# Growth-factor means/variances/covariances
i ~ 1
s ~ 1
i ~~ i
s ~~ s
i ~~ s
'

# Model with latent basis growth and autoregressive structured residuals
model_sr_lb <- paste(model_mi, sr_growth_lb, zero_lat_covs, sep="\n")

# Fit
fit_sr_lb <- lavaan::sem(
  model_sr_lb,
  data             = merged_data,
  ordered          = ordered_vars,
  estimator        = "WLSMV",
  parameterization = "theta",
  std.lv           = FALSE,
  meanstructure    = TRUE,
)

summary(fit_sr_lb, fit.measures = TRUE, standardized = TRUE)
