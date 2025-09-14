# --- Packages
library(lavaan)
library(semTools)

# --- Ages and item suffixes
ages <- c(3, 5, 7, 11, 14, 17)
item_suffixes <- c("task_completion","distracted","fidgeting","think_act",
                   "restless","temper","obedient","lying")

# --- Ensure intersection of indicators present at all ages (no silent misalignment)
exists_at_all <- function(suf) all(paste0("sc", ages, "_", suf) %in% names(merged_data))
common_suf <- item_suffixes[sapply(item_suffixes, exists_at_all)]
if (length(common_suf) < 3L) stop("Need at least 3 common indicators across all ages.")

# --- Variables per age (wide data)
var_by_age <- lapply(ages, function(a) paste0("sc", a, "_", common_suf))
names(var_by_age) <- paste0("t", ages)
ordered_vars <- unlist(var_by_age, use.names = FALSE)

# --- First-order latent factors per wave
fac_names <- paste0("SC_t", ages)
lhs <- fac_names
rhs <- sapply(ages, function(a) paste(paste0("sc", a, "_", common_suf), collapse = " + "))
configural_model <- paste(paste(lhs, "=~", rhs), collapse = "\n")

# --- Tell semTools which factors/indicators repeat over time
longFacNames <- list(SC = fac_names)
longIndNames <- setNames(
  lapply(common_suf, function(suf) paste0("sc", ages, "_", suf)),
  common_suf
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
time_center <- ages - 11
fac_names   <- paste0("SC_t", ages)

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
sr_growth <- paste0('
# Latent growth factors measured by the time-specific latent factors
i =~ ', paste(paste0("1*",  fac_names), collapse = " + "), '
s =~ ', paste(paste0(time_center, "*", fac_names), collapse = " + "), '

# Fix means of time-specific factors so i and s carry all means
', paste(paste0(fac_names, " ~ 0*1"), collapse = "\n"), '

# Growth-factor means and (co)variances
i ~ 1
s ~ 1
i ~~ i
s ~~ s
i ~~ s
')

# 3) Do NOT allow additional residual covariances among SC_t* (AR handles dependence)
zero_lat_covs <- combn(fac_names, 2, FUN = function(x) sprintf("%s ~~ 0*%s", x[1], x[2]))
zero_lat_covs <- paste(zero_lat_covs, collapse = "\n")

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
