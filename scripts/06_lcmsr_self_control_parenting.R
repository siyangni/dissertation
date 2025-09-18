# --- Packages
library(pacman)
p_load(lavaan)
p_load(semTools)

# --- Load data if not already loaded
utils::globalVariables(c("merged_data", "recoded_parenting_age7"))

if (!exists("merged_data")) {
  data_path <- "/home/siyang/dissertation_folder/data"
  merged_data <- readRDS(file.path(data_path, "merged1203.rds"))
}

# Check if self-control variables are recoded, if not, run recoding script
if (sum(grepl("^sc(3|5|7|11|14|17)_", names(merged_data))) == 0) {
  source("/home/siyang/dissertation_folder/dissertation/scripts/recode_self_control.R")
}

# --- Ages and item suffixes (self-control)
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
  auto             = 1L,        # (keeps your prior setting)
  return.fit       = FALSE
)

model_mi <- as.character(ME, single = TRUE)  # measurement part + constraints

# --- Second-order latent growth on the first-order factors
# Use irregular spacing and center at age 11 for interpretability
time_center <- ages - 11
fac_names   <- paste0("SC_t", ages)

# Autoregressive structured residuals (adjacent waves)
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

# Do NOT allow additional residual covariances among SC_t* (AR handles dependence)
zero_lat_covs <- combn(fac_names, 2, FUN = function(x) sprintf("%s ~~ 0*%s", x[1], x[2]))
zero_lat_covs <- paste(zero_lat_covs, collapse = "\n")

# -------------------------------------------------------------------
# Parenting at age 7 as a latent construct (separate data frame)
# -------------------------------------------------------------------
id_var <- "id"
parenting_items <- c("tell_off","take_away_treats","timeout","reason")

# Add parenting items to the 'ordered' vector used by lavaan
ordered_all <- unique(c(ordered_vars, parenting_items))

# Latent parenting factor at age 7 (ordered indicators)
parenting_model <- '
# Appropriate parenting at age 7 (higher = more appropriate)
P7 =~ tell_off + take_away_treats + timeout + reason
# Anchor latent mean at 0 for identification; variance is freely estimated
P7 ~ 0*1
P7 ~~ P7
'

# Structural paths: parenting predicting growth (and contemporaneous age-7 factor)
sr_covars <- '
i  ~ b_i*P7
s  ~ b_s*P7
SC_t7 ~ b_t7*P7   # Comment out this line to test growth-only effects
'

# -------------------------------------------------------------------
# Model 1: Linear LCM-SR + Parenting
# -------------------------------------------------------------------
model_mi_aug <- paste(model_mi, parenting_model, sep = "\n")
model_sr_parenting <- paste(model_mi_aug, sr_growth, sr_ar, zero_lat_covs, sr_covars, sep = "\n")

fit_sr_parenting <- lavaan::sem(
  model_sr_parenting,
  data             = merged_data,
  ordered          = ordered_all,
  estimator        = "ULSMV",        # fast & robust for many ordinals
  parameterization = "theta",
  std.lv           = FALSE,
  meanstructure    = TRUE
)

summary(fit_sr_parenting, fit.measures = TRUE, standardized = TRUE)

# Report robust/scaled fit indices only
print(round(fitMeasures(fit_sr_parenting,
      c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","srmr")), 3))

# Parenting effect on expected latent SC at each age (unstandardized unit-change in P7)
PE <- parameterEstimates(fit_sr_parenting)
b_i <- PE$est[PE$lhs == "i" & PE$op == "~" & PE$rhs == "P7"]
b_s <- PE$est[PE$lhs == "s" & PE$op == "~" & PE$rhs == "P7"]
parenting_effect_by_age <- b_i + b_s * time_center
names(parenting_effect_by_age) <- paste0("Age", ages)
print(round(parenting_effect_by_age, 3))

# Standardized effects (primary effect sizes)
STD <- standardizedSolution(fit_sr_parenting)
b_i_std  <- STD$est.std[STD$lhs == "i"     & STD$op == "~" & STD$rhs == "P7"]
b_s_std  <- STD$est.std[STD$lhs == "s"     & STD$op == "~" & STD$rhs == "P7"]
b_t7_std <- STD$est.std[STD$lhs == "SC_t7" & STD$op == "~" & STD$rhs == "P7"]
print(c(b_i_std = b_i_std, b_s_std = b_s_std, b_t7_std = b_t7_std))

# -------------------------------------------------------------------
# Model 2: LCM-SR with Latent Basis Growth + Parenting
# -------------------------------------------------------------------
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

model_sr_lb_parenting <- paste(model_mi_aug, sr_growth_lb, sr_ar, zero_lat_covs, sr_covars, sep = "\n")

fit_sr_lb_parenting <- lavaan::sem(
  model_sr_lb_parenting,
  data             = merged_data,
  ordered          = ordered_all,
  estimator        = "WLSMV",
  parameterization = "theta",
  std.lv           = FALSE,
  meanstructure    = TRUE,
  sampling.weights = "weight1"
)

summary(fit_sr_lb_parenting, fit.measures = TRUE, standardized = TRUE)
print(round(fitMeasures(fit_sr_lb_parenting,
      c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","srmr")), 3))
