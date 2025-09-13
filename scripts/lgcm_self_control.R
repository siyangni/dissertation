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
time_points <- ages
time_center <- time_points - 11  # c(-8, -6, -4, 0, 3, 6)

# Build all pairwise zero residual covariances among SC_t*
zero_lat_covs <- combn(fac_names, 2, FUN = function(x) sprintf("%s ~~ 0*%s", x[1], x[2]))
# Fix the means of the time-specific factors to 0
fac_means_zero <- paste(paste0(fac_names, " ~ 0*1"), collapse = "\n")

growth_syntax <- paste0('
# Second-order (factor-of-curves) growth on latent self-control
i =~ ', paste(paste0("1*", fac_names), collapse = " + "), '
s =~ ', paste(paste0(time_center, "*", fac_names), collapse = " + "), '

# Growth-factor means and (co)variances
i ~ 1
s ~ 1
i ~~ i
s ~~ s
i ~~ s

# Shut off residual covariances among time-specific factors
', paste(zero_lat_covs, collapse = "\n"), '
')

# --- Fit the full model
fit_lgcm <- lavaan::sem(paste(model_mi, fac_means_zero, growth_syntax, sep = "\n"),
                        data = merged_data,
                        ordered = ordered_vars,
                        estimator = "ULSMV",
                        parameterization = "theta",
                        std.lv = FALSE,             
                        meanstructure = TRUE)

summary(fit_lgcm, fit.measures = TRUE, standardized = TRUE)

# --- Report robust/scaled fit indices only
round(fitMeasures(fit_lgcm,
      c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","srmr")), 3)

# --- Implied mean trajectory of the latent self-control factor
PE <- parameterEstimates(fit_lgcm)
i_mean <- PE$est[PE$lhs == "i" & PE$op == "~1"]
s_mean <- PE$est[PE$lhs == "s" & PE$op == "~1"]
implied_SC_means <- i_mean + s_mean * time_center
names(implied_SC_means) <- paste0("Age", ages)
print(round(implied_SC_means, 3))
