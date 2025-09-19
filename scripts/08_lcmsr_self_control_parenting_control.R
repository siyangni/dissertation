# --- Packages
library(pacman)
p_load(lavaan)
p_load(semTools)


# --- Ages and item suffixes (self-control)
ages <- c(3, 5, 7, 11, 14, 17)
item_suffixes <- c("task_completion", "distracted", "fidgeting", "think_act", "temper", "obedient", "lying")

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

# --- Full longitudinal MI for ordinals: thresholds + loadings, correlated uniqueness across time
ME <- measEq.syntax(
  configural.model = configural_model,
  data             = merged_data,
  ordered          = ordered_vars,
  parameterization = "theta",
  ID.fac           = "std.lv",
  ID.cat           = "Wu.Estabrook.2016",
  longFacNames     = longFacNames,
  longIndNames     = longIndNames,
  long.equal       = c("thresholds","loadings"),
  auto             = 1L,       
  return.fit       = FALSE,
  estimator        = "WLSMV"
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
  estimator        = "WLSMV",     
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
  meanstructure    = TRUE
)

summary(fit_sr_lb_parenting, fit.measures = TRUE, standardized = TRUE)

print(round(fitMeasures(fit_sr_lb_parenting,
      c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","srmr")), 3))

# -------------------------------------------------------------------
# Wave-1 covariates and controlled models (Model 3A and 3B)
# -------------------------------------------------------------------

# --- Wave-1 covariates: names and preprocessing -----------------------------
covars_wave1 <- c(
  "sex_factor"
)

# Sanity check for presence
missing_cov <- setdiff(covars_wave1, names(merged_data))
if (length(missing_cov)) {
  stop("The following covariates are missing from 'merged_data': ",
       paste(missing_cov, collapse = ", "))
}

# Helper: dummy-code nominal/character factors; keep 0/1 binaries; z-score continuous
mk_covariates <- function(df, vars) {
  new_names <- character(0)
  for (v in vars) {
    x <- df[[v]]
    if (is.factor(x) || is.character(x)) {
      f <- factor(x)
      # Use most prevalent level as reference (robust default)
      ref <- names(sort(table(f), decreasing = TRUE))[1]
      f <- stats::relevel(f, ref)
      # Build dummies on non-missing rows, then place into full-length matrix (NA where f is NA)
      idx <- which(!is.na(f))
      MM_sub <- stats::model.matrix(~ f, data = data.frame(f = f[idx]))[, -1, drop = FALSE]
      # Safe, readable names: var_Level
      lvl_names <- make.names(levels(f))[-1]
      colnames(MM_sub) <- paste0(v, "_", lvl_names)
      MM_full <- matrix(NA_real_, nrow = nrow(df), ncol = ncol(MM_sub))
      colnames(MM_full) <- colnames(MM_sub)
      MM_full[idx, ] <- MM_sub
      for (nm in colnames(MM_full)) {
        df[[nm]] <- as.numeric(MM_full[, nm])
        df[[nm]][is.na(df[[nm]])] <- 0  # treat missing as reference level
      }
      message(sprintf("'%s': treated as nominal; reference = '%s'; dummies: %s",
                      v, ref, paste(colnames(MM_full), collapse = ", ")))
      new_names <- c(new_names, colnames(MM_full))
    } else if (is.logical(x)) {
      nm <- paste0(v, "_yes")
      df[[nm]] <- as.numeric(x)
      df[[nm]][is.na(df[[nm]])] <- 0   # default missing to 0
      new_names <- c(new_names, nm)
    } else if (is.numeric(x)) {
      uniq <- sort(unique(x[is.finite(x)]))
      if (length(uniq) == 2 && all(uniq %in% c(0, 1))) {
        # Already 0/1
        df[[v]][is.na(df[[v]])] <- 0   # default missing to 0
        new_names <- c(new_names, v)
      } else {
        nm <- paste0(v, "_z")
        z <- as.numeric(scale(x))
        z[is.na(z)] <- 0               # mean-impute (0 after z-score)
        df[[nm]] <- z
        new_names <- c(new_names, nm)
      }
    } else {
      stop("Unsupported type for variable: ", v)
    }
  }
  list(data = df, covar_names = unique(new_names))
}

prep <- mk_covariates(merged_data, covars_wave1)
merged_data      <- prep$data
covariate_names  <- prep$covar_names
if (!length(covariate_names)) stop("No usable covariates were created.")

# Do NOT add these covariates to 'ordered' (they are exogenous predictors)
ordered_all <- unique(c(ordered_vars, parenting_items))

# --- Structural syntax: add covariates to growth factors and SC_t7 -----------
#   Also regress P7 on controls to adjust for wave-1 confounding (recommended).
gi_terms <- paste(covariate_names, collapse = " + ")
gs_terms <- gi_terms  # same set on s

sr_covars_controls <- paste0('\n# Growth factors and age-7 factor regressed on P7 and Wave-1 controls\n',
'i    ~ b_i*P7',  if (nchar(gi_terms))  paste0(" + ", gi_terms)  else "", '\n',
's    ~ b_s*P7',  if (nchar(gs_terms))  paste0(" + ", gs_terms)  else "", '\n',
'SC_t7 ~ b_t7*P7\n')

# --- Model 3A: Linear LCM-SR + Parenting + Wave-1 controls -------------------
model_sr_parenting_ctrl <- paste(model_mi_aug, sr_growth, sr_ar, zero_lat_covs,
                                 sr_covars_controls, sep = "\n")

fit_sr_parenting_ctrl <- lavaan::sem(
  model_sr_parenting_ctrl,
  data             = merged_data,
  ordered          = ordered_all,
  estimator        = "WLSMV",        # recommended for ordinal indicators
  parameterization = "theta",
  std.lv           = FALSE,
  meanstructure    = TRUE,
  fixed.x          = TRUE
)

summary(fit_sr_parenting_ctrl, fit.measures = TRUE, standardized = TRUE)

cat("\n--- Linear LCM-SR + controls: fit ---\n")
print(summary(fit_sr_parenting_ctrl, fit.measures = TRUE, standardized = TRUE))
print(round(fitMeasures(fit_sr_parenting_ctrl,
      c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","srmr")), 3))

# Parenting effect by age (unstandardized), as before
PE <- parameterEstimates(fit_sr_parenting_ctrl)
b_i <- PE$est[PE$lhs == "i"  & PE$op == "~" & PE$rhs == "P7"]
b_s <- PE$est[PE$lhs == "s"  & PE$op == "~" & PE$rhs == "P7"]
parenting_effect_by_age <- b_i + b_s * time_center
names(parenting_effect_by_age) <- paste0("Age", ages)
cat("\n--- Unstandardized parenting effect on latent SC by age ---\n")
print(round(parenting_effect_by_age, 3))

# Effects of each control on expected latent SC at each age (unstandardized)
gi <- setNames(PE$est[PE$lhs == "i"  & PE$op == "~" & PE$rhs %in% covariate_names],
               PE$rhs[PE$lhs == "i"  & PE$op == "~" & PE$rhs %in% covariate_names])
gs <- setNames(PE$est[PE$lhs == "s"  & PE$op == "~" & PE$rhs %in% covariate_names],
               PE$rhs[PE$lhs == "s"  & PE$op == "~" & PE$rhs %in% covariate_names])

covar_effects_by_age <- sapply(time_center, function(tc) gi + gs * tc)
colnames(covar_effects_by_age) <- paste0("Age", ages)
cat("\n--- Unstandardized control effects on latent SC by age (rows = controls) ---\n")
print(round(covar_effects_by_age, 3))

# Standardized primary effects
STD <- standardizedSolution(fit_sr_parenting_ctrl)
b_i_std  <- STD$est.std[STD$lhs == "i"     & STD$op == "~" & STD$rhs == "P7"]
b_s_std  <- STD$est.std[STD$lhs == "s"     & STD$op == "~" & STD$rhs == "P7"]
b_t7_std <- STD$est.std[STD$lhs == "SC_t7" & STD$op == "~" & STD$rhs == "P7"]

gi_std <- setNames(STD$est.std[STD$lhs == "i" & STD$op == "~" & STD$rhs %in% covariate_names],
                   STD$rhs[STD$lhs == "i" & STD$op == "~" & STD$rhs %in% covariate_names])
gs_std <- setNames(STD$est.std[STD$lhs == "s" & STD$op == "~" & STD$rhs %in% covariate_names],
                   STD$rhs[STD$lhs == "s" & STD$op == "~" & STD$rhs %in% covariate_names])

covar_effects_by_age_std <- sapply(time_center, function(tc) gi_std + gs_std * tc)
colnames(covar_effects_by_age_std) <- paste0("Age", ages)

cat("\n--- Standardized effects ---\n")
print(c(b_i_std = b_i_std, b_s_std = b_s_std, b_t7_std = b_t7_std))
cat("\n(Standardized control effects by age available in object 'covar_effects_by_age_std'.)\n")

# --- Model 3B: Latent-Basis LCM-SR + Parenting + Wave-1 controls -------------
model_sr_lb_parenting_ctrl <- paste(model_mi_aug, sr_growth_lb, sr_ar, zero_lat_covs,
                                    sr_covars_controls, sep = "\n")

fit_sr_lb_parenting_ctrl <- lavaan::sem(
  model_sr_lb_parenting_ctrl,
  data             = merged_data,
  ordered          = ordered_all,
  estimator        = "WLSMV",
  parameterization = "theta",
  std.lv           = FALSE,
  meanstructure    = TRUE,
  fixed.x          = TRUE
)

cat("\n--- Latent-basis LCM-SR + controls: fit ---\n")
print(summary(fit_sr_lb_parenting_ctrl, fit.measures = TRUE, standardized = TRUE))
print(round(fitMeasures(fit_sr_lb_parenting_ctrl,
      c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","srmr")), 3))


#