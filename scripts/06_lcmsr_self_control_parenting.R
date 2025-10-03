# --- (Optional but recommended) Grand-mean center the parenting TVC
#     so that growth-factor means remain interpretable when SC_t* ~ 0*1.
merged_data$parenting_age5_c  <- scale(merged_data$parenting_age5,  center = TRUE, scale = FALSE)
merged_data$parenting_age7_c  <- scale(merged_data$parenting_age7,  center = TRUE, scale = FALSE)
merged_data$parenting_age11_c <- scale(merged_data$parenting_age11, center = TRUE, scale = FALSE)

# Do not add exogenous covariates (e.g., sex, race) to 'ordered_vars'

# --- LCM-SR with Latent-Basis Growth (your Model 2), unchanged core
sr_growth_lb <- '
# Intercept measured equally across waves
i =~ 1*SC_t5 + 1*SC_t7 + 1*SC_t11 + 1*SC_t14 + 1*SC_t17

# Latent-basis slope: first=0, last=1; middle freely estimated
s =~ 0*SC_t5 + lb7*SC_t7 + lb11*SC_t11 + lb14*SC_t14 + 1*SC_t17

# Time-specific factor means fixed to 0 so growth factors carry the mean
SC_t5  ~ 0*1
SC_t7  ~ 0*1
SC_t11 ~ 0*1
SC_t14 ~ 0*1
SC_t17 ~ 0*1

# Growth-factor means, variances, covariance
i ~ 1
s ~ 1
i ~~ i
s ~~ s
i ~~ s
'

# Center the age-14 and 17 parental monitoring covariate
merged_data$pm14_c <- scale(merged_data$parental_monitoring_age14, center = TRUE, scale = FALSE)
merged_data$pm17_c <- scale(merged_data$ngcoutw00, center = TRUE, scale = FALSE)

# --- NEW: Time-varying covariate (TVC): Parenting at ages 5, 7, 11
#     Regress each time-specific latent factor on the contemporaneous parenting composite.
#     Effects are free across time by default; uncomment constraints below to test equality.
tvc_block <- '
# Time-varying covariate (grand-mean centered)
SC_t5  ~ b_p5*parenting_age5_c
SC_t7  ~ b_p7*parenting_age7_c
SC_t11 ~ b_p11*parenting_age11_c
SC_t14 ~ b_pm14*pm14_c
SC_t17 ~ b_pm17*pm17_c
'

# --- NEW: Time-invariant covariates (TICs) predicting growth factors i and s
#     Coefficients are labeled to facilitate interpretation and Wald tests.

# --- Impute missing covariate data (required for sampling weights + fixed.x = TRUE)
#     Mean imputation for parents_education_binary (25% missing)
merged_data$parents_education_binary_imp <- merged_data$parents_education_binary
missing_idx <- is.na(merged_data$parents_education_binary_imp)
if (sum(missing_idx) > 0) {
  impute_value <- mean(merged_data$parents_education_binary, na.rm = TRUE)
  merged_data$parents_education_binary_imp[missing_idx] <- impute_value
  message(sprintf("Imputed %d missing values (%.1f%%) for parents_education_binary with mean = %.3f",
                  sum(missing_idx), 100*mean(missing_idx), impute_value))
}

# --- Update tic_block to use imputed variable
tic_block <- '
i ~ sex_i*sex
   + cog_i*cognitive_ability
   + lbw_i*low_birthweight
   + it_i*infant_temperament
   + hfae_i*heavy_fetal_alcohol_exposure
   + marital_i*marital_status
   + race_i*race
   + parents_education_i*parents_education_binary_imp

s ~ sex_s*sex
   + cog_s*cognitive_ability
   + lbw_s*low_birthweight
   + it_s*infant_temperament
   + hfae_s*heavy_fetal_alcohol_exposure
   + marital_s*marital_status
   + race_s*race
   + parents_education_s*parents_education_binary_imp
'

# --- Assemble the full Model 2 with TVC + TICs + AR structured residuals
#     (Note: we include sr_ar here — this was missing in your earlier paste for Model 2.)
model_sr_lb_cov <- paste(model_mi,
                         sr_growth_lb,
                         sr_ar,
                         zero_lat_covs,
                         tvc_block,
                         tic_block,
                         sep = "\n")

# --- Fit (with enhanced convergence options)
fit_sr_lb_cov <- lavaan::sem(
  model_sr_lb_cov,
  data             = merged_data,
  ordered          = ordered_vars,
  estimator        = "WLSMV",
  parameterization = "theta",
  std.lv           = FALSE,
  meanstructure    = TRUE,
  sampling.weights = "weight1",
  fixed.x          = TRUE,            # Required with sampling.weights
  optim.method     = "nlminb",        # Can be more stable than default
  control          = list(iter.max = 1000, eval.max = 1000)  # Increase iterations
)

# --- Output: summary and robust/scaled fit indices (WLSMV)
summary(fit_sr_lb_cov, fit.measures = TRUE, standardized = TRUE)

if (lavaan::lavInspect(fit_sr_lb_cov, "converged")) {
  print(round(fitMeasures(fit_sr_lb_cov,
        c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","srmr")), 3))
} else {
  message("Model did not converge; skipping fit measures.")
}

# --- Extract key results: TVC effects and TIC effects on growth
PE <- parameterEstimates(fit_sr_lb_cov, standardized = TRUE)

# TVC effects (parenting -> SC_t*)
tvc_rows <- PE[PE$op == "~" & PE$lhs %in% c("SC_t5","SC_t7","SC_t11") &
                 PE$rhs %in% c("parenting_age5_c","parenting_age7_c","parenting_age11_c"),
               c("lhs","op","rhs","est","se","pvalue","std.all")]
tvc_rows

# TIC effects on growth factors
tic_rows <- PE[PE$op == "~" & PE$lhs %in% c("i","s") &
                 PE$rhs %in% c("parental_monitoring_age14","ngcoutw00","sex_factor",
                               "cognitive_ability","infant_temperament",
                               "low_birthweight","heavy_fetal_alcohol_exposure"),
               c("lhs","op","rhs","est","se","pvalue","std.all")]
tic_rows

# --- Implied latent-basis mean trajectory (using estimated basis loadings)
i_mean <- PE$est[PE$lhs == "i" & PE$op == "~1"]
s_mean <- PE$est[PE$lhs == "s" & PE$op == "~1"]

# Extract estimated latent-basis loadings for s (0, lb7, lb11, lb14, 1)
lb7  <- PE$est[PE$lhs == "s" & PE$op == "=~" & PE$rhs == "SC_t7"]
lb11 <- PE$est[PE$lhs == "s" & PE$op == "=~" & PE$rhs == "SC_t11"]
lb14 <- PE$est[PE$lhs == "s" & PE$op == "=~" & PE$rhs == "SC_t14"]
basis_loadings <- c(Age5 = 0, Age7 = lb7, Age11 = lb11, Age14 = lb14, Age17 = 1)

implied_SC_means_lb <- i_mean + s_mean * basis_loadings
round(implied_SC_means_lb, 3)
