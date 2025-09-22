# =============================================================================
# Self-control scale evaluation across ages (3,5,7,11,14,17)
# Requires that 'merged_data' with the recoded variables already exists, e.g.,
# columns named: sc{age}_{key}, where keys are the eight items below.
# =============================================================================

# --------------------------
# 0) Packages and data check
# --------------------------
library(pacman)
p_load(tidyverse, psych, lavaan, GPArotation)

# --------------------------
# 1) Configuration
# --------------------------
ages <- c(3, 5, 7, 11, 14, 17)

# Items to evaluate for the self-control scale
keys <- c("task_completion", "distracted", "fidgeting", "restless",
          "think_act", "temper", "obedient", "lying")

# Minimum number of items and observations required to attempt each analysis
min_items <- 3L
min_obs   <- 20L

# Tetrachoric requires dichotomous items. We dichotomize 0 vs (1 or 2).
# You can change the threshold below if you prefer a different split.
dichotize_fun <- function(x) {
  ifelse(is.na(x), NA_integer_, ifelse(x > 0, 1L, 0L))
}

# --------------------------
# 2) Utility helpers
# --------------------------
alpha_from_cor <- function(R) {
  # Cronbach's alpha from a correlation matrix
  if (any(is.na(R))) return(NA_real_)
  k <- ncol(R)
  rbar <- mean(R[lower.tri(R)], na.rm = TRUE)
  (k * rbar) / (1 + (k - 1) * rbar)
}

safe_polychoric <- function(dat) {
  # dat: data.frame/matrix of ordered 0,1,2 integers
  out <- tryCatch(
    psych::polychoric(dat),
    error = function(e) {
      message("polychoric failed: ", conditionMessage(e), " — falling back to Pearson.")
      list(rho = suppressWarnings(cor(dat, use = "pairwise.complete.obs")))
    }
  )
  out$rho
}

safe_tetrachoric <- function(dat_bin) {
  # dat_bin: data.frame/matrix of 0/1 integers
  out <- tryCatch(
    psych::tetrachoric(dat_bin),
    error = function(e) {
      message("tetrachoric failed: ", conditionMessage(e), " — returning NA.")
      return(NULL)
    }
  )
  if (is.null(out)) return(matrix(NA_real_, ncol = ncol(dat_bin), nrow = ncol(dat_bin),
                                  dimnames = list(colnames(dat_bin), colnames(dat_bin))))
  out$rho
}

safe_omega_from_R <- function(R, n) {
  out <- tryCatch(
    psych::omega(R, n.obs = n, plot = FALSE),
    error = function(e) {
      message("omega failed: ", conditionMessage(e))
      return(NULL)
    }
  )
  out
}

safe_cfa <- function(dat_ord, items) {
  model <- paste0("SelfCtrl =~ ", paste(items, collapse = " + "))
  fit <- tryCatch(
    lavaan::cfa(model,
                data = dat_ord,
                ordered = items,
                estimator = "WLSMV",
                std.lv = TRUE,
                missing = "pairwise"),
    error = function(e) {
      message("CFA failed: ", conditionMessage(e))
      return(NULL)
    }
  )
  fit
}

safe_principal_from_R <- function(R) {
  R_s <- tryCatch(psych::cor.smooth(R), error = function(e) R)
  out <- tryCatch(
    psych::principal(r = R_s, nfactors = 1, rotate = "none", covar = FALSE),
    error = function(e) {
      message("PCA failed: ", conditionMessage(e))
      return(NULL)
    }
  )
  list(pca = out, R = R_s)
}

summarize_fit <- function(fit) {
  if (is.null(fit)) return(tibble(cfi = NA_real_, tli = NA_real_, rmsea = NA_real_, srmr = NA_real_,
                                  chisq = NA_real_, df = NA_real_, pval = NA_real_))
  fm <- lavaan::fitMeasures(fit, c("cfi","tli","rmsea","srmr","chisq","df","pvalue"))
  tibble(cfi = unname(fm["cfi"]),
         tli = unname(fm["tli"]),
         rmsea = unname(fm["rmsea"]),
         srmr = unname(fm["srmr"]),
         chisq = unname(fm["chisq"]),
         df = unname(fm["df"]),
         pval = unname(fm["pvalue"]))
}

extract_cfa_loadings <- function(fit) {
  if (is.null(fit)) return(tibble(item = character(), std_loading = numeric()))
  loadings <- lavaan::standardizedSolution(fit)
  loadings %>%
    filter(op == "=~") %>%
    transmute(item = rhs, std_loading = est.std)
}

# --------------------------
# 3) Main analysis loop
# --------------------------
results <- vector("list", length(ages))
names(results) <- paste0("age_", ages)

summary_rows <- list()

for (age in ages) {
  message("\n==========================")
  message("Analyzing age ", age)
  message("==========================")
  
  item_cols <- paste0("sc", age, "_", keys)
  item_cols <- item_cols[item_cols %in% names(df)]
  if (length(item_cols) < min_items) {
    message("Age ", age, ": fewer than ", min_items, " requested items found. Skipping.")
    results[[paste0("age_", age)]] <- list(
      age = age, n_items = length(item_cols), note = "Insufficient items"
    )
    next
  }
  
  # Keep only selected columns, ensure integer 0/1/2, then drop rows without any observed values
  dat_raw <- df %>% select(all_of(item_cols))
  # Try to coerce to numeric (in case factor/labelled)
  dat_num <- dat_raw %>% mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.x)))))
  
  # Row-wise complete cases for alpha and omega (pairwise correlations are handled inside psych when needed)
  dat_cc <- dat_num %>% drop_na()
  n_obs_all <- nrow(dat_num)
  n_obs_cc  <- nrow(dat_cc)
  
  if (n_obs_cc < min_obs) {
    message("Age ", age, ": fewer than ", min_obs, " complete observations (n=", n_obs_cc, "). Analyses may be unstable.")
  }
  
  # 3.1 Cronbach's alpha on raw 0-2 scores
  a_cron <- tryCatch(
    {
      aa <- psych::alpha(dat_cc, check.keys = FALSE, warnings = FALSE)
      aa$total$std.alpha
    },
    error = function(e) {
      message("Cronbach alpha failed: ", conditionMessage(e)); NA_real_
    }
  )
  
  # 3.2 Tetrachoric alpha (dichotomize: 0 vs 1-2)
  dat_bin <- dat_num %>% mutate(across(everything(), dichotize_fun)) %>% drop_na()
  n_obs_tet <- nrow(dat_bin)
  R_tet <- if (n_obs_tet >= min_obs && ncol(dat_bin) >= min_items) {
    safe_tetrachoric(dat_bin)
  } else {
    matrix(NA_real_, ncol = ncol(dat_bin), nrow = ncol(dat_bin),
           dimnames = list(colnames(dat_bin), colnames(dat_bin)))
  }
  a_tetra <- if (all(!is.na(R_tet))) alpha_from_cor(R_tet) else NA_real_
  
  # 3.3 Ordinal omega (polychoric)
  dat_poly <- dat_num %>% drop_na()
  n_obs_poly <- nrow(dat_poly)
  R_poly <- if (n_obs_poly >= min_obs && ncol(dat_poly) >= min_items) {
    safe_polychoric(dat_poly)
  } else {
    suppressWarnings(cor(dat_poly, use = "pairwise.complete.obs"))
  }
  # Smooth if needed
  R_poly_s <- tryCatch(psych::cor.smooth(R_poly), error = function(e) R_poly)
  
  omega_out <- if (!any(is.na(R_poly_s))) safe_omega_from_R(R_poly_s, n = n_obs_poly) else NULL
  omega_total <- if (!is.null(omega_out)) unname(omega_out$omega.tot) else NA_real_
  omega_hier  <- if (!is.null(omega_out)) unname(omega_out$omega_h)   else NA_real_
  
  # 3.4 CFA (single factor, ordered, WLSMV)
  dat_ord <- dat_num %>%
    mutate(across(everything(), ~ factor(.x, levels = c(0,1,2), ordered = TRUE))) %>%
    drop_na()
  cfa_fit <- if (nrow(dat_ord) >= min_obs) safe_cfa(dat_ord, colnames(dat_ord)) else NULL
  cfa_fit_tbl <- summarize_fit(cfa_fit)
  cfa_load_tbl <- extract_cfa_loadings(cfa_fit)
  
  # 3.5 PCA (on polychoric R)
  pca_list <- if (!any(is.na(R_poly_s))) safe_principal_from_R(R_poly_s) else list(pca = NULL, R = R_poly_s)
  eigvals <- tryCatch(eigen(pca_list$R, only.values = TRUE)$values, error = function(e) rep(NA_real_, ncol(dat_cc)))
  ev1     <- if (length(eigvals)) eigvals[1] else NA_real_
  prop1   <- if (length(eigvals)) ev1 / sum(eigvals) else NA_real_
  pca_load_tbl <- if (!is.null(pca_list$pca)) {
    tibble(item = rownames(pca_list$pca$loadings),
           pc1_loading = as.numeric(pca_list$pca$loadings[,1]))
  } else {
    tibble(item = colnames(R_poly_s), pc1_loading = NA_real_)
  }
  
  # Collect per-age results
  res <- list(
    age            = age,
    items_used     = colnames(dat_cc),
    n_obs_all      = n_obs_all,
    n_obs_complete = n_obs_cc,
    n_obs_tetra    = n_obs_tet,
    n_obs_poly     = n_obs_poly,
    alpha_cronbach = a_cron,
    alpha_tetra    = a_tetra,
    omega_total    = omega_total,
    omega_hier     = omega_hier,
    R_tetra        = R_tet,
    R_poly         = R_poly_s,
    cfa_fit        = cfa_fit,
    cfa_fit_meas   = cfa_fit_tbl,
    cfa_loadings   = cfa_load_tbl,
    pca_ev1        = ev1,
    pca_prop1      = prop1,
    pca_loadings   = pca_load_tbl
  )
  results[[paste0("age_", age)]] <- res
  
  # Print a concise one-line summary for this age
  summary_rows[[length(summary_rows) + 1L]] <- tibble(
    age = age,
    n_items = length(colnames(dat_cc)),
    n_obs = n_obs_cc,
    alpha_tetra = round(a_tetra, 3),
    alpha_cron  = round(a_cron, 3),
    omega_total = round(omega_total, 3),
    omega_hier  = round(omega_hier, 3),
    cfi   = round(cfa_fit_tbl$cfi, 3),
    tli   = round(cfa_fit_tbl$tli, 3),
    rmsea = round(cfa_fit_tbl$rmsea, 3),
    srmr  = round(cfa_fit_tbl$srmr, 3),
    pc1_ev = round(ev1, 3),
    pc1_prop = round(prop1, 3)
  )
}

summary_tbl <- bind_rows(summary_rows) %>%
  arrange(age)

# --------------------------
# 4) Output
# --------------------------
message("\n========== SUMMARY BY AGE ==========")
print(summary_tbl, n = Inf, width = Inf)

message("\nNote:")
message(" - alpha_tetra is Cronbach's alpha computed from a tetrachoric correlation matrix after dichotomizing items (0 vs 1–2).")
message(" - omega_total and omega_hier are computed from a polychoric correlation matrix (ordinal omega).")
message(" - CFA uses a single-factor model with WLSMV for ordered categories; loadings and fit indices are stored.")
message(" - PCA is performed on the polychoric correlation matrix; pc1_prop is the proportion of variance explained by the first component.")

# The 'results' object holds detailed objects for each age:
#   results$age_3$cfa_loadings, results$age_3$cfa_fit_meas, results$age_3$pca_loadings, etc.
# You can inspect them directly, for example:
# results$age_11$cfa_loadings
# results$age_11$cfa_fit_meas
# results$age_11$pca_loadings










# =============================================================================
# Add-ons: Parallel Analysis, Item Diagnostics, and Invariance Skeleton
# Assumes you already ran your earlier script and have:
#   - merged_data (data.frame / tibble)
#   - ages <- c(3, 5, 7, 11, 14, 17)
#   - keys <- c("task_completion","distracted","fidgeting","restless",
#               "think_act","temper","obedient","lying")
#   - results (optional; will be created/extended)
# =============================================================================

library(pacman)
p_load(tidyverse, psych, lavaan, semTools)

stopifnot(exists("merged_data"))
df <- as_tibble(merged_data)

# Ensure 'ages' and 'keys' exist if not in the current environment
if (!exists("ages")) ages <- c(3, 5, 7, 11, 14, 17)
if (!exists("keys")) keys <- c("task_completion","distracted","fidgeting","restless",
                               "think_act","temper","obedient","lying")

# Create results container if absent
if (!exists("results")) {
  results <- vector("list", length(ages))
  names(results) <- paste0("age_", ages)
}

# --------------------------
# (a) Parallel analysis helper
# --------------------------
run_pa_poly <- function(dat_ord_num, n_iter = 500L) {
  # dat_ord_num: numeric 0–2, rows = people, cols = items (no NAs)
  out <- tryCatch(
    psych::fa.parallel(dat_ord_num, fa = "fa", fm = "minres",
                       cor = "poly", n.iter = n_iter, plot = FALSE),
    error = function(e) {
      message("fa.parallel (poly) failed: ", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(out)) return(list(nfact = NA_integer_, ncomp = NA_integer_,
                                fa_ev = NA, pc_ev = NA,
                                fa_sim = NA, pc_sim = NA))
  list(
    nfact = unname(out$nfact %||% NA_integer_),
    ncomp = unname(out$ncomp %||% NA_integer_),
    fa_ev = out$fa.values,
    pc_ev = out$pc.values,
    fa_sim = out$fa.sim,
    pc_sim = out$pc.sim
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------
# (b) Item diagnostics: CITC + CFA loadings with flags
# ---------------------------------------------
flag_item_table <- function(item_tbl, citc_thr_caution = .30, load_thr_caution = .50, load_thr_review = .40) {
  # item_tbl must have columns: item, citc, std_loading
  item_tbl %>%
    mutate(
      flag_citc   = case_when(is.na(citc)          ~ "missing",
                              citc < 0.20          ~ "review",
                              citc < citc_thr_caution ~ "caution",
                              TRUE                 ~ "ok"),
      flag_loading = case_when(is.na(std_loading)     ~ "missing",
                               std_loading < load_thr_review ~ "review",
                               std_loading < load_thr_caution ~ "caution",
                               TRUE                   ~ "ok"),
      any_flag = pmax((flag_citc != "ok"),
                      (flag_loading != "ok"), na.rm = TRUE)
    ) %>%
    arrange(desc(any_flag), item)
}

# ---------------------------------------------
# Main per-age loop to add PA + item diagnostics
# ---------------------------------------------
for (age in ages) {
  item_cols <- paste0("sc", age, "_", keys)
  item_cols <- item_cols[item_cols %in% names(df)]
  if (length(item_cols) < 3L) {
    message("Age ", age, ": not enough items found. Skipping PA/Diagnostics.")
    next
  }

  dat_raw <- df %>% select(all_of(item_cols))
  dat_num <- dat_raw %>% mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.x)))))

  # Complete cases for analyses that need them
  dat_cc <- dat_num %>% drop_na()

  # ---------- (a) Parallel analysis on polychoric ----------
  pa_res <- if (nrow(dat_cc) >= 50L) run_pa_poly(dat_cc) else list(nfact=NA_integer_, ncomp=NA_integer_,
                                                                   fa_ev=NA, pc_ev=NA, fa_sim=NA, pc_sim=NA)

  # ---------- (b) Item diagnostics ----------
  # CITC from psych::alpha (raw 0–2, complete cases)
  alpha_obj <- tryCatch(psych::alpha(dat_cc, check.keys = FALSE, warnings = FALSE),
                        error = function(e) NULL)
  citc_tbl <- if (!is.null(alpha_obj) && !is.null(alpha_obj$item.stats)) {
    tibble(
      item_full = rownames(alpha_obj$item.stats),
      citc = as.numeric(alpha_obj$item.stats[, "r.drop"])
    )
  } else {
    tibble(item_full = colnames(dat_cc), citc = NA_real_)
  }

  # CFA loadings (standardized) from your earlier CFA approach
  # Build ordered factors (0,1,2), fit single-factor CFA with WLSMV
  dat_ord <- dat_num %>%
    mutate(across(everything(), ~ factor(.x, levels = c(0,1,2), ordered = TRUE))) %>%
    drop_na()
  cfa_fit <- tryCatch(
    cfa(paste0("SelfCtrl =~ ", paste(colnames(dat_ord), collapse = " + ")),
        data = dat_ord, ordered = colnames(dat_ord),
        estimator = "WLSMV", std.lv = TRUE, missing = "pairwise"),
    error = function(e) NULL
  )
  load_tbl <- if (!is.null(cfa_fit)) {
    lavaan::standardizedSolution(cfa_fit) %>%
      filter(op == "=~") %>%
      transmute(item_full = rhs, std_loading = est.std)
  } else {
    tibble(item_full = colnames(dat_cc), std_loading = NA_real_)
  }

  # Clean item names to base keys
  item_map <- tibble(
    item_full = item_cols,
    item = sub(paste0("^sc", age, "_"), "", item_cols)
  )

  diag_tbl <- item_map %>%
    left_join(citc_tbl, by = "item_full") %>%
    left_join(load_tbl, by = "item_full") %>%
    select(item, citc, std_loading) %>%
    flag_item_table()

  # Stash in results[[age_*]]
  slot <- paste0("age_", age)
  if (is.null(results[[slot]]) || !is.list(results[[slot]])) results[[slot]] <- list()
  results[[slot]]$parallel_analysis <- pa_res
  results[[slot]]$item_diagnostics  <- diag_tbl

  # Print a quick readable table per age
  message("\n--- Item diagnostics (age ", age, ") ---")
  print(diag_tbl, n = nrow(diag_tbl))
  if (!is.na(pa_res$nfact)) {
    message("Parallel analysis (poly): suggested factors = ", pa_res$nfact,
            "; suggested components = ", pa_res$ncomp, ".")
  } else {
    message("Parallel analysis not available (insufficient complete cases).")
  }
}




# =====================================================================
# (c) Measurement invariance skeleton across ages (ordered, WLSMV)
#     Using semTools::measEq.syntax with Wu & Estabrook (2016) ID.
# =====================================================================

# 1) Build a long data set: one row per person per age, columns = the 8 items
#    We assume rows represent the same children across ages (IDs are row numbers).
df$id_rr <- seq_len(nrow(df))

make_age_block <- function(age) {
  cols <- paste0("sc", age, "_", keys)
  cols <- cols[cols %in% names(df)]
  if (length(cols) != length(keys)) {
    # Fill any missing with NA to keep equal columns
    missing <- setdiff(paste0("sc", age, "_", keys), cols)
    for (m in missing) df[[m]] <<- NA_real_
    cols <- paste0("sc", age, "_", keys)
  }
  out <- df %>% select(id_rr, all_of(cols)) %>%
    setNames(c("id_rr", keys)) %>%
    mutate(age = factor(age, levels = ages))
  out
}

sc_long <- map_dfr(ages, make_age_block)

# Treat items as ordered (0,1,2) for WLSMV
sc_long <- sc_long %>%
  mutate(across(all_of(keys), ~ factor(.x, levels = c(0,1,2), ordered = TRUE)))

# Remove rows with all-NA items for a given age
all_na_row <- sc_long %>%
  select(all_of(keys)) %>%
  apply(1, function(r) all(is.na(r)))
sc_long <- sc_long[!all_na_row, , drop = FALSE]

# 2) Specify single-factor model
onefactor_model <- paste0("SelfCtrl =~ ", paste(keys, collapse = " + "))

# 3) Fit the sequence using direct lavaan syntax
#    We'll use group.equal parameter in cfa() instead of manual syntax

# All models start with the same configural syntax
cfg_syntax <- onefactor_model

fit_cfg <- cfa(cfg_syntax, data = sc_long, group = "age",
               ordered = keys, estimator = "WLSMV", parameterization = "theta")

fit_thr <- cfa(cfg_syntax, data = sc_long, group = "age",
               ordered = keys, estimator = "WLSMV", parameterization = "theta",
               group.equal = c("thresholds"))

fit_met <- cfa(cfg_syntax, data = sc_long, group = "age",
               ordered = keys, estimator = "WLSMV", parameterization = "theta",
               group.equal = c("thresholds", "loadings"))

# 4) Summarize key fit indices and deltas
want <- c("cfi","tli","rmsea","srmr","chisq","df")
mi_tbl <- bind_rows(
  bind_cols(level = "configural", as_tibble_row(fitMeasures(fit_cfg, want)[want])),
  bind_cols(level = "thresholds", as_tibble_row(fitMeasures(fit_thr, want)[want])),
  bind_cols(level = "thr+load",   as_tibble_row(fitMeasures(fit_met, want)[want]))
) %>%
  mutate(across(-level, ~ as.numeric(.x))) %>%
  arrange(factor(level, levels = c("configural","thresholds","thr+load"))) %>%
  mutate(
    d_cfi   = c(NA, diff(cfi)),
    d_rmsea = c(NA, diff(rmsea)),
    d_srmr  = c(NA, diff(srmr))
  )

print(mi_tbl, n = Inf)

message("\nGuides for judging changes (heuristics, not laws): ",
        "ΔCFI ≤ .01 (Cheung & Rensvold, 2002); ",
        "Chen (2007): with unequal/smaller groups, ΔCFI ≤ .005 & ΔRMSEA ≤ .010; ",
        "ΔSRMR ≤ .030 (metric) and ≤ .015 (scalar/thresholds+loadings).")

# Optional: Store MI fits
mi_results <- list(fit_configural = fit_cfg, fit_thresholds = fit_thr, fit_thr_load = fit_met)









# ======================================================================
# Self-control scale: (1) Leave-One-Out metrics, (2) Bifactor indices,
# and (3) MI report helper with partial-invariance suggestions
# Assumes:
#   - merged_data in memory with columns like sc{age}_{key}
#   - ages <- c(3, 5, 7, 11, 14, 17)
#   - keys <- c("task_completion","distracted","fidgeting","restless",
#               "think_act","temper","obedient","lying")
# ======================================================================

library(pacman)
p_load(tidyverse, psych, lavaan, semTools, GPArotation,
       BifactorIndicesCalculator, EFAtools)

stopifnot(exists("merged_data"))
df <- as_tibble(merged_data)
if (!exists("ages")) ages <- c(3, 5, 7, 11, 14, 17)
if (!exists("keys")) keys <- c("task_completion","distracted","fidgeting","restless",
                               "think_act","temper","obedient","lying")

# --------------------------
# Shared utilities
# --------------------------
dichotize_fun <- function(x) ifelse(is.na(x), NA_integer_, ifelse(x > 0, 1L, 0L))

alpha_from_cor <- function(R) {
  if (length(R) == 1L || any(is.na(R))) return(NA_real_)
  k <- ncol(R); rbar <- mean(R[lower.tri(R)], na.rm = TRUE)
  (k * rbar) / (1 + (k - 1) * rbar)
}

safe_polychoric <- function(dat) {
  out <- tryCatch(psych::polychoric(dat), error = function(e) NULL)
  if (is.null(out) || is.null(out$rho)) suppressWarnings(cor(dat, use = "pairwise.complete.obs")) else out$rho
}

safe_tetrachoric <- function(dat_bin) {
  out <- tryCatch(psych::tetrachoric(dat_bin), error = function(e) NULL)
  if (is.null(out) || is.null(out$rho)) {
    matrix(NA_real_, ncol = ncol(dat_bin), nrow = ncol(dat_bin),
           dimnames = list(colnames(dat_bin), colnames(dat_bin)))
  } else out$rho
}

safe_omega_from_R <- function(R, n) {
  tryCatch(psych::omega(R, n.obs = n, plot = FALSE), error = function(e) NULL)
}

summarize_cfa <- function(fit) {
  if (is.null(fit)) return(tibble(cfi = NA_real_, tli = NA_real_, rmsea = NA_real_, srmr = NA_real_))
  fm <- lavaan::fitMeasures(fit, c("cfi","tli","rmsea","srmr"))
  tibble(cfi = unname(fm["cfi"]), tli = unname(fm["tli"]),
         rmsea = unname(fm["rmsea"]), srmr = unname(fm["srmr"]))
}

onefactor_cfa <- function(dat_num) {
  dat_ord <- dat_num %>%
    mutate(across(everything(), ~ factor(.x, levels = c(0,1,2), ordered = TRUE))) %>% drop_na()
  if (nrow(dat_ord) < 20) return(NULL)
  model <- paste0("SelfCtrl =~ ", paste(colnames(dat_ord), collapse = " + "))
  tryCatch(
    lavaan::cfa(model, data = dat_ord, ordered = colnames(dat_ord),
                estimator = "WLSMV", std.lv = TRUE, missing = "pairwise"),
    error = function(e) NULL
  )
}

pca_from_R <- function(R) {
  R_s <- tryCatch(psych::cor.smooth(R), error = function(e) R)
  eig <- tryCatch(eigen(R_s, only.values = TRUE)$values, error = function(e) rep(NA_real_, ncol(R_s)))
  list(ev1 = eig[1], prop1 = if (all(is.finite(eig))) eig[1] / sum(eig) else NA_real_)
}

# =========================================================================
# (1) Leave-One-Out (LOO) per age: recompute core metrics after dropping
#     each item once. Returns a tidy table 'loo_summary'.
# =========================================================================
compute_metrics <- function(dat_num) {
  # Cronbach's alpha (raw 0-2)
  dat_cc <- dat_num %>% drop_na()
  a_cron <- tryCatch(psych::alpha(dat_cc, warnings = FALSE)$total$std.alpha, error = function(e) NA_real_)
  # Tetrachoric alpha
  dat_bin <- dat_num %>% mutate(across(everything(), dichotize_fun)) %>% drop_na()
  R_tet <- if (nrow(dat_bin) >= 20 && ncol(dat_bin) >= 2) safe_tetrachoric(dat_bin) else NULL
  a_tetra <- if (!is.null(R_tet) && all(is.finite(R_tet))) alpha_from_cor(R_tet) else NA_real_
  # Ordinal omega (polychoric)
  dat_poly <- dat_num %>% drop_na()
  R_poly <- if (nrow(dat_poly) >= 20 && ncol(dat_poly) >= 2) safe_polychoric(dat_poly) else suppressWarnings(cor(dat_poly, use = "pairwise.complete.obs"))
  R_poly_s <- tryCatch(psych::cor.smooth(R_poly), error = function(e) R_poly)
  om <- if (!any(is.na(R_poly_s))) safe_omega_from_R(R_poly_s, n = nrow(dat_poly)) else NULL
  omega_total <- if (!is.null(om)) unname(om$omega.tot) else NA_real_
  omega_hier  <- if (!is.null(om)) unname(om$omega_h)   else NA_real_
  # CFA (WLSMV, ordered)
  cfa_fit <- onefactor_cfa(dat_num)
  cfa_meas <- summarize_cfa(cfa_fit)
  # PCA (polychoric R)
  pca <- if (!any(is.na(R_poly_s))) pca_from_R(R_poly_s) else list(ev1 = NA_real_, prop1 = NA_real_)
  list(alpha_cron = a_cron, alpha_tetra = a_tetra,
       omega_total = omega_total, omega_hier = omega_hier,
       cfi = cfa_meas$cfi, tli = cfa_meas$tli, rmsea = cfa_meas$rmsea, srmr = cfa_meas$srmr,
       pc1_ev = pca$ev1, pc1_prop = pca$prop1)
}

loo_summary_rows <- list()

for (age in ages) {
  base_cols <- paste0("sc", age, "_", keys)
  base_cols <- base_cols[base_cols %in% names(df)]
  if (length(base_cols) < 3L) {
    message("Age ", age, ": insufficient items; skipping LOO.")
    next
  }
  dat_all <- df %>% select(all_of(base_cols)) %>% mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.x)))))
  # Leave-one-out over the keys we actually have
  have_keys <- sub(paste0("^sc", age, "_"), "", base_cols)
  for (drop_item in have_keys) {
    kept <- setdiff(base_cols, paste0("sc", age, "_", drop_item))
    dat_num <- dat_all %>% select(all_of(kept))
    m <- compute_metrics(dat_num)
    loo_summary_rows[[length(loo_summary_rows) + 1L]] <- tibble(
      age = age, dropped = drop_item, n_items = ncol(dat_num),
      n_obs_complete = nrow(dat_num %>% drop_na()),
      alpha_tetra = round(m$alpha_tetra, 3),
      alpha_cron  = round(m$alpha_cron, 3),
      omega_total = round(m$omega_total, 3),
      omega_hier  = round(m$omega_hier, 3),
      cfi = round(m$cfi, 3), tli = round(m$tli, 3),
      rmsea = round(m$rmsea, 3), srmr = round(m$srmr, 3),
      pc1_ev = round(m$pc1_ev, 3), pc1_prop = round(m$pc1_prop, 3)
    )
  }
}
loo_summary <- bind_rows(loo_summary_rows) %>% arrange(age, dropped)
message("\n==== Leave-One-Out summary (per age, dropping one item) ====")
print(loo_summary, n = Inf, width = Inf)

# =========================================================================
# (3) Measurement-invariance (MI) report helper
#     Input: three fitted lavaan models from the same data and grouping:
#       - fit_cfg       : configural
#       - fit_thr       : thresholds equal across groups (ordinal step 1)
#       - fit_thr_load  : thresholds + loadings equal (ordinal step 2)
#     Output: table with deltas + auto-flags, and top-N suggestions for
#     partial invariance (thresholds and loadings) from modification indices.
# =========================================================================

# Check if required variables exist
if (!exists("sc_long")) {
  stop("sc_long not found. Please run the measurement invariance skeleton section first.")
}
mi_report <- function(fit_cfg, fit_thr, fit_thr_load, n_suggest = 5,
                      chen_conservative = TRUE) {

  want <- c("cfi","rmsea","srmr")
  m_cfg <- lavaan::fitMeasures(fit_cfg, want)
  m_thr <- lavaan::fitMeasures(fit_thr, want)
  m_met <- lavaan::fitMeasures(fit_thr_load, want)

  d_thr <- c(cfi = unname(m_thr["cfi"] - m_cfg["cfi"]),
             rmsea = unname(m_thr["rmsea"] - m_cfg["rmsea"]),
             srmr = unname(m_thr["srmr"] - m_cfg["srmr"]))
  d_met <- c(cfi = unname(m_met["cfi"] - m_thr["cfi"]),
             rmsea = unname(m_met["rmsea"] - m_thr["rmsea"]),
             srmr = unname(m_met["srmr"] - m_thr["srmr"]))

  # Decision rules: Chen (2007) conservative defaults; Cheung & Rensvold (2002) looser.
  thr_rule <- list(cfi = -0.005, rmsea = 0.010, srmr = 0.015)  # thresholds step (scalar-like)
  met_rule <- list(cfi = -0.005, rmsea = 0.010, srmr = 0.030)  # loadings step (metric-like)
  if (!chen_conservative) {
    thr_rule <- list(cfi = -0.010, rmsea = 0.015, srmr = 0.015)
    met_rule <- list(cfi = -0.010, rmsea = 0.015, srmr = 0.030)
  }

  flag_thr <- (d_thr["cfi"] < thr_rule$cfi) || (d_thr["rmsea"] > thr_rule$rmsea) || (d_thr["srmr"] > thr_rule$srmr)
  flag_met <- (d_met["cfi"] < met_rule$cfi) || (d_met["rmsea"] > met_rule$rmsea) || (d_met["srmr"] > met_rule$srmr)

  # Suggest partial invariance frees from modification indices
  suggest_free <- function(fit, type = c("thresholds","loadings"), n = 5) {
    type <- match.arg(type)
    mi <- tryCatch(lavaan::modindices(fit, sort. = TRUE), error = function(e) NULL)
    if (is.null(mi)) return(tibble())
    if (type == "thresholds") {
      cand <- mi %>% filter(op == "|" & mi > 0) %>%
        transmute(item = lhs, threshold = rhs, group = group, mi, epc = epc)
    } else {
      cand <- mi %>% filter(op == "=~" & mi > 0) %>%
        transmute(item = rhs, param = paste(lhs, op, rhs), group = group, mi, epc = epc)
    }
    cand %>% slice_head(n = min(n, nrow(cand)))
  }

  thr_suggest <- if (flag_thr) suggest_free(fit_thr, "thresholds", n_suggest) else tibble()
  met_suggest <- if (flag_met) suggest_free(fit_thr_load, "loadings", n_suggest) else tibble()

  out_tbl <- tibble(
    level = c("configural","thresholds","thr+loadings"),
    cfi = c(unname(m_cfg["cfi"]), unname(m_thr["cfi"]), unname(m_met["cfi"])),
    rmsea = c(unname(m_cfg["rmsea"]), unname(m_thr["rmsea"]), unname(m_met["rmsea"])),
    srmr = c(unname(m_cfg["srmr"]), unname(m_thr["srmr"]), unname(m_met["srmr"]))
  ) %>%
    mutate(d_cfi = c(NA, diff(cfi)),
           d_rmsea = c(NA, diff(rmsea)),
           d_srmr = c(NA, diff(srmr)),
           flag = c(NA,
                    if (flag_thr) "review thresholds" else "ok",
                    if (flag_met) "review loadings" else "ok"))

  list(table = out_tbl,
       suggest_thresholds = thr_suggest,
       suggest_loadings   = met_suggest)
}

# Example usage (uncomment if you already created these fits earlier):
mi_out <- mi_report(fit_cfg, fit_thr, fit_met, n_suggest = 8, chen_conservative = TRUE)
print(mi_out$table)
if (nrow(mi_out$suggest_thresholds)) { message("\nTop threshold frees to consider:"); print(mi_out$suggest_thresholds) }
if (nrow(mi_out$suggest_loadings))   { message("\nTop loading frees to consider:");   print(mi_out$suggest_loadings) }





# ============================================================
# One-click MI refit on 7-item set + "winner's table" by age
# ============================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(lavaan)
  library(semTools)
  library(psych)
})

# -----------------------------
# CONFIG — adjust if you need to
# -----------------------------
group_var <- "age"   # grouping variable (factor or integer ages)
all_keys  <- c("task_completion","distracted","fidgeting",
               "restless","think_act","temper","obedient","lying")
drop_item <- "restless"                  # 7-item deletion for MI
keys7     <- setdiff(all_keys, drop_item)
onefactor_model_7 <- paste0("SelfCtrl =~ ", paste(keys7, collapse = " + "))

# Δ-fit rules (Chen 2007 conservative defaults)
thr_rules <- list(cfi = -0.005, rmsea = 0.010, srmr = 0.015)  # thresholds step
met_rules <- list(cfi = -0.005, rmsea = 0.010, srmr = 0.030)  # loadings step


# ----------------------------------------------------------
# Utilities for MI Δ-fit and iterative threshold freeing
# (safe to replace your prior versions)
# ----------------------------------------------------------
delta_decision <- function(fit_prev, fit_next, rules) {
  want <- c("cfi","rmsea","srmr")
  a <- lavaan::fitMeasures(fit_prev, want)
  b <- lavaan::fitMeasures(fit_next, want)
  d <- c(cfi = unname(b["cfi"] - a["cfi"]),
         rmsea = unname(b["rmsea"] - a["rmsea"]),
         srmr = unname(b["srmr"] - a["srmr"]))
  ok <- (d["cfi"] >= rules$cfi) && (d["rmsea"] <= rules$rmsea) && (d["srmr"] <= rules$srmr)
  list(delta = d, ok = ok,
       table = tibble::tibble(cfi = unname(b["cfi"]), rmsea = unname(b["rmsea"]), srmr = unname(b["srmr"]),
                              d_cfi = d["cfi"], d_rmsea = d["rmsea"], d_srmr = d["srmr"]))
}

iterative_free_thresholds <- function(base_syntax, data, group, ordered_keys,
                                      rules, max_iter = 10, mi_cut = 3.84, verbose = TRUE) {
  # Configural
  cfg <- lavaan::cfa(base_syntax, data = data, group = group, ordered = ordered_keys,
                     estimator = "WLSMV", parameterization = "theta", std.lv = TRUE)

  # Thresholds equal
  thr_syntax <- semTools::measEq.syntax(
    configural.model = base_syntax,
    data = data, group = group,
    ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
    parameterization = "theta",
    group.equal = c("thresholds"),
    ordered = ordered_keys
  )
  fit_thr <- lavaan::cfa(as.character(thr_syntax),              # <-- as.character()
                         data = data, group = group, ordered = ordered_keys,
                         estimator = "WLSMV", parameterization = "theta")

  step1 <- delta_decision(cfg, fit_thr, rules)
  freed <- tibble::tibble(item = character(), threshold = character(), group = integer(), mi = double())

  iter <- 0
  while (!step1$ok && iter < max_iter) {
    iter <- iter + 1
    mi_tbl <- tryCatch(lavaan::modindices(fit_thr, sort. = TRUE), error = function(e) NULL)
    if (is.null(mi_tbl) || !nrow(mi_tbl)) break
    cand <- mi_tbl %>% dplyr::filter(op == "|" & mi >= mi_cut)
    if (!nrow(cand)) break
    top1 <- cand %>% dplyr::slice(1) %>% dplyr::transmute(
      item = lhs, threshold = rhs, group = group, mi = mi
    )
    freed <- dplyr::bind_rows(freed, top1)

    thr_syntax <- semTools::measEq.syntax(
      configural.model = base_syntax,
      data = data, group = group,
      ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
      parameterization = "theta",
      group.equal = c("thresholds"),
      ordered = ordered_keys,
      group.partial = paste0(top1$item, "|", top1$threshold)
    )
    fit_thr <- lavaan::cfa(as.character(thr_syntax),            # <-- as.character()
                           data = data, group = group, ordered = ordered_keys,
                           estimator = "WLSMV", parameterization = "theta")

    step1 <- delta_decision(cfg, fit_thr, rules)
    if (verbose) message(sprintf(
      "[iter %d] Freed threshold %s|%s; ΔCFI=%.3f ΔRMSEA=%.3f ΔSRMR=%.3f; ok=%s",
      iter, top1$item, top1$threshold,
      step1$delta["cfi"], step1$delta["rmsea"], step1$delta["srmr"],
      step1$ok))
  }

  list(cfg = cfg, thr = fit_thr, freed_thresholds = freed, step1 = step1)
}

# ----------------------------------------------------------
# One-click MI on 7 items (drop 'restless'), with optional auto threshold freeing
# Accepts EITHER wide df + ages OR an already-long sc_long (must have columns age + 7 items)
# Returns $mi_table.
# ----------------------------------------------------------
run_mi_7items <- function(data,
                          ages = NULL,
                          auto_free_thresholds = FALSE,
                          max_thr_iter = 10,
                          mi_cut = 3.84,
                          verbose = TRUE) {
  suppressPackageStartupMessages({
    library(tidyverse); library(lavaan); library(semTools)
  })
  keys7 <- c("task_completion","distracted","fidgeting",
             "think_act","temper","obedient","lying")

  # --- Build (or accept) long data ---
  if (all(c("age", keys7) %in% names(data))) {
    sc7_long <- data %>% dplyr::select(dplyr::any_of(c("id_rr","age", keys7)))
    if (!is.null(ages)) sc7_long$age <- factor(sc7_long$age, levels = ages) else sc7_long$age <- factor(sc7_long$age)
  } else {
    stopifnot(!is.null(ages))
    df <- tibble::as_tibble(data)
    df$id_rr <- seq_len(nrow(df))
    make_age_block7 <- function(age) {
      cols <- paste0("sc", age, "_", keys7)
      cols <- cols[cols %in% names(df)]
      missing <- setdiff(paste0("sc", age, "_", keys7), cols)
      for (m in missing) df[[m]] <<- NA_real_
      cols <- paste0("sc", age, "_", keys7)
      df %>%
        dplyr::select(id_rr, dplyr::all_of(cols)) %>%
        stats::setNames(c("id_rr", keys7)) %>%
        dplyr::mutate(age = factor(age, levels = ages))
    }
    sc7_long <- purrr::map_dfr(ages, make_age_block7)
  }

  # Treat as ordered (0/1/2)
  sc7_long <- sc7_long %>% dplyr::mutate(across(all_of(keys7), ~ factor(.x, levels = c(0,1,2), ordered = TRUE)))
  all_na <- sc7_long %>% dplyr::select(all_of(keys7)) %>% apply(1, function(r) all(is.na(r)))
  sc7_long <- sc7_long[!all_na, , drop = FALSE]

  onefactor7 <- paste0("SelfCtrl =~ ", paste(keys7, collapse = " + "))

  # --- Configural ---
  fit_cfg <- lavaan::cfa(onefactor7, data = sc7_long, group = "age",
                         ordered = keys7, estimator = "WLSMV",
                         parameterization = "theta", std.lv = TRUE)

  # --- Thresholds (with optional auto-freeing) ---
  if (isTRUE(auto_free_thresholds)) {
    rules <- list(cfi = -0.005, rmsea = 0.010, srmr = 0.015)  # Chen (2007) conservative
    iter <- iterative_free_thresholds(
      base_syntax = onefactor7,
      data = sc7_long, group = "age",
      ordered_keys = keys7,
      rules = rules, max_iter = max_thr_iter, mi_cut = mi_cut, verbose = verbose
    )
    fit_thr <- iter$thr
    freed_thresholds <- iter$freed_thresholds
  } else {
    fit_thr <- lavaan::cfa(onefactor7, data = sc7_long, group = "age",
                           ordered = keys7, estimator = "WLSMV",
                           parameterization = "theta", group.equal = c("thresholds"))
    freed_thresholds <- tibble::tibble(item = character(), threshold = character(), group = integer(), mi = double())
  }

  # --- Thresholds + Loadings (respect any freed thresholds) ---
  if (nrow(freed_thresholds)) {
    partial_vec <- paste0(freed_thresholds$item, "|", freed_thresholds$threshold)
    syn <- semTools::measEq.syntax(
      configural.model = onefactor7,
      data = sc7_long, group = "age",
      ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
      parameterization = "theta",
      group.equal = c("thresholds","loadings"),
      ordered = keys7,
      group.partial = partial_vec
    )
    fit_met <- lavaan::cfa(as.character(syn),                    # <-- as.character()
                           data = sc7_long, group = "age",
                           ordered = keys7, estimator = "WLSMV",
                           parameterization = "theta")
  } else {
    fit_met <- lavaan::cfa(onefactor7, data = sc7_long, group = "age",
                           ordered = keys7, estimator = "WLSMV",
                           parameterization = "theta",
                           group.equal = c("thresholds","loadings"))
  }

  want <- c("cfi","tli","rmsea","srmr","chisq","df")
  mi_tbl <- dplyr::bind_rows(
    tibble::tibble(level = "configural")  %>% dplyr::bind_cols(tibble::as_tibble_row(lavaan::fitMeasures(fit_cfg, want)[want])),
    tibble::tibble(level = "thresholds")  %>% dplyr::bind_cols(tibble::as_tibble_row(lavaan::fitMeasures(fit_thr, want)[want])),
    tibble::tibble(level = "thr+load")    %>% dplyr::bind_cols(tibble::as_tibble_row(lavaan::fitMeasures(fit_met, want)[want]))
  ) %>%
    dplyr::mutate(dplyr::across(-level, ~ as.numeric(.x))) %>%
    dplyr::mutate(
      d_cfi   = c(NA, diff(cfi)),
      d_rmsea = c(NA, diff(rmsea)),
      d_srmr  = c(NA, diff(srmr))
    )

  list(
    mi_table = mi_tbl,
    fit_cfg  = fit_cfg,
    fit_thr  = fit_thr,
    fit_met  = fit_met,
    freed_thresholds = freed_thresholds,
    sc7_long = sc7_long
  )
}


mi7 <- run_mi_7items(df, ages = ages, auto_free_thresholds = FALSE)
mi7$mi_table

mi7_auto <- run_mi_7items(df, ages = ages, auto_free_thresholds = TRUE, max_thr_iter = 8)
mi7_auto$mi_table
mi7_auto$freed_thresholds







##### Auto Free Loadings #####

# ===========================
# MI utilities (updated)
# ===========================
suppressPackageStartupMessages({
  library(tidyverse)
  library(lavaan)
  library(semTools)
})

# Decision rule (Chen, 2007 conservative defaults)
.mi_rules_thr <- list(cfi = -0.005, rmsea = 0.010, srmr = 0.015)  # thresholds step
.mi_rules_met <- list(cfi = -0.005, rmsea = 0.010, srmr = 0.030)  # loadings step

.delta_decision <- function(fit_prev, fit_next, rules) {
  want <- c("cfi","rmsea","srmr")
  a <- lavaan::fitMeasures(fit_prev, want)
  b <- lavaan::fitMeasures(fit_next, want)
  d <- c(cfi = unname(b["cfi"] - a["cfi"]),
         rmsea = unname(b["rmsea"] - a["rmsea"]),
         srmr = unname(b["srmr"] - a["srmr"]))
  ok <- (d["cfi"] >= rules$cfi) && (d["rmsea"] <= rules$rmsea) && (d["srmr"] <= rules$srmr)
  list(delta = d, ok = ok)
}

# ---------- C) Loadings loop (partial metric invariance via lavTestScore) ----------
.iter_free_loadings <- function(base_syntax, data, group, ordered_keys,
                                fit_thr, thr_partials = character(),
                                rules = .mi_rules_met, max_iter = 10,
                                mi_cut = 3.84, verbose = TRUE) {

  # Start from metric model (thresholds + loadings equal), respecting any freed thresholds
  fit_met <- lavaan::cfa(base_syntax, data = data, group = group,
                         ordered = ordered_keys, estimator = "WLSMV",
                         parameterization = "theta", std.lv = TRUE,
                         group.equal = c("thresholds","loadings"),
                         group.partial = thr_partials)

  step2 <- .delta_decision(fit_thr, fit_met, rules)

  freedL <- tibble::tibble(item = character(),
                           factor = character(),
                           X2 = double(),
                           p.value = double())
  gp <- thr_partials

  iter <- 0
  while (!step2$ok && iter < max_iter) {
    iter <- iter + 1

    # Parameter and constraints tables
    PT <- lavaan::parTable(fit_met)
    CT <- PT[PT$op == "==", , drop = FALSE]
    rownames(CT) <- NULL

    # Identify equality constraints that involve FACTOR LOADINGS
    loading_rows <- PT$op == "=~"
    loading_plabs <- unique(PT$plabel[loading_rows & nzchar(PT$plabel)])

    idx <- which(CT$lhs %in% loading_plabs | CT$rhs %in% loading_plabs)
    if (length(idx) == 0L) break

    # Score tests for releasing those equality constraints
    st <- lavaan::lavTestScore(fit_met, release = idx, univariate = TRUE)
    uni <- st$uni
    uni$ct_row <- idx

    # Choose the strongest violation meeting cutoff (X2 >= mi_cut, df==1)
    cand <- uni[order(-uni$X2), , drop = FALSE]
    cand <- subset(cand, df == 1 & X2 >= mi_cut)
    if (nrow(cand) == 0L) break
    top <- cand[1, ]

    # Map the constraint (plabels) back to the specific loading (factor =~ item)
    plab <- if (top$lhs %in% loading_plabs) top$lhs else top$rhs
    rowL <- PT[PT$plabel == plab & PT$op == "=~", , drop = FALSE][1, ]
    fac_name  <- rowL$lhs
    item_name <- rowL$rhs

    # Free this loading across groups via group.partial
    gp <- unique(c(gp, paste0(fac_name, " =~ ", item_name)))

    fit_met <- lavaan::cfa(base_syntax, data = data, group = group,
                           ordered = ordered_keys, estimator = "WLSMV",
                           parameterization = "theta", std.lv = TRUE,
                           group.equal = c("thresholds","loadings"),
                           group.partial = gp)

    freedL <- dplyr::bind_rows(
      freedL,
      tibble::tibble(item = item_name, factor = fac_name,
                     X2 = as.numeric(top$X2), p.value = as.numeric(top$p.value))
    )

    step2 <- .delta_decision(fit_thr, fit_met, rules)
    if (verbose) message(sprintf(
      "[metric iter %d] Freed loading %s =~ %s; ΔCFI=%.3f ΔRMSEA=%.3f ΔSRMR=%.3f; ok=%s",
      iter, fac_name, item_name,
      step2$delta["cfi"], step2$delta["rmsea"], step2$delta["srmr"], step2$ok))
  }

  list(met = fit_met, freed_loadings = freedL, group_partial = gp)
}

# =====================================================
# One-click MI on 7 items with options:
#   - wu_id: TRUE uses Wu & Estabrook identification (configural ≡ thresholds)
#   - auto_free_thresholds: iterative partial thresholds (works when wu_id = FALSE)
#   - auto_free_loadings: iterative partial loadings after thresholds step
# =====================================================
run_mi_7items <- function(data,
                          ages = NULL,
                          auto_free_thresholds = FALSE,
                          auto_free_loadings = FALSE,
                          wu_id = TRUE,
                          max_thr_iter = 10,
                          max_load_iter = 10,
                          mi_cut = 3.84,
                          verbose = TRUE) {

  keys7 <- c("task_completion","distracted","fidgeting","think_act",
             "temper","obedient","lying")

  # Accept a long data or build it from wide
  if (all(c("age", keys7) %in% names(data))) {
    sc7_long <- data %>% dplyr::select(dplyr::any_of(c("id_rr","age", keys7)))
    sc7_long$age <- if (!is.null(ages)) factor(sc7_long$age, levels = ages) else factor(sc7_long$age)
  } else {
    stopifnot(!is.null(ages))
    df <- tibble::as_tibble(data); df$id_rr <- seq_len(nrow(df))
    make_age_block7 <- function(age) {
      cols <- paste0("sc", age, "_", keys7)
      cols <- cols[cols %in% names(df)]
      missing <- setdiff(paste0("sc", age, "_", keys7), cols)
      for (m in missing) df[[m]] <<- NA_real_
      cols <- paste0("sc", age, "_", keys7)
      df %>%
        dplyr::select(id_rr, dplyr::all_of(cols)) %>%
        stats::setNames(c("id_rr", keys7)) %>%
        dplyr::mutate(age = factor(age, levels = ages))
    }
    sc7_long <- purrr::map_dfr(ages, make_age_block7)
  }



