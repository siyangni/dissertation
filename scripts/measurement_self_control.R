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







