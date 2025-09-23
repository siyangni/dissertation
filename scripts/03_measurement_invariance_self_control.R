# =====================================================================
# (c) Measurement invariance skeleton across ages (ordered, WLSMV)
#     Using semTools::measEq.syntax with Wu & Estabrook (2016) ID.
# =====================================================================

# 1) Build a long data set: one row per person per age, columns = the 8 items
#    We assume rows represent the same children across ages (IDs are row numbers).
ages <- c(5, 7, 11, 14, 17)

# Items to evaluate for the self-control scale
keys <- c("task_completion", "distracted", "think_act", "temper", "obedient", "lying", "fidgeting")


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























###############################################################
suppressPackageStartupMessages({
  library(tidyverse)
  library(lavaan)
  library(semTools)
})

# -------------------------
# CONFIG / INPUTS
# -------------------------
# 'sc_long' already built in your code; it must contain:
#   - age: factor with levels in desired order
#   - items in 'keys' as ordered factors (0,1,2)
stopifnot(all(c("age") %in% names(sc_long)))
stopifnot(all(keys %in% names(sc_long)))
sc_long <- sc_long %>%
  mutate(across(all_of(keys), ~ factor(.x, levels = c(0,1,2), ordered = TRUE)))
ages <- levels(sc_long$age)
onefactor_model <- paste0("SelfCtrl =~ ", paste(keys, collapse = " + "))

# -------------------------
# 1) Fit configural, thresholds, scalar (thr+load), strict (optional)
# -------------------------
# Configural
fit_cfg <- cfa(onefactor_model, data = sc_long, group = "age",
               ordered = keys, estimator = "WLSMV",
               parameterization = "theta", std.lv = TRUE)

# Thresholds equal (Wu & Estabrook identification)
fit_thr <- cfa(onefactor_model, data = sc_long, group = "age",
               ordered = keys, estimator = "WLSMV",
               parameterization = "theta", std.lv = TRUE,
               group.equal = c("thresholds"))

# Scalar analogue for ordered: thresholds + loadings equal
fit_scalar <- cfa(onefactor_model, data = sc_long, group = "age",
                  ordered = keys, estimator = "WLSMV",
                  parameterization = "theta", std.lv = TRUE,
                  group.equal = c("thresholds", "loadings"))

# OPTIONAL: Strict (add residual variances equality). Not typically required.
fit_strict <- tryCatch(
  cfa(onefactor_model, data = sc_long, group = "age",
      ordered = keys, estimator = "WLSMV",
      parameterization = "theta", std.lv = TRUE,
      group.equal = c("thresholds", "loadings", "residuals")),
  error = function(e) NULL
)

# -------------------------
# 2) Δ‑fit table across steps
# -------------------------
want <- c("cfi","tli","rmsea","srmr","chisq","df")
mi_tbl <- bind_rows(
  tibble(level = "configural") %>% bind_cols(as_tibble_row(fitMeasures(fit_cfg, want)[want])),
  tibble(level = "thresholds") %>% bind_cols(as_tibble_row(fitMeasures(fit_thr, want)[want])),
  tibble(level = "scalar_thr+load") %>% bind_cols(as_tibble_row(fitMeasures(fit_scalar, want)[want])),
  tibble(level = "strict_thr+load+resid") %>% bind_cols(as_tibble_row(if (!is.null(fit_strict)) fitMeasures(fit_strict, want)[want] else setNames(rep(NA_real_, length(want)), want)))
) %>%
  mutate(across(-level, ~ as.numeric(.x))) %>%
  mutate(
    d_cfi   = c(NA, diff(cfi)),
    d_rmsea = c(NA, diff(rmsea)),
    d_srmr  = c(NA, diff(srmr))
  )
print(mi_tbl, n = Inf, width = Inf)

message("\nHeuristics (ordinal): scalar analogue = thresholds+loadings. ",
        "Judge deltas with ΔCFI ≤ .01, ΔRMSEA ≤ .015 (and small ΔSRMR).")

# -------------------------
# 3) Latent mean comparison after scalar invariance
# -------------------------
# Under multi-group CFA, the latent mean of the reference group (first level of 'age') is fixed to 0,
# others are freely estimated. These are directly comparable once thresholds+loadings are invariant. 
# (Scalar analogue for ordered indicators.)  See Wu & Estabrook (2016) and tutorials. 
pe <- parameterEstimates(fit_scalar) %>%
  filter(op == "~1", lhs == "SelfCtrl") %>%
  select(group, est, se, z, pvalue)
lat_means <- tibble(
  age = ages[pe$group],
  mean = pe$est, se = pe$se, z = pe$z, p = pe$pvalue
)
message("\nLatent means relative to reference group (", ages[1], "):")
print(lat_means, n = Inf)

# Optional global test: equal latent means across all ages (constrain lv.means)
fit_scalar_eqmeans <- cfa(onefactor_model, data = sc_long, group = "age",
                          ordered = keys, estimator = "WLSMV",
                          parameterization = "theta", std.lv = TRUE,
                          group.equal = c("thresholds", "loadings", "means"))
eq_tbl <- tibble(
  model = c("scalar_thr+load", "scalar+equal_lv.means"),
  cfi = c(fitMeasures(fit_scalar, "cfi"), fitMeasures(fit_scalar_eqmeans, "cfi")),
  rmsea = c(fitMeasures(fit_scalar, "rmsea"), fitMeasures(fit_scalar_eqmeans, "rmsea")),
  srmr = c(fitMeasures(fit_scalar, "srmr"), fitMeasures(fit_scalar_eqmeans, "srmr"))
) %>%
  mutate(d_cfi = c(NA, diff(cfi)),
         d_rmsea = c(NA, diff(rmsea)),
         d_srmr = c(NA, diff(srmr)))
message("\nTesting equality of latent means across ages (Δ‑fit from scalar):")
print(eq_tbl, n = Inf)

# -------------------------
# 4) Item‑level diagnostics
#    A) Modification indices (MIs): thresholds and loadings
#    B) Score tests for equality constraints (release candidates)
# -------------------------

# -- A) Raw modification indices --
# Thresholds: inspect MIs from the thresholds-equal model
mi_thr <- tryCatch(modindices(fit_thr, sort. = TRUE), error = function(e) NULL)
if (!is.null(mi_thr)) {
  mi_thr_tbl <- mi_thr %>%
    filter(op == "|", mi > 0) %>%
    transmute(item = lhs, threshold = rhs, group, mi, epc) %>%
    arrange(desc(mi))
  message("\nTop threshold MIs (consider freeing these 'item|threshold' in specific groups if needed):")
  print(head(mi_thr_tbl, 20), n = 20)
}

# Loadings: inspect MIs from the scalar (thr+load) model
mi_load <- tryCatch(modindices(fit_scalar, sort. = TRUE), error = function(e) NULL)
if (!is.null(mi_load)) {
  mi_load_tbl <- mi_load %>%
    filter(op == "=~", mi > 0) %>%
    transmute(factor = lhs, item = rhs, group, mi, epc) %>%
    arrange(desc(mi))
  message("\nTop loading MIs (consider freeing these loadings in specific groups for partial metric if needed):")
  print(head(mi_load_tbl, 20), n = 20)
}

# -- B) Score tests (release equality constraints) --
# For loadings equality: use lavTestScore against equality constraints.
list_score_violations_loadings <- function(fit, top_n = 10, cutoff = 3.84) {
  PT <- parTable(fit)
  CT <- PT[PT$op == "==", , drop = FALSE]
  rownames(CT) <- NULL

  # plabels for loadings
  loading_rows <- PT$op == "=~"
  plabs <- unique(PT$plabel[loading_rows & nzchar(PT$plabel)])

  idx <- which(CT$lhs %in% plabs | CT$rhs %in% plabs)
  if (!length(idx)) return(tibble())

  st <- lavTestScore(fit, release = idx, univariate = TRUE)
  uni <- st$uni %>% arrange(desc(X2))
  cand <- uni %>% filter(df == 1, X2 >= cutoff)
  if (!nrow(cand)) return(tibble())

  # Map plabel (lhs/rhs of constraint) back to (factor =~ item)
  map_one <- function(plab) {
    rowL <- PT[PT$op == "=~" & PT$plabel == plab, , drop = FALSE]
    if (!nrow(rowL)) return(tibble())
    tibble(factor = rowL$lhs[1], item = rowL$rhs[1], plabel = plab)
  }
  mapped <- bind_rows(lapply(unique(c(cand$lhs, cand$rhs)), map_one)) %>% distinct()
  out <- cand %>%
    mutate(plabel = ifelse(lhs %in% mapped$plabel, lhs, rhs)) %>%
    left_join(mapped, by = "plabel") %>%
    transmute(factor, item, X2, p.value) %>%
    arrange(desc(X2)) %>%
    slice_head(n = top_n)
  out
}

# For thresholds equality: analogous score tests against threshold constraints.
list_score_violations_thresholds <- function(fit, top_n = 10, cutoff = 3.84) {
  PT <- parTable(fit)
  CT <- PT[PT$op == "==", , drop = FALSE]
  rownames(CT) <- NULL

  thr_rows <- PT$op == "|"
  plabs <- unique(PT$plabel[thr_rows & nzchar(PT$plabel)])

  idx <- which(CT$lhs %in% plabs | CT$rhs %in% plabs)
  if (!length(idx)) return(tibble())

  st <- lavTestScore(fit, release = idx, univariate = TRUE)
  uni <- st$uni %>% arrange(desc(X2))
  cand <- uni %>% filter(df == 1, X2 >= cutoff)
  if (!nrow(cand)) return(tibble())

  # Map plabel back to the specific threshold (item|t#)
  map_thr <- function(plab) {
    rowT <- PT[PT$op == "|" & PT$plabel == plab, , drop = FALSE]
    if (!nrow(rowT)) return(tibble())
    tibble(item = rowT$lhs[1], threshold = rowT$rhs[1], plabel = plab)
  }
  mapped <- bind_rows(lapply(unique(c(cand$lhs, cand$rhs)), map_thr)) %>% distinct()
  out <- cand %>%
    mutate(plabel = ifelse(lhs %in% mapped$plabel, lhs, rhs)) %>%
    left_join(mapped, by = "plabel") %>%
    transmute(item, threshold, X2, p.value) %>%
    arrange(desc(X2)) %>%
    slice_head(n = top_n)
  out
}

message("\nScore tests for equality constraints (loadings) — strongest release candidates:")
print(list_score_violations_loadings(fit_scalar, top_n = 12))

message("\nScore tests for equality constraints (thresholds) — strongest release candidates:")
print(list_score_violations_thresholds(fit_thr, top_n = 12))

# -------------------------
# 5) (Optional) Partial invariance refits
#    If a few loadings/thresholds clearly violate equality, you can free them via semTools::measEq.syntax
# -------------------------
# Example: Suppose you want to free 1 threshold and 1 loading discovered above.
# Build a 'group.partial' vector with "item|t1" and "Factor =~ item" entries.
# (Replace with actual entries from the score-test tables above.)
# gp <- c("task_completion|t1", "SelfCtrl =~ distracted")  # EXAMPLE ONLY
# syn_scalar_pi <- measEq.syntax(
#   configural.model = onefactor_model,
#   data = sc_long, group = "age",
#   ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
#   parameterization = "theta",
#   group.equal = c("thresholds","loadings"),
#   ordered = keys,
#   group.partial = gp
# )
# fit_scalar_pi <- cfa(as.character(syn_scalar_pi), data = sc_long, group = "age",
#                      ordered = keys, estimator = "WLSMV", parameterization = "theta", std.lv = TRUE)
# fitMeasures(fit_scalar_pi, c("cfi","rmsea","srmr"))

# -------------------------
# 6) Helper function: score_table_equalities (combines threshold and loading score tests)
# -------------------------
score_table_equalities <- function(fit, type = c("thresholds", "loadings"), cutoff = 3.84, top_n = 15) {
  type <- match.arg(type)
  
  if (type == "thresholds") {
    return(list_score_violations_thresholds(fit, top_n = top_n, cutoff = cutoff))
  } else {
    return(list_score_violations_loadings(fit, top_n = top_n, cutoff = cutoff))
  }
}

# End of script
