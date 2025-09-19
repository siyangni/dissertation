# =============================================================================
# LONGITUDINAL MEASUREMENT INVARIANCE TESTING (Binary 0/1 items)
# =============================================================================
# Cite: Wu H, Estabrook R. (2016) Psychometrika, 81(4):1014–1045.
# DOI: 10.1007/s11336-016-9506-0
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(pacman)
p_load(tidyverse, glue, lavaan, semTools)

# --- Inputs ------------------------------------------------------------------
ages <- c(3, 5, 7, 11, 14, 17)
item_suffixes <- c(
  "task_completion", "distracted", "fidgeting", "restless",
  "think_act", "temper", "obedient", "lying"
)

# NOTE: merged_data must already exist in the environment as a wide dataset ----
stopifnot(exists("merged_data"), is.data.frame(merged_data))

# --- Helpers -----------------------------------------------------------------

# Build a canonical variable name from age and item suffix
vname <- function(age, suf) glue("sc{age}_{suf}")

# Safe column pull; returns NULL if column is missing
pull_col <- function(data, nm) if (nm %in% names(data)) data[[nm]] else NULL

# Tabulate observed categories for a single ordered variable (returns tibble)
tabulate_var <- function(data, var) {
  x <- pull_col(data, var)
  if (is.null(x)) {
    tibble(var, level = NA_character_, n = NA_integer_)
  } else {
    as_tibble(table(x, useNA = "no"), .name_repair = "unique") |>
      rename(level = 1, n = 2) |>
      mutate(var = var, .before = 1)
  }
}

# Get distinct non-NA category count for a variable
n_cats <- function(data, var) {
  x <- pull_col(data, var)
  if (is.null(x)) 0L else n_distinct(x, na.rm = TRUE)
}

# Pretty printer for fit measures (robust/Scaled)
fit_measures_tbl <- function(fit) {
  lavaan::fitMeasures(fit, c(
    "chisq.scaled", "df",
    "cfi.scaled", "tli.scaled",
    "rmsea.scaled", "srmr"
  )) |>
    enframe(name = "measure", value = "value") |>
    mutate(value = round(as.numeric(value), 3)) |>
    pivot_wider(names_from = measure, values_from = value)
}

# Utility for binding list of tibbles safely
lbind <- function(lst) dplyr::bind_rows(lst)

# --- 1) Identify the exact intersection of indicators present at all ages ----

# Grid of all requested (age, suffix) combinations
grid_all <- tidyr::crossing(age = ages, suf = item_suffixes) |>
  mutate(var = vname(age, suf))

# Flag presence in the data
grid_present <- grid_all |>
  mutate(in_data = var %in% names(merged_data))

# Keep only suffixes where every age-specific variable exists
common_suf <- grid_present |>
  group_by(suf) |>
  summarize(all_present = all(in_data), .groups = "drop") |>
  filter(all_present) |>
  pull(suf)

if (length(common_suf) < 3L) {
  stop("Need at least 3 common indicators across all ages.")
}

message("Common indicators across all ages: ",
        paste(common_suf, collapse = ", "))

# --- 2) Construct the ordered variable vectors (per age and overall) ----------

var_by_age <- tibble(age = ages) |>
  mutate(vars = map(age, ~ vname(.x, common_suf))) |>
  mutate(tlabel = paste0("t", age))

ordered_vars <- var_by_age |>
  summarize(vars = flatten_chr(vars)) |>
  pull(vars)

# --- 3) Ensure an ID column ---------------------------------------------------
if (!("id" %in% names(merged_data))) {
  merged_data <- merged_data |>
    mutate(id = row_number(), .before = 1)
}

# --- 4) Optional sparsity & prevalence checks --------------------------------

# Tabulate all common items across all ages
sparsity_tbl <- var_by_age |>
  select(vars) |>
  summarize(vars = flatten_chr(vars)) |>
  pull(vars) |>
  map(\(v) tabulate_var(merged_data, v)) |>
  lbind()

# Flag very sparse categories (n < 5)
sparse_flags <- sparsity_tbl |>
  filter(!is.na(n), n < 5) |>
  arrange(var, n)

if (nrow(sparse_flags) > 0) {
  message("Sparse categories detected (n < 5):")
  sparse_flags |>
    mutate(msg = glue("{var} -> {level} (n={n})")) |>
    pull(msg) |>
    walk(message)
}

# Extreme prevalence check for binary items
prev_tbl <- tibble(var = ordered_vars) |>
  mutate(p1 = map_dbl(var, ~ mean(merged_data[[.x]] == 1, na.rm = TRUE))) |>
  arrange(p1)

extreme_prev <- prev_tbl |>
  filter(p1 < .05 | p1 > .95)

if (nrow(extreme_prev) > 0) {
  message("Extreme prevalence detected for some items (p(1) < .05 or > .95):")
  print(extreme_prev, n = nrow(extreme_prev))
}

# --- 5) Coerce variables to ordered factors (levels 0 < 1) -------------------

# Do this *after* determining ordered_vars
merged_data <- merged_data |>
  mutate(across(all_of(ordered_vars), ~ factor(.x, levels = c(0, 1), ordered = TRUE)))

# --- 6) Write a longitudinal CFA with time-specific factors -------------------

fac_names <- paste0("SC_t", ages)

# Build right-hand sides per age
rhs_by_age <- var_by_age |>
  mutate(rhs = map_chr(vars, ~ paste(.x, collapse = " + ")))

# Base CFA (factor loadings per time)
configural_model <- tibble(lhs = fac_names, rhs = rhs_by_age$rhs) |>
  mutate(line = glue("{lhs} =~ {rhs}")) |>
  pull(line) |>
  paste(collapse = "\n")

# Residual correlations for the same item across *adjacent* ages
# This helps capture stable method/wording effects over time.
adjacent_pairs <- tibble(
  age_L = ages[-length(ages)],
  age_R = ages[-1]
)

resid_lines <- map(common_suf, function(s) {
  paste0(
    "sc", adjacent_pairs$age_L, "_", s,
    " ~~ ",
    "sc", adjacent_pairs$age_R, "_", s
  )
}) |>
  flatten_chr() |>
  paste(collapse = "\n")

configural_model <- paste(configural_model, resid_lines, sep = "\n")

# --- 7) Prepare measEq.syntax inputs -----------------------------------------

longFacNames <- list(SC = fac_names)

# semTools needs a named list of repeated indicators across waves
longIndNames <- setNames(
  object = map(common_suf, ~ vname(ages, .x)),
  nm = common_suf
)

# Category counts per indicator (diagnostic)
cats_tbl <- tibble(var = ordered_vars) |>
  mutate(n_cat = map_int(var, ~ n_cats(merged_data, .x)))
print(table(cats_tbl$n_cat))
print(head(arrange(cats_tbl, n_cat), 20))

# --- 8) Fit nested invariance models (WLSMV, thresholds-first) ---------------

base_args <- list(
  configural.model = configural_model,
  data             = merged_data,
  ordered          = ordered_vars,
  parameterization = "theta",
  ID.fac           = "std.lv",
  ID.cat           = "Wu.Estabrook.2016",
  longFacNames     = longFacNames,
  longIndNames     = longIndNames,
  auto             = 1L,
  return.fit       = TRUE,
  estimator        = "WLSMV"
)

# Configural (no longitudinal equality constraints)
fit_config <- do.call(measEq.syntax, base_args)

# Threshold invariance
fit_thresh <- do.call(
  measEq.syntax,
  modifyList(base_args, list(long.equal = "thresholds"))
)

# Threshold + loading (metric, categorical)
fit_metricCat <- do.call(
  measEq.syntax,
  modifyList(base_args, list(long.equal = c("thresholds", "loadings")))
)

# --- 9) Compare fits and extract key indices ---------------------------------

# Model comparison (robust where available)
comp <- compareFit(fit_config, fit_thresh, fit_metricCat)
print(comp)

fit_keys <- list(
  Configural   = fit_config,
  Thresholds   = fit_thresh,
  Thresh_Loads = fit_metricCat
) |>
  imap(\(fit, name) fit_measures_tbl(fit) |> mutate(model = name)) |>
  bind_rows() |>
  relocate(model)

print(fit_keys)

# Robust difference tests for WLSMV
print(lavTestLRT(fit_config, fit_thresh))
print(lavTestLRT(fit_thresh, fit_metricCat))
print(lavInspect(fit_config, "options")$test)

# --- 10) Score tests to locate noninvariant constraints ----------------------

# Univariate score test results for strongest model (thresholds + loadings)
sc <- lavTestScore(fit_metricCat)
uni <- as.data.frame(sc$uni)

# Parameter table; keep only free parameters and tag
pt <- parameterTable(fit_metricCat) |>
  as_tibble() |>
  mutate(pid = paste0(".p", free, "."))

pt_free <- pt |>
  filter(free > 0L) |>
  select(pid, lhs, op, rhs, group, block, label)

# Keep only equality constraints from the score test
uni_eq <- uni |>
  as_tibble() |>
  filter(op == "==") |>
  select(lhs, op, rhs, X2)

# Join each side of the equality back to parameter metadata
mL <- uni_eq |>
  left_join(
    pt_free |> rename_with(~ paste0(.x, "_L")),
    by = c("lhs" = "pid_L")
  )

mLR <- mL |>
  left_join(
    pt_free |> rename_with(~ paste0(.x, "_R")),
    by = c("rhs" = "pid_R")
  )

worst <- mLR |>
  arrange(desc(X2)) |>
  select(
    X2, lhs, rhs,
    lhs_L, op_L, rhs_L, group_L, block_L, label_L,
    lhs_R, op_R, rhs_R, group_R, block_R, label_R
  )

# Peek at the top 40 misfitting equalities
print(slice_head(worst, n = 40), n = 40)
