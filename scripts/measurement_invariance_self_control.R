# =============================================================================
# LONGITUDINAL MEASUREMENT INVARIANCE TESTING
# =============================================================================
# Cite: Wu H, Estabrook R. Identification of Confirmatory Factor Analysis Models of Different Levels of Invariance for Ordered Categorical Outcomes. Psychometrika. 2016 Dec;81(4):1014-1045. doi: 10.1007/s11336-016-9506-0. Epub 2016 Jul 11. PMID: 27402166; PMCID: PMC5458787.

## === LONGITUDINAL MI FOR ORDINAL ITEMS (WLSMV, thresholds-first) ===
library(lavaan)
library(semTools)

ages <- c(3, 5, 7, 11, 14, 17)
item_suffixes <- c("task_completion","distracted","fidgeting","think_act",
                   "restless","temper","obedient","lying")

# 1) Build the exact intersection of indicators present at all ages
exists_at_all <- function(suf) all(paste0("sc", ages, "_", suf) %in% names(merged_data))
common_suf <- item_suffixes[sapply(item_suffixes, exists_at_all)]
if (length(common_suf) < 3L) stop("Need at least 3 common indicators across all ages.")

# 2) Construct var list per age (wide data; no listwise deletion yet)
var_by_age <- lapply(ages, function(a) paste0("sc", a, "_", common_suf))
names(var_by_age) <- paste0("t", ages)

# 3) Ensure an ID column
if (!"id" %in% names(merged_data)) merged_data$id <- seq_len(nrow(merged_data))

# 4) (Optional) basic sparsity check per age/indicator
for (a in ages) {
  for (suf in common_suf) {
    v <- paste0("sc", a, "_", suf)
    tab <- table(merged_data[[v]], useNA = "no")
    if (any(tab < 5)) {
      message("Sparse category: ", v, " -> ", paste(names(tab)[tab < 5], collapse=", "))
    }
  }
}

# 5) Write a longitudinal CFA (time-specific factors; same indicators each time)
fac_names <- paste0("SC_t", ages)
lhs <- fac_names
rhs <- sapply(ages, function(a) paste(paste0("sc", a, "_", common_suf), collapse = " + "))
configural_model <- paste(paste(lhs, "=~", rhs), collapse = "\n")

# 6) Use measEq.syntax to generate and fit the nested models
#    - Wu & Estabrook identification for categorical items
#    - thresholds-first sequence
#    - correlate residuals of the same item across adjacent waves (auto = 1L)
longFacNames <- list(SC = fac_names)
ordered_vars <- unlist(var_by_age, use.names = FALSE)

## Count distinct categories observed for every ordered indicator
cats <- sapply(ordered_vars, function(v) length(unique(na.omit(merged_data[[v]]))))
table(cats)                 # how many 2-, 3-, 4-, 5-category items?
sort(cats)[1:20]            # which items have the fewest categories?

## flag any very sparse categories (n<5) by item and age
for (v in ordered_vars) {
  tab <- table(merged_data[[v]], useNA = "no")
  if (any(tab < 5)) message("Sparse category in ", v, ": ", paste(names(tab)[tab < 5], collapse = ", "))
}

# =======================================================
# Configural
## Build longIndNames so semTools knows which items repeat across waves
longIndNames <- setNames(
  lapply(common_suf, function(suf) paste0("sc", ages, "_", suf)),
  common_suf
)

fit_config <- measEq.syntax(
  configural.model = configural_model,
  data             = merged_data,
  ordered          = ordered_vars,
  parameterization = "theta",
  ID.fac           = "std.lv",
  ID.cat           = "Wu.Estabrook.2016",
  longFacNames     = longFacNames,
  longIndNames     = longIndNames, 
  auto             = 1L, # correlates residuals for adjacent waves
  return.fit       = TRUE,
  estimator        = "WLSMV"
)

# Threshold invariance (across time)
fit_thresh <- measEq.syntax(
  configural.model = configural_model,
  data             = merged_data,
  ordered          = ordered_vars,
  parameterization = "theta",
  ID.fac           = "std.lv",
  ID.cat           = "Wu.Estabrook.2016",
  longFacNames     = longFacNames,
  longIndNames     = longIndNames, 
  long.equal       = "thresholds",
  auto             = 1L,
  return.fit       = TRUE,
  estimator        = "WLSMV"
)

# Thresholds + loadings invariance (strong/“scalar” for categorical)
fit_metricCat <- measEq.syntax(
  configural.model = configural_model,
  data             = merged_data,
  ordered          = ordered_vars,
  parameterization = "theta",
  ID.fac           = "std.lv",
  ID.cat           = "Wu.Estabrook.2016",
  longFacNames     = longFacNames,
  longIndNames     = longIndNames,
  long.equal       = c("thresholds", "loadings"),
  auto             = 1L,
  return.fit       = TRUE,
  estimator        = "WLSMV"
)

# 7) Compare fits (robust/scaled) and extract key indices
comp <- compareFit(fit_config, fit_thresh, fit_metricCat)
print(comp)

key <- function(fit) lavaan::fitMeasures(fit,
  c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","srmr"))
fm <- rbind(
  Configural = key(fit_config),
  Thresholds = key(fit_thresh),
  Thresh_Load = key(fit_metricCat)
)
print(round(fm, 3))

# 8) Robust difference tests for WLSMV (scaled/shifted)
#    (lavTestLRT picks the appropriate method; check which via lavInspect)
print(lavTestLRT(fit_config, fit_thresh))
print(lavTestLRT(fit_thresh, fit_metricCat))
print(lavInspect(fit_config, "options")$test)

# 9) Score tests to locate noninvariant thresholds/loadings (targeted freeing)
## Robust score-test table extraction that works across lavaan versions
sc <- lavTestScore(fit_metricCat)   # or fit of interest under constraints
uni <- tryCatch(as.data.frame(sc$uni), error = function(e) NULL)

if (!is.null(uni) && nrow(uni) > 0) {
  stat_candidates <- c("mi","X2","stat","LM.Stat","LMStat","score","Test.Stat")
  stat_col <- intersect(stat_candidates, names(uni))
  if (length(stat_col) == 0L) {
    message("Score test found but no recognizable statistic column; showing first rows.")
    print(head(uni, 20))
  } else {
    stat_col <- stat_col[1]
    ord <- order(uni[[stat_col]], decreasing = TRUE)
    keep <- intersect(c("lhs","op","rhs","group", stat_col, "epc","epc.lv"), names(uni))
    print(head(uni[ord, keep, drop = FALSE], 50))
  }
} else {
  message("No score-test results returned.")
}

