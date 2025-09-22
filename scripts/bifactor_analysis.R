# =========================================================================
# Standalone Bifactor Analysis for Self-Control Scale
# Run this after data preparation scripts (01_data_preparation.R and 02_recode_self_control.R)
# =========================================================================

library(pacman)
p_load(tidyverse, lavaan, BifactorIndicesCalculator, EFAtools)

# Check data
stopifnot(exists("merged_data"))
df <- as_tibble(merged_data)

ages <- c(3, 5, 7, 11, 14, 17)
keys <- c("task_completion", "distracted", "fidgeting", "restless",
          "think_act", "temper", "obedient", "lying")

# Content clusters for bifactor model
groups <- list(
  AttentionExec = c("task_completion","distracted"),
  Hyperactivity = c("restless","fidgeting"),
  BehaviorReg   = c("obedient","lying","temper","think_act")
)

build_bifactor_model <- function(all_items, groups) {
  # General factor
  g_line <- paste0("g =~ ", paste(all_items, collapse = " + "))
  # Group factors (no cross-loadings)
  grp_lines <- map2_chr(names(groups), groups, ~ paste0(.x, " =~ ", paste(.y, collapse = " + ")))
  # Orthogonality: group factors uncorrelated with g and with each other
  ortho <- c(paste0("g ~~ 0*", names(groups), collapse = "\n"),
             combn(names(groups), 2, FUN = function(p) paste0(p[1], " ~~ 0*", p[2]), simplify = TRUE))
  paste(c(g_line, grp_lines, ortho), collapse = "\n")
}

bifactor_rows <- list()

for (age in ages) {
  message("\n=== Processing age ", age, " ===")

  # Find available columns for this age
  cols <- paste0("sc", age, "_", keys)
  cols <- cols[cols %in% names(df)]
  message("Found ", length(cols), " self-control columns for age ", age)

  if (length(cols) < 5L) {
    message("Age ", age, ": not enough items for bifactor; skipping.")
    next
  }

  # Prepare data
  dat_num <- df %>% select(all_of(cols)) %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.x)))))
  dat_ord <- dat_num %>%
    mutate(across(everything(), ~ factor(.x, levels = c(0,1,2), ordered = TRUE))) %>%
    drop_na()

  message("Age ", age, ": ", nrow(dat_ord), " complete cases after processing")

  if (nrow(dat_ord) < 100L) {
    message("Age ", age, ": too few complete cases for bifactor; skipping.")
    next
  }

  # Build model with correct column names
  all_items <- colnames(dat_ord)
  usable_groups <- lapply(groups, function(v) {
    full_names <- paste0("sc", age, "_", v)
    intersect(full_names, all_items)
  })

  bif_model <- build_bifactor_model(all_items, usable_groups)
  message("Model syntax created successfully")

  # Fit bifactor model
  fit_bi <- tryCatch(
    lavaan::cfa(bif_model, data = dat_ord, ordered = colnames(dat_ord),
                estimator = "WLSMV", std.lv = TRUE, parameterization = "theta"),
    error = function(e) {
      message("Age ", age, ": bifactor fit failed with error: ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(fit_bi)) {
    message("Age ", age, ": bifactor model did not converge")
    next
  }

  message("Age ", age, ": bifactor model converged successfully")

  # Extract bifactor indices
  bi <- tryCatch(
    BifactorIndicesCalculator::bifactorIndices(fit_bi, standardized = TRUE),
    error = function(e) NULL
  )

  if (!is.null(bi)) {
    ecv  <- as.numeric(bi$ECV$ECV)
    puc  <- as.numeric(bi$PUC)
    omegah <- as.numeric(bi$OmegaH$General)
  } else {
    # Fallback to EFAtools::OMEGA
    om2 <- tryCatch(EFAtools::OMEGA(fit_bi), error = function(e) NULL)
    ecv <- if (!is.null(om2)) as.numeric(om2$ECV) else NA_real_
    puc <- if (!is.null(om2)) as.numeric(om2$PUC) else NA_real_
    omegah <- if (!is.null(om2)) as.numeric(om2$omega_h) else NA_real_
  }

  # Get fit measures
  fm <- lavaan::fitMeasures(fit_bi, c("cfi","tli","rmsea","srmr"))

  # Store results
  bifactor_rows[[length(bifactor_rows)+1L]] <- tibble(
    age = age,
    cfi = round(unname(fm["cfi"]), 3),
    tli = round(unname(fm["tli"]), 3),
    rmsea = round(unname(fm["rmsea"]), 3),
    srmr  = round(unname(fm["srmr"]), 3),
    ecv = round(ecv, 3),
    puc = round(puc, 3),
    omega_h = round(omegah, 3)
  )

  message("Age ", age, ": completed successfully")
}

# Display results
if (length(bifactor_rows) > 0) {
  bifactor_tbl <- bind_rows(bifactor_rows) %>% arrange(age)
  message("\n==== Bifactor Analysis Results ====")
  print(bifactor_tbl, n = Inf)
} else {
  message("\nNo bifactor models converged successfully")
}
