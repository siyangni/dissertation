# =============================================================================
# Growth Mixture Models (GMM) for Self-Control (Ages 5–17)
# Extends 04_lcmsr_self_control.R by:
#   1) Re-fitting the longitudinal MI measurement model for SC indicators
#   2) Extracting MI-consistent factor scores for SC_t5, SC_t7, SC_t11, SC_t14, SC_t17
#   3) Fitting growth mixture models (1–6 classes) on factor-score trajectories
#   4) Comparing models (AIC, BIC, SABIC, entropy) and exporting class assignments
#   5) Plotting average trajectories by latent class
# =============================================================================

library(pacman)
p_load(tidyverse, lavaan, semTools, lcmm, ggplot2, e1071)

# ------------------------------
# Data: ensure merged_data exists
# ------------------------------
if (!exists("merged_data")) {
  data_path <- "/home/siyang/dissertation_folder/data"
  merged_data <- readRDS(file.path(data_path, "merged0917.rds"))
}

if (!"weight1" %in% names(merged_data)) {
  warning("weight1 not found in merged_data; setting unit weights.")
  merged_data$weight1 <- 1
}

# Create subject id if missing
subject_id_var <- if ("mcsid" %in% names(merged_data)) "mcsid" else NULL
if (is.null(subject_id_var)) {
  merged_data$.row_id <- seq_len(nrow(merged_data))
  subject_id_var <- ".row_id"
}

# ---------------------------------------------
# Variables and longitudinal measurement model
# ---------------------------------------------
var_by_age <- list(
  t5  = c("sc5_task_completion", "sc5_distracted", "sc5_fidgeting", "sc5_think_act",
          "sc5_temper", "sc5_obedient", "sc5_lying"),
  t7  = c("sc7_task_completion", "sc7_distracted", "sc7_fidgeting", "sc7_think_act",
          "sc7_temper", "sc7_obedient", "sc7_lying"),
  t11 = c("sc11_task_completion", "sc11_distracted", "sc11_fidgeting", "sc11_think_act",
          "sc11_temper", "sc11_obedient", "sc11_lying"),
  t14 = c("sc14_task_completion", "sc14_distracted", "sc14_fidgeting", "sc14_think_act",
          "sc14_temper", "sc14_obedient", "sc14_lying"),
  t17 = c("sc17_task_completion", "sc17_distracted", "sc17_fidgeting", "sc17_think_act",
          "sc17_temper", "sc17_obedient", "sc17_lying")
)

ordered_vars <- unlist(var_by_age, use.names = FALSE)

configural_model <- '
SC_t5  =~ sc5_task_completion + sc5_distracted + sc5_fidgeting + sc5_think_act + sc5_temper + sc5_obedient + sc5_lying
SC_t7  =~ sc7_task_completion + sc7_distracted + sc7_fidgeting + sc7_think_act + sc7_temper + sc7_obedient + sc7_lying
SC_t11 =~ sc11_task_completion + sc11_distracted + sc11_fidgeting + sc11_think_act + sc11_temper + sc11_obedient + sc11_lying
SC_t14 =~ sc14_task_completion + sc14_distracted + sc14_fidgeting + sc14_think_act + sc14_temper + sc14_obedient + sc14_lying
SC_t17 =~ sc17_task_completion + sc17_distracted + sc17_fidgeting + sc17_think_act + sc17_temper + sc17_obedient + sc17_lying
'

longFacNames <- list(SC = c("SC_t5", "SC_t7", "SC_t11", "SC_t14", "SC_t17"))
longIndNames <- list(
  task_completion = c("sc5_task_completion", "sc7_task_completion", "sc11_task_completion", "sc14_task_completion", "sc17_task_completion"),
  distracted = c("sc5_distracted", "sc7_distracted", "sc11_distracted", "sc14_distracted", "sc17_distracted"),
  fidgeting = c("sc5_fidgeting", "sc7_fidgeting", "sc11_fidgeting", "sc14_fidgeting", "sc17_fidgeting"),
  think_act = c("sc5_think_act", "sc7_think_act", "sc11_think_act", "sc14_think_act", "sc17_think_act"),
  temper = c("sc5_temper", "sc7_temper", "sc11_temper", "sc14_temper", "sc17_temper"),
  obedient = c("sc5_obedient", "sc7_obedient", "sc11_obedient", "sc14_obedient", "sc17_obedient"),
  lying = c("sc5_lying", "sc7_lying", "sc11_lying", "sc14_lying", "sc17_lying")
)

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
  auto             = 1L,   # correlated uniqueness for same item across adjacent waves
  return.fit       = FALSE
)

model_mi <- as.character(ME, single = TRUE)

fit_mi <- lavaan::sem(
  model_mi,
  data             = merged_data,
  ordered          = ordered_vars,
  estimator        = "WLSMV",
  parameterization = "theta",
  std.lv           = FALSE,
  meanstructure    = TRUE,
  sampling.weights = "weight1"
)

cat("\nMeasurement model (MI) fit complete.\n")

# ---------------------------------------------
# Factor scores for SC_t5..SC_t17 (MI-consistent)
# ---------------------------------------------
# Indices of rows used by lavaan (align IDs/weights to factor scores)
used_idx <- lavInspect(fit_mi, "case.idx")
if (is.list(used_idx)) used_idx <- used_idx[[1]]

fscores <- lavPredict(fit_mi, type = "lv", method = "regression")

# Keep only the SC_t* factors in the intended order
sc_cols <- c("SC_t5", "SC_t7", "SC_t11", "SC_t14", "SC_t17")
sc_cols <- sc_cols[sc_cols %in% colnames(fscores)]

# Align IDs and weights to the factor score rows (cases used by lavaan)
ids_vec <- merged_data[[subject_id_var]][used_idx]
w_vec   <- merged_data$weight1[used_idx]

fs_df <- as_tibble(fscores[, sc_cols, drop = FALSE]) %>%
  mutate(
    !!subject_id_var := ids_vec,
    weight1 = w_vec
  )

# ---------------------------------------------
# Long format for lcmm::hlme
# ---------------------------------------------
age_map <- tibble(
  wave = sc_cols,
  age  = c(5, 7, 11, 14, 17)[seq_along(sc_cols)]
)

sc_long <- fs_df %>%
  pivot_longer(all_of(sc_cols), names_to = "wave", values_to = "SC") %>%
  left_join(age_map, by = "wave") %>%
  mutate(
    time = age - 11,
    id = as.numeric(as.factor(.data[[subject_id_var]]))
  ) %>%
  select(id, age, time, SC, weight1) %>%
  arrange(id, age)

# Remove rows with all-missing per id
valid_ids <- sc_long %>% group_by(id) %>% summarise(has_obs = any(!is.na(SC)), .groups = "drop") %>% filter(has_obs) %>% pull(id)
sc_long <- sc_long %>% filter(id %in% valid_ids)

N_ids <- n_distinct(sc_long$id)
cat("\nPrepared long data for hlme. Unique subjects:", N_ids, "\n")

# Print data diagnostics
cat("\nData diagnostics:\n")
cat("  Total observations:", nrow(sc_long), "\n")
cat("  Missing SC values:", sum(is.na(sc_long$SC)), "(", 
    round(100 * sum(is.na(sc_long$SC)) / nrow(sc_long), 1), "%)\n")

obs_per_id <- sc_long %>%
  group_by(id) %>%
  summarise(n_obs = sum(!is.na(SC)), .groups = "drop")

cat("  Observations per subject:\n")
cat("    Min:", min(obs_per_id$n_obs), "\n")
cat("    Median:", median(obs_per_id$n_obs), "\n")
cat("    Mean:", round(mean(obs_per_id$n_obs), 2), "\n")
cat("    Max:", max(obs_per_id$n_obs), "\n")

# Summary statistics of SC factor scores
sc_stats <- sc_long %>% filter(!is.na(SC)) %>% pull(SC)
cat("  SC factor score distribution:\n")
cat("    Range: [", round(min(sc_stats), 2), ",", round(max(sc_stats), 2), "]\n")
cat("    Mean:", round(mean(sc_stats), 2), ", SD:", round(sd(sc_stats), 2), "\n")
cat("    Skewness:", round(e1071::skewness(sc_stats), 2), "\n")

# ---------------------------------------------
# Fit 1-6 class models manually
# ---------------------------------------------
set.seed(12345)
models <- list()

# Model 1: 1-class model
cat("\nFitting 1-class model...\n")
models[["ng1"]] <- lcmm::hlme(
  fixed     = SC ~ time,
  random    = ~ time,
  subject   = "id",
  data      = sc_long,
  na.action = 1,
  verbose   = FALSE,
  maxiter   = 1000
)
if (models[["ng1"]]$conv %in% c(1, 2, 3)) {
  cat("Successfully fitted 1-class model (conv=", models[["ng1"]]$conv, ")\n", sep = "")
} else {
  cat("Warning: 1-class model did not converge properly (conv=", models[["ng1"]]$conv, ")\n", sep = "")
}

summary(models[["ng1"]])

# ---------------------------------------------
# Plot 1-class model trajectory
# ---------------------------------------------
if (models[["ng1"]]$conv %in% c(1, 2, 3)) {
  # Calculate overall trajectory (no classes, just one trajectory)
  traj_summary_ng1 <- sc_long %>%
    filter(!is.na(SC)) %>%
    group_by(age) %>%
    summarise(
      mean_SC = weighted.mean(SC, w = weight1, na.rm = TRUE),
      se_SC = sqrt(var(SC, na.rm = TRUE) / n()),
      n = n(),
      .groups = "drop"
    )
  
  # Plot trajectory
  p_traj_ng1 <- ggplot(traj_summary_ng1, aes(x = age, y = mean_SC)) +
    geom_line(size = 1.1, color = "#2CA02C") +
    geom_point(size = 2, color = "#2CA02C") +
    geom_errorbar(aes(ymin = mean_SC - se_SC, ymax = mean_SC + se_SC), 
                  width = 0.3, color = "#2CA02C") +
    scale_x_continuous(breaks = c(5, 7, 11, 14, 17)) +
    labs(
      title = "Self-Control Trajectory (1-Class Model)",
      x = "Age",
      y = "Average latent self-control (factor score)"
    ) +
    theme_minimal(base_size = 12)
  
  print(p_traj_ng1)
  
  cat("\n1-Class Model Results:\n")
  cat("Overall trajectory (single latent class):\n")
  print(traj_summary_ng1)
  
} else {
  cat("1-class model did not converge successfully.\n")
}

# Model 2: 2-class model
cat("\nFitting 2-class model...\n")
models[["ng2"]] <- try(lcmm::hlme(
  fixed     = SC ~ time,
  mixture   = ~ time,
  random    = ~ time,
  subject   = "id",
  ng        = 2,
  data      = sc_long,
  B         = models[["ng1"]],
  na.action = 1,
  verbose   = FALSE,
  maxiter   = 1000
), silent = TRUE)

if (!inherits(models[["ng2"]], "try-error") && models[["ng2"]]$conv %in% c(1, 2, 3)) {
  cat("Successfully fitted 2-class model (conv=", models[["ng2"]]$conv, ")\n", sep = "")
} else {
  cat("Warning: 2-class model did not converge. Trying multiple random starts...\n")
  best_ll <- -Inf
  for (i in 1:10) {
    m_try <- try(lcmm::hlme(
      fixed     = SC ~ time,
      mixture   = ~ time,
      random    = ~ time,
      subject   = "id",
      ng        = 2,
      data      = sc_long,
      na.action = 1,
      verbose   = FALSE,
      maxiter   = 1000
    ), silent = TRUE)
    if (!inherits(m_try, "try-error") && m_try$conv %in% c(1, 2, 3)) {
      if (m_try$loglik > best_ll) {
        models[["ng2"]] <- m_try
        best_ll <- m_try$loglik
      }
    }
  }
  if (!inherits(models[["ng2"]], "try-error") && models[["ng2"]]$conv %in% c(1, 2, 3)) {
    cat("Successfully fitted 2-class model after multiple starts (conv=", models[["ng2"]]$conv, ", loglik=", round(best_ll, 2), ")\n", sep = "")
  } else {
    cat("Failed to converge 2-class model after 10 attempts\n")
  }
}

summary(models[["ng2"]])

# ---------------------------------------------
# Plot 2-class model trajectories
# ---------------------------------------------
if (!inherits(models[["ng2"]], "try-error") && models[["ng2"]]$conv %in% c(1, 2, 3)) {
  # Get class assignments for 2-class model
  pprob_df <- as_tibble(models[["ng2"]]$pprob)
  
  # The first column is the ID, find it dynamically
  id_col <- names(pprob_df)[1]
  
  class_assign_ng2 <- pprob_df %>%
    rename(id = !!id_col) %>%
    mutate(class = as.integer(class)) %>%
    select(id, class, starts_with("prob"))
  
  # Merge with longitudinal data
  sc_long_cls_ng2 <- sc_long %>% 
    left_join(class_assign_ng2 %>% select(id, class), by = "id")
  
  # Calculate trajectory summaries by class
  traj_summary_ng2 <- sc_long_cls_ng2 %>%
    filter(!is.na(class), !is.na(SC)) %>%
    group_by(class, age) %>%
    summarise(
      mean_SC = weighted.mean(SC, w = weight1, na.rm = TRUE),
      se_SC = sqrt(var(SC, na.rm = TRUE) / n()),
      n = n(),
      .groups = "drop"
    )
  
  # Plot trajectories
  p_traj_ng2 <- ggplot(traj_summary_ng2, aes(x = age, y = mean_SC, color = factor(class), group = class)) +
    geom_line(size = 1.1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = mean_SC - se_SC, ymax = mean_SC + se_SC), width = 0.3) +
    scale_x_continuous(breaks = c(5, 7, 11, 14, 17)) +
    scale_color_manual(values = c("#E31A1C", "#1F78B4")) +
    labs(
      title = "Self-Control Trajectories by Class (2-Class Model)",
      x = "Age",
      y = "Average latent self-control (factor score)",
      color = "Class"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  print(p_traj_ng2)
  
  # Print class sizes
  class_sizes <- class_assign_ng2 %>%
    count(class) %>%
    mutate(percentage = round(100 * n / sum(n), 1))
  
  cat("\n2-Class Model Results:\n")
  cat("Class sizes:\n")
  print(class_sizes)
  
  # Print mean trajectories
  cat("\nMean trajectories by class:\n")
  print(traj_summary_ng2)
  
} else {
  cat("2-class model did not converge successfully.\n")
}

# Model 3: 3-class model
cat("\nFitting 3-class model...\n")
best_init <- if (!inherits(models[["ng2"]], "try-error") && models[["ng2"]]$conv %in% c(1, 2, 3)) {
  models[["ng2"]]
} else {
  models[["ng1"]]
}

# Try simpler random structure first
models[["ng3"]] <- try(lcmm::hlme(
  fixed     = SC ~ time,
  mixture   = ~ time,
  random    = ~ 1,           # random intercept only
  idiag     = TRUE,          # diagonal covariance
  subject   = "id",
  ng        = 3,
  data      = sc_long,
  B         = best_init,
  na.action = 1,
  verbose   = FALSE,
  maxiter   = 1000
), silent = TRUE)

if (!inherits(models[["ng3"]], "try-error") && models[["ng3"]]$conv %in% c(1, 2, 3)) {
  cat("Successfully fitted 3-class model with simple RE (conv=", models[["ng3"]]$conv, ")\n", sep = "")
} else {
  cat("Warning: 3-class model did not converge. Trying multiple random starts with different structures...\n")
  best_ll <- -Inf
  
  # Try 20 attempts with different structures
  for (i in 1:20) {
    # Alternate between simple and complex structures
    if (i %% 2 == 1) {
      # Simple structure
      m_try <- try(lcmm::hlme(
        fixed     = SC ~ time,
        mixture   = ~ time,
        random    = ~ 1,
        idiag     = TRUE,
        subject   = "id",
        ng        = 3,
        data      = sc_long,
        na.action = 1,
        verbose   = FALSE,
        maxiter   = 1000
      ), silent = TRUE)
    } else {
      # Complex structure
      m_try <- try(lcmm::hlme(
        fixed     = SC ~ time,
        mixture   = ~ time,
        random    = ~ time,
        subject   = "id",
        ng        = 3,
        data      = sc_long,
        na.action = 1,
        verbose   = FALSE,
        maxiter   = 1000
      ), silent = TRUE)
    }
    
    if (!inherits(m_try, "try-error") && m_try$conv %in% c(1, 2, 3)) {
      if (m_try$loglik > best_ll) {
        models[["ng3"]] <- m_try
        best_ll <- m_try$loglik
      }
    }
  }
  
  if (!inherits(models[["ng3"]], "try-error") && models[["ng3"]]$conv %in% c(1, 2, 3)) {
    cat("Successfully fitted 3-class model after multiple starts (conv=", models[["ng3"]]$conv, ", loglik=", round(best_ll, 2), ")\n", sep = "")
  } else {
    cat("Failed to converge 3-class model after 20 attempts - data may not support 3 classes\n")
  }
}

# Model 4: 4-class model
cat("\nFitting 4-class model...\n")
best_init <- if (!inherits(models[["ng3"]], "try-error") && models[["ng3"]]$conv %in% c(1, 2, 3)) {
  models[["ng3"]]
} else if (!inherits(models[["ng2"]], "try-error") && models[["ng2"]]$conv %in% c(1, 2, 3)) {
  models[["ng2"]]
} else {
  models[["ng1"]]
}

# Try simpler random structure first
models[["ng4"]] <- try(lcmm::hlme(
  fixed     = SC ~ time,
  mixture   = ~ time,
  random    = ~ 1,           # random intercept only
  idiag     = TRUE,          # diagonal covariance
  subject   = "id",
  ng        = 4,
  data      = sc_long,
  B         = best_init,
  na.action = 1,
  verbose   = FALSE,
  maxiter   = 1000
), silent = TRUE)

if (!inherits(models[["ng4"]], "try-error") && models[["ng4"]]$conv %in% c(1, 2, 3)) {
  cat("Successfully fitted 4-class model with simple RE (conv=", models[["ng4"]]$conv, ")\n", sep = "")
} else {
  cat("Warning: 4-class model did not converge. Trying multiple random starts with different structures...\n")
  best_ll <- -Inf
  
  # Try 20 attempts with different structures
  for (i in 1:20) {
    # Alternate between simple and complex structures
    if (i %% 2 == 1) {
      # Simple structure
      m_try <- try(lcmm::hlme(
        fixed     = SC ~ time,
        mixture   = ~ time,
        random    = ~ 1,
        idiag     = TRUE,
        subject   = "id",
        ng        = 4,
        data      = sc_long,
        na.action = 1,
        verbose   = FALSE,
        maxiter   = 1000
      ), silent = TRUE)
    } else {
      # Complex structure
      m_try <- try(lcmm::hlme(
        fixed     = SC ~ time,
        mixture   = ~ time,
        random    = ~ time,
        subject   = "id",
        ng        = 4,
        data      = sc_long,
        na.action = 1,
        verbose   = FALSE,
        maxiter   = 1000
      ), silent = TRUE)
    }
    
    if (!inherits(m_try, "try-error") && m_try$conv %in% c(1, 2, 3)) {
      if (m_try$loglik > best_ll) {
        models[["ng4"]] <- m_try
        best_ll <- m_try$loglik
      }
    }
  }
  
  if (!inherits(models[["ng4"]], "try-error") && models[["ng4"]]$conv %in% c(1, 2, 3)) {
    cat("Successfully fitted 4-class model after multiple starts (conv=", models[["ng4"]]$conv, ", loglik=", round(best_ll, 2), ")\n", sep = "")
  } else {
    cat("Failed to converge 4-class model after 20 attempts - data may not support 4 classes\n")
  }
}

# Model 5: 5-class model
cat("\nFitting 5-class model...\n")
best_init <- if (!inherits(models[["ng4"]], "try-error") && models[["ng4"]]$conv %in% c(1, 2, 3)) {
  models[["ng4"]]
} else if (!inherits(models[["ng3"]], "try-error") && models[["ng3"]]$conv %in% c(1, 2, 3)) {
  models[["ng3"]]
} else if (!inherits(models[["ng2"]], "try-error") && models[["ng2"]]$conv %in% c(1, 2, 3)) {
  models[["ng2"]]
} else {
  models[["ng1"]]
}

# Try simpler random structure first
models[["ng5"]] <- try(lcmm::hlme(
  fixed     = SC ~ time,
  mixture   = ~ time,
  random    = ~ 1,           # random intercept only
  idiag     = TRUE,          # diagonal covariance
  subject   = "id",
  ng        = 5,
  data      = sc_long,
  B         = best_init,
  na.action = 1,
  verbose   = FALSE,
  maxiter   = 1000
), silent = TRUE)

if (!inherits(models[["ng5"]], "try-error") && models[["ng5"]]$conv %in% c(1, 2, 3)) {
  cat("Successfully fitted 5-class model with simple RE (conv=", models[["ng5"]]$conv, ")\n", sep = "")
} else {
  cat("Warning: 5-class model did not converge. Trying multiple random starts with different structures...\n")
  best_ll <- -Inf
  
  # Try 20 attempts with different structures
  for (i in 1:20) {
    # Alternate between simple and complex structures
    if (i %% 2 == 1) {
      # Simple structure
      m_try <- try(lcmm::hlme(
        fixed     = SC ~ time,
        mixture   = ~ time,
        random    = ~ 1,
        idiag     = TRUE,
        subject   = "id",
        ng        = 5,
        data      = sc_long,
        na.action = 1,
        verbose   = FALSE,
        maxiter   = 1000
      ), silent = TRUE)
    } else {
      # Complex structure
      m_try <- try(lcmm::hlme(
        fixed     = SC ~ time,
        mixture   = ~ time,
        random    = ~ time,
        subject   = "id",
        ng        = 5,
        data      = sc_long,
        na.action = 1,
        verbose   = FALSE,
        maxiter   = 1000
      ), silent = TRUE)
    }
    
    if (!inherits(m_try, "try-error") && m_try$conv %in% c(1, 2, 3)) {
      if (m_try$loglik > best_ll) {
        models[["ng5"]] <- m_try
        best_ll <- m_try$loglik
      }
    }
  }
  
  if (!inherits(models[["ng5"]], "try-error") && models[["ng5"]]$conv %in% c(1, 2, 3)) {
    cat("Successfully fitted 5-class model after multiple starts (conv=", models[["ng5"]]$conv, ", loglik=", round(best_ll, 2), ")\n", sep = "")
  } else {
    cat("Failed to converge 5-class model after 20 attempts - data may not support 5 classes\n")
  }
}

# Model 6: 6-class model
cat("\nFitting 6-class model...\n")
best_init <- if (!inherits(models[["ng5"]], "try-error") && models[["ng5"]]$conv %in% c(1, 2, 3)) {
  models[["ng5"]]
} else if (!inherits(models[["ng4"]], "try-error") && models[["ng4"]]$conv %in% c(1, 2, 3)) {
  models[["ng4"]]
} else if (!inherits(models[["ng3"]], "try-error") && models[["ng3"]]$conv %in% c(1, 2, 3)) {
  models[["ng3"]]
} else if (!inherits(models[["ng2"]], "try-error") && models[["ng2"]]$conv %in% c(1, 2, 3)) {
  models[["ng2"]]
} else {
  models[["ng1"]]
}

# Try simpler random structure first
models[["ng6"]] <- try(lcmm::hlme(
  fixed     = SC ~ time,
  mixture   = ~ time,
  random    = ~ 1,           # random intercept only
  idiag     = TRUE,          # diagonal covariance
  subject   = "id",
  ng        = 6,
  data      = sc_long,
  B         = best_init,
  na.action = 1,
  verbose   = FALSE,
  maxiter   = 1000
), silent = TRUE)

if (!inherits(models[["ng6"]], "try-error") && models[["ng6"]]$conv %in% c(1, 2, 3)) {
  cat("Successfully fitted 6-class model with simple RE (conv=", models[["ng6"]]$conv, ")\n", sep = "")
} else {
  cat("Warning: 6-class model did not converge. Trying multiple random starts with different structures...\n")
  best_ll <- -Inf
  
  # Try 20 attempts with different structures
  for (i in 1:20) {
    # Alternate between simple and complex structures
    if (i %% 2 == 1) {
      # Simple structure
      m_try <- try(lcmm::hlme(
        fixed     = SC ~ time,
        mixture   = ~ time,
        random    = ~ 1,
        idiag     = TRUE,
        subject   = "id",
        ng        = 6,
        data      = sc_long,
        na.action = 1,
        verbose   = FALSE,
        maxiter   = 1000
      ), silent = TRUE)
    } else {
      # Complex structure
      m_try <- try(lcmm::hlme(
        fixed     = SC ~ time,
        mixture   = ~ time,
        random    = ~ time,
        subject   = "id",
        ng        = 6,
        data      = sc_long,
        na.action = 1,
        verbose   = FALSE,
        maxiter   = 1000
      ), silent = TRUE)
    }
    
    if (!inherits(m_try, "try-error") && m_try$conv %in% c(1, 2, 3)) {
      if (m_try$loglik > best_ll) {
        models[["ng6"]] <- m_try
        best_ll <- m_try$loglik
      }
    }
  }
  
  if (!inherits(models[["ng6"]], "try-error") && models[["ng6"]]$conv %in% c(1, 2, 3)) {
    cat("Successfully fitted 6-class model after multiple starts (conv=", models[["ng6"]]$conv, ", loglik=", round(best_ll, 2), ")\n", sep = "")
  } else {
    cat("Failed to converge 6-class model after 20 attempts - data may not support 6 classes\n")
  }
}

# ---------------------------------------------
# Fit indices and entropy
# ---------------------------------------------
calc_entropy <- function(mod) {
  if (is.null(mod) || is.null(mod$pprob)) return(NA_real_)
  p <- as.matrix(mod$pprob[, grepl("^prob", names(mod$pprob)), drop = FALSE])
  if (!ncol(p)) return(NA_real_)
  p[p <= 0] <- .Machine$double.eps
  G <- ncol(p)
  1 + sum(p * log(p)) / (nrow(p) * log(G))
}

get_npm <- function(m) {
  if (!is.null(m$npm) && length(m$npm) == 1) as.numeric(m$npm) else length(m$best)
}

safe_AIC <- function(m) if (!is.null(m$AIC)) as.numeric(m$AIC) else AIC(m)
safe_BIC <- function(m) if (!is.null(m$BIC)) as.numeric(m$BIC) else BIC(m)

fit_tbl <- purrr::map_dfr(names(models), function(nm) {
  m <- models[[nm]]
  if (is.null(m) || inherits(m, "try-error")) return(NULL)
  if (is.null(m$conv) || !m$conv %in% c(1, 2, 3)) return(NULL)
  ll   <- as.numeric(m$loglik)
  npm  <- get_npm(m)
  tibble::tibble(
    model   = nm,
    ng      = if (!is.null(m$ng) && length(m$ng) == 1) m$ng else 1,
    loglik  = ll,
    AIC     = safe_AIC(m),
    BIC     = safe_BIC(m),
    SABIC   = -2 * ll + npm * log((N_ids + 2) / 24),  # Mplus-style sample-size adjusted BIC
    entropy = calc_entropy(m)
  )
}) %>% dplyr::arrange(ng)

print(fit_tbl)



