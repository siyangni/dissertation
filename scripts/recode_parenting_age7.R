library(pacman)
p_load(tidyverse)
p_load(stringr)
p_load(lavaan)
p_load(dplyr)
p_load(haven)

missing_codes <- c(-9, -8, -1)

recode_parenting <- function(dat) {
  if (!is.data.frame(dat)) {
    stop("Input must be a data frame")
  }
  
  cat("Starting parenting variable recoding...\n")
  
  # Handle haven_labelled columns by converting them to numeric first
  d <- dat %>% 
    mutate(across(everything(), ~{
      if (inherits(.x, "haven_labelled")) {
        # Use haven's as_factor to convert, then to numeric if needed
        # Or use haven::zap_labels to remove labels and convert to underlying values
        numeric_vals <- haven::zap_labels(.x)
        ifelse(numeric_vals %in% missing_codes, NA, numeric_vals)
      } else {
        # For non-haven_labelled columns, apply missing code replacement directly
        ifelse(.x %in% missing_codes, NA, .x)
      }
    }))

  # 1) Monitoring and structure
  if ("dmtvrla0" %in% names(d)) d <- d %>% mutate(tv_rules_time  = case_when(dmtvrla0 == 1 ~ 1, dmtvrla0 == 2 ~ 0, TRUE ~ NA_real_))
  if ("dmtvrha0" %in% names(d)) d <- d %>% mutate(tv_rules_hours = case_when(dmtvrha0 == 1 ~ 1, dmtvrha0 == 2 ~ 0, TRUE ~ NA_real_))
  if ("dmtvrma0" %in% names(d)) d <- d %>% mutate(no_tv_bedroom  = case_when(dmtvrma0 == 1 ~ 0, dmtvrma0 == 2 ~ 1, TRUE ~ NA_real_))
  if ("dmberea0" %in% names(d)) d <- d %>% mutate(regular_bedtime = (as.numeric(.data$dmberea0) - 1) / 3)

  # Outdoor supervision
  if (all(c("dmplog00","dmploua0") %in% names(d))) {
    d <- d %>%
      mutate(outdoor_opportunity = case_when(dmplog00 == 1 ~ 1, dmplog00 == 2 ~ 0, TRUE ~ NA_real_),
             supervision_outdoor = case_when(dmplog00 == 1 & dmploua0 == 2 ~ 1,
                                             dmplog00 == 1 & dmploua0 == 1 ~ 0,
                                             TRUE ~ NA_real_))
  }

  # Internet (default neutral)
  if (all(c("dminlna0","dmintha0") %in% names(d))) {
    d <- d %>%
      mutate(internet_use_allowed = case_when(dminlna0 == 1 & dmintha0 == 1 ~ 0,
                                              dminlna0 == 1 & dmintha0 == 2 ~ 1,
                                              TRUE ~ NA_real_),
             internet_control_strict = case_when(dmintha0 == 2 | dminlna0 == 2 ~ 1,
                                                 dminlna0 == 1 & dmintha0 == 1 ~ 0,
                                                 TRUE ~ NA_real_))
  }

  # Family time
  if ("dmfrtv00" %in% names(d)) d <- d %>% mutate(family_time_home = (6 - as.numeric(.data$dmfrtv00)) / 5)

  # Evening meal (single-coded)
  if ("dmevwoaa" %in% names(d)) {
    d <- d %>% mutate(
      evening_meal_parent_presence = case_when(
        dmevwoaa == 1 ~ 0.00,
        dmevwoaa == 2 ~ 1.00,
        dmevwoaa %in% c(4,5) ~ 0.66,
        dmevwoaa %in% c(3,6) ~ 0.33,
        dmevwoaa %in% c(7,8) ~ NA_real_,
        dmevwoaa == 95 ~ 0.50,
        TRUE ~ NA_real_
      ))
  }

  # Evening meal (one-hot multi-select alternative)
  evwo_cols <- names(d)[str_detect(names(d), "^dmevwoaa_")]
  if (length(evwo_cols) > 0) {
    # coalesce across selected categories with weights and take the maximum
    get_w <- function(code) ifelse(paste0("dmevwoaa_", code) %in% names(d), d[[paste0("dmevwoaa_", code)]], 0)
    weighted <- list(
      get_w("2") * 1.00,
      get_w("4") * 0.66, get_w("5") * 0.66,
      get_w("3") * 0.33, get_w("6") * 0.33,
      get_w("1") * 0.00,
      ifelse(paste0("dmevwoaa_95") %in% names(d), d[["dmevwoaa_95"]] * 0.50, 0)
    )
    maxw <- do.call(pmax, c(weighted, na.rm = TRUE))
    # If only code 7 or 8 is present, set to NA
    only_no_meal <- (get_w("7") == 1 | get_w("8") == 1) & maxw == 0
    maxw[only_no_meal] <- NA_real_
    d$evening_meal_parent_presence <- maxw
  }

  # 2) Sanctioning
  recode_app <- function(x) { x <- ifelse(x == 6, NA, x); (as.numeric(x) - 1) / 4 }
  recode_rev <- function(x) { x <- ifelse(x == 6, NA, x); (5 - as.numeric(x)) / 4 }

  if ("dmditea0" %in% names(d)) d$tell_off <- recode_app(d$dmditea0)
  if ("dmditra0" %in% names(d)) d$take_away_treats <- recode_app(d$dmditra0)
  if ("dmdibna0" %in% names(d)) d$timeout <- recode_app(d$dmdibna0)
  if ("dmdisma0" %in% names(d)) d$smack_rev <- recode_rev(d$dmdisma0)
  if ("dmdisha0" %in% names(d)) d$shout_rev <- recode_rev(d$dmdisha0)
  if ("dmdibra0" %in% names(d)) d$bribe_rev <- recode_rev(d$dmdibra0)
  if ("dmdiiga0" %in% names(d)) d$ignore_rev <- recode_rev(d$dmdiiga0)

  # 3) Warmth and attachment
  if ("dmschca0" %in% names(d)) d <- d %>% mutate(parent_child_closeness = ifelse(.data$dmschca0 == 5, NA, (as.numeric(.data$dmschca0) - 1) / 3))
  if ("dmenlia0" %in% names(d)) d <- d %>% mutate(enjoy_listen_do = (as.numeric(.data$dmenlia0) - 1) / 4)
  if ("dmexafa0" %in% names(d)) d <- d %>% mutate(express_affection = (as.numeric(.data$dmexafa0) - 1) / 4)
  if ("dcsc0019" %in% names(d)) d <- d %>% mutate(child_weekend_fun = (3 - as.numeric(.data$dcsc0019)) / 2)
  if ("dcsc0020" %in% names(d)) d <- d %>% mutate(child_disclosure_home = as.numeric(.data$dcsc0020) / 3)

  # 4) Involvement and stimulation
  map_pa <- function(x) (6 - as.numeric(x)) / 5
  
  # Define the mapping between original variables and new variable names
  involvement_mapping <- c(
    "dmreofa0" = "reading_together",
    "dmsitsa0" = "storytelling", 
    "dmplmua0" = "music_together",
    "dmpamaa0" = "arts_crafts",
    "dmactia0" = "active_play",
    "GAME" = "indoor_games",
    "WALK" = "park_visits",
    "BEDR" = "partner_puts_to_bed",
    "LOOK" = "partner_looks_after"
  )
  
  # Apply the mapping
  for (orig_var in names(involvement_mapping)) {
    if (orig_var %in% names(d)) {
      new_var <- involvement_mapping[orig_var]
      d[[new_var]] <- map_pa(d[[orig_var]])
    }
  }

  # Subindexes (require at least half non-missing)
  row_mean <- function(df_cols, min_frac = 0.5) {
    X <- d[, df_cols, drop = FALSE]
    m <- rowMeans(X, na.rm = TRUE)
    valid <- rowSums(!is.na(X))
    m[valid < ceiling(min_frac * length(df_cols))] <- NA
    m
  }

  monitoring_vars <- intersect(c("tv_rules_time","tv_rules_hours","no_tv_bedroom","regular_bedtime",
                                 "supervision_outdoor","family_time_home","evening_meal_parent_presence",
                                 "internet_use_allowed"), names(d))
  if (length(monitoring_vars) > 0) d$monitoring_structure_index <- row_mean(monitoring_vars)

  appropriate_vars <- intersect(c("tell_off","take_away_treats","timeout"), names(d))
  if (length(appropriate_vars) > 0) d$appropriate_sanctioning_index <- row_mean(appropriate_vars)

  harsh_vars <- intersect(c("smack_rev","shout_rev","bribe_rev","ignore_rev"), names(d))
  if (length(harsh_vars) > 0) d$harsh_inconsistent_index <- row_mean(harsh_vars)

  warmth_vars <- intersect(c("parent_child_closeness","enjoy_listen_do","express_affection",
                             "child_weekend_fun","child_disclosure_home"), names(d))
  if (length(warmth_vars) > 0) d$warmth_attachment_index <- row_mean(warmth_vars)

  involvement_vars <- intersect(c("reading_together","storytelling","music_together","arts_crafts",
                                  "active_play","indoor_games","park_visits",
                                  "partner_puts_to_bed","partner_looks_after"), names(d))
  if (length(involvement_vars) > 0) d$involvement_stimulation_index <- row_mean(involvement_vars)

  # Grand composite: simple mean over all atomic recoded items
  atomic <- setdiff(names(d), names(dat))
  atomic <- atomic[!str_detect(atomic, "_index$")]
  if (length(atomic) > 0) d$parenting_composite_mean <- row_mean(atomic)

  # Print summary of created variables
  new_vars <- setdiff(names(d), names(dat))
  cat("Created", length(new_vars), "new parenting variables:\n")
  if (length(new_vars) > 0) {
    cat(paste0("- ", new_vars), sep = "\n")
  }
  cat("Parenting variable recoding completed.\n")
  
  d
}

# Recode parenting (check if merged_data exists)
if (!exists("merged_data")) {
  stop("merged_data not found. Please run data_preparation.R first.")
}

recoded_parenting_age7 <- recode_parenting(merged_data)




# ============================================================================
# CONFIRMATORY FACTOR ANALYSIS (CFA) FOR PARENTING CONSTRUCT
# ============================================================================

# Check data availability for CFA variables
cat("Checking data availability for CFA variables...\n")
cfa_vars <- c("tv_rules_time", "tv_rules_hours", "no_tv_bedroom", "regular_bedtime",
              "supervision_outdoor", "family_time_home", "evening_meal_parent_presence",
              "internet_use_allowed", "tell_off", "take_away_treats", "timeout",
              "smack_rev", "shout_rev", "bribe_rev", "ignore_rev",
              "parent_child_closeness", "enjoy_listen_do", "express_affection",
              "child_weekend_fun", "child_disclosure_home", "reading_together",
              "storytelling", "music_together", "arts_crafts", "active_play")

# Check which variables exist and have sufficient data
available_vars <- intersect(cfa_vars, names(recoded_parenting_age7))
cat("Available CFA variables:", length(available_vars), "out of", length(cfa_vars), "\n")

# Check missing data patterns
missing_summary <- recoded_parenting_age7 %>%
  select(all_of(available_vars)) %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(total_n = nrow(recoded_parenting_age7),
         missing_pct = round(100 * missing_count / total_n, 1)) %>%
  arrange(desc(missing_pct))

print(missing_summary, n = 30)

# Remove variables with >50% missing data
usable_vars <- missing_summary %>%
  filter(missing_pct <= 50) %>%
  pull(variable)

cat("\nUsable variables for CFA (<=50% missing):", length(usable_vars), "\n")
cat("Variables:", paste(usable_vars, collapse = ", "), "\n")

# ============================================================================
# APPROACH 1: CFA with Continuous Recoded Variables (0-1 scale)
# ============================================================================
if (length(usable_vars) >= 3) {
  cat("\n=== APPROACH 1: Continuous CFA (ML estimator) ===\n")
  
  # Create model string with available variables
  model_continuous <- paste("parenting =~", paste(usable_vars, collapse = " + "))
  cat("Model specification:\n", model_continuous, "\n")
  
  # Fit CFA with ML estimator (appropriate for continuous variables)
  fit_continuous <- lavaan::cfa(model_continuous, 
                               data = recoded_parenting_age7,
                               estimator = "ML", 
                               missing = "fiml")  # Full Information ML for missing data
  
  cat("\n--- Continuous CFA Results ---\n")
  summary(fit_continuous, fit.measures = TRUE, standardized = TRUE)
  
  # ============================================================================
  # APPROACH 2: CFA with Original Ordinal Variables (WLSMV estimator)
  # ============================================================================
  cat("\n=== APPROACH 2: Ordinal CFA (WLSMV estimator) ===\n")
  
  # Map recoded variables back to original variables
  var_mapping <- c(
    "tv_rules_time" = "dmtvrla0",
    "tv_rules_hours" = "dmtvrha0", 
    "no_tv_bedroom" = "dmtvrma0",
    "regular_bedtime" = "dmberea0",
    "supervision_outdoor" = "dmploua0",
    "family_time_home" = "dmfrtv00",
    "evening_meal_parent_presence" = "dmevwoaa",
    "internet_use_allowed" = "dminlna0",  # Using first internet variable
    "tell_off" = "dmditea0",
    "take_away_treats" = "dmditra0",
    "timeout" = "dmdibna0",
    "smack_rev" = "dmdisma0",
    "shout_rev" = "dmdisha0", 
    "bribe_rev" = "dmdibra0",
    "ignore_rev" = "dmdiiga0",
    "parent_child_closeness" = "dmschca0",
    "enjoy_listen_do" = "dmenlia0",
    "express_affection" = "dmexafa0",
    "child_weekend_fun" = "dcsc0019",
    "child_disclosure_home" = "dcsc0020"
  )
  
  # Get original variables that correspond to usable recoded variables
  usable_original <- var_mapping[usable_vars]
  usable_original <- usable_original[!is.na(usable_original)]
  available_original <- intersect(usable_original, names(recoded_parenting_age7))
  
  if (length(available_original) >= 3) {
    # Create model with original variable names
    model_ordinal <- paste("parenting =~", paste(available_original, collapse = " + "))
    cat("Ordinal model specification:\n", model_ordinal, "\n")
    
    # Prepare data with ordered factors
    ordinal_data <- recoded_parenting_age7 %>%
      mutate(across(all_of(available_original), ~{
        # Convert to ordered factor, removing missing codes
        .x[.x %in% c(-9, -8, -1)] <- NA
        ordered(.x)
      }))
    
    # Fit ordinal CFA
    fit_ordinal <- lavaan::cfa(model_ordinal,
                              data = ordinal_data,
                              estimator = "WLSMV",
                              ordered = available_original,
                              missing = "pairwise")
    
    cat("\n--- Ordinal CFA Results ---\n")
    summary(fit_ordinal, fit.measures = TRUE, standardized = TRUE)
  } else {
    cat("Insufficient original ordinal variables available for ordinal CFA\n")
  }
  
} else {
  cat("Insufficient variables for CFA analysis (need at least 3)\n")
}
