library(pacman)
# Load all necessary packages. dplyr and stringr are part of tidyverse.
p_load(tidyverse, psych, lavaan, haven)

# --- Define Global Constants and Helper Functions ---

# Define missing value codes
missing_codes <- c(-9, -8, -1)

# Helper to recode a 1-5 scale to 0-1 (NA for 6)
recode_app <- function(x) {
  x_num <- as.numeric(x)
  x_num <- ifelse(x_num == 6, NA_real_, x_num)
  (x_num - 1) / 4
}

# Helper to reverse-recode a 1-5 scale to 0-1 (NA for 6)
recode_rev <- function(x) {
  x_num <- as.numeric(x)
  x_num <- ifelse(x_num == 6, NA_real_, x_num)
  (5 - x_num) / 4
}

# Helper to recode a 1-6 involvement scale to 0-1 (6 is high -> 1)
map_pa <- function(x) {
  (6 - as.numeric(x)) / 5
}

#' Calculate row-wise mean with a minimum valid item threshold
#'
#' @param data The data frame.
#' @param cols A character vector of column names to average.
#' @param min_frac The minimum fraction of non-NA items required (default 0.5).
#' @return A numeric vector of row means.
row_mean <- function(data, cols, min_frac = 0.5) {
  # Select only columns that actually exist in the data
  valid_cols <- intersect(cols, names(data))
  
  if (length(valid_cols) == 0) {
    # If no valid columns, return a vector of NAs
    return(rep(NA_real_, nrow(data)))
  }
  
  X <- data[, valid_cols, drop = FALSE]
  m <- rowMeans(X, na.rm = TRUE)
  
  # Count valid (non-NA) items per row
  valid_count <- rowSums(!is.na(X))
  min_valid <- ceiling(min_frac * length(valid_cols))
  
  # Set to NA if not enough valid items
  m[valid_count < min_valid] <- NA_real_
  
  # Handle rows where all items were NA (rowMeans returns NaN)
  m[is.nan(m)] <- NA_real_
  
  m
}


# --- Main Parenting Recode Function ---

recode_parenting <- function(dat) {
  if (!is.data.frame(dat)) {
    stop("Input must be a data frame")
  }
  
  cat("Starting parenting variable recoding...\n")
  
  # --- 1. Identify all source variables ---
  # This is much more efficient than processing all columns
  source_vars <- c(
    # Monitoring
    "dmtvrla0", "dmtvrha0", "dmtvrma0", "dmberea0", "dmplog00", "dmploua0",
    "dminlna0", "dmintha0", "dmfrtv00", "dmevwoaa",
    # Sanctioning
    "dmditea0", "dmditra0", "dmdibna0", "dmdirea0", "dmdisma0", 
    "dmdisha0", "dmdibra0", "dmdiiga0",
    # Warmth
    "dmschca0", "dmenlia0", "dmexafa0", "dcsc0019", "dcsc0020",
    # Involvement
    "dmreofa0", "dmsitsa0", "dmplmua0", "dmpamaa0", "dmactia0", 
    "dmgamea0", "dmwalka0", "BEDR", "LOOK",
    # Additional parenting/behavioral variables
    "epdibn00", "epditr00", "epdire00",
    "fpwhet00", "fpwhot00", "fpwhat00",
    "fcoutw00", "fcotwi00", "fcotwd00",
    "gcoutw00",
    "cmdibna0", "cmditra0", "cmdirea0"
  )
  
  # Find multi-select evening meal columns
  evwo_cols <- names(dat)[str_detect(names(dat), "^dmevwoaa_")]
  all_source_vars <- c(source_vars, evwo_cols)
  
  # Find which of these are actually in the data
  existing_source_vars <- intersect(all_source_vars, names(dat))
  
  if (length(existing_source_vars) == 0) {
    cat("No source parenting variables found. Returning original data.\n")
    return(dat)
  }
  
  # --- 2. Remove any pre-existing derived variables to prevent conflicts ---
  cat("Removing any pre-existing derived variables...\n")
  derived_vars_to_remove <- c(
    "evening_meal_parent_presence", "evening_meal_parent_presence_single", 
    "evening_meal_parent_presence_multi"
  )
  existing_derived <- intersect(derived_vars_to_remove, names(dat))
  if (length(existing_derived) > 0) {
    d <- dat %>% select(-all_of(existing_derived))
    cat("Removed", length(existing_derived), "pre-existing derived variables\n")
  } else {
    d <- dat
  }
  
  # --- 3. Handle missing codes and haven labels ---
  # Process only the columns we need, not 'across(everything())'
  cat("Cleaning missing codes and labels...\n")
  d <- d %>%
    mutate(across(all_of(existing_source_vars), ~{
      vals <- .x
      if (inherits(vals, "haven_labelled")) {
        vals <- haven::zap_labels(vals)
      }
      # Coerce to numeric and set missing codes to NA
      ifelse(vals %in% missing_codes, NA_real_, as.numeric(vals))
    }))
  
  # --- 4. Recode Variables ---
  
  # 1) Monitoring and structure (Complex, custom recodes)
  if ("dmtvrla0" %in% names(d)) d <- d %>% mutate(tv_rules_time  = case_when(dmtvrla0 == 1 ~ 1, dmtvrla0 == 2 ~ 0, TRUE ~ NA_real_))
  if ("dmtvrha0" %in% names(d)) d <- d %>% mutate(tv_rules_hours = case_when(dmtvrha0 == 1 ~ 1, dmtvrha0 == 2 ~ 0, TRUE ~ NA_real_))
  if ("dmtvrma0" %in% names(d)) d <- d %>% mutate(no_tv_bedroom  = case_when(dmtvrma0 == 1 ~ 0, dmtvrma0 == 2 ~ 1, TRUE ~ NA_real_))
  if ("dmberea0" %in% names(d)) d <- d %>% mutate(regular_bedtime = (dmberea0 - 1) / 3)
  if ("dmfrtv00" %in% names(d)) d <- d %>% mutate(family_time_home = (6 - dmfrtv00) / 5)
  
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
  
  # Evening meal (single-coded)
  if ("dmevwoaa" %in% names(d)) {
    d <- d %>% mutate(
      evening_meal_parent_presence_single = case_when(
        dmevwoaa == 1 ~ 0.00,
        dmevwoaa == 2 ~ 1.00,
        dmevwoaa %in% c(4,5) ~ 0.66,
        dmevwoaa %in% c(3,6) ~ 0.33,
        dmevwoaa %in% c(7,8) ~ NA_real_, # Explicitly NA for "no meal"
        dmevwoaa == 95 ~ 0.50, # "Sometimes"
        TRUE ~ NA_real_
      ))
  }
  
  # Evening meal (one-hot multi-select alternative)
  if (length(evwo_cols) > 0) {
    cat("Processing multi-select evening meal columns...\n")
    
    # Helper to safely get weighted value, 0 if column doesn't exist/is 0/NA
    get_w <- function(code, weight) {
      colname <- paste0("dmevwoaa_", code)
      if (colname %in% names(d)) {
        # Replace NA with 0 before multiplying
        return(ifelse(is.na(d[[colname]]), 0, d[[colname]]) * weight) 
      } else {
        return(rep(0, nrow(d))) # Return vector of 0s
      }
    }
    
    # Create a list of weighted vectors
    weighted_vectors <- list(
      get_w("2", 1.00),
      get_w("4", 0.66), get_w("5", 0.66),
      get_w("3", 0.33), get_w("6", 0.33),
      get_w("1", 0.00),
      get_w("95", 0.50)
    )
    
    # Calculate the maximum weight (presence) using pmax
    max_w <- do.call(pmax, c(weighted_vectors, na.rm = TRUE))
    
    # Check for "no meal" codes (7 or 8)
    no_meal_7 <- get_w("7", 1) # Weight doesn't matter, just need 0 or 1
    no_meal_8 <- get_w("8", 1)
    
    # If only "no meal" (7 or 8) is selected and no other value, set to NA
    only_no_meal <- (no_meal_7 == 1 | no_meal_8 == 1) & (max_w == 0)
    max_w[only_no_meal] <- NA_real_
    
    d$evening_meal_parent_presence_multi <- max_w
  }
  
  # Coalesce single and multi-select evening meal variables
  if (exists("evening_meal_parent_presence_multi", where = d)) {
    d <- d %>%
      mutate(
        evening_meal_parent_presence = coalesce(
          evening_meal_parent_presence_multi, 
          evening_meal_parent_presence_single
        )
      ) %>%
      select(-evening_meal_parent_presence_multi) # Clean up
    if ("evening_meal_parent_presence_single" %in% names(d)) {
      d <- d %>% select(-evening_meal_parent_presence_single)
    }
  } else if ("evening_meal_parent_presence_single" %in% names(d)) {
    # If no multi-select, just rename the single-select
    d <- d %>% rename(evening_meal_parent_presence = evening_meal_parent_presence_single)
  }

  
  # 2) Sanctioning (Using clean mapping)
  sanction_mapping <- list(
    "dmditea0" = list(name = "tell_off",         fun = recode_app),
    "dmdisma0" = list(name = "smack_rev",        fun = recode_rev),
    "dmdisha0" = list(name = "shout_rev",        fun = recode_rev),
    "dmdibra0" = list(name = "bribe_rev",        fun = recode_rev),
    "dmdiiga0" = list(name = "ignore_rev",       fun = recode_rev)
  )
  
  for (orig_var in names(sanction_mapping)) {
    if (orig_var %in% names(d)) {
      map <- sanction_mapping[[orig_var]]
      d[[map$name]] <- map$fun(d[[orig_var]])
    }
  }

  # 3) Warmth and attachment (Mix of mapping and custom)
  if ("dmschca0" %in% names(d)) d <- d %>% mutate(parent_child_closeness = ifelse(dmschca0 == 5, NA, (dmschca0 - 1) / 3))
  if ("dmenlia0" %in% names(d)) d <- d %>% mutate(enjoy_listen_do = (dmenlia0 - 1) / 4)
  if ("dmexafa0" %in% names(d)) d <- d %>% mutate(express_affection = (dmexafa0 - 1) / 4)
  if ("dcsc0019" %in% names(d)) d <- d %>% mutate(child_weekend_fun = (3 - dcsc0019) / 2)
  if ("dcsc0020" %in% names(d)) d <- d %>% mutate(child_disclosure_home = dcsc0020 / 3)

  # 4) Involvement and stimulation (Using clean mapping)
  involvement_mapping <- c(
    "dmreofa0" = "reading_together",
    "dmsitsa0" = "storytelling", 
    "dmplmua0" = "music_together",
    "dmpamaa0" = "arts_crafts",
    "dmactia0" = "active_play",
    "dmgamea0" = "indoor_games",
    "dmwalka0" = "park_visits",
    "BEDR" = "partner_puts_to_bed",
    "LOOK" = "partner_looks_after"
  )
  
  for (orig_var in names(involvement_mapping)) {
    if (orig_var %in% names(d)) {
      new_var <- involvement_mapping[orig_var]
      d[[new_var]] <- map_pa(d[[orig_var]])
    }
  }

  # 5) Additional recoding for specific parenting/behavioral variables
  
  # Recode dmdibna0, dmditra0, dmdirea0 for age 7: negatives and 6 -> NA, subtract 1 from others
  if ("dmdibna0" %in% names(d)) {
    d <- d %>% mutate(ndmdibna0 = case_when(
      dmdibna0 < 0 | dmdibna0 == 6 ~ NA_real_,
      !is.na(dmdibna0) ~ dmdibna0 - 1,
      TRUE ~ NA_real_
    ))
  }
  
  if ("dmditra0" %in% names(d)) {
    d <- d %>% mutate(ndmditra0 = case_when(
      dmditra0 < 0 | dmditra0 == 6 ~ NA_real_,
      !is.na(dmditra0) ~ dmditra0 - 1,
      TRUE ~ NA_real_
    ))
  }
  
  if ("dmdirea0" %in% names(d)) {
    d <- d %>% mutate(ndmdirea0 = case_when(
      dmdirea0 < 0 | dmdirea0 == 6 ~ NA_real_,
      !is.na(dmdirea0) ~ dmdirea0 - 1,
      TRUE ~ NA_real_
    ))
  }
  
  # Recode epdibn00, epditr00, epdire00 for age 11: negatives and 6 -> NA, subtract 1 from others
  if ("epdibn00" %in% names(d)) {
    d <- d %>% mutate(nepdibn00 = case_when(
      epdibn00 < 0 | epdibn00 == 6 ~ NA_real_,
      !is.na(epdibn00) ~ epdibn00 - 1,
      TRUE ~ NA_real_
    ))
  }
  
  if ("epditr00" %in% names(d)) {
    d <- d %>% mutate(nepditr00 = case_when(
      epditr00 < 0 | epditr00 == 6 ~ NA_real_,
      !is.na(epditr00) ~ epditr00 - 1,
      TRUE ~ NA_real_
    ))
  }
  
  if ("epdire00" %in% names(d)) {
    d <- d %>% mutate(nepdire00 = case_when(
      epdire00 < 0 | epdire00 == 6 ~ NA_real_,
      !is.na(epdire00) ~ epdire00 - 1,
      TRUE ~ NA_real_
    ))
  }
  
  # Recode fpwhet00, fpwhot00, fpwhat00: begin from 0, reverse code, negatives -> NA
  # Assuming original scale is 1-5 or similar
  if ("fpwhet00" %in% names(d)) {
    d <- d %>% mutate(nfpwhet00 = case_when(
      fpwhet00 < 0 ~ NA_real_,
      !is.na(fpwhet00) ~ {
        max_val <- max(fpwhet00[fpwhet00 >= 0], na.rm = TRUE)
        max_val - fpwhet00
      },
      TRUE ~ NA_real_
    ))
  }
  
  if ("fpwhot00" %in% names(d)) {
    d <- d %>% mutate(nfpwhot00 = case_when(
      fpwhot00 < 0 ~ NA_real_,
      !is.na(fpwhot00) ~ {
        max_val <- max(fpwhot00[fpwhot00 >= 0], na.rm = TRUE)
        max_val - fpwhot00
      },
      TRUE ~ NA_real_
    ))
  }
  
  if ("fpwhat00" %in% names(d)) {
    d <- d %>% mutate(nfpwhat00 = case_when(
      fpwhat00 < 0 ~ NA_real_,
      !is.na(fpwhat00) ~ {
        max_val <- max(fpwhat00[fpwhat00 >= 0], na.rm = TRUE)
        max_val - fpwhat00
      },
      TRUE ~ NA_real_
    ))
  }
  
  # Recode fcoutw00, fcotwi00, fcotwd00: begin from 0, reverse code, negatives -> NA
  if ("fcoutw00" %in% names(d)) {
    d <- d %>% mutate(nfcoutw00 = case_when(
      fcoutw00 < 0 ~ NA_real_,
      !is.na(fcoutw00) ~ {
        max_val <- max(fcoutw00[fcoutw00 >= 0], na.rm = TRUE)
        max_val - fcoutw00
      },
      TRUE ~ NA_real_
    ))
  }
  
  if ("fcotwi00" %in% names(d)) {
    d <- d %>% mutate(nfcotwi00 = case_when(
      fcotwi00 < 0 ~ NA_real_,
      !is.na(fcotwi00) ~ {
        max_val <- max(fcotwi00[fcotwi00 >= 0], na.rm = TRUE)
        max_val - fcotwi00
      },
      TRUE ~ NA_real_
    ))
  }
  
  if ("fcotwd00" %in% names(d)) {
    d <- d %>% mutate(nfcotwd00 = case_when(
      fcotwd00 < 0 ~ NA_real_,
      !is.na(fcotwd00) ~ {
        max_val <- max(fcotwd00[fcotwd00 >= 0], na.rm = TRUE)
        max_val - fcotwd00
      },
      TRUE ~ NA_real_
    ))
  }
  
  # Recode gcoutw00: reverse code, 5 and 7 -> NA
  if ("gcoutw00" %in% names(d)) {
    d <- d %>% mutate(ngcoutw00 = case_when(
      gcoutw00 %in% c(5, 7) ~ NA_real_,
      !is.na(gcoutw00) ~ {
        max_val <- max(gcoutw00[!(gcoutw00 %in% c(5, 7))], na.rm = TRUE)
        max_val - gcoutw00
      },
      TRUE ~ NA_real_
    ))
  }
  
  # Recode cmdibna0, cmditra0, cmdirea0: negatives and 6 -> NA, subtract 1 from others
  if ("cmdibna0" %in% names(d)) {
    d <- d %>% mutate(ncmdibna0 = case_when(
      cmdibna0 < 0 | cmdibna0 == 6 ~ NA_real_,
      !is.na(cmdibna0) ~ cmdibna0 - 1,
      TRUE ~ NA_real_
    ))
  }
  
  if ("cmditra0" %in% names(d)) {
    d <- d %>% mutate(ncmditra0 = case_when(
      cmditra0 < 0 | cmditra0 == 6 ~ NA_real_,
      !is.na(cmditra0) ~ cmditra0 - 1,
      TRUE ~ NA_real_
    ))
  }
  
  if ("cmdirea0" %in% names(d)) {
    d <- d %>% mutate(ncmdirea0 = case_when(
      cmdirea0 < 0 | cmdirea0 == 6 ~ NA_real_,
      !is.na(cmdirea0) ~ cmdirea0 - 1,
      TRUE ~ NA_real_
    ))
  }

  # --- 5. Create Composite Variables ---
  cat("Calculating composite variables...\n")
  
  # Parenting Age 11 composite (from nepdibn00, nepditr00, nepdire00)
  parenting_age11_vars <- c("nepdibn00", "nepditr00", "nepdire00")
  d$parenting_age11 <- row_mean(d, parenting_age11_vars)
  
  # Parenting Age 7 composite (from ndmdibna0, ndmditra0, ndmdirea0)
  parenting_age7_vars <- c("ndmdibna0", "ndmditra0", "ndmdirea0")
  d$parenting_age7 <- row_mean(d, parenting_age7_vars)
  
  # Parenting Age 5 composite (from ncmdibna0, ncmditra0, ncmdirea0)
  parenting_age5_vars <- c("ncmdibna0", "ncmditra0", "ncmdirea0")
  d$parenting_age5 <- row_mean(d, parenting_age5_vars)
  
  # Parental Monitoring Age 14 composite
  parental_monitoring_age14_vars <- c("nfpwhet00", "nfpwhot00", "nfpwhat00", 
                                       "nfcoutw00", "nfcotwi00", "nfcotwd00")
  d$parental_monitoring_age14 <- row_mean(d, parental_monitoring_age14_vars)

  # --- 6. Create Subindexes ---
  cat("Calculating subindexes...\n")
  
  # Define item lists for indexes
  monitoring_vars <- c("tv_rules_time", "tv_rules_hours", "no_tv_bedroom", "regular_bedtime",
                       "supervision_outdoor", "family_time_home", "evening_meal_parent_presence",
                       "internet_use_allowed")
  appropriate_vars <- c("tell_off")
  harsh_vars <- c("smack_rev", "shout_rev", "bribe_rev", "ignore_rev")
  warmth_vars <- c("parent_child_closeness", "enjoy_listen_do", "express_affection",
                   "child_weekend_fun", "child_disclosure_home")
  involvement_vars <- c("reading_together", "storytelling", "music_together", "arts_crafts",
                        "active_play", "indoor_games", "park_visits",
                        "partner_puts_to_bed", "partner_looks_after")
  
  # Calculate row means using the external helper
  d$monitoring_structure_index    <- row_mean(d, monitoring_vars)
  d$appropriate_sanctioning_index <- row_mean(d, appropriate_vars)
  d$harsh_inconsistent_index      <- row_mean(d, harsh_vars)
  d$warmth_attachment_index       <- row_mean(d, warmth_vars)
  d$involvement_stimulation_index <- row_mean(d, involvement_vars)

  # --- 7. Grand composite ---
  # Simple mean over all atomic recoded items
  all_new_vars <- setdiff(names(d), names(dat))
  atomic_vars <- all_new_vars[!str_detect(all_new_vars, "_index$")]
  
  if (length(atomic_vars) > 0) {
    cat("Calculating grand composite mean...\n")
    d$parenting_composite_mean <- row_mean(d, atomic_vars)
  }

  # --- 8. Final summary and return ---
  cat("Created", length(all_new_vars), "new parenting variables:\n")
  if (length(all_new_vars) > 0) {
    # Print only indexes for brevity
    cat(paste0("- ", all_new_vars[str_detect(all_new_vars, "_index$") | str_detect(all_new_vars, "composite")]), sep = "\n")
  }
  cat("Parenting variable recoding completed.\n")
  
  return(d)
}


# ============================================================================
# SCRIPT EXECUTION
# ============================================================================

# Check if merged_data exists
if (!exists("merged_data")) {
  stop("merged_data not found. Please load your data first.")
}

# Run the recoding function and append new variables to merged_data
# This single call replaces the two redundant calls from the original script.
merged_data <- recode_parenting(merged_data)

# ============================================================================
# MEASUREMENT ANALYSIS
# (All references to 'recoded_parenting_age7' are updated to 'merged_data')
# ============================================================================

cat("\n=== STARTING MEASUREMENT ANALYSIS ===\n")

# Revised subscale item sets
monitoring <- c("tv_rules_time", "tv_rules_hours", "regular_bedtime")
psych::alpha(merged_data[, monitoring], check.keys = TRUE)

appropriate <- c("tell_off")
# Note: timeout, take_away_treats, and reason are now coded separately as age-specific parenting composites
# psych::alpha(merged_data[, appropriate], check.keys = TRUE)

harsh <- c("smack_rev", "shout_rev", "bribe_rev", "ignore_rev")
psych::alpha(merged_data[, harsh], check.keys = TRUE)

warmth <- c("parent_child_closeness", "enjoy_listen_do", "express_affection")
psych::alpha(merged_data[, warmth], check.keys = TRUE)

involve <- c("reading_together", "storytelling", "music_together", "arts_crafts",
             "active_play", "indoor_games", "park_visits", "family_time_home")
psych::alpha(merged_data[, involve], check.keys = TRUE)

overall <- c(appropriate, warmth, involve)
psych::alpha(merged_data[, overall], check.keys = TRUE)

# Example table for monitoring variables
for (v in monitoring) {
  if (v %in% names(merged_data)) {
    cat("\n", v, "\n")
    tab <- table(merged_data[[v]], useNA = "ifany")
    print(tab)
    print(round(100 * prop.table(tab), 1))
  }
}

# ============================================================================
# CONFIRMATORY FACTOR ANALYSIS (CFA) FOR PARENTING CONSTRUCT
# (All references to 'recoded_parenting_age7' are updated to 'merged_data')
# ============================================================================

cat("\n=== STARTING CFA ANALYSIS ===\n")
cat("Checking data availability for CFA variables...\n")
cfa_vars <- c("tv_rules_time", "tv_rules_hours", "no_tv_bedroom", "regular_bedtime",
              "supervision_outdoor", "family_time_home", "evening_meal_parent_presence",
              "internet_use_allowed", "tell_off",
              "smack_rev", "shout_rev", "bribe_rev", "ignore_rev",
              "parent_child_closeness", "enjoy_listen_do", "express_affection",
              "child_weekend_fun", "child_disclosure_home", "reading_together",
              "storytelling", "music_together", "arts_crafts", "active_play")

# Check which variables exist
available_vars <- intersect(cfa_vars, names(merged_data))
cat("Available CFA variables:", length(available_vars), "out of", length(cfa_vars), "\n")

if (length(available_vars) == 0) {
  stop("No CFA variables are present in the data. Stopping analysis.")
}

# Check missing data patterns
missing_summary <- merged_data %>%
  select(all_of(available_vars)) %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(total_n = nrow(merged_data),
         missing_pct = round(100 * missing_count / total_n, 1)) %>%
  arrange(desc(missing_pct))

print(missing_summary, n = 30)

# Remove variables with >50% missing data
usable_vars <- missing_summary %>%
  filter(missing_pct <= 50) %>%
  pull(variable)

cat("\nUsable variables for CFA (<=50% missing):", length(usable_vars), "\n")
if (length(usable_vars) > 0) {
  cat("Variables:", paste(usable_vars, collapse = ", "), "\n")
}

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
                               data = merged_data,
                               estimator = "ML", 
                               missing = "fiml")  # Full Information ML
  
  cat("\n--- Continuous CFA Results ---\n")
  summary(fit_continuous, fit.measures = TRUE, standardized = TRUE)
  
  # ============================================================================
  # APPROACH 2: CFA with Original Ordinal Variables (WLSMV estimator)
  # ============================================================================
  cat("\n=== APPROACH 2: Ordinal CFA (WLSMV estimator) ===\n")
  
  # Map recoded variables back to original variables
  var_mapping <- c(
    "tv_rules_time" = "dmtvrla0", "tv_rules_hours" = "dmtvrha0", 
    "no_tv_bedroom" = "dmtvrma0", "regular_bedtime" = "dmberea0",
    "supervision_outdoor" = "dmploua0", "family_time_home" = "dmfrtv00",
    "evening_meal_parent_presence" = "dmevwoaa", "internet_use_allowed" = "dminlna0",
    "tell_off" = "dmditea0",
    "smack_rev" = "dmdisma0", "shout_rev" = "dmdisha0", "bribe_rev" = "dmdibra0",
    "ignore_rev" = "dmdiiga0", "parent_child_closeness" = "dmschca0",
    "enjoy_listen_do" = "dmenlia0", "express_affection" = "dmexafa0",
    "child_weekend_fun" = "dcsc0019", "child_disclosure_home" = "dcsc0020"
  )
  
  # Get original variables that correspond to usable recoded variables
  usable_original <- var_mapping[usable_vars]
  usable_original <- usable_original[!is.na(usable_original)]
  available_original <- intersect(usable_original, names(merged_data))
  
  if (length(available_original) >= 3) {
    # Create model with original variable names
    model_ordinal <- paste("parenting =~", paste(available_original, collapse = " + "))
    cat("Ordinal model specification:\n", model_ordinal, "\n")
    
    # Prepare data with ordered factors
    ordinal_data <- merged_data %>%
      mutate(across(all_of(available_original), ~{
        # Convert to ordered factor, removing missing codes
        .x[.x %in% missing_codes] <- NA
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

# ============================================================================
# EXPORT DATA TO CSV FORMAT
# ============================================================================

cat("\n=== EXPORTING DATA TO CSV FORMAT ===\n")

# Define output path
output_path <- "/home/siyang/dissertation_folder/dissertation/data/merged_data_with_parenting.csv"

# Export to CSV format
write_csv(merged_data, output_path, na = "")

cat("Data successfully exported to:", output_path, "\n")
cat("Total observations:", nrow(merged_data), "\n")
cat("Total variables:", ncol(merged_data), "\n")
