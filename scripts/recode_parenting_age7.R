library(pacman)
p_load(tidyverse)
p_load(stringr)
p_load(lavaan)
p_load(dplyr)
p_load(haven)
p_load(psych)

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
  if ("dmdirea0" %in% names(d)) d$reason <- recode_app(d$dmdirea0)
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
    "dmgamea0" = "indoor_games",
    "dmwalka0" = "park_visits",
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
merged_data <- recode_parenting(merged_data)



###### Measurement  #######

# Revised subscale item sets
monitoring <- c("tv_rules_time","tv_rules_hours","regular_bedtime")
psych::alpha(recoded_parenting_age7[, monitoring], check.keys=TRUE)

appropriate <- c("tell_off","take_away_treats","timeout", "reason")
psych::alpha(recoded_parenting_age7[, appropriate], check.keys=TRUE)

harsh <- c("smack_rev","shout_rev","bribe_rev","ignore_rev")
psych::alpha(recoded_parenting_age7[, harsh], check.keys=TRUE)

warmth <- c("parent_child_closeness","enjoy_listen_do","express_affection")
psych::alpha(recoded_parenting_age7[, warmth], check.keys=TRUE)

involve <- c("reading_together","storytelling","music_together","arts_crafts",
                       "active_play","indoor_games", "park_visits", "family_time_home")
psych::alpha(recoded_parenting_age7[, involve], check.keys=TRUE)

overall <- c(appropriate, warmth, involve)
psych::alpha(recoded_parenting_age7[, overall], check.keys=TRUE)


for (v in monitoring) {
  cat("\n", v, "\n")
  tab <- table(recoded_parenting_age7[[v]], useNA = "ifany")
  print(tab)
  print(round(100 * prop.table(tab), 1))
}



# PCA and Factor Analysis for 'appropriate' items
# PCA
pca_app <- psych::principal(X_app_complete, nfactors = 1, rotate = "none", scores = FALSE)
print(pca_app)

# Factor Analysis
fa_app <- psych::fa(X_app_complete, nfactors = 1, rotate = "oblimin", fm = "minres")
print(fa_app)


