# =============================================================================
# Unified recoding and Mplus export
# - Consolidates recoding from:
#   - scripts/02_recode_self_control.R
#   - scripts/05_recode_parenting_age7.R (parenting recode function only)
#   - scripts/07_recode_control_vars.R (control variables function only)
# - Renames variables to <=8-char Mplus-friendly names
# - Exports space-delimited Mplus.dat with NA as -9999 (no header)
# - Writes updated name map CSV for reproducibility
# =============================================================================

library(pacman)
p_load(tidyverse, haven, readr, stringr)

# -----------------------------------------------------------------------------
# Preconditions
# -----------------------------------------------------------------------------
if (!exists("merged_data")) {
  stop("merged_data not found. Please load your combined dataset into `merged_data`.")
}

# Work on a tibble copy
df <- as_tibble(merged_data)

# -----------------------------------------------------------------------------
# 1) Self-control recode (adapted from scripts/02_recode_self_control.R)
# -----------------------------------------------------------------------------

positive_keys <- c(
  "think_act","good_friend","liked_children","considerate","sharing",
  "helpful","kind_younger","volunteer_help","task_completion","obedient"
)
negative_keys <- c(
  "solitary","distracted","better_adults","temper","worries",
  "unhappy","nervous","fears","restless","fidgeting","lying"
)

source_vars_positive <- list(
  think_act      = c("3"="bmsdsta0","5"="cmsdsta0","7"="dmsdsta0","11"="epsdst00","14"="fpsdst00","17"="gpsdst00"),
  good_friend    = c("3"="bmsdgfa0","5"="cmsdgfa0","7"="dmsdgfa0","11"="epsdgf00","14"="fpsdgf00","17"="gpsdgf00"),
  liked_children = c("3"="bmsdlca0","5"="cmsdlca0","7"="dmsdlca0","11"="epsdlc00","14"="fpsdlc00","17"="gpsdlc00"),
  considerate    = c("3"="bmsdpfa0","5"="cmsdpfa0","7"="dmsdpfa0","11"="epsdpf00","14"="fpsdpf00","17"="gpsdpf00"),
  sharing        = c("3"="bmsdsra0","5"="cmsdsra0","7"="dmsdsra0","11"="epsdsr00","14"="fpsdsr00","17"="gpsdsr00"),
  helpful        = c("3"="bmsdhua0","5"="cmsdhua0","7"="dmsdhua0","11"="epsdhu00","14"="fpsdhu00","17"="gpsdhu00"),
  kind_younger   = c("3"="bmsdkya0","5"="cmsdkya0","7"="dmsdkya0","11"="epsdky00","14"="fpsdky00","17"="gpsdky00"),
  volunteer_help = c("3"="bmsdvha0","5"="cmsdvha0","7"="dmsdvha0","11"="epsdvh00","14"="fpsdvh00","17"="gpsdvh00"),
  task_completion= c("3"="bmsdtea0","5"="cmsdtea0","7"="dmsdtea0","11"="epsdte00","14"="fpsdte00","17"="gpsdte00"),
  obedient       = c("3"="bmsdora0","5"="cmsdora0","7"="dmsdora0","11"="epsdor00","14"="fpsdor00","17"="gpsdor00")
)

source_vars_negative <- list(
  solitary      = c("3"="bmsdspa0","5"="cmsdspa0","7"="dmsdspa0","11"="epsdsp00","14"="fpsdsp00","17"="gpsdsp00"),
  distracted    = c("3"="bmsddca0","5"="cmsddca0","7"="dmsddca0","11"="epsddc00","14"="fpsddc00","17"="gpsddc00"),
  better_adults = c("3"="bmsdgba0","5"="cmsdgba0","7"="dmsdgba0","11"="epsdgb00","14"="fpsdgb00","17"="gpsdgb00"),
  temper        = c("3"="bmsdtta0","5"="cmsdtta0","7"="dmsdtta0","11"="epsdtt00","14"="fpsdtt00","17"="gpsdtt00"),
  worries       = c("3"="bmsdmwa0","5"="cmsdmwa0","7"="dmsdmwa0","11"="epsdmw00","14"="fpsdmw00","17"="gpsdmw00"),
  unhappy       = c("3"="bmsduda0","5"="cmsduda0","7"="dmsduda0","11"="epsdud00","14"="fpsdud00","17"="gpsdud00"),
  nervous       = c("3"="bmsdnca0","5"="cmsdnca0","7"="dmsdnca0","11"="epsdnc00","14"="fpsdnc00","17"="gpsdnc00"),
  fears         = c("3"="bmsdfea0","5"="cmsdfea0","7"="dmsdfea0","11"="epsdfe00","14"="fpsdfe00","17"="gpsdfe00"),
  restless      = c("3"="bmsdpba0","5"="cmsdpba0","7"="dmsdpba0","11"="epsdpb00","14"="fpsdpb00","17"="gpsdpb00"),
  fidgeting     = c("3"="bmsdfsa0","5"="cmsdfsa0","7"="dmsdfsa0","11"="epsdfs00","14"="fpsdfs00","17"="gpsdfs00"),
  lying         = c("3"="bmsdoaa0","5"="cmsdoaa0","7"="dmsdoaa0","11"="epsdoa00","14"="fpsdoa00","17"="gpsdoa00")
)

ages <- c(3, 5, 7, 11, 14, 17)

for (age in ages) {
  map_pos <- purrr::imap_dfr(source_vars_positive, ~tibble(
    key = .y, age = as.integer(names(.x)), var = unname(.x), polarity = "positive"
  )) %>%
    filter(age == !!age, var %in% names(df))

  map_neg <- purrr::imap_dfr(source_vars_negative, ~tibble(
    key = .y, age = as.integer(names(.x)), var = unname(.x), polarity = "negative"
  )) %>%
    filter(age == !!age, var %in% names(df))

  mapping <- bind_rows(map_pos, map_neg) %>%
    mutate(target = paste0("sc", age, "_", key))

  if (nrow(mapping) == 0) next

  pos_vars <- mapping %>% filter(polarity == "positive") %>% pull(var)
  if (length(pos_vars)) {
    df <- df %>%
      mutate(across(
        all_of(pos_vars),
        function(x) {
          x_num <- tryCatch(
            {
              if (requireNamespace("haven", quietly = TRUE)) {
                suppressWarnings(as.numeric(haven::zap_labels(x)))
              } else {
                suppressWarnings(as.numeric(if (is.factor(x) || is.character(x)) as.character(x) else x))
              }
            },
            error = function(e) suppressWarnings(as.numeric(if (is.factor(x) || is.character(x)) as.character(x) else x))
          )
          x_num[!(x_num %in% c(1,2,3))] <- NA_real_
          out <- dplyr::case_when(
            is.na(x_num) ~ NA_real_,
            x_num == 1   ~ 0,
            x_num == 2   ~ 1,
            x_num == 3   ~ 2
          )
          as.integer(out)
        },
        .names = "tmp__{col}"
      ))
  }

  neg_vars <- mapping %>% filter(polarity == "negative") %>% pull(var)
  if (length(neg_vars)) {
    df <- df %>%
      mutate(across(
        all_of(neg_vars),
        function(x) {
          x_num <- tryCatch(
            {
              if (requireNamespace("haven", quietly = TRUE)) {
                suppressWarnings(as.numeric(haven::zap_labels(x)))
              } else {
                suppressWarnings(as.numeric(if (is.factor(x) || is.character(x)) as.character(x) else x))
              }
            },
            error = function(e) suppressWarnings(as.numeric(if (is.factor(x) || is.character(x)) as.character(x) else x))
          )
          x_num[!(x_num %in% c(1,2,3))] <- NA_real_
          out <- dplyr::case_when(
            is.na(x_num) ~ NA_real_,
            x_num == 3   ~ 0,
            x_num == 1   ~ 2,
            x_num == 2   ~ 1
          )
          as.integer(out)
        },
        .names = "tmp__{col}"
      ))
  }

  tmp_names <- paste0("tmp__", mapping$var)
  tmp_in_df <- tmp_names[tmp_names %in% names(df)]
  map_keep  <- mapping[match(tmp_in_df, tmp_names), , drop = FALSE]
  if (length(tmp_in_df)) {
    df <- df %>% rename(!!!setNames(nm = map_keep$target, object = tmp_in_df))
  }
}

merged_data <- df

# -----------------------------------------------------------------------------
# 2) Parenting recode (function adapted from scripts/05_recode_parenting_age7.R)
# -----------------------------------------------------------------------------

missing_codes <- c(-9, -8, -1)

recode_app <- function(x) {
  x_num <- as.numeric(x)
  x_num <- ifelse(x_num == 6, NA_real_, x_num)
  (x_num - 1) / 4
}

recode_rev <- function(x) {
  x_num <- as.numeric(x)
  x_num <- ifelse(x_num == 6, NA_real_, x_num)
  (5 - x_num) / 4
}

map_pa <- function(x) {
  (6 - as.numeric(x)) / 5
}

row_mean <- function(data, cols, min_frac = 0.5) {
  valid_cols <- intersect(cols, names(data))
  if (length(valid_cols) == 0) return(rep(NA_real_, nrow(data)))
  X <- data[, valid_cols, drop = FALSE]
  m <- rowMeans(X, na.rm = TRUE)
  valid_count <- rowSums(!is.na(X))
  min_valid <- ceiling(min_frac * length(valid_cols))
  m[valid_count < min_valid] <- NA_real_
  m[is.nan(m)] <- NA_real_
  m
}

recode_parenting <- function(dat) {
  if (!is.data.frame(dat)) stop("Input must be a data frame")
  d <- dat

  source_vars <- c(
    "dmtvrla0", "dmtvrha0", "dmtvrma0", "dmberea0", "dmplog00", "dmploua0",
    "dminlna0", "dmintha0", "dmfrtv00", "dmevwoaa",
    "dmditea0", "dmditra0", "dmdibna0", "dmdirea0", "dmdisma0",
    "dmdisha0", "dmdibra0", "dmdiiga0",
    "dmschca0", "dmenlia0", "dmexafa0", "dcsc0019", "dcsc0020",
    "dmreofa0", "dmsitsa0", "dmplmua0", "dmpamaa0", "dmactia0",
    "dmgamea0", "dmwalka0", "BEDR", "LOOK",
    "epdibn00", "epditr00", "epdire00",
    "fpwhet00", "fpwhot00", "fpwhat00",
    "fcoutw00", "fcotwi00", "fcotwd00",
    "gcoutw00",
    "cmdibna0", "cmditra0", "cmdirea0"
  )

  evwo_cols <- names(d)[str_detect(names(d), "^dmevwoaa_")]
  all_source_vars <- c(source_vars, evwo_cols)
  existing_source_vars <- intersect(all_source_vars, names(d))
  if (length(existing_source_vars) == 0) return(dat)

  d <- d %>%
    mutate(across(all_of(existing_source_vars), ~{
      vals <- .x
      if (inherits(vals, "haven_labelled")) vals <- haven::zap_labels(vals)
      ifelse(vals %in% missing_codes, NA_real_, as.numeric(vals))
    }))

  if ("dmtvrla0" %in% names(d)) d <- d %>% mutate(tv_rules_time  = case_when(dmtvrla0 == 1 ~ 1, dmtvrla0 == 2 ~ 0, TRUE ~ NA_real_))
  if ("dmtvrha0" %in% names(d)) d <- d %>% mutate(tv_rules_hours = case_when(dmtvrha0 == 1 ~ 1, dmtvrha0 == 2 ~ 0, TRUE ~ NA_real_))
  if ("dmtvrma0" %in% names(d)) d <- d %>% mutate(no_tv_bedroom  = case_when(dmtvrma0 == 1 ~ 0, dmtvrma0 == 2 ~ 1, TRUE ~ NA_real_))
  if ("dmberea0" %in% names(d)) d <- d %>% mutate(regular_bedtime = (dmberea0 - 1) / 3)
  if ("dmfrtv00" %in% names(d)) d <- d %>% mutate(family_time_home = (6 - dmfrtv00) / 5)

  if (all(c("dmplog00","dmploua0") %in% names(d))) {
    d <- d %>%
      mutate(outdoor_opportunity = case_when(dmplog00 == 1 ~ 1, dmplog00 == 2 ~ 0, TRUE ~ NA_real_),
             supervision_outdoor = case_when(dmplog00 == 1 & dmploua0 == 2 ~ 1,
                                             dmplog00 == 1 & dmploua0 == 1 ~ 0,
                                             TRUE ~ NA_real_))
  }

  if (all(c("dminlna0","dmintha0") %in% names(d))) {
    d <- d %>%
      mutate(internet_use_allowed = case_when(dminlna0 == 1 & dmintha0 == 1 ~ 0,
                                              dminlna0 == 1 & dmintha0 == 2 ~ 1,
                                              TRUE ~ NA_real_),
             internet_control_strict = case_when(dmintha0 == 2 | dminlna0 == 2 ~ 1,
                                                 dminlna0 == 1 & dmintha0 == 1 ~ 0,
                                                 TRUE ~ NA_real_))
  }

  if ("dmevwoaa" %in% names(d)) {
    d <- d %>% mutate(
      evening_meal_parent_presence_single = case_when(
        dmevwoaa == 1 ~ 0.00,
        dmevwoaa == 2 ~ 1.00,
        dmevwoaa %in% c(4,5) ~ 0.66,
        dmevwoaa %in% c(3,6) ~ 0.33,
        dmevwoaa %in% c(7,8) ~ NA_real_,
        dmevwoaa == 95 ~ 0.50,
        TRUE ~ NA_real_
      ))
  }

  if (length(evwo_cols) > 0) {
    get_w <- function(code, weight) {
      colname <- paste0("dmevwoaa_", code)
      if (colname %in% names(d)) {
        return(ifelse(is.na(d[[colname]]), 0, d[[colname]]) * weight)
      } else {
        return(rep(0, nrow(d)))
      }
    }
    weighted_vectors <- list(
      get_w("2", 1.00),
      get_w("4", 0.66), get_w("5", 0.66),
      get_w("3", 0.33), get_w("6", 0.33),
      get_w("1", 0.00),
      get_w("95", 0.50)
    )
    max_w <- do.call(pmax, c(weighted_vectors, na.rm = TRUE))
    no_meal_7 <- get_w("7", 1)
    no_meal_8 <- get_w("8", 1)
    only_no_meal <- (no_meal_7 == 1 | no_meal_8 == 1) & (max_w == 0)
    max_w[only_no_meal] <- NA_real_
    d$evening_meal_parent_presence_multi <- max_w
  }

  if (exists("evening_meal_parent_presence_multi", where = d)) {
    d <- d %>%
      mutate(
        evening_meal_parent_presence = coalesce(
          evening_meal_parent_presence_multi,
          evening_meal_parent_presence_single
        )
      ) %>%
      select(-evening_meal_parent_presence_multi)
    if ("evening_meal_parent_presence_single" %in% names(d)) {
      d <- d %>% select(-evening_meal_parent_presence_single)
    }
  } else if ("evening_meal_parent_presence_single" %in% names(d)) {
    d <- d %>% rename(evening_meal_parent_presence = evening_meal_parent_presence_single)
  }

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

  if ("dmschca0" %in% names(d)) d <- d %>% mutate(parent_child_closeness = ifelse(dmschca0 == 5, NA, (dmschca0 - 1) / 3))
  if ("dmenlia0" %in% names(d)) d <- d %>% mutate(enjoy_listen_do = (dmenlia0 - 1) / 4)
  if ("dmexafa0" %in% names(d)) d <- d %>% mutate(express_affection = (dmexafa0 - 1) / 4)
  if ("dcsc0019" %in% names(d)) d <- d %>% mutate(child_weekend_fun = (3 - dcsc0019) / 2)
  if ("dcsc0020" %in% names(d)) d <- d %>% mutate(child_disclosure_home = dcsc0020 / 3)

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

  if ("fpwhet00" %in% names(d)) {
    d <- d %>% mutate(nfpwhet00 = case_when(
      fpwhet00 < 0 ~ NA_real_,
      !is.na(fpwhet00) ~ { max_val <- max(fpwhet00[fpwhet00 >= 0], na.rm = TRUE); max_val - fpwhet00 },
      TRUE ~ NA_real_
    ))
  }
  if ("fpwhot00" %in% names(d)) {
    d <- d %>% mutate(nfpwhot00 = case_when(
      fpwhot00 < 0 ~ NA_real_,
      !is.na(fpwhot00) ~ { max_val <- max(fpwhot00[fpwhot00 >= 0], na.rm = TRUE); max_val - fpwhot00 },
      TRUE ~ NA_real_
    ))
  }
  if ("fpwhat00" %in% names(d)) {
    d <- d %>% mutate(nfpwhat00 = case_when(
      fpwhat00 < 0 ~ NA_real_,
      !is.na(fpwhat00) ~ { max_val <- max(fpwhat00[fpwhat00 >= 0], na.rm = TRUE); max_val - fpwhat00 },
      TRUE ~ NA_real_
    ))
  }

  if ("fcoutw00" %in% names(d)) {
    d <- d %>% mutate(nfcoutw00 = case_when(
      fcoutw00 < 0 ~ NA_real_,
      !is.na(fcoutw00) ~ { max_val <- max(fcoutw00[fcoutw00 >= 0], na.rm = TRUE); max_val - fcoutw00 },
      TRUE ~ NA_real_
    ))
  }
  if ("fcotwi00" %in% names(d)) {
    d <- d %>% mutate(nfcotwi00 = case_when(
      fcotwi00 < 0 ~ NA_real_,
      !is.na(fcotwi00) ~ { max_val <- max(fcotwi00[fcotwi00 >= 0], na.rm = TRUE); max_val - fcotwi00 },
      TRUE ~ NA_real_
    ))
  }
  if ("fcotwd00" %in% names(d)) {
    d <- d %>% mutate(nfcotwd00 = case_when(
      fcotwd00 < 0 ~ NA_real_,
      !is.na(fcotwd00) ~ { max_val <- max(fcotwd00[fcotwd00 >= 0], na.rm = TRUE); max_val - fcotwd00 },
      TRUE ~ NA_real_
    ))
  }

  if ("gcoutw00" %in% names(d)) {
    d <- d %>% mutate(ngcoutw00 = case_when(
      gcoutw00 %in% c(5, 7) ~ NA_real_,
      !is.na(gcoutw00) ~ { max_val <- max(gcoutw00[!(gcoutw00 %in% c(5, 7))], na.rm = TRUE); max_val - gcoutw00 },
      TRUE ~ NA_real_
    ))
  }

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

  parenting_age11_vars <- c("nepdibn00", "nepditr00", "nepdire00")
  d$parenting_age11 <- row_mean(d, parenting_age11_vars)
  parenting_age7_vars <- c("ndmdibna0", "ndmditra0", "ndmdirea0")
  d$parenting_age7 <- row_mean(d, parenting_age7_vars)
  parenting_age5_vars <- c("ncmdibna0", "ncmditra0", "ncmdirea0")
  d$parenting_age5 <- row_mean(d, parenting_age5_vars)

  parental_monitoring_age14_vars <- c("nfpwhet00", "nfpwhot00", "nfpwhat00",
                                       "nfcoutw00", "nfcotwi00", "nfcotwd00")
  d$parental_monitoring_age14 <- row_mean(d, parental_monitoring_age14_vars)

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

  d$monitoring_structure_index    <- row_mean(d, monitoring_vars)
  d$appropriate_sanctioning_index <- row_mean(d, appropriate_vars)
  d$harsh_inconsistent_index      <- row_mean(d, harsh_vars)
  d$warmth_attachment_index       <- row_mean(d, warmth_vars)
  d$involvement_stimulation_index <- row_mean(d, involvement_vars)

  all_new_vars <- setdiff(names(d), names(dat))
  atomic_vars <- all_new_vars[!str_detect(all_new_vars, "_index$")]
  if (length(atomic_vars) > 0) {
    d$parenting_composite_mean <- row_mean(d, atomic_vars)
  }
  d
}

merged_data <- recode_parenting(merged_data)

# -----------------------------------------------------------------------------
# 3) Control variables recode (function adapted from scripts/07_recode_control_vars.R)
# -----------------------------------------------------------------------------

recode_control_variables <- function(data) {
  data_recode <- data
  data_recode$parents_education <- dplyr::case_when(
    data_recode$amacqu00 %in% c(-1, -8, -9, 95, 96) ~ NA_real_,
    TRUE ~ data_recode$amacqu00 - 1
  )
  data_recode$parents_education_binary <- dplyr::case_when(
    data_recode$parents_education %in% c(0, 1) ~ 1,
    data_recode$parents_education %in% c(2, 3, 4, 5) ~ 0,
    TRUE ~ NA_real_
  )
  data_recode$sex <- dplyr::case_when(
    data_recode$ahcsexa0 == 1 ~ 0,
    data_recode$ahcsexa0 == 2 ~ 1,
    TRUE ~ NA_real_
  )
  data_recode$race <- dplyr::case_when(
    data_recode$adceeaa0 %in% c(-9, -8, -1) ~ NA_real_,
    data_recode$adceeaa0 %in% c(1, 2, 3) ~ 1,
    data_recode$adceeaa0 %in% c(8, 9, 10, 11, 15) ~ 2,
    data_recode$adceeaa0 %in% c(12, 13, 14) ~ 3,
    data_recode$adceeaa0 %in% c(4, 5, 6, 7, 95) ~ 4,
    TRUE ~ NA_real_
  )
  data_recode$race_binary <- dplyr::case_when(
    data_recode$race == 1 ~ 0,
    data_recode$race %in% c(2, 3, 4) ~ 1,
    TRUE ~ NA_real_
  )
  data_recode$marital_status <- dplyr::case_when(
    data_recode$amfcin00 %in% c(9, 8, -1) ~ NA_real_,
    data_recode$amfcin00 %in% c(1, 4, 5, 6) ~ 0,
    data_recode$amfcin00 %in% c(2, 3) ~ 1,
    TRUE ~ NA_real_
  )
  data_recode$parents_income_couple <- dplyr::case_when(
    data_recode$amnico00 %in% c(-1, 96, 97) ~ NA_real_,
    TRUE ~ data_recode$amnico00
  )
  data_recode$parents_income_lone_parent <- dplyr::case_when(
    data_recode$amnilp00 %in% c(-1, 96, 97) ~ NA_real_,
    TRUE ~ data_recode$amnilp00
  )
  data_recode$low_birthweight <- dplyr::case_when(
    data_recode$adbwgta0 < 2.5 ~ 1,
    data_recode$adbwgta0 >= 2.5 ~ 0,
    TRUE ~ NA_real_
  )
  clean_temp <- function(x) {
    dplyr::case_when(
      x < 0 ~ NA_real_,
      x == 6 ~ NA_real_,
      TRUE ~ as.numeric(x)
    )
  }
  data_recode$namhapna0 <- clean_temp(data_recode$amhapna0)
  data_recode$namunfaa0 <- clean_temp(data_recode$amunfaa0)
  data_recode$nambrusa0 <- clean_temp(data_recode$ambrusa0)
  data_recode$namfeeda0 <- clean_temp(data_recode$amfeeda0)
  data_recode$naminjua0 <- clean_temp(data_recode$aminjua0)
  data_recode$nambatha0 <- clean_temp(data_recode$ambatha0)
  data_recode$namwarya0 <- clean_temp(data_recode$amwarya0)
  data_recode$nambshya0 <- clean_temp(data_recode$ambshya0)
  data_recode$namfreta0 <- clean_temp(data_recode$amfreta0)
  data_recode$namsleea0 <- clean_temp(data_recode$amsleea0)
  data_recode$nammilka0 <- clean_temp(data_recode$ammilka0)
  data_recode$namsltia0 <- clean_temp(data_recode$amsltia0)
  data_recode$namnapsa0 <- clean_temp(data_recode$amnapsa0)
  data_recode$namsofoa0 <- clean_temp(data_recode$amsofoa0)
  data_recode$nambatha0 <- ifelse(is.na(data_recode$nambatha0), NA_real_, 6 - data_recode$nambatha0)
  data_recode$namwarya0 <- ifelse(is.na(data_recode$namwarya0), NA_real_, 6 - data_recode$namwarya0)
  data_recode$nambshya0 <- ifelse(is.na(data_recode$nambshya0), NA_real_, 6 - data_recode$nambshya0)
  data_recode$namfreta0 <- ifelse(is.na(data_recode$namfreta0), NA_real_, 6 - data_recode$namfreta0)
  data_recode$namsleea0 <- ifelse(is.na(data_recode$namsleea0), NA_real_, 6 - data_recode$namsleea0)
  temp_vars_n <- c(
    "namhapna0", "namunfaa0", "nambrusa0", "namfeeda0", "naminjua0",
    "nambatha0", "namwarya0", "nambshya0", "namfreta0", "namsleea0",
    "nammilka0", "namsltia0", "namnapsa0", "namsofoa0"
  )
  data_recode$infant_temperament_raw <- rowSums(
    data_recode[, temp_vars_n],
    na.rm = FALSE
  )
  data_recode$infant_temperament <- as.numeric(scale(data_recode$infant_temperament_raw))
  data_recode$drinking_freq_per_week <- dplyr::case_when(
    data_recode$amdrof00 == 1 ~ 7,
    data_recode$amdrof00 == 2 ~ 5,
    data_recode$amdrof00 == 3 ~ 3.5,
    data_recode$amdrof00 == 4 ~ 1.5,
    data_recode$amdrof00 == 5 ~ 0.375,
    data_recode$amdrof00 == 6 ~ 0.25,
    data_recode$amdrof00 == 7 ~ 0,
    TRUE ~ NA_real_
  )
  data_recode$units_per_day_clean <- dplyr::case_when(
    data_recode$ampuda00 >= 0 & data_recode$ampuda00 <= 22 ~ data_recode$ampuda00,
    TRUE ~ NA_real_
  )
  data_recode$units_per_week <- ifelse(
    !is.na(data_recode$drinking_freq_per_week) & data_recode$drinking_freq_per_week == 0,
    0,
    data_recode$drinking_freq_per_week * data_recode$units_per_day_clean
  )
  data_recode$heavy_fetal_alcohol_exposure <- dplyr::case_when(
    data_recode$drinking_freq_per_week == 0 ~ 0,
    !is.na(data_recode$units_per_day_clean) & data_recode$units_per_day_clean >= 5 ~ 1,
    !is.na(data_recode$units_per_week) & data_recode$units_per_week >= 7 ~ 1,
    !is.na(data_recode$drinking_freq_per_week) & !is.na(data_recode$units_per_day_clean) ~ 0,
    TRUE ~ NA_real_
  )
  data_recode$cognitive_ability_raw <- dplyr::case_when(
    data_recode$bdsrcs00 < 0 ~ NA_real_,
    TRUE ~ data_recode$bdsrcs00
  )
  data_recode$cognitive_ability <- as.numeric(scale(data_recode$cognitive_ability_raw))
  data_recode
}

merged_data <- recode_control_variables(merged_data)

# -----------------------------------------------------------------------------
# 4) Build Mplus-friendly name map and export
# -----------------------------------------------------------------------------

# Desired abbreviations for self-control keys (<=4 chars so sc{age}{abbr} <= 8)
self_control_abbr <- c(
  think_act = "thac",
  good_friend = "gofr",
  liked_children = "like",
  considerate = "cons",
  sharing = "shar",
  helpful = "help",
  kind_younger = "kind",
  volunteer_help = "volu",
  task_completion = "tcom",
  obedient = "obey",
  solitary = "soli",
  distracted = "dist",
  better_adults = "bett",
  temper = "temp",
  worries = "worr",
  unhappy = "unha",
  nervous = "nerv",
  fears = "fear",
  restless = "rest",
  fidgeting = "fidg",
  lying = "lyin"
)

# Collect all candidate variables to export
id_vars <- intersect(c("mcsid"), names(merged_data))

sc_old <- unlist(lapply(ages, function(age) paste0("sc", age, "_", names(self_control_abbr))))
sc_old <- intersect(sc_old, names(merged_data))

# Map self-control to requested compact names
sc_map <- tibble(
  old_name = sc_old
) %>%
  mutate(
    key = sub("^sc(3|5|7|11|14|17)_", "", old_name),
    age = sub("^sc(3|5|7|11|14|17)_.*$", "\\1", old_name),
    mplus_name = paste0("sc", age, self_control_abbr[key])
  ) %>%
  select(old_name, mplus_name)

# Other numeric variables generated by parenting/control recodes
numeric_cols <- names(merged_data)[sapply(merged_data, is.numeric)]
other_old <- setdiff(intersect(numeric_cols, names(merged_data)), c(sc_old))

# Try to reuse existing map where available
existing_map_path <- "/home/siyang/dissertation_folder/dissertation/data/mplus/Mplus_name_map.csv"
existing_map <- NULL
if (file.exists(existing_map_path)) {
  suppressWarnings({
    existing_map <- tryCatch(readr::read_csv(existing_map_path, show_col_types = FALSE), error = function(e) NULL)
  })
}

reused_map <- tibble(old_name = character(), mplus_name = character())
if (!is.null(existing_map) && all(c("old_name","mplus_name") %in% names(existing_map))) {
  reused_map <- existing_map %>% filter(old_name %in% other_old) %>% select(old_name, mplus_name)
}

to_mplus <- function(x) {
  x2 <- gsub("_", "", x)
  substr(x2, 1, 8)
}

generated_map <- tibble(old_name = setdiff(other_old, reused_map$old_name)) %>%
  mutate(mplus_name = to_mplus(old_name))

full_map <- bind_rows(sc_map, reused_map, generated_map)

# Always include ID mapping so it appears in the name map
if (length(id_vars) > 0) {
  id_map <- tibble(old_name = id_vars, mplus_name = id_vars)
  full_map <- bind_rows(id_map, full_map)
}

# Ensure <=8 chars and unique names globally
full_map$mplus_name <- substr(full_map$mplus_name, 1, 8)
if (any(duplicated(full_map$mplus_name))) {
  # Deduplicate by appending numeric suffix within 8 chars
  make_unique_8 <- function(vec) {
    out <- vec
    seen <- list()
    for (i in seq_along(out)) {
      base <- out[i]
      if (is.null(seen[[base]])) {
        seen[[base]] <- 0
      } else {
        seen[[base]] <- seen[[base]] + 1
      }
      n <- seen[[base]]
      if (n > 0) {
        suf <- as.character(n)
        out[i] <- substr(base, 1, max(0, 8 - nchar(suf)))
        out[i] <- paste0(out[i], suf)
      }
    }
    out
  }
  full_map$mplus_name <- make_unique_8(full_map$mplus_name)
}

# Keep only variables that actually exist in merged_data
full_map <- full_map %>% filter(old_name %in% names(merged_data))

# Apply renaming to a copy for export
export_df <- merged_data
# Use base R to safely rename existing columns (old -> new)
map_keep <- full_map %>% filter(old_name %in% names(export_df))
if (nrow(map_keep)) {
  idx <- match(map_keep$old_name, names(export_df))
  names(export_df)[idx] <- map_keep$mplus_name
}

# Determine final export order: id first (if present), then sorted self-control by age, then others
sc_new <- full_map %>% filter(old_name %in% sc_old) %>% select(mplus_name) %>% pull()
other_new <- setdiff(full_map$mplus_name, sc_new)
export_vars <- c(id_vars, sc_new, sort(other_new))
export_vars <- unique(export_vars)
export_vars <- intersect(export_vars, names(export_df))

# Ensure ID is numeric for Mplus: strip non-digits from mcsid
if ("mcsid" %in% names(export_df)) {
  export_df$mcsid <- suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(export_df$mcsid))))
}

# Ensure all exported columns are numeric for Mplus
non_numeric <- export_vars[!sapply(export_df[export_vars], is.numeric)]
if (length(non_numeric) > 0) {
  warning("Dropping non-numeric columns from Mplus export: ", paste(non_numeric, collapse = ", "))
  export_vars <- setdiff(export_vars, non_numeric)
}

# Paths
out_dir <- "/home/siyang/dissertation_folder/dissertation/data/mplus"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
dat_path <- file.path(out_dir, "Mplus.dat")
map_path <- file.path(out_dir, "Mplus_name_map.csv")
order_path <- file.path(out_dir, "Mplus_variable_order.txt")

# Write .dat (space-delimited, no header), using . for NA
readr::write_delim(export_df[, export_vars, drop = FALSE], file = dat_path, delim = " ", na = ".", col_names = FALSE)

# Write updated name map
readr::write_csv(full_map, map_path)

# Also write the final variable order for Mplus VARIABLE= names; convenience only
writeLines(export_vars, con = order_path)

cat("Mplus export complete:\n")
cat("- Data: ", dat_path, "\n", sep = "")
cat("- Name map: ", map_path, "\n", sep = "")
cat("- Variable order: ", order_path, "\n", sep = "")

# -----------------------------------------------------------------------------
# 5) Export subset for model_sr_lb_cov (only required variables)
# -----------------------------------------------------------------------------

# Old variable names referenced by the model (if present in data)
tvc_old <- c("parenting_age5_c", "parenting_age7_c", "parenting_age11_c", "pm14_c", "pm17_c")
tic_old <- c("sex", "cognitive_ability", "low_birthweight", "infant_temperament",
             "heavy_fetal_alcohol_exposure", "marital_status", "race", "parents_education_binary")

# Map old -> mplus names using full_map, then keep those that are in export_df
lookup_mplus <- function(old_names) {
  res <- full_map %>% filter(old_name %in% old_names) %>% pull(mplus_name)
  intersect(res, names(export_df))
}

id_mplus <- intersect(id_vars, names(export_df))
sc_mplus <- full_map %>% filter(old_name %in% sc_old) %>% pull(mplus_name)
sc_mplus <- intersect(sc_mplus, names(export_df))
tvc_mplus <- lookup_mplus(tvc_old)
tic_mplus <- lookup_mplus(tic_old)

# Include survey weight if available
wt_mplus <- intersect(c("weight1"), names(export_df))

model_vars <- unique(c(id_mplus, sc_mplus, tvc_mplus, tic_mplus, wt_mplus))

model_dat_path <- file.path(out_dir, "Mplus_model_sr_lb_cov.dat")
model_map_path <- file.path(out_dir, "Mplus_model_sr_lb_cov_name_map.csv")
model_order_path <- file.path(out_dir, "Mplus_model_sr_lb_cov_variable_order.txt")

# Write subset .dat (space-delimited, no header), using '.' for NA
readr::write_delim(export_df[, model_vars, drop = FALSE], file = model_dat_path, delim = " ", na = ".", col_names = FALSE)

# Write subset name map and order
full_map %>% filter(mplus_name %in% model_vars) %>% readr::write_csv(model_map_path)
writeLines(model_vars, con = model_order_path)

cat("Subset export for model_sr_lb_cov complete:\n")
cat("- Data: ", model_dat_path, "\n", sep = "")
cat("- Name map: ", model_map_path, "\n", sep = "")
cat("- Variable order: ", model_order_path, "\n", sep = "")


