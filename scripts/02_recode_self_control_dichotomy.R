# =============================================================================
# Recoding self-control to dichotomies
# =============================================================================
# Positive items: 1 -> 0; 2 or 3 -> 1
# Negative items: 3 -> 0; 1 or 2 -> 1
# Non {1,2,3} values -> NA
# =============================================================================
library(pacman)
p_load(tidyverse, haven)

stopifnot(exists("merged_data"))
df <- as_tibble(merged_data)

# -----------------------------------------
# 1) Define keys and source-variable maps
# -----------------------------------------
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

# ---------------------------------------------------
# 2) Loop over all ages and recode to 0/1 in-place
# ---------------------------------------------------
ages <- c(3, 5, 7, 11, 14, 17)

for (age in ages) {
  message("\n=== Dichotomizing age ", age, " ===")

  # Build a tidy mapping for this age (only variables present in df)
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

  # Skip if nothing to do
  if (nrow(mapping) == 0) {
    message("No source variables found for age ", age, ". Skipping.")
    next
  }

  # -----------------------------
  # POSITIVE items: 1 -> 0; 2/3 -> 1
  # -----------------------------
  pos_vars <- mapping %>% filter(polarity == "positive") %>% pull(var)

  if (length(pos_vars)) {
    df <- df %>%
      mutate(across(
        all_of(pos_vars),
        \(x) {
          # Coerce to numeric while handling labels, factors, characters
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
            is.na(x_num)      ~ NA_real_,
            x_num == 1        ~ 0,
            x_num %in% c(2,3) ~ 1
          )
          as.integer(out)
        },
        .names = "tmp__{col}"
      ))
    message("Dichotomized positives: ", paste(pos_vars, collapse = ", "))
  } else {
    message("No positive source variables found for age ", age, ".")
  }

  # -----------------------------
  # NEGATIVE items: 3 -> 0; 1/2 -> 1
  # -----------------------------
  neg_vars <- mapping %>% filter(polarity == "negative") %>% pull(var)

  if (length(neg_vars)) {
    df <- df %>%
      mutate(across(
        all_of(neg_vars),
        \(x) {
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
            is.na(x_num)      ~ NA_real_,
            x_num == 3        ~ 0,
            x_num %in% c(1,2) ~ 1
          )
          as.integer(out)
        },
        .names = "tmp__{col}"
      ))
    message("Dichotomized negatives: ", paste(neg_vars, collapse = ", "))
  } else {
    message("No negative source variables found for age ", age, ".")
  }

  # ------------------------------------------
  # Rename temporaries: tmp__<src> -> sc{age}_{key}
  # ------------------------------------------
  tmp_names <- paste0("tmp__", mapping$var)
  tmp_in_df <- tmp_names[tmp_names %in% names(df)]
  map_keep  <- mapping[match(tmp_in_df, tmp_names), , drop = FALSE]

  if (length(tmp_in_df)) {
    df <- df %>% rename(!!!setNames(nm = map_keep$target, object = tmp_in_df))
    message("Created variables (", length(tmp_in_df), "): ",
            paste(map_keep$target, collapse = ", "))
  } else {
    message("No new variables created for age ", age, ".")
  }

  # ------------------------------------------
  # Quick availability and prevalence check
  # ------------------------------------------
  wanted <- mapping$target
  have   <- sum(wanted %in% names(df))
  message("Age ", age, ": ", have, " / ", length(wanted), " dichotomous variables present.")

  created_cols <- wanted[wanted %in% names(df)]
  if (length(created_cols)) {
    prev <- df %>%
      summarise(across(all_of(created_cols), ~ mean(.x == 1, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "p_one")
    print(prev)
  }
}

# Push back to merged_data
merged_data <- df

# Final overall count
total_new <- sum(grepl("^sc(3|5|7|11|14|17)_", names(merged_data)))
message("\n=== All ages processed. Total new dichotomous variables: ", total_new, " ===")
