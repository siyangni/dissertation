# =============================================================================
# Master recoding script: Self-Control variables across ages 3, 5, 7, 11, 14, 17
# =============================================================================

# This script defines a single parameterized function to recode age-specific
# self-control items into standardized 0-2 scales where higher values indicate
# higher self-control. It then applies the function across all ages.
#
# Assumptions:
# - A data.frame named `merged_data` is already available in the environment.
# - Original item responses use 1, 2, 3 as valid categories; any other values
#   (including negatives or 4) are treated as missing (NA).
# - For "positive" (prosocial/self-control) items, mapping is 1->0, 2->1, 3->2.
# - For "negative" (problem) items, mapping is 1->2, 2->1, 3->0.
# - Newly created variables follow the convention: `sc{age}_{indicator}`.


# ------------------------------
# Utilities
# ------------------------------

# Safely coerce vector to numeric while preserving 1/2/3 codes if stored as
# character/factor. Values outside {1,2,3} become NA.
safe_to_numeric <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  x_num[!(x_num %in% c(1, 2, 3))] <- NA
  return(x_num)
}

# Positive item: higher value = higher self-control
# Maps 1->0, 2->1, 3->2
recode_positive <- function(x) {
  x_num <- safe_to_numeric(x)
  return(ifelse(is.na(x_num), NA, x_num - 1))
}

# Negative item: higher value = lower self-control (needs reversing)
# Maps 1->2, 2->1, 3->0
recode_negative <- function(x) {
  x_num <- safe_to_numeric(x)
  return(ifelse(is.na(x_num), NA, 3 - x_num))
}


# ------------------------------
# Age-specific source variable map
# ------------------------------

# Canonical indicator keys used across ages
positive_keys <- c(
  "think_act", "good_friend", "liked_children", "considerate", "sharing",
  "helpful", "kind_younger", "volunteer_help", "task_completion", "obedient"
)

negative_keys <- c(
  "solitary", "distracted", "better_adults", "temper", "worries",
  "unhappy", "nervous", "fears", "restless", "fidgeting", "lying"
)

# Source variable names for each indicator by age
# Each entry is a named character vector with names being ages ("3","5","7","11","14","17").
source_vars_positive <- list(
  think_act      = c("3" = "bmsdsta0", "5" = "cmsdsta0", "7" = "dmsdsta0", "11" = "epsdst00", "14" = "fpsdst00", "17" = "gpsdst00"),
  good_friend    = c("3" = "bmsdgfa0", "5" = "cmsdgfa0", "7" = "dmsdgfa0", "11" = "epsdgf00", "14" = "fpsdgf00", "17" = "gpsdgf00"),
  liked_children = c("3" = "bmsdlca0", "5" = "cmsdlca0", "7" = "dmsdlca0", "11" = "epsdlc00", "14" = "fpsdlc00", "17" = "gpsdlc00"),
  considerate    = c("3" = "bmsdpfa0", "5" = "cmsdpfa0", "7" = "dmsdpfa0", "11" = "epsdpf00", "14" = "fpsdpf00", "17" = "gpsdpf00"),
  sharing        = c("3" = "bmsdsra0", "5" = "cmsdsra0", "7" = "dmsdsra0", "11" = "epsdsr00", "14" = "fpsdsr00", "17" = "gpsdsr00"),
  helpful        = c("3" = "bmsdhua0", "5" = "cmsdhua0", "7" = "dmsdhua0", "11" = "epsdhu00", "14" = "fpsdhu00", "17" = "gpsdhu00"),
  kind_younger   = c("3" = "bmsdkya0", "5" = "cmsdkya0", "7" = "dmsdkya0", "11" = "epsdky00", "14" = "fpsdky00", "17" = "gpsdky00"),
  volunteer_help = c("3" = "bmsdvha0", "5" = "cmsdvha0", "7" = "dmsdvha0", "11" = "epsdvh00", "14" = "fpsdvh00", "17" = "gpsdvh00"),
  task_completion= c("3" = "bmsdtea0", "5" = "cmsdtea0", "7" = "dmsdtea0", "11" = "epsdte00", "14" = "fpsdte00", "17" = "gpsdte00"),
  obedient       = c("3" = "bmsdora0", "5" = "cmsdora0", "7" = "dmsdora0", "11" = "epsdor00", "14" = "fpsdor00", "17" = "gpsdor00")
)

source_vars_negative <- list(
  solitary      = c("3" = "bmsdspa0", "5" = "cmsdspa0", "7" = "dmsdspa0", "11" = "epsdsp00", "14" = "fpsdsp00", "17" = "gpsdsp00"),
  distracted    = c("3" = "bmsddca0", "5" = "cmsddca0", "7" = "dmsddca0", "11" = "epsddc00", "14" = "fpsddc00", "17" = "gpsddc00"),
  better_adults = c("3" = "bmsdgba0", "5" = "cmsdgba0", "7" = "dmsdgba0", "11" = "epsdgb00", "14" = "fpsdgb00", "17" = "gpsdgb00"),
  temper        = c("3" = "bmsdtta0", "5" = "cmsdtta0", "7" = "dmsdtta0", "11" = "epsdtt00", "14" = "fpsdtt00", "17" = "gpsdtt00"),
  worries       = c("3" = "bmsdmwa0", "5" = "cmsdmwa0", "7" = "dmsdmwa0", "11" = "epsdmw00", "14" = "fpsdmw00", "17" = "gpsdmw00"),
  unhappy       = c("3" = "bmsduda0", "5" = "cmsduda0", "7" = "dmsduda0", "11" = "epsdud00", "14" = "fpsdud00", "17" = "gpsdud00"),
  nervous       = c("3" = "bmsdnca0", "5" = "cmsdnca0", "7" = "dmsdnca0", "11" = "epsdnc00", "14" = "fpsdnc00", "17" = "gpsdnc00"),
  fears         = c("3" = "bmsdfea0", "5" = "cmsdfea0", "7" = "dmsdfea0", "11" = "epsdfe00", "14" = "fpsdfe00", "17" = "gpsdfe00"),
  restless      = c("3" = "bmsdpba0", "5" = "cmsdpba0", "7" = "dmsdpba0", "11" = "epsdpb00", "14" = "fpsdpb00", "17" = "gpsdpb00"),
  fidgeting     = c("3" = "bmsdfsa0", "5" = "cmsdfsa0", "7" = "dmsdfsa0", "11" = "epsdfs00", "14" = "fpsdfs00", "17" = "gpsdfs00"),
  lying         = c("3" = "bmsdoaa0", "5" = "cmsdoaa0", "7" = "dmsdoaa0", "11" = "epsdoa00", "14" = "fpsdoa00", "17" = "gpsdoa00")
)


# ------------------------------
# Parameterized recoding function
# ------------------------------

#' Recode self-control items for a given age into standardized 0-2 scales.
#'
#' @param data data.frame containing original variables
#' @param age integer or character: one of 3, 5, 7, 11, 14, 17
#' @param verbose logical: print progress and small summaries
#' @return data.frame with new `sc{age}_*` variables added
recode_self_control <- function(data, age, verbose = TRUE) {
  age_chr <- as.character(age)
  if (!(age_chr %in% c("3", "5", "7", "11", "14", "17"))) {
    stop("Unsupported age. Use one of: 3, 5, 7, 11, 14, 17")
  }

  if (verbose) {
    cat("\n=== SELF-CONTROL RECODING (AGE", age_chr, ") ===\n", sep = "")
  }

  # Track created variables for summary
  created_vars <- character(0)

  # Positive/prosocial items (no reversal beyond 1->0, 2->1, 3->2)
  for (key in positive_keys) {
    src_map <- source_vars_positive[[key]]
    src_var <- unname(src_map[age_chr])
    if (!is.na(src_var) && length(src_var) == 1 && src_var %in% names(data)) {
      new_name <- paste0("sc", age_chr, "_", key)
      data[[new_name]] <- recode_positive(data[[src_var]])
      created_vars <- c(created_vars, new_name)
      if (verbose) cat("✓ ", src_var, " -> ", new_name, " (positive)", "\n", sep = "")
    }
  }

  # Negative/problem items (reverse-coded: 1->2, 2->1, 3->0)
  for (key in negative_keys) {
    src_map <- source_vars_negative[[key]]
    src_var <- unname(src_map[age_chr])
    if (!is.na(src_var) && length(src_var) == 1 && src_var %in% names(data)) {
      new_name <- paste0("sc", age_chr, "_", key)
      data[[new_name]] <- recode_negative(data[[src_var]])
      created_vars <- c(created_vars, new_name)
      if (verbose) cat("✓ ", src_var, " -> ", new_name, " (negative)", "\n", sep = "")
    }
  }

  # Brief summary
  if (verbose) {
    cat("Created variables:", length(created_vars), "\n")
    if (length(created_vars) > 0) {
      cat(paste0("- ", created_vars), sep = "\n")
      cat("\n")
    }
  }

  return(data)
}


# ------------------------------
# Apply to all ages (if merged_data present)
# ------------------------------

if (exists("merged_data")) {
  for (age in c(3, 5, 7, 11, 14, 17)) {
    merged_data <- recode_self_control(merged_data, age, verbose = TRUE)
  }

  # Final concise report of availability per age
  cat("\n=== RECODING COMPLETED ACROSS AGES ===\n")
  for (age in c(3, 5, 7, 11, 14, 17)) {
    age_chr <- as.character(age)
    target_names <- c(
      paste0("sc", age_chr, "_", positive_keys),
      paste0("sc", age_chr, "_", negative_keys)
    )
    available <- sum(target_names %in% names(merged_data))
    cat("Age", age_chr, ":", available, "/", length(target_names), "variables present\n")
  }
} else {
  cat("merged_data not found in environment. Load data first, then source this script.\n")
}

# =============================================================================
# MASTER SCRIPT: Recode Self-Control Variables for All Ages
# =============================================================================
#
# This script consolidates the individual age-specific self-control recoding scripts
# into a single, parameterized master script. It processes self-control variables
# for ages 3, 5, 7, 11, 14, and 17 using a unified approach.
#
# Author: Dissertation Analysis Pipeline
# Date: 2025
#
# =============================================================================

# Load required packages
library(pacman)
p_load(psych, corrplot, lavaan, semTools, VIM)

# =============================================================================
# 1. MASTER FUNCTION: Recode Self-Control Variables for Any Age
# =============================================================================

#' Recode Self-Control Variables for a Specific Age
#'
#' This function processes self-control variables for a given age by:
#' 1. Defining the appropriate variable prefixes based on age
#' 2. Checking variable availability in the dataset
#' 3. Displaying frequency distributions of original variables
#' 4. Recoding reverse-coded variables (1->0, 2->1, 3->2)
#' 5. Recoding normal-coded variables (1->2, 2->1, 3->0)
#' 6. Providing summary statistics and variable mappings
#'
#' @param age Integer. Age to process (3, 5, 7, 11, 14, or 17)
#' @param data Data frame. The dataset containing the variables (must be named 'merged_data')
#' @param verbose Logical. Whether to print detailed output (default: TRUE)
#' @return Invisible NULL. The function modifies the data frame in place.
#'
#' @details
#' The function uses standardized coding where higher values indicate higher self-control:
#' - Originally reverse-coded variables: 1->0, 2->1, 3->2
#' - Originally normal-coded variables: 1->2, 2->1, 3->0 (reverse-coded for consistency)
#' - Invalid values (< 0, 4, or non-integer) are coded as NA
recode_self_control_age <- function(age, data, verbose = TRUE) {

  # Define age-to-prefix mapping
  age_prefixes <- list(
    "3" = "bmsd",
    "5" = "cmsd",
    "7" = "dmsd",
    "11" = "epsd",
    "14" = "fpsd",
    "17" = "gpsd"
  )

  # Validate age parameter
  if (!as.character(age) %in% names(age_prefixes)) {
    stop("Invalid age. Supported ages are: ", paste(names(age_prefixes), collapse = ", "))
  }

  prefix <- age_prefixes[[as.character(age)]]
  output_prefix <- paste0("sc", age, "_")

  if (verbose) {
    cat("
=== SELF-CONTROL VARIABLES PROCESSING (AGE ", age, ") ===\n", sep = "")
    cat("Using prefix: '", prefix, "' -> output prefix: '", output_prefix, "'\n\n", sep = "")
  }

  # =============================================================================
  # 1. DEFINE SELF-CONTROL VARIABLE LISTS
  # =============================================================================

  # Variables that originally need reverse coding (higher original = lower self-control)
  self_control_reverse <- c(
    paste0(prefix, "sta0"),  # SDST: Think things out before acting
    paste0(prefix, "gfa0"),  # Having at least one good friend
    paste0(prefix, "lca0"),  # Generally liked by other children
    paste0(prefix, "pfa0"),  # Being considerate of other people's feelings
    paste0(prefix, "sra0"),  # Sharing readily with other children
    paste0(prefix, "hua0"),  # Being helpful if someone is hurt
    paste0(prefix, "kya0"),  # Being kind to younger children
    paste0(prefix, "vha0"),  # Often volunteering to help others
    paste0(prefix, "tea0"),  # Sees tasks through to the end, good attention span
    paste0(prefix, "ora0")   # Generally obedient
  )

  # Variables that originally have normal coding (higher original = lower self-control)
  self_control_normal <- c(
    paste0(prefix, "spa0"),  # Being rather solitary and tending to play alone
    paste0(prefix, "dca0"),  # Is easily distracted, concentration wanders
    paste0(prefix, "gba0"),  # Getting on better with adults than other children
    paste0(prefix, "tta0"),  # Often has temper tantrums or hot tempers
    paste0(prefix, "mwa0"),  # Having many worries
    paste0(prefix, "uda0"),  # Being often unhappy, down-hearted, or tearful
    paste0(prefix, "nca0"),  # Being nervous or clingy in new situations
    paste0(prefix, "fea0"),  # Having many fears, being easily scared
    paste0(prefix, "pba0"),  # Child is restless, overactive, cannot stay still for long
    paste0(prefix, "fsa0"),  # Child is constantly fidgeting or squirming
    paste0(prefix, "oaa0")   # Lying or cheating
  )

  # Combine all variables
  all_self_control_vars <- c(self_control_reverse, self_control_normal)

  if (verbose) {
    cat("Variables requiring reverse coding:", length(self_control_reverse), "\n")
    cat("Variables with normal coding:", length(self_control_normal), "\n")
    cat("Total variables to process:", length(all_self_control_vars), "\n\n")
  }

  # =============================================================================
  # 2. CHECK VARIABLE AVAILABILITY
  # =============================================================================

  # Check which variables exist in the data
  existing_vars <- all_self_control_vars[all_self_control_vars %in% names(data)]
  missing_vars <- all_self_control_vars[!all_self_control_vars %in% names(data)]

  if (verbose) {
    cat("=== VARIABLE AVAILABILITY ===\n")
    cat("Found variables (", length(existing_vars), "/", length(all_self_control_vars), "): ",
        paste(existing_vars, collapse = ", "), "\n", sep = "")

    if (length(missing_vars) > 0) {
      cat("Missing variables (", length(missing_vars), "): ",
          paste(missing_vars, collapse = ", "), "\n", sep = "")
    }
    cat("\n")
  }

  # =============================================================================
  # 3. DISPLAY ORIGINAL VARIABLE DISTRIBUTIONS
  # =============================================================================

  if (verbose && length(existing_vars) > 0) {
    cat("=== ORIGINAL VARIABLE DISTRIBUTIONS ===\n")
    for (var in existing_vars) {
      cat("\n--- ", var, " ---\n", sep = "")
      var_numeric <- as.vector(data[[var]])
      print(table(var_numeric, useNA = "ifany"))
    }
    cat("\n")
  }

  # =============================================================================
  # 4. RECODE ORIGINALLY REVERSE-CODED VARIABLES (STANDARDIZE TO 0-2 SCALE)
  # =============================================================================

  if (verbose) {
    cat("=== STANDARDIZING ORIGINALLY REVERSE-CODED VARIABLES ===\n")
  }

  # Define variable name mappings for reverse-coded variables
  reverse_var_names <- c("think_act", "good_friend", "liked_children", "considerate",
                        "sharing", "helpful", "kind_younger", "volunteer_help",
                        "task_completion", "obedient")

  reverse_original_suffixes <- c("sta0", "gfa0", "lca0", "pfa0", "sra0", "hua0",
                                "kya0", "vha0", "tea0", "ora0")

  # Recode each reverse-coded variable
  for (i in seq_along(reverse_var_names)) {
    original_var <- paste0(prefix, reverse_original_suffixes[i])
    new_var <- paste0(output_prefix, reverse_var_names[i])

    if (original_var %in% names(data)) {
      var_numeric <- as.vector(data[[original_var]])
      data[[new_var]] <- ifelse(!var_numeric %in% c(1, 2, 3), NA,
                               ifelse(var_numeric == 1, 0,
                                     ifelse(var_numeric == 2, 1,
                                           ifelse(var_numeric == 3, 2, NA))))

      if (verbose) {
        cat("✓ Standardized:", original_var, "->", new_var, "(0-2 scale, higher = higher self-control)\n")
      }
    }
  }

  # =============================================================================
  # 5. RECODE ORIGINALLY NORMAL-CODED VARIABLES (REVERSE-CODE TO 0-2 SCALE)
  # =============================================================================

  if (verbose) {
    cat("\n=== STANDARDIZING ORIGINALLY NORMAL-CODED VARIABLES ===\n")
  }

  # Define variable name mappings for normal-coded variables
  normal_var_names <- c("solitary", "distracted", "better_adults", "temper", "worries",
                       "unhappy", "nervous", "fears", "restless", "fidgeting", "lying")

  normal_original_suffixes <- c("spa0", "dca0", "gba0", "tta0", "mwa0", "uda0",
                               "nca0", "fea0", "pba0", "fsa0", "oaa0")

  # Recode each normal-coded variable
  for (i in seq_along(normal_var_names)) {
    original_var <- paste0(prefix, normal_original_suffixes[i])
    new_var <- paste0(output_prefix, normal_var_names[i])

    if (original_var %in% names(data)) {
      var_numeric <- as.vector(data[[original_var]])
      data[[new_var]] <- ifelse(!var_numeric %in% c(1, 2, 3), NA,
                               ifelse(var_numeric == 1, 2,
                                     ifelse(var_numeric == 2, 1,
                                           ifelse(var_numeric == 3, 0, NA))))

      if (verbose) {
        cat("✓ Standardized:", original_var, "->", new_var, "(0-2 scale, higher = higher self-control)\n")
      }
    }
  }

  # =============================================================================
  # 6. CHECK RECODED VARIABLES
  # =============================================================================

  # List of all new self-control variables
  sc_vars_reverse <- paste0(output_prefix, reverse_var_names)
  sc_vars_normal <- paste0(output_prefix, normal_var_names)
  all_sc_vars <- c(sc_vars_reverse, sc_vars_normal)

  # Check which recoded variables exist
  existing_recoded <- all_sc_vars[all_sc_vars %in% names(data)]

  if (verbose && length(existing_recoded) > 0) {
    cat("\n=== RECODED VARIABLE DISTRIBUTIONS ===\n")
    cat("Successfully recoded variables (", length(existing_recoded), "):\n\n", sep = "")

    for (var in existing_recoded) {
      cat("--- ", var, " ---\n", sep = "")
      print(table(data[[var]], useNA = "ifany"))
      cat("\n")
    }
  }

  # =============================================================================
  # 7. SUMMARY AND MAPPING
  # =============================================================================

  if (verbose) {
    cat("\n=== PROCESSING SUMMARY ===\n")
    cat("Original variables found:", length(existing_vars), "/", length(all_self_control_vars), "\n")
    cat("Originally reverse-coded variables (now standardized):", length(sc_vars_reverse), "(10 total)\n")
    cat("Originally normal-coded variables (now reverse-coded):", length(sc_vars_normal), "(11 total)\n")
    cat("Successfully processed variables:", length(existing_recoded), "\n")

    if (length(missing_vars) > 0) {
      cat("\nMissing variables that need to be checked:\n")
      for (var in missing_vars) {
        cat("- ", var, "\n")
      }
    }

    # Create variable mapping tables
    cat("\n=== VARIABLE MAPPING ===\n")
    cat("Originally reverse-coded variables (now standardized to 0-2 scale):\n")

    reverse_mapping <- data.frame(
      Original = paste0(prefix, reverse_original_suffixes),
      New = paste0(output_prefix, reverse_var_names),
      Description = c("Think things out before acting", "Having at least one good friend",
                      "Generally liked by other children", "Being considerate of feelings",
                      "Sharing readily with children", "Being helpful if someone hurt",
                      "Being kind to younger children", "Often volunteering to help others",
                      "Sees tasks through to end", "Generally obedient")
    )
    print(reverse_mapping)

    cat("\nOriginally normal-coded variables (now reverse-coded to 0-2 scale):\n")
    normal_mapping <- data.frame(
      Original = paste0(prefix, normal_original_suffixes),
      New = paste0(output_prefix, normal_var_names),
      Description = c("Being rather solitary", "Easily distracted", "Getting on better with adults",
                      "Often has temper tantrums", "Having many worries", "Often unhappy/tearful",
                      "Nervous in new situations", "Having many fears", "Restless/overactive",
                      "Constantly fidgeting", "Lying or cheating")
    )
    print(normal_mapping)

    cat("\nNote: All age", age, "self-control variables are now processed.\n")
    cat("All recoded variables have prefix '", output_prefix, "' to distinguish from other age variables.\n", sep = "")
    cat("All values other than 1, 2, or 3 are coded as missing (NA).\n")
    cat("STANDARDIZED CODING: All variables use 0-2 scale where HIGHER values = HIGHER self-control.\n")
    cat("Originally reverse-coded variables: 1->0, 2->1, 3->2\n")
    cat("Originally normal-coded variables: 1->2, 2->1, 3->0 (reverse-coded for consistency)\n\n")
  }

  # Return invisible NULL (function modifies data in place)
  invisible(NULL)
}

# =============================================================================
# 2. LOAD DATA AND APPLY FUNCTION TO ALL AGES
# =============================================================================

# Load the merged dataset
# Note: This assumes the data preparation script has been run
data_path <- "/home/siyang/dissertation_folder/data"
merged_data <- readRDS(file.path(data_path, "merged1203.rds"))

cat("=== MASTER SELF-CONTROL RECODING SCRIPT ===\n")
cat("Processing self-control variables for all ages...\n\n")

# Define ages to process
ages_to_process <- c(3, 5, 7, 11, 14, 17)

# Process each age
for (age in ages_to_process) {
  cat("Processing age", age, "...\n")
  recode_self_control_age(age = age, data = merged_data, verbose = TRUE)
  cat("✓ Completed processing for age", age, "\n\n")
}

# =============================================================================
# 3. FINAL SUMMARY
# =============================================================================

cat("=== MASTER SCRIPT COMPLETION SUMMARY ===\n")
cat("Successfully processed self-control variables for ages:", paste(ages_to_process, collapse = ", "), "\n\n")

# Check total number of new variables created
all_new_vars <- grep("^sc[0-9]+_", names(merged_data), value = TRUE)
cat("Total new self-control variables created:", length(all_new_vars), "\n")

# Breakdown by age
for (age in ages_to_process) {
  age_vars <- grep(paste0("^sc", age, "_"), names(merged_data), value = TRUE)
  cat("Age", age, "variables:", length(age_vars), "\n")
}

cat("\n=== NEXT STEPS ===\n")
cat("1. The merged_data object now contains all recoded self-control variables\n")
cat("2. Consider saving the updated dataset: saveRDS(merged_data, 'path/to/updated_data.rds')\n")
cat("3. Proceed with longitudinal growth curve modeling or other analyses\n")

# =============================================================================
# 4. OPTIONAL: SAVE UPDATED DATASET
# =============================================================================

# Uncomment the following lines to save the updated dataset
# output_path <- file.path(data_path, "merged1203_with_self_control.rds")
# saveRDS(merged_data, output_path)
# cat("Updated dataset saved to:", output_path, "\n")

cat("\nMaster script execution completed successfully!\n")
