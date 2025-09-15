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
# SIMPLIFIED MASTER SCRIPT: Recode Self-Control Variables for All Ages
# =============================================================================
#
# This script uses the correct variable naming conventions and the proven
# recode_self_control() function to process all ages consistently.
#
# =============================================================================

# Load required packages if not already loaded
if (!require(pacman, quietly = TRUE)) {
  install.packages("pacman")
  library(pacman)
}
p_load(psych, corrplot, lavaan, semTools, VIM)

# Load the merged dataset if not already loaded
if (!exists("merged_data")) {
  data_path <- "/home/siyang/dissertation_folder/data"
  merged_data <- readRDS(file.path(data_path, "merged1203.rds"))
  cat("Loaded merged_data from:", file.path(data_path, "merged1203.rds"), "\n")
}

cat("=== SIMPLIFIED MASTER SELF-CONTROL RECODING SCRIPT ===\n")
cat("Using the proven recode_self_control() function with correct variable names...\n\n")

# Apply the working function to all ages
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
  
  # Check total number of new variables created
  all_new_vars <- grep("^sc[0-9]+_", names(merged_data), value = TRUE)
  cat("\nTotal new self-control variables created:", length(all_new_vars), "\n")
  
  cat("\n=== SCRIPT EXECUTION COMPLETED SUCCESSFULLY ===\n")
  
} else {
  cat("merged_data not found in environment. Load data first, then source this script.\n")
}
