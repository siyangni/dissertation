# Test script to verify the self-control recoding fix
library(pacman)
p_load(psych, corrplot, lavaan, semTools, VIM)

# Load data
data_path <- "/home/siyang/dissertation_folder/data"
merged_data <- readRDS(file.path(data_path, "merged1203.rds"))

cat("=== TESTING SELF-CONTROL RECODING FIX ===\n")
cat("Original dataset dimensions:", dim(merged_data), "\n")

# Check if age 11, 14, 17 variables exist before recoding
cat("\n=== CHECKING ORIGINAL VARIABLES ===\n")
for (age in c(11, 14, 17)) {
  age_chr <- as.character(age)
  if (age == 11) prefix <- "epsd"
  if (age == 14) prefix <- "fpsd" 
  if (age == 17) prefix <- "gpsd"
  
  # Check a few key variables with correct naming (ending in 00)
  test_vars <- paste0(prefix, c("st00", "gf00", "dc00", "tt00"))
  found <- sum(test_vars %in% names(merged_data))
  cat("Age", age, "original variables (sample):", found, "/", length(test_vars), "found\n")
}

# Source the fixed script
source("scripts/recode_self_control.R")

# Check results
cat("\n=== CHECKING RECODED VARIABLES ===\n")
for (age in c(3, 5, 7, 11, 14, 17)) {
  age_vars <- grep(paste0("^sc", age, "_"), names(merged_data), value = TRUE)
  cat("Age", age, "recoded variables:", length(age_vars), "\n")
  if (length(age_vars) > 0 && length(age_vars) <= 5) {
    cat("  Examples:", paste(head(age_vars, 3), collapse = ", "), "\n")
  }
}

cat("\nTotal new self-control variables:", length(grep("^sc[0-9]+_", names(merged_data), value = TRUE)), "\n")
cat("=== TEST COMPLETED ===\n")
