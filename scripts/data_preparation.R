# =============================================================================
# AGE 11 RISK-TAKING VARIABLES: INTERACTIVE DATA ANALYSIS
# =============================================================================
# Load packages
library(pacman)
p_load(psych)

# =============================================================================
# 1. DATA LOADING
# =============================================================================

# Load the saved RDS file
data_path <- "/home/siyang/dissertation_folder/data"
merged_data <- readRDS(file.path(data_path, "merged1203.rds"))

# Basic info
cat("Dimensions:", dim(merged_data), "\n")
cat("Variables loaded successfully\n")




