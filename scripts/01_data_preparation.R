# Load required library for reading Stata files
library(pacman)
p_load(haven)
p_load(psych)
p_load(dplyr)

# Set the data directory path
data_path <- "/home/siyang/dissertation_folder/data"

# =============================================================================
# 1. DATA LOADING AND MERGING
# =============================================================================
# Load the RDS file (assuming this is merged1203.rds as mentioned in your request)
merged1203 <- readRDS(file.path(data_path, "merged1203.rds"))

# Load the Stata files from MCS directories
cat("Loading MCS1 data...\n")
mcs1_data <- read_dta(file.path(data_path, "MCS Data/MCS 1/stata11/mcs1_derived_variables.dta"))

cat("Loading MCS4 data...\n")
mcs4_data <- read_dta(file.path(data_path, "MCS Data/MCS 4/stata11_se/mcs4_cm_self_completion_final.dta"))

# Check dimensions of each dataset
cat("Dataset dimensions:\n")
cat("merged1203:", dim(merged1203)[1], "rows,", dim(merged1203)[2], "columns\n")
cat("mcs1_data:", dim(mcs1_data)[1], "rows,", dim(mcs1_data)[2], "columns\n")
cat("mcs4_data:", dim(mcs4_data)[1], "rows,", dim(mcs4_data)[2], "columns\n")

# Perform full outer joins on mcsid
# First merge merged1203 with mcs1_data
merged_temp <- merge(merged1203, mcs1_data, by = "mcsid", all = TRUE)

# Then merge the result with mcs4_data
merged_data <- merge(merged_temp, mcs4_data, by = "mcsid", all = TRUE)

# Clean up temporary object
rm(merged_temp)

# Display final dimensions
cat("Final merged dataset dimensions:\n")
dim(merged_data)

cat("Merge completed. Total rows:", nrow(merged_data), "\n")



