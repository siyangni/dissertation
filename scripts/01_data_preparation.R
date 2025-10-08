# Load required library for reading Stata files
library(pacman)
p_load(tidyverse, psych, haven)

# Set the data directory path
data_path <- "/home/siyang/dissertation_folder/data"

# Read the Stata file "merged 1203.dta" and save as merged1203.rds
# merged1203 <- read_dta(file.path(data_path, "merged1203.dta"))
# saveRDS(merged1203, file.path(data_path, "merged1203.rds"))

# Load the RDS file (assuming this is merged1203.rds as mentioned in your request)
# merged1203 <- readRDS(file.path(data_path, "merged1203.rds"))

# Load the Stata files from MCS directories
# mcs1_data <- read_dta(file.path(data_path, "MCS Data/MCS 1/stata11/mcs1_derived_variables.dta"))

# mcs4_data <- read_dta(file.path(data_path, "MCS Data/MCS 4/stata11_se/mcs4_cm_self_completion_final.dta"))

# Perform full outer joins on mcsid
# First merge merged1203 with mcs1_data
#merged_temp <- merge(merged1203, mcs1_data, by = "mcsid", all = TRUE)

# Then merge the result with mcs4_data
#merged_data <- merge(merged_temp, mcs4_data, by = "mcsid", all = TRUE)

# Clean up temporary object
#rm(merged_temp)

# Display final dimensions
# dim(merged_data)

# Save as merged 0917.rds
# saveRDS(merged_data, file.path(data_path, "merged0917.rds"))

# LOAD merged 0917.rds
merged_data <- readRDS(file.path(data_path, "merged0917.rds"))

