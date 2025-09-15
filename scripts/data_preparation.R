# Load required library for reading Stata files
library(pacman)
p_load(haven)
p_load(psych)

# Set the data directory path
# data_path <- "/home/siyang/dissertation_folder/data"

# Import the Stata data file
# merged_data <- read_dta(file.path(data_path, "merged1203.dta"))

# Display first few rows
#head(merged_data)

# Save as R data file (.rds format - more efficient for single objects)
# saveRDS(merged_data, file = file.path(data_path, "merged1203.rds"))
#cat("Data saved as RDS file: merged1203.rds\n")

# =============================================================================
# 1. DATA LOADING
# =============================================================================

# Load the saved RDS file
data_path <- "/home/siyang/dissertation_folder/data"
merged_data <- readRDS(file.path(data_path, "merged0914.rds"))

# Basic info
dim(merged_data)



