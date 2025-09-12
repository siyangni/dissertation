# Load required library for reading Stata files
library(pacman)
p_load(haven)

# Set the data directory path
data_path <- "/home/siyang/dissertation_folder/data"

# Import the Stata data file
merged_data <- read_dta(file.path(data_path, "merged1203.dta"))

# Display basic information about the imported data
cat("Data imported successfully!\n")
cat("Dimensions:", dim(merged_data), "\n")
cat("Column names (first 10):", head(names(merged_data), 10), "\n")

# Display first few rows
head(merged_data)

# Save as R data file (.rds format - more efficient for single objects)
saveRDS(merged_data, file = file.path(data_path, "merged1203.rds"))
cat("Data saved as RDS file: merged1203.rds\n")

