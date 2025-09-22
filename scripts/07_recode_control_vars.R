# Load required libraries
library(pacman)
p_load(tidyverse)

# =============================================================================
# Function to recode control variables
# =============================================================================

recode_control_variables <- function(data) {
  # Create a copy of the data to avoid modifying the original
  data_recode <- data
  
  # Parents' highest education at birth: amacqu00
  # Recode 1, 8, 9, 95, 96 as NA, all others minus 1
  data_recode$parents_education <- case_when(
    data_recode$amacqu00 %in% c(1, 8, 9, 95, 96) ~ NA_real_,
    TRUE ~ data_recode$amacqu00 - 1
  )
  
  # Sex at birth: ahcsexa0
  # Recode 1 as 0 (Male), 2 as 1 (Female)
  data_recode$sex <- case_when(
    data_recode$ahcsexa0 == 1 ~ 0,
    data_recode$ahcsexa0 == 2 ~ 1,
    TRUE ~ NA_real_  # Handle any other values as NA
  )

  # recode sex to factor
  data_recode$sex_factor <- factor(data_recode$sex, 
                                        levels = c(0, 1),
                                        labels = c("Male", "Female"))
  
  # Race at Birth: adceeaa0
  # Recode -9, -8, -1 as missing
  # 1,2,3 as white
  # 8,9,10,11,15 as Asian
  # 12,13,14 as Black
  # 4,5,6,7 as mixed
  # 95 as others
  data_recode$race <- case_when(
    data_recode$adceeaa0 %in% c(-9, -8, -1) ~ NA_real_,
    data_recode$adceeaa0 %in% c(1, 2, 3) ~ 1,  # White
    data_recode$adceeaa0 %in% c(8, 9, 10, 11, 15) ~ 2,  # Asian
    data_recode$adceeaa0 %in% c(12, 13, 14) ~ 3,  # Black
    data_recode$adceeaa0 %in% c(4, 5, 6, 7) ~ 4,  # Mixed
    data_recode$adceeaa0 == 95 ~ 5,  # Others
    TRUE ~ NA_real_  # Any other values as NA
  )
  
  # Create factor labels for race variable
  data_recode$race_factor <- factor(data_recode$race, 
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("White", "Asian", "Black", "Mixed", "Others"))
  
  # Parents Marital status at 9 month old: amfcin00
  # Recode 9,8,-1 as NA
  # 1,4,5,6 as not_married (0)
  # 2,3 as married (1)
  data_recode$marital_status <- case_when(
    data_recode$amfcin00 %in% c(9, 8, -1) ~ NA_real_,
    data_recode$c %in% c(1, 4, 5, 6) ~ 0,  # not_married
    data_recode$amfcin00 %in% c(2, 3) ~ 1,  # married
    TRUE ~ NA_real_  # Any other values as NA
  )
  
  # Create factor labels for marital status
  data_recode$marital_status_factor <- factor(data_recode$marital_status, 
                                        levels = c(0, 1),
                                        labels = c("Not Married", "Married"))
  
  # Parents Income at Birth: amnico00 (couple)
  # Recode -1, 96, 97 as NA, others leave as is
  data_recode$parents_income_couple <- case_when(
    data_recode$amnico00 %in% c(-1, 96, 97) ~ NA_real_,
    TRUE ~ data_recode$amnico00
  )
  
  # Parents Income at Birth: amnilp00 (lone parent)
  # Recode -1, 96, 97 as NA, others leave as is
  data_recode$parents_income_lone_parent <- case_when(
    data_recode$amnilp00 %in% c(-1, 96, 97) ~ NA_real_,
    TRUE ~ data_recode$amnilp00
  )
  
  # Return the recoded data
  return(data_recode)
}


# =============================================================================
# Example usage (uncomment to test)
# =============================================================================

# Load your data (assuming it's already loaded as 'merged_data')
# source("01_data_preparation.R")  # If you need to load the data first

# Apply recoding to your dataset
merged_data <- recode_control_variables(merged_data)

# Check the recoded variables
# summary(merged_data_recode$amacqu00)
# summary(merged_data_recode$ahcsexa0)
# summary(merged_data_recode$adceeaa0)
# summary(merged_data_recode$adceeaa0_factor)
# summary(merged_data_recode$amfcin00)
# summary(merged_data_recode$amfcin00_factor)
# summary(merged_data_recode$amnico00)
# summary(merged_data_recode$amnilp00)

# Save the recoded data if needed
# saveRDS(merged_data_recode, file = file.path("/home/siyang/dissertation_folder/data", "merged_data_recode.rds"))

cat("Recode control variables script loaded. Use recode_control_variables(data) to apply recoding.\n")