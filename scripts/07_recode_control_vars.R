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
    data_recode$amfcin00 %in% c(1, 4, 5, 6) ~ 0,  # not_married
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
  
  # Low birthweight: adbwgta0 (cohort member's weight in kilos)
  # Recode as 1 if under 2.5kg at birth, 0 otherwise
  data_recode$low_birthweight <- case_when(
    data_recode$adbwgta0 < 2.5 ~ 1,
    data_recode$adbwgta0 >= 2.5 ~ 0,
    TRUE ~ NA_real_  # Handle missing or invalid values as NA
  )
  
  # Infant Temperament Variables (9 months)
  # Recode: 3 or 2 = 0, 1 = 1, all other values = NA
  
  # Helper function to recode temperament variables
  recode_temp <- function(x) {
    case_when(
      x %in% c(2, 3) ~ 0,
      x == 1 ~ 1,
      TRUE ~ NA_real_
    )
  }
  
  # Recode individual temperament variables
  data_recode$amhapna0_r <- recode_temp(data_recode$amhapna0)  # Happy sounds during nappy change
  data_recode$amunfaa0_r <- recode_temp(data_recode$amunfaa0)  # Pleasant in unfamiliar places
  data_recode$ambrusa0_r <- recode_temp(data_recode$ambrusa0)  # Pleasant during brushing
  data_recode$amfeeda0_r <- recode_temp(data_recode$amfeeda0)  # Content during feeding interruptions
  data_recode$aminjua0_r <- recode_temp(data_recode$aminjua0)  # Pleasant with minor injuries
  data_recode$ambatha0_r <- recode_temp(data_recode$ambatha0)  # Objects to different bathing
  data_recode$amwarya0_r <- recode_temp(data_recode$amwarya0)  # Wary of strangers
  data_recode$ambshya0_r <- recode_temp(data_recode$ambshya0)  # Shy with other children
  data_recode$amfreta0_r <- recode_temp(data_recode$amfreta0)  # Fretful in new places
  data_recode$amsleea0_r <- recode_temp(data_recode$amsleea0)  # Bothered in different sleeping place
  data_recode$ammilka0_r <- recode_temp(data_recode$ammilka0)  # Regular milk feeding times
  data_recode$amsltia0_r <- recode_temp(data_recode$amsltia0)  # Regular sleep time
  data_recode$amnapsa0_r <- recode_temp(data_recode$amnapsa0)  # Regular nap length
  data_recode$amsofoa0_r <- recode_temp(data_recode$amsofoa0)  # Regular solid food times
  
  # Create infant_temperament by summing all recoded variables
  temp_vars <- c("amhapna0_r", "amunfaa0_r", "ambrusa0_r", "amfeeda0_r", "aminjua0_r",
                 "ambatha0_r", "amwarya0_r", "ambshya0_r", "amfreta0_r", "amsleea0_r",
                 "ammilka0_r", "amsltia0_r", "amnapsa0_r", "amsofoa0_r")
  
  data_recode$infant_temperament <- rowSums(
    data_recode[, temp_vars],
    na.rm = FALSE  # If any variable is NA, the sum will be NA
  )
  
  # Heavy fetal alcohol exposure
  # amdrof00: Frequency of drinking during pregnancy
  # 1=Every day, 2=5-6 times/week, 3=3-4 times/week, 4=1-2 times/week,
  # 5=1-2 times/month, 6=Less than once/month, 7=Never
  # ampuda00: Units per day when drinking (0-22 valid, negative=missing)
  
  # Convert drinking frequency to times per week
  data_recode$drinking_freq_per_week <- case_when(
    data_recode$amdrof00 == 1 ~ 7,      # Every day
    data_recode$amdrof00 == 2 ~ 5.5,    # 5-6 times per week (average)
    data_recode$amdrof00 == 3 ~ 3.5,    # 3-4 times per week (average)
    data_recode$amdrof00 == 4 ~ 1.5,    # 1-2 times per week (average)
    data_recode$amdrof00 == 5 ~ 0.375,  # 1-2 times per month (1.5/4 weeks)
    data_recode$amdrof00 == 6 ~ 0.25,   # Less than once a month
    data_recode$amdrof00 == 7 ~ 0,      # Never
    TRUE ~ NA_real_  # All other values as NA
  )
  
  # Clean units per day (set negative values to NA, keep 0-22 as valid)
  data_recode$units_per_day_clean <- case_when(
    data_recode$ampuda00 >= 0 & data_recode$ampuda00 <= 22 ~ data_recode$ampuda00,
    TRUE ~ NA_real_
  )
  
  # Calculate units per week
  data_recode$units_per_week <- data_recode$drinking_freq_per_week * data_recode$units_per_day_clean
  
  # Create heavy_fetal_alcohol_exposure
  # 1 if: 7+ units per week OR 6+ units per occasion
  # 0 otherwise
  # NA if missing data
  data_recode$heavy_fetal_alcohol_exposure <- case_when(
    is.na(data_recode$units_per_week) | is.na(data_recode$units_per_day_clean) ~ NA_real_,
    data_recode$units_per_week >= 7 | data_recode$units_per_day_clean >= 6 ~ 1,
    TRUE ~ 0
  )
  
  # Cognitive Ability (Age 3) - BSRA School Readiness Composite
  # bdsrcs00: Bracken School Readiness Composite Standardised Score
  # Age-adjusted score (mean=100, SD=15)
  # Recode negative values as NA, then standardize to mean=0, SD=1
  # Higher scores indicate greater understanding of basic concepts and school readiness
  data_recode$cognitive_ability_raw <- case_when(
    data_recode$bdsrcs00 < 0 ~ NA_real_,  # Recode all negative missing codes as NA
    TRUE ~ data_recode$bdsrcs00
  )
  
  # Standardize cognitive ability (mean=0, SD=1)
  # Higher scores = better cognitive ability
  data_recode$cognitive_ability <- as.numeric(scale(data_recode$cognitive_ability_raw))
  
  # Return the recoded data
  return(data_recode)
}




# Apply recoding to your dataset
merged_data <- recode_control_variables(merged_data)
