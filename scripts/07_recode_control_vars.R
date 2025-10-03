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
  # Recode -1, -8, -9, 95, 96 as NA, all others minus 1
  data_recode$parents_education <- dplyr::case_when(
    data_recode$amacqu00 %in% c(-1, -8, -9, 95, 96) ~ NA_real_,
    TRUE ~ data_recode$amacqu00 - 1
  )
  
  # Binary version of parents' education
  # 1 if parents_education is 0 or 1 (lower education)
  # 0 if parents_education is 2, 3, 4, or 5 (higher education)
  data_recode$parents_education_binary <- dplyr::case_when(
    data_recode$parents_education %in% c(0, 1) ~ 1,
    data_recode$parents_education %in% c(2, 3, 4, 5) ~ 0,
    TRUE ~ NA_real_  # Missing values remain NA
  )
  
  # Sex at birth: ahcsexa0
  # Recode 1 as 0 (Male), 2 as 1 (Female)
  data_recode$sex <- dplyr::case_when(
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
  data_recode$race <- dplyr::case_when(
    data_recode$adceeaa0 %in% c(-9, -8, -1) ~ NA_real_,
    data_recode$adceeaa0 %in% c(1, 2, 3) ~ 1,  # White
    data_recode$adceeaa0 %in% c(8, 9, 10, 11, 15) ~ 2,  # Asian
    data_recode$adceeaa0 %in% c(12, 13, 14) ~ 3,  # Black
    data_recode$adceeaa0 %in% c(4, 5, 6, 7, 95) ~ 4,  # Mixed and Others
    TRUE ~ NA_real_  # Any other values as NA
  )
  
  # Create factor labels for race variable
  data_recode$race_factor <- factor(data_recode$race, 
                                        levels = c(1, 2, 3, 4),
                                        labels = c("White", "Asian", "Black", "Mixed_and_Others"))
  
  # Create binary race variable: White vs Non-White
  data_recode$race_binary <- dplyr::case_when(
    data_recode$race == 1 ~ 0,  # White
    data_recode$race %in% c(2, 3, 4) ~ 1,  # Non-White (Asian, Black, Mixed_and_Others)
    TRUE ~ NA_real_  # Missing values remain NA
  )
  
  # Create factor labels for binary race variable
  data_recode$race_binary_factor <- factor(data_recode$race_binary,
                                           levels = c(0, 1),
                                           labels = c("White", "Non-White"))
  
  
  # Parents Marital status at 9 month old: amfcin00
  # Recode 9,8,-1 as NA
  # 1,4,5,6 as not_married (0)
  # 2,3 as married (1)
  data_recode$marital_status <- dplyr::case_when(
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
  data_recode$parents_income_couple <- dplyr::case_when(
    data_recode$amnico00 %in% c(-1, 96, 97) ~ NA_real_,
    TRUE ~ data_recode$amnico00
  )
  
  # Parents Income at Birth: amnilp00 (lone parent)
  # Recode -1, 96, 97 as NA, others leave as is
  data_recode$parents_income_lone_parent <- dplyr::case_when(
    data_recode$amnilp00 %in% c(-1, 96, 97) ~ NA_real_,
    TRUE ~ data_recode$amnilp00
  )
  
  # Low birthweight: adbwgta0 (cohort member's weight in kilos)
  # Recode as 1 if under 2.5kg at birth, 0 otherwise
  data_recode$low_birthweight <- dplyr::case_when(
    data_recode$adbwgta0 < 2.5 ~ 1,
    data_recode$adbwgta0 >= 2.5 ~ 0,
    TRUE ~ NA_real_  # Handle missing or invalid values as NA
  )
  
  # Infant Temperament (9 months): clean and recode
  # Rule: all negative values and 6 -> NA; create 'n' prefixed variables
  # Reverse-code specified items on a 1–5 scale (new = 6 - value)
  clean_temp <- function(x) {
    dplyr::case_when(
      x < 0 ~ NA_real_,
      x == 6 ~ NA_real_,
      TRUE ~ as.numeric(x)
    )
  }

  # Clean all items (produce 'n' + original name)
  data_recode$namhapna0 <- clean_temp(data_recode$amhapna0)
  data_recode$namunfaa0 <- clean_temp(data_recode$amunfaa0)
  data_recode$nambrusa0 <- clean_temp(data_recode$ambrusa0)
  data_recode$namfeeda0 <- clean_temp(data_recode$amfeeda0)
  data_recode$naminjua0 <- clean_temp(data_recode$aminjua0)
  data_recode$nambatha0 <- clean_temp(data_recode$ambatha0)
  data_recode$namwarya0 <- clean_temp(data_recode$amwarya0)
  data_recode$nambshya0 <- clean_temp(data_recode$ambshya0)
  data_recode$namfreta0 <- clean_temp(data_recode$amfreta0)
  data_recode$namsleea0 <- clean_temp(data_recode$amsleea0)
  data_recode$nammilka0 <- clean_temp(data_recode$ammilka0)
  data_recode$namsltia0 <- clean_temp(data_recode$amsltia0)
  data_recode$namnapsa0 <- clean_temp(data_recode$amnapsa0)
  data_recode$namsofoa0 <- clean_temp(data_recode$amsofoa0)

  # Reverse-code designated five items (ambatha0, amwarya0, ambshya0, amfreta0, amsleea0)
  data_recode$nambatha0 <- ifelse(is.na(data_recode$nambatha0), NA_real_, 6 - data_recode$nambatha0)
  data_recode$namwarya0 <- ifelse(is.na(data_recode$namwarya0), NA_real_, 6 - data_recode$namwarya0)
  data_recode$nambshya0 <- ifelse(is.na(data_recode$nambshya0), NA_real_, 6 - data_recode$nambshya0)
  data_recode$namfreta0 <- ifelse(is.na(data_recode$namfreta0), NA_real_, 6 - data_recode$namfreta0)
  data_recode$namsleea0 <- ifelse(is.na(data_recode$namsleea0), NA_real_, 6 - data_recode$namsleea0)

  # Create infant_temperament: sum of all 14 recoded items, then standardize
  temp_vars_n <- c(
    "namhapna0", "namunfaa0", "nambrusa0", "namfeeda0", "naminjua0",
    "nambatha0", "namwarya0", "nambshya0", "namfreta0", "namsleea0",
    "nammilka0", "namsltia0", "namnapsa0", "namsofoa0"
  )

  data_recode$infant_temperament_raw <- rowSums(
    data_recode[, temp_vars_n],
    na.rm = FALSE
  )

  data_recode$infant_temperament <- as.numeric(scale(data_recode$infant_temperament_raw))
  
  # Heavy fetal alcohol exposure
  # amdrof00: Frequency of drinking during pregnancy
  # 1=Every day, 2=5-6 times/week, 3=3-4 times/week, 4=1-2 times/week,
  # 5=1-2 times/month, 6=Less than once/month, 7=Never
  # ampuda00: Units per day when drinking (0-22 valid, negative=missing)
  
  # Convert drinking frequency to times per week
  data_recode$drinking_freq_per_week <- dplyr::case_when(
    data_recode$amdrof00 == 1 ~ 7,      # Every day
    data_recode$amdrof00 == 2 ~ 5,     # 5-6 times per week (average)
    data_recode$amdrof00 == 3 ~ 3.5,    # 3-4 times per week (average)
    data_recode$amdrof00 == 4 ~ 1.5,    # 1-2 times per week (average)
    data_recode$amdrof00 == 5 ~ 0.375,  # 1-2 times per month (1.5/4 weeks)
    data_recode$amdrof00 == 6 ~ 0.25,   # Less than once a month
    data_recode$amdrof00 == 7 ~ 0,      # Never
    TRUE ~ NA_real_  # All other values as NA
  )
  
  # Clean units per day (set negative values to NA, keep 0-22 as valid)
  data_recode$units_per_day_clean <- dplyr::case_when(
    data_recode$ampuda00 >= 0 & data_recode$ampuda00 <= 22 ~ data_recode$ampuda00,
    TRUE ~ NA_real_
  )
  
  # Calculate units per week
  # Treat abstainers (frequency == 0) as 0 even if units_per_day is missing/not asked
  data_recode$units_per_week <- ifelse(
    !is.na(data_recode$drinking_freq_per_week) & data_recode$drinking_freq_per_week == 0,
    0,
    data_recode$drinking_freq_per_week * data_recode$units_per_day_clean
  )
  
  # Create heavy_fetal_alcohol_exposure
  # Rules:
  # - Abstainers (freq == 0): definitely not heavy (0), even if units/day missing
  # - Heavy if per-occasion >= 5 (requires units/day known)
  # - Heavy if weekly >= 7 (requires units/week known)
  # - If both inputs known and thresholds not met => 0
  # - Otherwise => NA (insufficient info)
  data_recode$heavy_fetal_alcohol_exposure <- dplyr::case_when(
    data_recode$drinking_freq_per_week == 0 ~ 0,
    !is.na(data_recode$units_per_day_clean) & data_recode$units_per_day_clean >= 5 ~ 1,
    !is.na(data_recode$units_per_week) & data_recode$units_per_week >= 7 ~ 1,
    !is.na(data_recode$drinking_freq_per_week) & !is.na(data_recode$units_per_day_clean) ~ 0,
    TRUE ~ NA_real_
  )
  
  # Cognitive Ability (Age 3) - BSRA School Readiness Composite
  # bdsrcs00: Bracken School Readiness Composite Standardised Score
  # Age-adjusted score (mean=100, SD=15)
  # Recode negative values as NA, then standardize to mean=0, SD=1
  # Higher scores indicate greater understanding of basic concepts and school readiness
  data_recode$cognitive_ability_raw <- dplyr::case_when(
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

