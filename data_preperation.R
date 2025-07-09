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

# =============================================================================
# 2. VARIABLE EXPLORATION
# =============================================================================

# Age 11 risk-taking variables
risk_vars <- c("ecq45x00", "ecq25x00", "cgtriskt", "epsdoa00", "epsdor00", "ecq27x00")

# Check existing variables
existing_vars <- risk_vars[risk_vars %in% names(merged_data)]
missing_vars <- risk_vars[!risk_vars %in% names(merged_data)]

cat("Found:", paste(existing_vars, collapse = ", "), "\n")
if(length(missing_vars) > 0) cat("Missing:", paste(missing_vars, collapse = ", "), "\n")

# Frequency tables for existing variables
for(var in existing_vars) {
  cat("\n--- ", var, " ---\n")
  print(table(merged_data[[var]], useNA = "ifany"))
}

# =============================================================================
# 3. VARIABLE RECODING
# =============================================================================

# Recode variables
if("ecq45x00" %in% names(merged_data)) {
  merged_data$risk5_sch <- ifelse(merged_data$ecq45x00 == -8, NA,
                                  ifelse(merged_data$ecq45x00 == 2, 0,
                                         ifelse(merged_data$ecq45x00 == 1, 1, merged_data$ecq45x00)))
}

if("ecq25x00" %in% names(merged_data)) {
  merged_data$risk5_rude <- ifelse(merged_data$ecq25x00 == -8, NA,
                                   ifelse(merged_data$ecq25x00 == 2, 0,
                                          ifelse(merged_data$ecq25x00 == 1, 1, merged_data$ecq25x00)))
}

if("epsdoa00" %in% names(merged_data)) {
  merged_data$risk5_lie <- ifelse(merged_data$epsdoa00 %in% c(-1, 4), NA,
                                  ifelse(merged_data$epsdoa00 == 1, 0,
                                         ifelse(merged_data$epsdoa00 == 2, 1,
                                                ifelse(merged_data$epsdoa00 == 3, 2, merged_data$epsdoa00))))
}

if("epsdor00" %in% names(merged_data)) {
  merged_data$risk5_obe <- ifelse(merged_data$epsdor00 %in% c(-1, 4), NA,
                                  ifelse(merged_data$epsdor00 == 1, 2,
                                         ifelse(merged_data$epsdor00 == 2, 1,
                                                ifelse(merged_data$epsdor00 == 3, 0, merged_data$epsdor00))))
}

if("ecq27x00" %in% names(merged_data)) {
  merged_data$risk5_wpp <- ifelse(merged_data$ecq27x00 == -8, NA,
                                  ifelse(merged_data$ecq27x00 == 2, 0,
                                         ifelse(merged_data$ecq27x00 == 1, 1, merged_data$ecq27x00)))
}

if("cgtriskt" %in% names(merged_data)) {
  merged_data$risk5_cgtriskt <- ifelse(merged_data$cgtriskt == -9, NA, merged_data$cgtriskt)
}

# Check recoded variables
recoded_vars <- c("risk5_sch", "risk5_rude", "risk5_lie", "risk5_obe", "risk5_wpp", "risk5_cgtriskt")
existing_recoded <- recoded_vars[recoded_vars %in% names(merged_data)]

cat("\nRecoded variables:\n")
for(var in existing_recoded) {
  cat("\n--- ", var, " ---\n")
  print(table(merged_data[[var]], useNA = "ifany"))
}




