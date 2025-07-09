# =============================================================================
# 4. RELIABILITY ANALYSIS - Age 11 Risk-Taking Variables
# =============================================================================

# Prepare data
risk_data <- merged_data[, existing_recoded, drop = FALSE]
risk_data_clean <- risk_data[rowSums(!is.na(risk_data)) > 0, ]

cat("\nReliability Analysis - All Variables:")
cat("\nVariables:", ncol(risk_data_clean))
cat("\nObservations:", nrow(risk_data_clean), "\n")

# Cronbach's alpha
alpha_all <- alpha(risk_data_clean, check.keys = TRUE)
cat("\nCronbach's Alpha:", round(alpha_all$total$std.alpha, 3))
cat("\nItems:", alpha_all$nvar)
cat("\nN:", alpha_all$n.obs, "\n")

# Display key item statistics
cat("Available columns:", paste(names(alpha_all$item.stats), collapse = ", "), "\n")
print(round(alpha_all$item.stats, 3))

# =============================================================================
# 5. SIMPLE CORRELATION AND FACTOR ANALYSIS - Age 11 Risk-Taking Variables
# =============================================================================

# Prepare data for analysis
efa_data <- na.omit(risk_data_clean)
cat("\nSimple Analysis - All Variables:")
cat("\nSample size:", nrow(efa_data))
cat("\nVariables:", ncol(efa_data), "\n")

# Correlation matrix
cat("\nCorrelation Matrix:\n")
print(round(cor(efa_data), 3))

# Simple 1-factor analysis
cat("\n--- 1-Factor Solution ---\n")
fa1 <- fa(efa_data, nfactors = 1, rotate = "none", fm = "pa")
print(round(fa1$loadings, 3))
cat("Variance explained:", round(fa1$Vaccounted[2], 3), "\n")

# Simple 2-factor analysis
cat("\n--- 2-Factor Solution ---\n")
fa2 <- fa(efa_data, nfactors = 2, rotate = "varimax", fm = "pa")
print(round(fa2$loadings, 3))
cat("Variance explained:", round(fa2$Vaccounted[2, ], 3), "\n")

# =============================================================================
# 6. ANALYSIS WITHOUT CGTRISKT
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("ANALYSIS WITHOUT CGTRISKT\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Prepare data without cgtriskt
vars_no_cgtriskt <- c("risk5_sch", "risk5_rude", "risk5_lie", "risk5_obe", "risk5_wpp")
risk_data_no_cgt <- merged_data[, vars_no_cgtriskt, drop = FALSE]
risk_data_clean_no_cgt <- risk_data_no_cgt[rowSums(!is.na(risk_data_no_cgt)) > 0, ]

cat("\nVariables:", ncol(risk_data_clean_no_cgt))
cat("\nObservations:", nrow(risk_data_clean_no_cgt), "\n")

# =============================================================================
# 7. RELIABILITY ANALYSIS - WITHOUT CGTRISKT
# =============================================================================

# Cronbach's alpha
alpha_no_cgt <- alpha(risk_data_clean_no_cgt, check.keys = TRUE)
cat("\nCronbach's Alpha:", round(alpha_no_cgt$total$std.alpha, 3))
cat("\nItems:", alpha_no_cgt$nvar)
cat("\nN:", alpha_no_cgt$n.obs, "\n")

print(round(alpha_no_cgt$item.stats, 3))

# =============================================================================
# 8. SIMPLE CORRELATION AND FACTOR ANALYSIS - WITHOUT CGTRISKT
# =============================================================================

# Prepare data for analysis
efa_data_no_cgt <- na.omit(risk_data_clean_no_cgt)
cat("\nSimple Analysis - Without CGTRISKT:")
cat("\nSample size:", nrow(efa_data_no_cgt))
cat("\nVariables:", ncol(efa_data_no_cgt), "\n")

# Correlation matrix
cat("\nCorrelation Matrix:\n")
print(round(cor(efa_data_no_cgt), 3))

# Simple 1-factor analysis
cat("\n--- 1-Factor Solution ---\n")
fa1_no_cgt <- fa(efa_data_no_cgt, nfactors = 1, rotate = "none", fm = "pa")
print(round(fa1_no_cgt$loadings, 3))
cat("Variance explained:", round(fa1_no_cgt$Vaccounted[2], 3), "\n")

# Simple 2-factor analysis
cat("\n--- 2-Factor Solution ---\n")
fa2_no_cgt <- fa(efa_data_no_cgt, nfactors = 2, rotate = "varimax", fm = "pa")
print(round(fa2_no_cgt$loadings, 3))
cat("Variance explained:", round(fa2_no_cgt$Vaccounted[2, ], 3), "\n")

# =============================================================================
# 9. SIMPLE COMPARISON SUMMARY
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("SUMMARY COMPARISON\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Compare alphas
cat("\nCronbach's Alpha Comparison:")
cat("\nWith cgtriskt:   ", round(alpha_all$total$std.alpha, 3))
cat("\nWithout cgtriskt:", round(alpha_no_cgt$total$std.alpha, 3))
cat("\nChange:          ", round(alpha_no_cgt$total$std.alpha - alpha_all$total$std.alpha, 3), "\n")

cat("\nRecommendation: Use analysis WITHOUT cgtriskt for better psychometric properties\n")

