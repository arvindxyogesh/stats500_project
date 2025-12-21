# STATS 500 Project: Statistical Enhancements
# Advanced statistical techniques for Master's level analysis

# Load libraries
library(tidyverse)
library(broom)
library(boot)

cat("Loading data for enhanced statistical analysis...\n")

# Load cleaned data
clean_data <- read.csv("data/processed/micromobility_clean.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Time_of_Day = factor(Time_of_Day, 
                        levels = c("Early AM", "AM Peak", "Midday", "PM Peak", "Evening", "Late Night")),
    Day_Type = factor(Day_Type, levels = c("Weekday", "Weekend")),
    Vehicle_Type = as.factor(Vehicle_Type),
    Member_Type = as.factor(Member_Type)
  )

clean_data$Duration_Minutes <- clean_data$Duration / 60

# Use the same sample for consistency
set.seed(500)
analysis_sample <- clean_data %>% sample_n(50000)

cat("Running enhanced statistical analyses...\n")

# 1. BOOTSTRAPPING FOR CONFIDENCE INTERVALS
cat("--- BOOTSTRAPPING CONFIDENCE INTERVALS ---\n")

# Bootstrap function for distance coefficient
boot_function <- function(data, indices) {
  sample_data <- data[indices, ]
  model <- lm(Duration_Minutes ~ Distance, data = sample_data)
  return(coef(model)["Distance"])
}

# Run bootstrap
boot_results <- boot(analysis_sample, boot_function, R = 1000)
boot_ci <- boot.ci(boot_results, type = "perc")

cat("Bootstrap Results for Distance Coefficient:\n")
cat("Original coefficient:", round(coef(lm(Duration_Minutes ~ Distance, data = analysis_sample))["Distance"], 5), "\n")
cat("Bootstrap 95% CI: [", round(boot_ci$percent[4], 5), ", ", round(boot_ci$percent[5], 5), "]\n")
cat("Bootstrap SE:", round(sd(boot_results$t), 5), "\n\n")

# 2. MODEL COMPARISON WITH AIC/BIC
cat("--- MODEL COMPARISON WITH AIC/BIC ---\n")

# Fit competing models
model_simple <- lm(Duration_Minutes ~ Distance, data = analysis_sample)
model_multiple <- lm(Duration_Minutes ~ Distance + Vehicle_Type + Time_of_Day + Day_Type, 
                     data = analysis_sample)
model_log <- lm(log(Duration_Minutes + 1) ~ Distance + Vehicle_Type + Time_of_Day + Day_Type, 
                data = analysis_sample)

# Create model comparison table
model_comparison <- data.frame(
  Model = c("Simple (Distance only)", "Multiple Regression", "Log-Transformed"),
  R_squared = c(summary(model_simple)$r.squared, 
                summary(model_multiple)$r.squared,
                summary(model_log)$r.squared),
  Adj_R_squared = c(summary(model_simple)$adj.r.squared,
                   summary(model_multiple)$adj.r.squared,
                   summary(model_log)$adj.r.squared),
  AIC = c(AIC(model_simple), AIC(model_multiple), AIC(model_log)),
  BIC = c(BIC(model_simple), BIC(model_multiple), BIC(model_log))
)

print(model_comparison)
cat("\n")

# 3. ROBUST REGRESSION FOR OUTLIER HANDLING
cat("--- ROBUST REGRESSION ANALYSIS ---\n")
library(MASS)

robust_model <- rlm(Duration_Minutes ~ Distance + Vehicle_Type + Time_of_Day + Day_Type,
                    data = analysis_sample)

cat("Robust Regression Results:\n")
cat("Robust R-squared equivalent:", 1 - (var(robust_model$residuals) / var(analysis_sample$Duration_Minutes)), "\n")

# Compare coefficients
cat("\nComparison of Key Coefficients:\n")
cat("Vehicle Type (Scooter) - OLS vs Robust:\n")
cat("OLS:", round(coef(model_multiple)["Vehicle_Typescooter"], 4), "\n")
cat("Robust:", round(coef(robust_model)["Vehicle_Typescooter"], 4), "\n")

# 4. CROSS-VALIDATION FOR MODEL PERFORMANCE
cat("--- CROSS-VALIDATION ASSESSMENT ---\n")
library(caret)

set.seed(500)
train_control <- trainControl(method = "cv", number = 5)

# CV for multiple regression model
cv_model <- train(Duration_Minutes ~ Distance + Vehicle_Type + Time_of_Day + Day_Type,
                  data = analysis_sample,
                  method = "lm",
                  trControl = train_control)

cat("Cross-Validation Results:\n")
cat("CV R-squared:", round(mean(cv_model$resample$Rsquared), 4), "\n")
cat("CV RMSE:", round(mean(cv_model$resample$RMSE), 4), "\n\n")

# 5. EFFECT SIZE CALCULATIONS
cat("--- EFFECT SIZE ANALYSIS ---\n")

# Cohen's d for vehicle type comparison
vehicle_comparison <- analysis_sample %>% filter(Vehicle_Type %in% c("scooter", "bicycle"))
scooter_duration <- vehicle_comparison$Duration_Minutes[vehicle_comparison$Vehicle_Type == "scooter"]
bicycle_duration <- vehicle_comparison$Duration_Minutes[vehicle_comparison$Vehicle_Type == "bicycle"]

cohens_d <- (mean(scooter_duration) - mean(bicycle_duration)) / 
  sqrt((sd(scooter_duration)^2 + sd(bicycle_duration)^2) / 2)

cat("Effect Sizes:\n")
cat("Scooter vs Bicycle - Cohen's d:", round(cohens_d, 3), "\n")
cat("Interpretation:", ifelse(abs(cohens_d) < 0.2, "Negligible", 
                             ifelse(abs(cohens_d) < 0.5, "Small",
                                    ifelse(abs(cohens_d) < 0.8, "Medium", "Large"))), "effect size\n")

# Save enhanced results
cat("\nSaving enhanced statistical results...\n")

write.csv(model_comparison, "output/tables/model_comparison_aic_bic.csv", row.names = FALSE)

# Bootstrap results
bootstrap_summary <- data.frame(
  Statistic = c("Original Coefficient", "Bootstrap Mean", "Bootstrap SE", "95% CI Lower", "95% CI Upper"),
  Value = c(round(coef(lm(Duration_Minutes ~ Distance, data = analysis_sample))["Distance"], 5),
            round(mean(boot_results$t), 5),
            round(sd(boot_results$t), 5),
            round(boot_ci$percent[4], 5),
            round(boot_ci$percent[5], 5))
)

write.csv(bootstrap_summary, "output/tables/bootstrap_results.csv", row.names = FALSE)

# Effect sizes
effect_sizes <- data.frame(
  Comparison = c("Scooter vs Bicycle"),
  Cohens_d = round(cohens_d, 3),
  Interpretation = ifelse(abs(cohens_d) < 0.2, "Negligible", 
                         ifelse(abs(cohens_d) < 0.5, "Small",
                                ifelse(abs(cohens_d) < 0.8, "Medium", "Large")))
)

write.csv(effect_sizes, "output/tables/effect_sizes.csv", row.names = FALSE)

cat("✅ Enhanced statistical analysis complete!\n")
cat("Results saved to:\n")
cat("- output/tables/model_comparison_aic_bic.csv\n")
cat("- output/tables/bootstrap_results.csv\n")
cat("- output/tables/effect_sizes.csv\n")