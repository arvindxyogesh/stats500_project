# STATS 500 Project: Enhanced Statistical Analysis
# Addressing data issues and improving models

# Load libraries
library(tidyverse)
library(broom)

cat("Loading data for enhanced analysis...\n")

# Load cleaned data
clean_data <- read.csv("data/processed/micromobility_clean.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Time_of_Day = factor(Time_of_Day, 
                        levels = c("Early AM", "AM Peak", "Midday", "PM Peak", "Evening", "Late Night")),
    Day_Type = factor(Day_Type, levels = c("Weekday", "Weekend")),
    Vehicle_Type = as.factor(Vehicle_Type),
    Member_Type = as.factor(Member_Type)
  )

# Convert duration to minutes
clean_data$Duration_Minutes <- clean_data$Duration / 60

# Take a sample for analysis
set.seed(500)
analysis_sample <- clean_data %>% sample_n(50000)

cat("Enhanced data summary:\n")
cat("Sample size:", nrow(analysis_sample), "\n")
cat("Mean trip duration:", round(mean(analysis_sample$Duration_Minutes), 2), "minutes\n")
cat("Mean trip distance:", round(mean(analysis_sample$Distance), 2), "meters\n")
cat("Vehicle types:", paste(levels(analysis_sample$Vehicle_Type), collapse = ", "), "\n")
cat("Member types:", paste(levels(analysis_sample$Member_Type), collapse = ", "), "\n\n")

## ENHANCED MODEL 1: Log-transformed response variable (addresses non-normality)
cat("--- ENHANCED MODEL 1: Log-Transformed Duration ---\n")
model_log <- lm(log(Duration_Minutes + 1) ~ Distance + Member_Type + Vehicle_Type + Time_of_Day + Day_Type, 
                data = analysis_sample)

summary_model_log <- summary(model_log)
cat("Log-transformed model R-squared:", round(summary_model_log$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary_model_log$adj.r.squared, 4), "\n")

# Key coefficients from log model
cat("\nKey coefficients (log model):\n")
coef_log <- coef(model_log)
significant_coefs <- which(summary_model_log$coefficients[,4] < 0.05)
cat("Significant predictors:", names(significant_coefs), "\n\n")

## ENHANCED MODEL 2: Focus on key research questions with robust testing
cat("--- ENHANCED MODEL 2: Core Research Questions ---\n")

# Research Question 1: Vehicle Type differences (Scooter vs Bicycle)
cat("Research Question 1: Scooter vs Bicycle Trip Durations\n")
vehicle_data <- analysis_sample %>% filter(Vehicle_Type %in% c("scooter", "bicycle"))
t_test_vehicle <- t.test(Duration_Minutes ~ Vehicle_Type, data = vehicle_data)
cat("T-test p-value:", format.pval(t_test_vehicle$p.value, digits = 4), "\n")
if(t_test_vehicle$p.value < 0.05) {
  cat("SIGNIFICANT: Scooters and bicycles have different trip durations\n")
  cat("Mean scooter duration:", round(t_test_vehicle$estimate[1], 2), "minutes\n")
  cat("Mean bicycle duration:", round(t_test_vehicle$estimate[2], 2), "minutes\n")
} else {
  cat("NOT SIGNIFICANT: No difference between scooter and bicycle trip durations\n")
}

# Research Question 2: Weekday vs Weekend patterns
cat("\nResearch Question 2: Weekday vs Weekend Patterns\n")
t_test_day <- t.test(Duration_Minutes ~ Day_Type, data = analysis_sample)
cat("T-test p-value:", format.pval(t_test_day$p.value, digits = 4), "\n")
if(t_test_day$p.value < 0.05) {
  cat("SIGNIFICANT: Weekday and weekend trips have different durations\n")
  cat("Mean weekday duration:", round(t_test_day$estimate[1], 2), "minutes\n")
  cat("Mean weekend duration:", round(t_test_day$estimate[2], 2), "minutes\n")
} else {
  cat("NOT SIGNIFICANT: No difference between weekday and weekend trip durations\n")
}

# Research Question 3: Time of Day patterns
cat("\nResearch Question 3: Time of Day Patterns\n")
time_anova <- aov(Duration_Minutes ~ Time_of_Day, data = analysis_sample)
time_summary <- summary(time_anova)
cat("ANOVA F-value:", round(time_summary[[1]]$`F value`[1], 3), "\n")
cat("ANOVA p-value:", format.pval(time_summary[[1]]$`Pr(>F)`[1], digits = 4), "\n")
if(time_summary[[1]]$`Pr(>F)`[1] < 0.05) {
  cat("SIGNIFICANT: Trip durations vary by time of day\n")
  # Post-hoc test to see which times differ
  time_means <- analysis_sample %>%
    group_by(Time_of_Day) %>%
    summarise(Mean_Duration = round(mean(Duration_Minutes), 2))
  cat("Mean durations by time:\n")
  print(time_means)
} else {
  cat("NOT SIGNIFICANT: No difference in trip durations across times of day\n")
}

## ENHANCED MODEL 3: Robust regression without Member_Type (since it's all subscribers)
cat("\n--- ENHANCED MODEL 3: Robust Model (Excluding Problematic Variables) ---\n")
model_robust <- lm(Duration_Minutes ~ Distance + Vehicle_Type + Time_of_Day + Day_Type, 
                   data = analysis_sample)

summary_robust <- summary(model_robust)
cat("Robust model R-squared:", round(summary_robust$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary_robust$adj.r.squared, 4), "\n")

# Save enhanced results
cat("\nSaving enhanced analysis results...\n")

# Create a comprehensive results table
results_table <- data.frame(
  Research_Question = c("Scooter vs Bicycle", 
                       "Weekday vs Weekend", 
                       "Time of Day Variation"),
  Test_Statistic = c(round(t_test_vehicle$statistic, 3),
                    round(t_test_day$statistic, 3),
                    round(time_summary[[1]]$`F value`[1], 3)),
  P_Value = c(format.pval(t_test_vehicle$p.value, digits = 4),
              format.pval(t_test_day$p.value, digits = 4),
              format.pval(time_summary[[1]]$`Pr(>F)`[1], digits = 4)),
  Significant = c(ifelse(t_test_vehicle$p.value < 0.05, "YES", "NO"),
                  ifelse(t_test_day$p.value < 0.05, "YES", "NO"),
                  ifelse(time_summary[[1]]$`Pr(>F)`[1] < 0.05, "YES", "NO")),
  Interpretation = c(
    paste("Scooter:", round(t_test_vehicle$estimate[1], 2), "min vs Bicycle:", round(t_test_vehicle$estimate[2], 2), "min"),
    paste("Weekday:", round(t_test_day$estimate[1], 2), "min vs Weekend:", round(t_test_day$estimate[2], 2), "min"),
    "ANOVA test for time variation"
  )
)

write.csv(results_table, "output/tables/enhanced_analysis_results.csv", row.names = FALSE)

# Save model summaries
capture.output(summary(model_log), file = "output/tables/enhanced_model_log.txt")
capture.output(summary(model_robust), file = "output/tables/enhanced_model_robust.txt")

cat("✅ Enhanced analysis complete!\n")
cat("Key research questions tested and results saved to:\n")
cat("- output/tables/enhanced_analysis_results.csv\n")
cat("- output/tables/enhanced_model_log.txt\n")
cat("- output/tables/enhanced_model_robust.txt\n")