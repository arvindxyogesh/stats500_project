# STATS 500 Project: Statistical Analysis Script
# Core regression modeling and hypothesis testing

# Load libraries
library(tidyverse)
library(broom)  # For tidy regression output
library(stargazer)  # For publication-ready regression tables

cat("Loading cleaned data for statistical analysis...\n")

# Load cleaned data
clean_data <- read.csv("data/processed/micromobility_clean.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Time_of_Day = factor(Time_of_Day, 
                        levels = c("Early AM", "AM Peak", "Midday", "PM Peak", "Evening", "Late Night")),
    Day_Type = factor(Day_Type, levels = c("Weekday", "Weekend")),
    Member_Type = as.factor(Member_Type),
    Vehicle_Type = as.factor(Vehicle_Type)
  )

# Convert duration to minutes for better interpretation
clean_data$Duration_Minutes <- clean_data$Duration / 60

cat("Starting statistical analysis...\n")

# For computational efficiency, let's take a systematic sample of 50,000 observations
set.seed(500)  # For reproducibility
analysis_sample <- clean_data %>% 
  sample_n(50000)

cat("Analysis sample dimensions:", dim(analysis_sample), "\n\n")

## MODEL 1: Simple Linear Regression (Duration vs Distance)
cat("--- MODEL 1: Simple Linear Regression ---\n")
model1 <- lm(Duration_Minutes ~ Distance, data = analysis_sample)
summary_model1 <- summary(model1)

cat("R-squared:", round(summary_model1$r.squared, 4), "\n")
cat("Distance coefficient:", round(coef(model1)["Distance"], 4), "\n")
cat("Interpretation: Each additional meter adds", round(coef(model1)["Distance"], 4), "minutes to trip duration\n\n")

## MODEL 2: Multiple Regression
cat("--- MODEL 2: Multiple Regression ---\n")
model2 <- lm(Duration_Minutes ~ Distance + Member_Type + Vehicle_Type + Time_of_Day + Day_Type, 
             data = analysis_sample)
summary_model2 <- summary(model2)

cat("Multiple R-squared:", round(summary_model2$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary_model2$adj.r.squared, 4), "\n\n")

# Key findings from Model 2
cat("Key findings from Multiple Regression:\n")
coefficients_model2 <- coef(model2)
significant_vars <- names(which(summary_model2$coefficients[,4] < 0.05))
cat("Significant predictors (p < 0.05):", paste(significant_vars, collapse = ", "), "\n\n")

## MODEL 3: Interaction Model
cat("--- MODEL 3: Interaction Model ---\n")
model3 <- lm(Duration_Minutes ~ Distance + Member_Type * Time_of_Day + Vehicle_Type + Day_Type, 
             data = analysis_sample)
summary_model3 <- summary(model3)

cat("Interaction Model R-squared:", round(summary_model3$r.squared, 4), "\n")

# Check if interaction terms are significant
interaction_terms <- grep(":", names(coef(model3)), value = TRUE)
if(length(interaction_terms) > 0) {
  cat("Interaction terms:", paste(interaction_terms, collapse = ", "), "\n")
  interaction_pvalues <- summary_model3$coefficients[interaction_terms, 4]
  significant_interactions <- names(which(interaction_pvalues < 0.05))
  if(length(significant_interactions) > 0) {
    cat("Significant interactions:", paste(significant_interactions, collapse = ", "), "\n")
  } else {
    cat("No significant interactions found.\n")
  }
}

## Save regression results
cat("\nSaving regression results...\n")

# Save model summaries as text files
capture.output(summary(model1), file = "output/tables/model1_simple_regression.txt")
capture.output(summary(model2), file = "output/tables/model2_multiple_regression.txt")
capture.output(summary(model3), file = "output/tables/model3_interaction_regression.txt")

# Create a nice regression table for the report
stargazer(model1, model2, model3,
          type = "html",
          out = "output/tables/regression_results.html",
          title = "Regression Models Predicting Trip Duration",
          align = TRUE,
          dep.var.labels = "Trip Duration (minutes)",
          covariate.labels = c("Distance", "Member Type: Subscriber", 
                              "Vehicle Type: Scooter", "Time: AM Peak", "Time: Midday",
                              "Time: PM Peak", "Time: Evening", "Time: Late Night",
                              "Day Type: Weekend", "Subscriber × AM Peak", 
                              "Subscriber × Midday", "Subscriber × PM Peak",
                              "Subscriber × Evening", "Subscriber × Late Night"),
          notes = "Reference categories: Casual Member, Bicycle, Early AM Time, Weekday")

## Create diagnostic plots
cat("Creating diagnostic plots...\n")

# Residuals vs Fitted for Model 2
p_diagnostics <- ggplot(data = analysis_sample, aes(x = fitted(model2), y = resid(model2))) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values (Model 2)",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()

ggsave("output/figures/residuals_plot.png", p_diagnostics, width = 8, height = 6, dpi = 300)

## Summary statistics for report
cat("\n--- SUMMARY STATISTICS ---\n")
cat("Mean trip duration:", round(mean(analysis_sample$Duration_Minutes, na.rm = TRUE), 2), "minutes\n")
cat("Mean trip distance:", round(mean(analysis_sample$Distance, na.rm = TRUE), 2), "meters\n")
cat("Proportion of subscribers:", round(mean(analysis_sample$Member_Type == "Subscriber"), 3), "\n")
cat("Proportion of scooters:", round(mean(analysis_sample$Vehicle_Type == "scooter"), 3), "\n")

cat("\n✅ Statistical analysis complete!\n")
cat("Results saved to:\n")
cat("- output/tables/model1_simple_regression.txt\n")
cat("- output/tables/model2_multiple_regression.txt\n") 
cat("- output/tables/model3_interaction_regression.txt\n")
cat("- output/tables/regression_results.html\n")
cat("- output/figures/residuals_plot.png\n")