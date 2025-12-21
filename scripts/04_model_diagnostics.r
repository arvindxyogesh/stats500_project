# STATS 500 Project: Model Diagnostics Script
# Comprehensive model validation and assumption checking

# Load libraries
library(tidyverse)
library(broom)

cat("Loading data for model diagnostics...\n")

# Load cleaned data
clean_data <- read.csv("data/processed/micromobility_clean.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Time_of_Day = factor(Time_of_Day, 
                        levels = c("Early AM", "AM Peak", "Midday", "PM Peak", "Evening", "Late Night")),
    Day_Type = factor(Day_Type, levels = c("Weekday", "Weekend")),
    Member_Type = as.factor(Member_Type),
    Vehicle_Type = as.factor(Vehicle_Type)
  )

clean_data$Duration_Minutes <- clean_data$Duration / 60

# Use the same sample as in analysis for consistency
set.seed(500)
analysis_sample <- clean_data %>% sample_n(50000)

cat("Running model diagnostics...\n")

# Fit the main multiple regression model
model <- lm(Duration_Minutes ~ Distance + Member_Type + Vehicle_Type + Time_of_Day + Day_Type, 
            data = analysis_sample)

# 1. Normality of residuals
cat("1. Checking normality of residuals...\n")
residuals <- resid(model)
shapiro_test <- shapiro.test(sample(residuals, 5000))  # Shapiro-Wilk test (limited to 5000 obs)

cat("Shapiro-Wilk normality test p-value:", round(shapiro_test$p.value, 4), "\n")
if(shapiro_test$p.value < 0.05) {
  cat("WARNING: Residuals are not normally distributed (p < 0.05)\n")
} else {
  cat("Residuals appear normally distributed (p >= 0.05)\n")
}

# 2. Homoscedasticity (constant variance)
cat("\n2. Checking homoscedasticity...\n")
# Plot residuals vs fitted
p_homoscedasticity <- ggplot(data = analysis_sample, aes(x = fitted(model), y = resid(model))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values - Checking Homoscedasticity",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()

ggsave("output/figures/homoscedasticity_check.png", p_homoscedasticity, width = 8, height = 6, dpi = 300)

# 3. Q-Q plot for normality
cat("3. Creating Q-Q plot for normality check...\n")
p_qq <- ggplot(data = analysis_sample, aes(sample = resid(model))) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot - Checking Normality of Residuals",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

ggsave("output/figures/qq_plot.png", p_qq, width = 8, height = 6, dpi = 300)

# 4. Multicollinearity check
cat("4. Checking for multicollinearity...\n")
# For categorical variables, we can check variance inflation factors
# Simple correlation check for numeric variables
numeric_vars <- analysis_sample %>% 
  select(Distance, Duration_Minutes) %>%
  cor(use = "complete.obs")

cat("Correlation between Distance and Duration:", round(numeric_vars[1,2], 4), "\n")

# 5. Influence points (leverage)
cat("5. Checking for influential points...\n")
influence_stats <- influence.measures(model)
high_leverage <- which(apply(influence_stats$is.inf, 1, any))
cat("Number of highly influential observations:", length(high_leverage), "\n")

# Save diagnostic summary
diagnostic_summary <- data.frame(
  Test = c("Normality (Shapiro-Wilk p-value)", 
           "Highly influential observations",
           "Correlation (Distance-Duration)"),
  Result = c(round(shapiro_test$p.value, 4),
             length(high_leverage),
             round(numeric_vars[1,2], 4)),
  Interpretation = c(ifelse(shapiro_test$p.value < 0.05, 
                           "Non-normal residuals", "Normal residuals"),
                    ifelse(length(high_leverage) > 50, 
                           "Many influential points", "Few influential points"),
                    ifelse(abs(numeric_vars[1,2]) > 0.7, 
                           "High correlation", "Moderate correlation"))
)

write.csv(diagnostic_summary, "output/tables/model_diagnostics_summary.csv", row.names = FALSE)

cat("\n✅ Model diagnostics complete!\n")
cat("Diagnostic plots saved to output/figures/\n")
cat("Diagnostic summary saved to output/tables/model_diagnostics_summary.csv\n")