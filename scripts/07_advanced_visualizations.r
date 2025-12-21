# STATS 500 Project: Advanced Visualizations
# Master's level visualizations for enhanced interpretation

# Load libraries
library(tidyverse)
library(patchwork)
library(ggridges)

cat("Creating advanced visualizations...\n")

# Load cleaned data
clean_data <- read.csv("data/processed/micromobility_clean.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Time_of_Day = factor(Time_of_Day, 
                        levels = c("Early AM", "AM Peak", "Midday", "PM Peak", "Evening", "Late Night")),
    Day_Type = factor(Day_Type, levels = c("Weekday", "Weekend")),
    Vehicle_Type = as.factor(Vehicle_Type)
  )

clean_data$Duration_Minutes <- clean_data$Duration / 60

# Sample for visualization clarity
set.seed(500)
viz_sample <- clean_data %>% sample_n(10000)

# 1. INTERACTION PLOT: Vehicle Type × Time of Day
cat("Creating interaction plots...\n")

interaction_data <- viz_sample %>%
  group_by(Vehicle_Type, Time_of_Day) %>%
  summarise(
    Mean_Duration = mean(Duration_Minutes),
    SE = sd(Duration_Minutes) / sqrt(n()),
    .groups = 'drop'
  )

p1 <- ggplot(interaction_data, aes(x = Time_of_Day, y = Mean_Duration, color = Vehicle_Type, group = Vehicle_Type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_Duration - SE, ymax = Mean_Duration + SE), width = 0.1) +
  labs(title = "Interaction Plot: Trip Duration by Vehicle Type and Time of Day",
       subtitle = "Error bars represent standard errors",
       x = "Time of Day", y = "Mean Duration (minutes)", color = "Vehicle Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. RIDGELINE PLOT: Duration distributions by time of day
cat("Creating ridgeline plot...\n")

p2 <- ggplot(viz_sample, aes(x = Duration_Minutes, y = Time_of_Day, fill = Time_of_Day)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9) +
  labs(title = "Distribution of Trip Durations by Time of Day",
       x = "Duration (minutes)", y = "Time of Day") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(0, 60) # Focus on main distribution

# 3. MODEL DIAGNOSTICS ENHANCED
cat("Creating enhanced diagnostic plots...\n")

model <- lm(Duration_Minutes ~ Distance + Vehicle_Type + Time_of_Day + Day_Type, data = viz_sample)

# Residuals by predictor variables
p3 <- ggplot(viz_sample, aes(x = Distance, y = resid(model))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Distance - Checking Linearity",
       x = "Distance (meters)", y = "Residuals") +
  theme_minimal()

# 4. EFFECT SIZE VISUALIZATION
cat("Creating effect size visualization...\n")

vehicle_effect <- viz_sample %>%
  filter(Vehicle_Type %in% c("scooter", "bicycle")) %>%
  group_by(Vehicle_Type) %>%
  summarise(
    Mean_Duration = mean(Duration_Minutes),
    CI_lower = Mean_Duration - 1.96 * sd(Duration_Minutes) / sqrt(n()),
    CI_upper = Mean_Duration + 1.96 * sd(Duration_Minutes) / sqrt(n())
  )

p4 <- ggplot(vehicle_effect, aes(x = Vehicle_Type, y = Mean_Duration, fill = Vehicle_Type)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  labs(title = "Vehicle Type Effect on Trip Duration",
       subtitle = "Error bars represent 95% confidence intervals",
       x = "Vehicle Type", y = "Mean Duration (minutes)") +
  theme_minimal() +
  theme(legend.position = "none")

# Save advanced visualizations
ggsave("output/figures/advanced_interaction_plot.png", p1, width = 10, height = 6, dpi = 300)
ggsave("output/figures/advanced_ridgeline_plot.png", p2, width = 10, height = 6, dpi = 300)
ggsave("output/figures/advanced_residuals_plot.png", p3, width = 10, height = 6, dpi = 300)
ggsave("output/figures/advanced_effect_plot.png", p4, width = 8, height = 6, dpi = 300)

cat("✅ Advanced visualizations created!\n")
cat("Saved to output/figures/ with 'advanced_' prefix\n")