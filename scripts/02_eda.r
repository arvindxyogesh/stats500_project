# STATS 500 Project: Exploratory Data Analysis Script
# Creates all EDA visualizations for the report

# Load libraries
library(tidyverse)
library(ggplot2)
library(patchwork) # For combining plots

cat("Loading cleaned data for EDA...\n")

# Load cleaned data
clean_data <- read.csv("data/processed/micromobility_clean.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Time_of_Day = factor(Time_of_Day, 
                        levels = c("Early AM", "AM Peak", "Midday", "PM Peak", "Evening", "Late Night")),
    Day_Type = factor(Day_Type, levels = c("Weekday", "Weekend")),
    Member_Type = as.factor(Member_Type),
    Vehicle_Type = as.factor(Vehicle_Type)
  )

cat("Creating exploratory visualizations...\n")

# 1. Distribution of Trip Duration
p1 <- ggplot(clean_data, aes(x = Duration / 60)) +  # Convert to minutes
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of Trip Duration",
       x = "Duration (minutes)", y = "Count") +
  theme_minimal()

# 2. Distribution of Trip Distance
p2 <- ggplot(clean_data, aes(x = Distance)) +
  geom_histogram(bins = 50, fill = "darkorange", alpha = 0.7) +
  labs(title = "Distribution of Trip Distance",
       x = "Distance (miles)", y = "Count") +
  theme_minimal()

# 3. Duration vs Distance scatter plot
p3 <- ggplot(clean_data, aes(x = Distance, y = Duration / 60)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Trip Duration vs Distance",
       x = "Distance (miles)", y = "Duration (minutes)") +
  theme_minimal()

# Save individual plots
ggsave("output/figures/duration_distribution.png", p1, width = 8, height = 6, dpi = 300)
ggsave("output/figures/distance_distribution.png", p2, width = 8, height = 6, dpi = 300)
ggsave("output/figures/duration_vs_distance.png", p3, width = 8, height = 6, dpi = 300)

cat("✅ Basic EDA plots saved to output/figures/\n")