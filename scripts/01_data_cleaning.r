# STATS 500 Project: Data Cleaning Script
# Loads existing CSV file and cleans data

# Load libraries
library(tidyverse)
library(lubridate)

# Create directories if they don't exist
dirs_to_create <- c("data/processed", "output/figures", "output/tables")
for(dir in dirs_to_create) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

cat("Loading existing micromobility data...\n")

# Load your downloaded CSV file
micromobility_raw <- read.csv("data/raw/micromobility_raw.csv", stringsAsFactors = FALSE)

cat("Raw data dimensions:", dim(micromobility_raw), "\n")

# Data cleaning pipeline
clean_data <- micromobility_raw %>%
  # Select and rename relevant columns based on your CSV structure
  select(
    Trip_ID = ID,
    Device_ID = Device.ID,
    Vehicle_Type = Vehicle.Type,
    Duration = Trip.Duration,
    Distance = Trip.Distance,
    Start_Time = Start.Time,
    End_Time = End.Time,
    Hour = Hour,
    Day_of_Week = Day.of.Week,
    Month = Month,
    Year = Year..US.Central.
  ) %>%
  
  # Clean numeric columns - remove commas and convert to numeric
  mutate(
    Duration = as.numeric(gsub(",", "", Duration)),
    Distance = as.numeric(gsub(",", "", Distance))
  ) %>%
  
  # Convert to proper data types and create features
  mutate(
    Start_Time = as.POSIXct(Start_Time, format = "%Y %b %d %I:%M:%S %p"),
    
    # Create time of day bins using existing Hour column
    Time_of_Day = case_when(
      Hour >= 5 & Hour < 7 ~ "Early AM",
      Hour >= 7 & Hour < 10 ~ "AM Peak", 
      Hour >= 10 & Hour < 16 ~ "Midday",
      Hour >= 16 & Hour < 19 ~ "PM Peak",
      Hour >= 19 & Hour < 23 ~ "Evening",
      TRUE ~ "Late Night"
    ),
    
    # Create day type from Day_of_Week (1=Sunday, 7=Saturday typically)
    Day_Type = if_else(Day_of_Week %in% c(1, 7), "Weekend", "Weekday"),
    
    # Create Member_Type (we'll use Device_ID as proxy - devices with many trips = subscribers)
    Trip_Count = ave(Trip_ID, Device_ID, FUN = length),
    Member_Type = if_else(Trip_Count > 10, "Subscriber", "Casual")
  ) %>%
  
  # Filter out invalid records
  drop_na(Duration, Distance) %>%
  filter(
    Duration > 60 & Duration < 24 * 3600,    # 1 minute to 24 hours
    Distance > 0 & Distance < 50000,         # Positive distance (in meters? adjust as needed)
    !is.na(Time_of_Day)
  ) %>%
  
  # Convert to factors with meaningful order
  mutate(
    Time_of_Day = factor(Time_of_Day, 
                        levels = c("Early AM", "AM Peak", "Midday", "PM Peak", "Evening", "Late Night")),
    Day_Type = factor(Day_Type, levels = c("Weekday", "Weekend")),
    Member_Type = as.factor(Member_Type),
    Vehicle_Type = as.factor(Vehicle_Type)
  ) %>%
  
  # Select final columns for analysis
  select(Trip_ID, Duration, Distance, Vehicle_Type, Member_Type, 
         Time_of_Day, Day_Type, Hour, Day_of_Week, Month, Year)

# Save cleaned data
write.csv(clean_data, "data/processed/micromobility_clean.csv", row.names = FALSE)

cat("✅ Data cleaning complete!\n")
cat("Cleaned data dimensions:", dim(clean_data), "\n")
cat("Cleaned data saved to: data/processed/micromobility_clean.csv\n")

# Print summary of cleaning process
original_rows <- nrow(micromobility_raw)
cleaned_rows <- nrow(clean_data)
removed_rows <- original_rows - cleaned_rows

cat("\n--- Data Cleaning Summary ---\n")
cat("Original number of rows:", original_rows, "\n")
cat("Rows removed during cleaning:", removed_rows, "\n")
cat("Percentage of data kept:", round(cleaned_rows/original_rows * 100, 2), "%\n")

# Show structure of cleaned data
cat("\nStructure of cleaned data:\n")
glimpse(clean_data)

# Basic summary
cat("\nBasic summary of cleaned data:\n")
summary(clean_data %>% select(Duration, Distance, Vehicle_Type, Member_Type, Time_of_Day, Day_Type))