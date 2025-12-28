Analyzing Urban Micro-Mobility Trip Duration Determinants
Project Overview

This project investigates the factors that influence trip duration in urban micro-mobility systems, including bicycles, scooters, mopeds, and cars. Using a dataset of 50,000 real-world trips, the study examines how distance, vehicle type, time of day, and day type jointly shape travel time.

The project emphasizes statistical rigor, model diagnostics, and interpretability, demonstrating why log-transformed regression models are more appropriate for skewed, heteroscedastic transportation data.

Objectives

Quantify the relationship between trip distance and duration

Compare travel efficiency across vehicle types after controlling for distance

Analyze temporal effects (time of day, weekday vs. weekend)

Evaluate and compare linear, multivariate, and log-transformed regression models

Demonstrate best practices in model diagnostics and validation

Dataset

Size: 50,000 micro-mobility trips

Key Variables:

Trip duration (minutes)

Trip distance (meters)

Vehicle type (bicycle, scooter, moped, car)

Time of day (Early AM, AM Peak, Midday, PM Peak, Evening, Late Night)

Day type (weekday/weekend)

Membership status (subscriber/casual)

The dataset exhibits strong right-skewness and outliers, typical of mobility data.

Methodology

Exploratory Data Analysis

Distribution analysis of trip duration and distance

Visualization of duration–distance relationship

Statistical Modeling

Simple linear regression (Duration ~ Distance)

Multiple regression with modal and temporal controls

Log-transformed regression: log(Duration + 1)

Model Diagnostics

Shapiro–Wilk test for normality

Residual vs. fitted plots

Q–Q plots

Cook’s distance for influential observations

Model Evaluation

R² and Adjusted R²

AIC and BIC comparison

Bootstrap validation (1,000 resamples)

Key Findings

Distance is the strongest predictor of trip duration across all models

After controlling for distance:

Scooters and mopeds are more time-efficient than bicycles

AM Peak trips are slower, while midday trips are fastest

Weekend trips are shorter than weekday trips

Log-transformed models substantially improve model fit and residual behavior

Unadjusted mean comparisons can be misleading without controlling for distance

Tools & Technologies

Programming Language: R

Statistical Techniques:

Linear and multivariate regression

Log transformation

Bootstrap resampling

ANOVA and hypothesis testing

Visualization: Distribution plots, interaction plots, residual diagnostics

Repository Structure
.
├── data/                # (Optional) Raw or processed datasets
├── analysis/            # Regression models and diagnostics
├── figures/             # Plots and visualizations
├── report/
│   └── Stats_500_project_report.pdf
├── README.md

Results & Implications

Highlights the importance of diagnostic-driven model selection

Provides actionable insights for:

Micro-mobility operators (pricing, fleet allocation)

Urban planners (peak management, infrastructure planning)

Demonstrates how statistical adjustment changes conclusions about mode efficiency

Limitations & Future Work

Single-city dataset limits generalizability

Does not include weather, traffic, or infrastructure quality

Future work could incorporate:

Cross-city comparisons

High-resolution temporal data

Machine learning models for nonlinear effects

Author

Arvind Yogesh Suresh Babu
Master’s Student, Data Science
University of Michigan, Ann Arbor
