#  Analyzing Urban Micro-Mobility Trip Duration Determinants

**Statistical Analysis of Trip Duration Across Micro-Mobility Modes**

---

##  Project Summary
This project analyzes **trip duration determinants in urban micro-mobility systems** (bicycles, scooters, mopeds, and cars) using **50,000 real-world trips**.  
The study focuses on how **distance, vehicle type, time of day, and day type** jointly influence travel time, with emphasis on **model diagnostics and statistical validity**.

---

##  Objectives
- Quantify the relationship between **trip distance and duration**
- Compare **vehicle efficiency** after controlling for distance
- Analyze **time-of-day and weekday/weekend effects**
- Evaluate **linear vs. log-transformed regression models**
- Demonstrate best practices in **regression diagnostics**

---

##  Dataset
- **Observations:** 50,000 trips  
- **Modes:** Bicycle, Scooter, Moped, Car  
- **Key Variables:**
  - Trip duration (minutes)
  - Trip distance (meters)
  - Vehicle type
  - Time of day (6 categories)
  - Day type (weekday / weekend)
  - Membership status

The data exhibits **strong right-skewness and influential outliers**, typical of transportation datasets.

---

##  Methodology
### 1. Exploratory Analysis
- Distribution analysis of duration and distance
- Visualization of duration–distance relationships

### 2. Statistical Models
- Simple Linear Regression  
- Multiple Regression with modal and temporal controls  
- **Log-Transformed Regression**: `log(Duration + 1)`

### 3. Model Diagnostics
- Shapiro–Wilk test
- Residual vs. fitted plots
- Q–Q plots
- Cook’s distance
- Bootstrap validation (1,000 resamples)

---

##  Key Findings
- **Distance** is the dominant predictor of trip duration
- After controlling for distance:
  - **Scooters and mopeds are faster than bicycles**
  - **AM peak trips are slower**, **midday trips are fastest**
  - **Weekend trips are shorter than weekday trips**
- **Log-transformed models** significantly improve fit and interpretability
- Raw averages are misleading without distance control

---

##  Tools & Techniques
- **Language:** R  
- **Methods:**  
  - Linear & multivariate regression  
  - Log transformation  
  - Bootstrap resampling  
  - ANOVA  
- **Visualization:** Residual diagnostics, interaction plots

---

##  Repository Structure
```text
.
├── analysis/
├── figures/
├── report/
│   └── Stats_500_project_report.pdf
├── README.md
```


##  Practical Implications

Supports dynamic pricing and fleet allocation

Informs urban transportation planning

Demonstrates diagnostic-driven statistical modeling


##  Limitations & Future Work

Single-city dataset

No weather or traffic data

Future work:

Cross-city analysis

Infrastructure and environmental variables

Nonlinear ML models


##  Author

Arvind Yogesh Suresh Babu
Master’s in Data Science
University of Michigan, Ann Arbor



