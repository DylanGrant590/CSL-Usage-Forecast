## 📊 CSL Computer Lab — Forecasting Pipeline

This project develops a **modular forecasting system** to predict daily student usage of the Commuting Students’ Lounge (CSL) Computer Lab at UWI Mona.

It combines multiple statistical and simulation-based approaches to generate reliable projections and evaluate uncertainty.

---

## 🚀 Overview

The pipeline integrates three forecasting techniques:

* **ARIMA (Time Series Model)** – captures trends and seasonality
* **Linear Regression** – models usage based on time and weekday patterns
* **Monte Carlo Simulation** – estimates variability and probability outcomes

Together, these models provide both **point forecasts** and **risk-based insights**.

---

## 🧠 Key Features

* Modular, reusable functions for each model
* Multi-model comparison for robustness
* Semester-aware forecasting (excludes weekends)
* Probabilistic analysis of target outcomes
* Clean visualization using `ggplot2`

---

## 📁 Project Structure

```
forecast_pipeline.R
│
├── Data Preparation
│   ├── load_data()
│   └── get_semester_calendar()
│
├── Models
│   ├── run_arima()
│   ├── run_regression()
│   └── run_monte_carlo()
│
├── Visualization
│   ├── plot_arima()
│   └── plot_monte_carlo()
│
└── Main Pipeline
    └── run_pipeline()
```

---

## 📌 Data Description

The dataset consists of **daily computer lab visits** recorded over:

* January – March 2026
* Weekdays only (academic schedule)

Additional engineered features include:

* `day_num` – sequential day index
* `week_num` – calendar week
* `weekday dummies` – captures day-specific effects

---

## ⚙️ Models Used

### 1. ARIMA Model

* Automatically selected using `auto.arima()`
* Accounts for time-series structure and seasonality
* Produces forecast intervals (80% and 95%)

---

### 2. Linear Regression

Predictors:

* Time trend (`day_num`)
* Weekly pattern (`week_num`)
* Day-of-week effects (Mon–Thu dummies)

Used to capture **systematic patterns in lab usage**.

---

### 3. Monte Carlo Simulation

* Generates **50,000 simulated scenarios**
* Based on historical mean and variance
* Outputs:

  * Expected total usage
  * Distribution (P10, Median, P90)
  * Probability of hitting a target (e.g., 2445 visits)

---

## 📈 Example Output

```
Observed visits: XXXX
Remaining days: XX

MODEL SUMMARY
ARIMA: XXXX
LM   : XXXX
MC   : XXXX (P≥2445: XX.X%)
```

---

## 📊 Visualization

* ARIMA forecast plotted with historical data
* Monte Carlo results can be extended to histograms

---

## 🛠️ Installation & Usage

### Requirements

```r
install.packages(c("forecast", "ggplot2", "tseries", "openxlsx"))
```

### Run the Pipeline

```r
source("forecast_pipeline.R")
results <- run_pipeline()
```

---

## 🎯 Applications

* Lab resource planning
* Staff scheduling
* Capacity management
* Data-driven decision making in academic environments

---

## 📌 Future Improvements

* Add external factors (exams, deadlines, holidays)
* Implement machine learning models (e.g., Random Forest, XGBoost)
* Improve Monte Carlo assumptions (non-normal distributions)
* Deploy as a dashboard (Shiny app)

---

## 👤 Author

**Dylan Grant**
🔗 GitHub: [https://github.com/DylanGrant590](https://github.com/DylanGrant590)
