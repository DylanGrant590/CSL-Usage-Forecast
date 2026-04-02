# CSL Computer Lab — Forecasting Pipeline

This project presents a modular forecasting pipeline developed in R to estimate daily usage of the CSL Computer Lab over an academic semester. The pipeline integrates multiple quantitative methods to produce reliable forecasts and capture uncertainty in future demand.

Rather than relying on a single model or predefined target, this approach emphasizes data-driven estimation using historical visit patterns.

## Objective

The primary objective of this project is to forecast total lab visits over a semester by:

* Modeling historical visit data
* Predicting future daily usage
* Comparing outputs from different forecasting techniques
* Quantifying uncertainty in projected totals

## Methodology

The pipeline applies three complementary modeling approaches:

### 1. ARIMA (Time Series Modeling)

The ARIMA model treats the data as a time series and captures temporal dependencies such as trends and short-term fluctuations. The model is automatically selected using `auto.arima()` and generates forecasts with confidence intervals.

### 2. Linear Regression (Deterministic Trends)

A multiple linear regression model is used to explain variation in visits based on:

* Sequential time trend (day number)
* Week of the year
* Day-of-week indicators (Monday to Thursday)

This approach captures systematic patterns in lab usage, particularly recurring weekday effects.

### 3. Monte Carlo Simulation (Stochastic Forecasting)

Monte Carlo simulation is used to model uncertainty by generating a large number of possible future scenarios. Simulated values are based on the historical mean and standard deviation of visits, with variability scaled to reduce extreme outcomes.

The simulation produces a distribution of total visits, summarized using:

* Mean
* Median
* Percentile range (10th to 90th)

## Project Structure

* `load_data()`
  Loads and consolidates observed visit data for January to March.

* `get_semester_calendar()`
  Generates all working days within the semester, excluding weekends.

* `run_arima()`
  Fits and forecasts using the ARIMA model.

* `run_regression()`
  Fits a regression model and predicts future visits.

* `run_monte_carlo()`
  Simulates future outcomes to estimate uncertainty.

* `run_pipeline()`
  Executes the full workflow and outputs model summaries.

## How to Run

1. Install required packages:

   ```r
   install.packages(c("forecast", "ggplot2", "tseries", "openxlsx"))
   ```

2. Run the script:

   ```r
   source("your_script_name.R")
   ```

3. Execute the pipeline:

   ```r
   results <- run_pipeline()
   ```

## Output

The pipeline provides:

* Total observed visits to date
* Number of remaining working days
* Forecasted total visits from:

  * ARIMA model
  * Linear regression model
  * Monte Carlo simulation (mean, median, and percentile range)

All results are also returned as a structured list for further analysis or visualization.

## Assumptions

* Lab usage follows patterns observable in historical data.
* Weekends are excluded from the forecasting horizon.
* Day-of-week effects are consistent over time.
* Simulated future visits follow a normal distribution with reduced variance.

## Limitations

* The dataset is relatively small, which may limit model accuracy.
* External factors (e.g., exams, holidays, system outages) are not explicitly included.
* The regression model assumes linear relationships and may not capture complex dynamics.
* Monte Carlo simulations rely on distributional assumptions that may not fully reflect real-world variability.

## Future Improvements

* Incorporate academic calendar events (e.g., exams, breaks)
* Develop an ensemble model combining all forecasts
* Add diagnostic plots and performance evaluation metrics
* Automate export of results to Excel or dashboards
* Extend the dataset to improve model robustness

This project demonstrates how combining statistical models with simulation techniques can provide a more comprehensive understanding of future outcomes, especially in settings with uncertainty and limited data.
