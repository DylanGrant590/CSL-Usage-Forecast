# csl-usage-forecast

Forecasting computer lab visit patterns at UWI's Commuting Students' Lounge using ARIMA, linear regression, and Monte Carlo simulation in R.

## Overview

This project analyses sign-in log data from the CSL computer lab (January–February 2026) to forecast facility usage for the remainder of Semester 2. The goal is to project whether the lab will reach 2,445 visits by semester end and identify peak periods to guide resource allocation.

## Data

- **January 2026:** 295 visits across 11 tracked days (Jan 19–31)
- **February 2026:** 616 visits across 17 tracked days (Feb 2–27)
- **Combined:** 911 visits over 28 working days

Hourly sign-in distribution data from February is used to model intraday demand patterns.

## Models

**ARIMA**
Fits a seasonal ARIMA model to the daily visit time series using `auto.arima()`. Captures the weekly usage rhythm and projects forward across the remaining 46 working days of the semester.

**Linear Regression**
Day-of-week model regressing daily visits on a time trend and weekday indicator variables. R² = 0.733, confirming that weekday effects are the primary driver of visit volume.

**Monte Carlo Simulation**
Runs 50,000 simulations of the remaining semester days. Each simulation draws daily visit counts from a normal distribution anchored to observed data, adjusted by day-of-week multipliers. Produces a full probability distribution over possible semester totals.

## Results

| Model | Semester Projection |
|---|---|
| ARIMA | 2,408 visits |
| Linear Regression | 3,605 visits |
| Monte Carlo (mean) | 2,456 visits |
| Monte Carlo (median) | 2,455 visits |

**P(reaching 2,445 visits) = 59.8%**

The Monte Carlo estimate is the most reliable — the linear regression skews high due to mid-semester spike days inflating the trend.

## Resource Allocation

Based on a target of 33 average visits/day and observed hourly distributions:

| Time Window | Load | Recommended Staffing |
|---|---|---|
| 07:00–09:00 | Low | 1 staff |
| 09:00–11:00 | Moderate | 2 staff |
| 11:00–14:00 | **Peak** | **3 staff** |
| 14:00–16:00 | Moderate | 2 staff |
| 16:00–19:00 | Low | 1 staff |

Peak hour is 12:00–13:00, accounting for roughly 17% of all daily sign-ins.

## Output

The script produces:
- 4 plots (ARIMA forecast, Monte Carlo distribution, hourly resource profile, regression fit)
- A formatted 6-sheet Excel workbook with day-by-day projections, model coefficients, Monte Carlo statistics, and the full resource allocation table

## Requirements

```r
install.packages(c("forecast", "ggplot2", "tseries", "openxlsx"))
```

## Usage

```r
Rscript csl_forecast.R
```

## File Structure

```
csl-usage-forecast/
├── csl_forecast.R       # Main analysis script
└── README.md
```
