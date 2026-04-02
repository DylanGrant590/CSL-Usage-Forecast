## ============================================================
## CSL Computer Lab — Modular Forecasting Pipeline
## Models: ARIMA · Linear Regression · Monte Carlo
## ============================================================

suppressPackageStartupMessages({
  library(forecast)
  library(ggplot2)
  library(tseries)
  library(openxlsx)
})

set.seed(42)

## ────────────────────────────────────────────────────────────
## 1. DATA PREPARATION
## ────────────────────────────────────────────────────────────

load_data <- function() {

  jan <- data.frame(
    date = as.Date(c("2026-01-19","2026-01-20","2026-01-21","2026-01-22","2026-01-23",
                     "2026-01-26","2026-01-27","2026-01-28","2026-01-29","2026-01-30")),
    visits = c(26, 30, 31, 10, 7, 56, 43, 39, 4, 49)
  )

  feb <- data.frame(
    date = as.Date(c("2026-02-02","2026-02-03","2026-02-04","2026-02-05","2026-02-06",
                     "2026-02-09","2026-02-10","2026-02-11","2026-02-12","2026-02-13",
                     "2026-02-16","2026-02-19","2026-02-23","2026-02-24","2026-02-25",
                     "2026-02-26","2026-02-27")),
    visits = c(41, 57, 41, 32, 29, 39, 71, 38, 32, 29, 46, 11, 43, 30, 34, 19, 24)
  )

  mar <- data.frame(
    date = as.Date(c("2026-03-02","2026-03-03","2026-03-04","2026-03-06",
                     "2026-03-09","2026-03-10","2026-03-11","2026-03-12","2026-03-13",
                     "2026-03-16","2026-03-17","2026-03-18","2026-03-19","2026-03-20",
                     "2026-03-23","2026-03-24","2026-03-25","2026-03-26","2026-03-27",
                     "2026-03-30","2026-03-31")),
    visits = c(57, 39, 31, 20, 30, 23, 31, 26, 47, 23, 32, 29, 25, 14, 12, 29, 31, 14, 46, 19, 50)
  )

  data <- rbind(jan, feb, mar)
  data$day_num <- seq_len(nrow(data))
  data$weekday <- weekdays(data$date)

  return(data)
}

get_semester_calendar <- function(start = "2026-01-19", end = "2026-04-30") {
  all_days <- seq(as.Date(start), as.Date(end), by = "day")
  work_days <- all_days[!weekdays(all_days) %in% c("Saturday","Sunday")]
  return(work_days)
}

## ────────────────────────────────────────────────────────────
## 2. ARIMA MODEL
## ────────────────────────────────────────────────────────────

run_arima <- function(data, h) {

  ts_data <- ts(data$visits, frequency = 5)

  fit <- auto.arima(ts_data, seasonal = TRUE,
                    stepwise = FALSE, approximation = FALSE)

  fc <- forecast(fit, h = h, level = c(80, 95))

  list(
    model = fit,
    forecast = fc,
    total = sum(data$visits) + sum(pmax(fc$mean, 0)),
    lower = sum(data$visits) + sum(pmax(fc$lower[,1], 0)),
    upper = sum(data$visits) + sum(pmax(fc$upper[,1], 0))
  )
}

## ────────────────────────────────────────────────────────────
## 3. LINEAR REGRESSION
## ────────────────────────────────────────────────────────────

run_regression <- function(data, future_dates) {

  data$week_num <- as.numeric(format(data$date, "%W"))
  data$is_mon <- as.integer(weekdays(data$date) == "Monday")
  data$is_tue <- as.integer(weekdays(data$date) == "Tuesday")
  data$is_wed <- as.integer(weekdays(data$date) == "Wednesday")
  data$is_thu <- as.integer(weekdays(data$date) == "Thursday")

  fit <- lm(visits ~ day_num + week_num + is_mon + is_tue + is_wed + is_thu, data = data)

  future <- data.frame(
    date = future_dates,
    day_num = (nrow(data)+1):(nrow(data)+length(future_dates)),
    week_num = as.numeric(format(future_dates, "%W")),
    weekday = weekdays(future_dates)
  )

  future$is_mon <- as.integer(future$weekday == "Monday")
  future$is_tue <- as.integer(future$weekday == "Tuesday")
  future$is_wed <- as.integer(future$weekday == "Wednesday")
  future$is_thu <- as.integer(future$weekday == "Thursday")

  pred <- predict(fit, newdata = future, interval = "prediction")

  list(
    model = fit,
    prediction = pred,
    total = sum(data$visits) + sum(pmax(pred[,"fit"], 0))
  )
}

## ────────────────────────────────────────────────────────────
## 4. MONTE CARLO SIMULATION (NO TARGET)
## ────────────────────────────────────────────────────────────

run_monte_carlo <- function(data, future_dates, n_sim = 50000) {

  mean_vis <- mean(data$visits)
  sd_vis   <- sd(data$visits)

  sim_totals <- replicate(n_sim, {
    sim <- rnorm(length(future_dates), mean_vis, sd_vis * 0.4)
    sum(data$visits) + sum(pmax(round(sim), 0))
  })

  list(
    mean   = mean(sim_totals),
    median = median(sim_totals),
    p10    = quantile(sim_totals, 0.10),
    p90    = quantile(sim_totals, 0.90)
  )
}

## ────────────────────────────────────────────────────────────
## 5. PLOTTING
## ────────────────────────────────────────────────────────────

plot_arima <- function(data, future_dates, arima_fc) {

  df <- data.frame(
    date = c(data$date, future_dates),
    visits = c(data$visits, as.numeric(arima_fc$mean))
  )

  ggplot(df, aes(date, visits)) +
    geom_line(color="#2E86C1") +
    geom_point(data=data, aes(date, visits), color="#1B3A5C") +
    theme_minimal() +
    labs(title="ARIMA Forecast", y="Visits")
}

plot_monte_carlo <- function(sim_results) {
  print(sim_results)
}

## ────────────────────────────────────────────────────────────
## 6. MAIN PIPELINE
## ────────────────────────────────────────────────────────────

run_pipeline <- function() {

  data <- load_data()
  work_days <- get_semester_calendar()

  remaining_days <- length(work_days) - nrow(data)
  future_dates <- work_days[(nrow(data)+1):length(work_days)]

  cat(sprintf("Observed visits: %d\n", sum(data$visits)))
  cat(sprintf("Remaining days: %d\n\n", remaining_days))

  arima <- run_arima(data, remaining_days)
  lm    <- run_regression(data, future_dates)
  mc    <- run_monte_carlo(data, future_dates)

  cat("MODEL SUMMARY\n")
  cat(sprintf("ARIMA: %.0f\n", arima$total))
  cat(sprintf("LM   : %.0f\n", lm$total))
  cat(sprintf("MC Mean   : %.0f\n", mc$mean))
  cat(sprintf("MC Median : %.0f\n", mc$median))
  cat(sprintf("MC Range  : %.0f – %.0f\n", mc$p10, mc$p90))

  list(arima=arima, lm=lm, mc=mc)
}

## ────────────────────────────────────────────────────────────
## RUN
## ────────────────────────────────────────────────────────────

results <- run_pipeline()
