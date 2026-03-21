## ============================================================
##  CSL Computer Lab — Predictive Modelling & Demand Forecast
##  Models: ARIMA · Linear Regression · Monte Carlo Simulation
##  Target: 2,445 semester visits | Semester: Jan–Apr 2026
## ============================================================

suppressPackageStartupMessages({
  library(forecast)
  library(ggplot2)
  library(tseries)
  library(openxlsx)
})

set.seed(42)

## ─── 0. RAW DATA ──────────────────────────────────────────────────────────────

jan_data <- data.frame(
  date  = as.Date(c("2026-01-19","2026-01-20","2026-01-21","2026-01-22","2026-01-23",
                     "2026-01-26","2026-01-27","2026-01-28","2026-01-29","2026-01-30","2026-01-31")),
  visits = c(26, 30, 31, 10, 7, 56, 43, 39, 4, 24, 25)
)

feb_data <- data.frame(
  date  = as.Date(c("2026-02-02","2026-02-03","2026-02-04","2026-02-05","2026-02-06",
                     "2026-02-09","2026-02-10","2026-02-11","2026-02-12","2026-02-13",
                     "2026-02-16","2026-02-19","2026-02-23","2026-02-24","2026-02-25",
                     "2026-02-26","2026-02-27")),
  visits = c(41, 57, 41, 32, 29, 39, 71, 38, 32, 29, 46, 11, 43, 30, 34, 19, 24)
)

hourly_data <- data.frame(
  hour     = 7:18,
  slot     = c("07-08","08-09","09-10","10-11","11-12","12-13",
               "13-14","14-15","15-16","16-17","17-18","18-19"),
  signins  = c(1, 37, 89, 90, 104, 106, 70, 38, 36, 20, 18, 3),
  category = c("Early Morning","Morning","Morning","Late Morning","Late Morning",
                "Lunchtime","Early Afternoon","Afternoon","Afternoon",
                "Late Afternoon","Evening","Evening")
)

## Combined observed series
all_data <- rbind(jan_data, feb_data)
all_data$day_num  <- seq_len(nrow(all_data))
all_data$weekday  <- weekdays(all_data$date)
all_data$is_weekend <- all_data$weekday %in% c("Saturday","Sunday")

## ─── 1. SEMESTER CALENDAR SETUP ───────────────────────────────────────────────
# UWI Semester 2 approx: Jan 19 – Apr 30, 2026
# Count working (Mon–Fri) days
sem_start <- as.Date("2026-01-19")
sem_end   <- as.Date("2026-04-30")
all_sem_days <- seq(sem_start, sem_end, by = "day")
work_days    <- all_sem_days[!weekdays(all_sem_days) %in% c("Saturday","Sunday")]
n_work_days  <- length(work_days)

cat(sprintf("Semester working days (Mon–Fri): %d\n", n_work_days))
cat(sprintf("Observed days so far: %d  |  Observed visits: %d\n",
            nrow(all_data), sum(all_data$visits)))

## ─── 2. ARIMA MODEL ───────────────────────────────────────────────────────────

ts_visits <- ts(all_data$visits, frequency = 5)   # weekly seasonality
adf_result <- adf.test(ts_visits, alternative = "stationary")
cat(sprintf("\nADF p-value: %.4f (%s)\n", adf_result$p.value,
            ifelse(adf_result$p.value < 0.05, "stationary", "non-stationary")))

arima_fit  <- auto.arima(ts_visits, seasonal = TRUE, stepwise = FALSE,
                          approximation = FALSE, trace = FALSE)
cat("\nARIMA order selected:", arima_fit$arma, "\n")
cat("AIC:", AIC(arima_fit), " | BIC:", BIC(arima_fit), "\n")

remaining_days <- n_work_days - nrow(all_data)
arima_fc   <- forecast(arima_fit, h = remaining_days, level = c(80, 95))

arima_proj_total   <- sum(all_data$visits) + sum(pmax(arima_fc$mean, 0))
arima_lo80_total   <- sum(all_data$visits) + sum(pmax(arima_fc$lower[,1], 0))
arima_hi80_total   <- sum(all_data$visits) + sum(pmax(arima_fc$upper[,1], 0))

cat(sprintf("\nARIMA Semester Projection: %.0f visits (80%% CI: %.0f–%.0f)\n",
            arima_proj_total, arima_lo80_total, arima_hi80_total))

## ─── 3. LINEAR REGRESSION MODEL ──────────────────────────────────────────────

all_data$week_num <- as.numeric(format(all_data$date, "%W")) -
                     as.numeric(format(all_data$date[1], "%W")) + 1
all_data$is_monday    <- as.integer(all_data$weekday == "Monday")
all_data$is_tuesday   <- as.integer(all_data$weekday == "Tuesday")
all_data$is_wednesday <- as.integer(all_data$weekday == "Wednesday")
all_data$is_thursday  <- as.integer(all_data$weekday == "Thursday")

lm_fit <- lm(visits ~ day_num + week_num + is_monday + is_tuesday +
               is_wednesday + is_thursday, data = all_data)

lm_summary <- summary(lm_fit)
cat(sprintf("\nLinear Regression  R² = %.4f  |  Adj-R² = %.4f\n",
            lm_summary$r.squared, lm_summary$adj.r.squared))

# Predict remaining days
future_dates <- work_days[(nrow(all_data) + 1):n_work_days]
future_df <- data.frame(
  date      = future_dates,
  day_num   = (nrow(all_data) + 1):n_work_days,
  week_num  = as.numeric(format(future_dates, "%W")) -
              as.numeric(format(all_data$date[1], "%W")) + 1,
  weekday   = weekdays(future_dates)
)
future_df$is_monday    <- as.integer(future_df$weekday == "Monday")
future_df$is_tuesday   <- as.integer(future_df$weekday == "Tuesday")
future_df$is_wednesday <- as.integer(future_df$weekday == "Wednesday")
future_df$is_thursday  <- as.integer(future_df$weekday == "Thursday")

lm_pred <- predict(lm_fit, newdata = future_df, interval = "prediction")
lm_pred_visits <- pmax(lm_pred[,"fit"], 0)

lm_proj_total <- sum(all_data$visits) + sum(lm_pred_visits)
lm_lo_total   <- sum(all_data$visits) + sum(pmax(lm_pred[,"lwr"], 0))
lm_hi_total   <- sum(all_data$visits) + sum(pmax(lm_pred[,"upr"], 0))

cat(sprintf("LM Semester Projection:    %.0f visits (95%% PI: %.0f–%.0f)\n",
            lm_proj_total, lm_lo_total, lm_hi_total))

## ─── 4. MONTE CARLO SIMULATION ────────────────────────────────────────────────

n_sim    <- 50000
mean_vis <- mean(all_data$visits)
sd_vis   <- sd(all_data$visits)

# Day-of-week multipliers from observed data
dow_mult <- tapply(all_data$visits, all_data$weekday, mean)
dow_mult <- dow_mult / mean(dow_mult)

# Simulate each remaining working day
sim_totals <- numeric(n_sim)
for (i in seq_len(n_sim)) {
  # Draw daily means from a normal around observed distribution
  daily_base <- rnorm(remaining_days, mean = mean_vis, sd = sd_vis * 0.4)
  # Apply DoW multipliers
  dow_adj <- dow_mult[weekdays(future_dates)]
  dow_adj[is.na(dow_adj)] <- 1
  daily_sim <- pmax(round(daily_base * dow_adj), 0)
  sim_totals[i] <- sum(all_data$visits) + sum(daily_sim)
}

mc_mean   <- mean(sim_totals)
mc_median <- median(sim_totals)
mc_p10    <- quantile(sim_totals, 0.10)
mc_p90    <- quantile(sim_totals, 0.90)
mc_p2_5   <- quantile(sim_totals, 0.025)
mc_p97_5  <- quantile(sim_totals, 0.975)
mc_prob_target <- mean(sim_totals >= 2445)

cat(sprintf("\nMonte Carlo (n=%d sims)\n", n_sim))
cat(sprintf("  Mean projection : %.0f visits\n", mc_mean))
cat(sprintf("  Median          : %.0f visits\n", mc_median))
cat(sprintf("  80%% range       : %.0f – %.0f\n", mc_p10, mc_p90))
cat(sprintf("  95%% range       : %.0f – %.0f\n", mc_p2_5, mc_p97_5))
cat(sprintf("  P(≥2,445 visits): %.1f%%\n", mc_prob_target * 100))

## ─── 5. RESOURCE ALLOCATION ───────────────────────────────────────────────────

# Hourly distribution shares from Feb data
hourly_data$share <- hourly_data$signins / sum(hourly_data$signins)
# Project onto 2,445 target
target_visits   <- 2445
daily_target    <- target_visits / n_work_days
peak_hours      <- hourly_data[hourly_data$signins >= 80, ]

# Staffing: assume 1 staff needed per 25 concurrent visitors
# Peak occupancy proxy: sign-in share × daily target
hourly_data$projected_daily <- round(hourly_data$share * daily_target, 1)
hourly_data$staff_needed    <- ceiling(hourly_data$projected_daily / 6)

cat("\n── Resource Allocation at 2,445-visit Target ─────────────────\n")
cat(sprintf("  Average daily visits needed : %.1f / day\n", daily_target))
cat(sprintf("  Peak hour (12-13)           : %.1f visits / day\n",
            hourly_data$projected_daily[hourly_data$hour == 12]))
cat(sprintf("  Recommended peak staffing   : %d staff (11:00–13:00)\n",
            max(hourly_data$staff_needed)))

## ─── 6. PLOTS ─────────────────────────────────────────────────────────────────

# --- Plot 1: Observed + ARIMA Forecast ---
arima_dates <- c(all_data$date, future_dates)
arima_vals  <- c(all_data$visits, as.numeric(arima_fc$mean))
arima_lo    <- c(rep(NA, nrow(all_data)), as.numeric(arima_fc$lower[,1]))
arima_hi    <- c(rep(NA, nrow(all_data)), as.numeric(arima_fc$upper[,1]))
obs_flag    <- c(rep("Observed", nrow(all_data)), rep("ARIMA Forecast", remaining_days))

plot_df <- data.frame(date=arima_dates, visits=arima_vals,
                      lo=arima_lo, hi=arima_hi, type=obs_flag)

p1 <- ggplot(plot_df, aes(x=date, y=visits, color=type)) +
  geom_ribbon(aes(ymin=lo, ymax=hi), fill="#5B8DB8", alpha=0.2, color=NA) +
  geom_line(linewidth=0.9) +
  geom_point(data=subset(plot_df, type=="Observed"), size=2) +
  geom_hline(yintercept=daily_target, linetype="dashed", color="#E05C2A", linewidth=0.8) +
  annotate("text", x=as.Date("2026-04-01"), y=daily_target+2,
           label=sprintf("Target avg: %.1f/day", daily_target),
           color="#E05C2A", size=3.2, hjust=0) +
  scale_color_manual(values=c("Observed"="#1B3A5C","ARIMA Forecast"="#5B8DB8")) +
  scale_x_date(date_labels="%b %d", date_breaks="2 weeks") +
  labs(title="CSL Computer Lab — ARIMA Forecast (Jan–Apr 2026)",
       subtitle=sprintf("Projected semester total: %.0f visits  |  Target: 2,445",
                        arima_proj_total),
       x=NULL, y="Daily Visits", color=NULL) +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold", color="#1B3A5C"),
        legend.position="bottom",
        axis.text.x=element_text(angle=30, hjust=1))

ggsave("/home/claude/plot1_arima.png", p1, width=11, height=5, dpi=150)
cat("\nSaved plot1_arima.png\n")

# --- Plot 2: Monte Carlo Distribution ---
mc_df <- data.frame(total=sim_totals)
p2 <- ggplot(mc_df, aes(x=total)) +
  geom_histogram(aes(fill=after_stat(x >= 2445)), bins=80, color=NA, alpha=0.85) +
  scale_fill_manual(values=c("FALSE"="#A8C4D9","TRUE"="#1B3A5C"),
                    labels=c("Below target","At/above 2,445")) +
  geom_vline(xintercept=2445, color="#E05C2A", linewidth=1.2, linetype="solid") +
  geom_vline(xintercept=mc_mean, color="#2E8B57", linewidth=1, linetype="dashed") +
  annotate("text", x=2445+30, y=Inf, vjust=1.5,
           label="Target: 2,445", color="#E05C2A", size=3.5, hjust=0) +
  annotate("text", x=mc_mean+30, y=Inf, vjust=3,
           label=sprintf("Mean: %.0f", mc_mean), color="#2E8B57", size=3.5, hjust=0) +
  labs(title="Monte Carlo Simulation — Semester Visit Distribution",
       subtitle=sprintf("n = %s simulations  |  P(≥2,445) = %.1f%%",
                        format(n_sim, big.mark=","), mc_prob_target*100),
       x="Projected Semester Visits", y="Frequency", fill=NULL) +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold", color="#1B3A5C"),
        legend.position="bottom")

ggsave("/home/claude/plot2_montecarlo.png", p2, width=10, height=5, dpi=150)
cat("Saved plot2_montecarlo.png\n")

# --- Plot 3: Hourly Resource Profile ---
p3 <- ggplot(hourly_data, aes(x=factor(hour), y=projected_daily, fill=category)) +
  geom_col(color="white", linewidth=0.4) +
  geom_text(aes(label=sprintf("%.1f", projected_daily)), vjust=-0.4, size=3) +
  scale_fill_manual(values=c(
    "Early Morning"="#D4E6F1","Morning"="#85C1E9","Late Morning"="#2E86C1",
    "Lunchtime"="#1B4F72","Early Afternoon"="#2874A6","Afternoon"="#5DADE2",
    "Late Afternoon"="#AED6F1","Evening"="#D6EAF8")) +
  scale_x_discrete(labels=hourly_data$slot) +
  labs(title="Projected Daily Hourly Demand — Resource Allocation Profile",
       subtitle=sprintf("Based on %.1f avg visits/day  |  Peak: 12:00–13:00 (%.1f visits)",
                        daily_target,
                        hourly_data$projected_daily[hourly_data$hour==12]),
       x="Hour Slot", y="Projected Daily Visits", fill="Period") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold", color="#1B3A5C"),
        axis.text.x=element_text(angle=30, hjust=1),
        legend.position="right")

ggsave("/home/claude/plot3_resources.png", p3, width=11, height=5, dpi=150)
cat("Saved plot3_resources.png\n")

# --- Plot 4: Linear Regression fit + projection ---
lm_fit_vals <- fitted(lm_fit)
all_data$fitted <- lm_fit_vals

future_df$projected <- lm_pred_visits
future_df$lo        <- pmax(lm_pred[,"lwr"], 0)
future_df$hi        <- lm_pred[,"upr"]

p4 <- ggplot() +
  geom_ribbon(data=future_df, aes(x=date, ymin=lo, ymax=hi),
              fill="#2E86C1", alpha=0.15) +
  geom_line(data=all_data, aes(x=date, y=fitted, color="Model Fit"), linewidth=0.8) +
  geom_point(data=all_data, aes(x=date, y=visits, color="Observed"), size=2.5) +
  geom_line(data=future_df, aes(x=date, y=projected, color="LR Projection"), linewidth=0.9) +
  geom_hline(yintercept=daily_target, linetype="dashed", color="#E05C2A", linewidth=0.7) +
  scale_color_manual(values=c("Observed"="#1B3A5C","Model Fit"="#2E86C1",
                               "LR Projection"="#27AE60")) +
  scale_x_date(date_labels="%b %d", date_breaks="2 weeks") +
  labs(title="Linear Regression — Observed vs Fitted vs Projected",
       subtitle=sprintf("R² = %.3f  |  Semester LR projection: %.0f visits",
                        lm_summary$r.squared, lm_proj_total),
       x=NULL, y="Daily Visits", color=NULL) +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold", color="#1B3A5C"),
        legend.position="bottom",
        axis.text.x=element_text(angle=30, hjust=1))

ggsave("/home/claude/plot4_regression.png", p4, width=11, height=5, dpi=150)
cat("Saved plot4_regression.png\n")

## ─── 7. EXPORT EXCEL WORKBOOK ─────────────────────────────────────────────────

wb <- createWorkbook()
modifyBaseFont(wb, fontName="Arial", fontSize=11)

hdr_style <- createStyle(fontColour="#FFFFFF", fgFill="#1B3A5C",
                          textDecoration="bold", halign="center", valign="center",
                          border="Bottom", borderColour="#5B8DB8", wrapText=TRUE)
num_style  <- createStyle(numFmt="#,##0", halign="center")
dec_style  <- createStyle(numFmt="0.0", halign="center")
pct_style  <- createStyle(numFmt="0.0%", halign="center")
bold_style <- createStyle(textDecoration="bold")
hi_style   <- createStyle(fgFill="#D5E8F3", textDecoration="bold", numFmt="#,##0")
tgt_style  <- createStyle(fgFill="#FFF3CD", textDecoration="bold")

## Sheet 1: Executive Summary
addWorksheet(wb, "Executive Summary")
s1 <- "Executive Summary"

writeData(wb, s1, "CSL Computer Lab — Semester Forecast Report", startRow=1, startCol=1)
addStyle(wb, s1, createStyle(fontSize=16, textDecoration="bold",
         fontColour="#1B3A5C"), rows=1, cols=1)

# Key metrics table
metrics <- data.frame(
  Metric     = c("Observed Days","Observed Visits","Target Semester Visits",
                 "Remaining Working Days","Avg Visits/Day (Observed)",
                 "Avg Visits/Day (Target)"),
  Value      = c(nrow(all_data), sum(all_data$visits), 2445,
                 remaining_days, round(mean(all_data$visits),1), round(daily_target,1))
)
writeData(wb, s1, "KEY METRICS", startRow=3, startCol=1)
addStyle(wb, s1, hdr_style, rows=3, cols=1:2)
writeData(wb, s1, metrics, startRow=4, startCol=1)
addStyle(wb, s1, num_style, rows=5:9, cols=2)

# Model summary table
model_summary <- data.frame(
  Model              = c("ARIMA","Linear Regression","Monte Carlo (Mean)",
                          "Monte Carlo (Median)"),
  `Semester Projection` = c(round(arima_proj_total),round(lm_proj_total),
                              round(mc_mean),round(mc_median)),
  `Lower Bound`       = c(round(arima_lo80_total),round(lm_lo_total),
                           round(mc_p10),round(mc_p2_5)),
  `Upper Bound`       = c(round(arima_hi80_total),round(lm_hi_total),
                           round(mc_p90),round(mc_p97_5)),
  `CI / Range`        = c("80% CI","95% PI","80% range","95% range"),
  check.names=FALSE
)
writeData(wb, s1, "MODEL PROJECTIONS", startRow=12, startCol=1)
addStyle(wb, s1, hdr_style, rows=12, cols=1:5)
writeData(wb, s1, model_summary, startRow=13, startCol=1, colNames=TRUE)
addStyle(wb, s1, hdr_style, rows=13, cols=1:5)
addStyle(wb, s1, num_style, rows=14:17, cols=2:4, gridExpand=TRUE)

# Monte Carlo probability note
writeData(wb, s1, sprintf("P(semester ≥ 2,445 visits) = %.1f%%", mc_prob_target*100),
          startRow=19, startCol=1)
addStyle(wb, s1, tgt_style, rows=19, cols=1:3, gridExpand=TRUE)

setColWidths(wb, s1, cols=1:5, widths=c(30,22,16,16,14))

## Sheet 2: ARIMA Results
addWorksheet(wb, "ARIMA Forecast")
s2 <- "ARIMA Forecast"

writeData(wb, s2, "ARIMA Forecast — Remaining Semester Days", startRow=1, startCol=1)
addStyle(wb, s2, createStyle(fontSize=14, textDecoration="bold",
         fontColour="#1B3A5C"), rows=1, cols=1)

arima_out <- data.frame(
  Date            = format(future_dates, "%A, %B %d, %Y"),
  `Point Forecast` = round(pmax(as.numeric(arima_fc$mean),0)),
  `Lower 80%`     = round(pmax(as.numeric(arima_fc$lower[,1]),0)),
  `Upper 80%`     = round(pmax(as.numeric(arima_fc$upper[,1]),0)),
  `Lower 95%`     = round(pmax(as.numeric(arima_fc$lower[,2]),0)),
  `Upper 95%`     = round(pmax(as.numeric(arima_fc$upper[,2]),0)),
  check.names=FALSE
)
writeData(wb, s2, arima_out, startRow=3, startCol=1)
addStyle(wb, s2, hdr_style, rows=3, cols=1:6)
addStyle(wb, s2, num_style, rows=4:(3+nrow(arima_out)), cols=2:6, gridExpand=TRUE)
setColWidths(wb, s2, cols=1:6, widths=c(28,16,12,12,12,12))

# Summary row
r_sum <- 4 + nrow(arima_out)
writeData(wb, s2, "TOTALS (Remaining)", startRow=r_sum, startCol=1)
writeData(wb, s2, round(sum(pmax(as.numeric(arima_fc$mean),0))), startRow=r_sum, startCol=2)
writeData(wb, s2, round(sum(pmax(as.numeric(arima_fc$lower[,1]),0))), startRow=r_sum, startCol=3)
writeData(wb, s2, round(sum(pmax(as.numeric(arima_fc$upper[,1]),0))), startRow=r_sum, startCol=4)
addStyle(wb, s2, hi_style, rows=r_sum, cols=1:6, gridExpand=TRUE)

## Sheet 3: Regression Results
addWorksheet(wb, "Linear Regression")
s3 <- "Linear Regression"

writeData(wb, s3, "Linear Regression — Day-of-Week Model", startRow=1, startCol=1)
addStyle(wb, s3, createStyle(fontSize=14, textDecoration="bold",
         fontColour="#1B3A5C"), rows=1, cols=1)

writeData(wb, s3, sprintf("R-squared: %.4f  |  Adj R-squared: %.4f  |  RMSE: %.2f",
                           lm_summary$r.squared, lm_summary$adj.r.squared,
                           sqrt(mean(lm_fit$residuals^2))),
          startRow=2, startCol=1)

lm_coef <- as.data.frame(lm_summary$coefficients)
lm_coef$Variable <- rownames(lm_coef)
lm_coef <- lm_coef[, c("Variable","Estimate","Std. Error","t value","Pr(>|t|)")]
colnames(lm_coef) <- c("Variable","Estimate","Std Error","t-value","p-value")
writeData(wb, s3, "Model Coefficients", startRow=4, startCol=1)
addStyle(wb, s3, hdr_style, rows=4, cols=1:5)
writeData(wb, s3, lm_coef, startRow=5, startCol=1)
addStyle(wb, s3, hdr_style, rows=5, cols=1:5)

lm_proj_out <- data.frame(
  Date            = format(future_df$date, "%A, %B %d, %Y"),
  Weekday         = future_df$weekday,
  `Projected Visits` = round(lm_pred_visits),
  `Lower 95%`     = round(pmax(lm_pred[,"lwr"],0)),
  `Upper 95%`     = round(lm_pred[,"upr"]),
  check.names=FALSE
)
r_start <- 5 + nrow(lm_coef) + 2
writeData(wb, s3, "Day-by-Day Projections", startRow=r_start, startCol=1)
addStyle(wb, s3, hdr_style, rows=r_start, cols=1:5)
writeData(wb, s3, lm_proj_out, startRow=r_start+1, startCol=1)
addStyle(wb, s3, hdr_style, rows=r_start+1, cols=1:5)
addStyle(wb, s3, num_style, rows=(r_start+2):(r_start+1+nrow(lm_proj_out)), cols=3:5, gridExpand=TRUE)
setColWidths(wb, s3, cols=1:5, widths=c(28,14,16,12,12))

## Sheet 4: Monte Carlo
addWorksheet(wb, "Monte Carlo")
s4 <- "Monte Carlo"

writeData(wb, s4, "Monte Carlo Simulation — Semester Visit Distribution", startRow=1, startCol=1)
addStyle(wb, s4, createStyle(fontSize=14, textDecoration="bold",
         fontColour="#1B3A5C"), rows=1, cols=1)

mc_summary <- data.frame(
  Statistic = c("Simulations","Observed Visits to Date","Simulated Remaining Days",
                "Mean Projection","Median Projection",
                "P10 (Pessimistic)","P25","P75","P90 (Optimistic)",
                "95% CI Lower","95% CI Upper","P(≥ 2,445 visits)"),
  Value     = c(format(n_sim, big.mark=","), sum(all_data$visits), remaining_days,
                round(mc_mean), round(mc_median),
                round(mc_p10), round(quantile(sim_totals,0.25)),
                round(quantile(sim_totals,0.75)), round(mc_p90),
                round(mc_p2_5), round(mc_p97_5),
                sprintf("%.1f%%", mc_prob_target*100))
)
writeData(wb, s4, mc_summary, startRow=3, startCol=1)
addStyle(wb, s4, hdr_style, rows=3, cols=1:2)

# Highlight the target probability row
addStyle(wb, s4, tgt_style, rows=14, cols=1:2, gridExpand=TRUE)
setColWidths(wb, s4, cols=1:2, widths=c(28,18))

## Sheet 5: Resource Allocation
addWorksheet(wb, "Resource Allocation")
s5 <- "Resource Allocation"

writeData(wb, s5, "Hourly Resource Allocation — 2,445-Visit Target", startRow=1, startCol=1)
addStyle(wb, s5, createStyle(fontSize=14, textDecoration="bold",
         fontColour="#1B3A5C"), rows=1, cols=1)

writeData(wb, s5, sprintf(
  "Average daily visits required: %.1f  |  Working days: %d  |  Total target: 2,445",
  daily_target, n_work_days), startRow=2, startCol=1)

res_out <- data.frame(
  Hour                = hourly_data$slot,
  Period              = hourly_data$category,
  `Feb Sign-ins`     = hourly_data$signins,
  `Share (%)`        = round(hourly_data$share * 100, 1),
  `Proj. Visits/Day` = round(hourly_data$projected_daily, 1),
  `Staff Needed`     = hourly_data$staff_needed,
  check.names=FALSE
)
writeData(wb, s5, res_out, startRow=4, startCol=1)
addStyle(wb, s5, hdr_style, rows=4, cols=1:6)
addStyle(wb, s5, num_style, rows=5:16, cols=3, gridExpand=TRUE)
addStyle(wb, s5, pct_style, rows=5:16, cols=4, gridExpand=TRUE)
addStyle(wb, s5, dec_style, rows=5:16, cols=5, gridExpand=TRUE)
addStyle(wb, s5, num_style, rows=5:16, cols=6, gridExpand=TRUE)

# Highlight peak hours
peak_rows <- which(hourly_data$signins >= 80) + 4
if (length(peak_rows) > 0) {
  addStyle(wb, s5, hi_style, rows=peak_rows, cols=1:6, gridExpand=TRUE)
}

# Staffing recommendations
recs <- data.frame(
  `Time Window`   = c("07:00–09:00","09:00–11:00","11:00–14:00 (PEAK)",
                       "14:00–16:00","16:00–19:00"),
  `Approx Load`   = c("Low","Moderate","High","Moderate","Low"),
  `Recommendation` = c(
    "Minimum 1 staff; use for setup/admin",
    "2 staff; lab opens to full usage",
    "3 staff; peak demand — all resources active",
    "2 staff; moderate traffic continues",
    "1 staff; wind-down, close checklist"),
  check.names=FALSE
)
writeData(wb, s5, recs, startRow=18, startCol=1)
addStyle(wb, s5, hdr_style, rows=18, cols=1:3)
r_peak_rec <- which(recs[,1] == "11:00–14:00 (PEAK)") + 18
addStyle(wb, s5, tgt_style, rows=r_peak_rec, cols=1:3, gridExpand=TRUE)
setColWidths(wb, s5, cols=1:6, widths=c(22,18,14,12,18,14))

## Sheet 6: Raw Data
addWorksheet(wb, "Raw Data")
s6 <- "Raw Data"
raw_export <- data.frame(
  Date    = format(all_data$date, "%Y-%m-%d"),
  Weekday = all_data$weekday,
  Month   = ifelse(all_data$date < as.Date("2026-02-01"),"January","February"),
  Visits  = all_data$visits,
  Fitted_LM = round(all_data$fitted, 1)
)
writeData(wb, s6, raw_export, startRow=1, startCol=1)
addStyle(wb, s6, hdr_style, rows=1, cols=1:5)
addStyle(wb, s6, num_style, rows=2:(1+nrow(raw_export)), cols=4:5, gridExpand=TRUE)
setColWidths(wb, s6, cols=1:5, widths=c(14,14,12,10,12))

## Save
out_path <- "/mnt/user-data/outputs/CSL_Forecast_Report.xlsx"
saveWorkbook(wb, out_path, overwrite=TRUE)
cat(sprintf("\nExcel workbook saved: %s\n", out_path))

cat("\n══════════════════════════════════════════════════════\n")
cat("  MODEL COMPARISON SUMMARY\n")
cat("══════════════════════════════════════════════════════\n")
cat(sprintf("  ARIMA projection         : %6.0f visits\n", arima_proj_total))
cat(sprintf("  Linear Regression        : %6.0f visits\n", lm_proj_total))
cat(sprintf("  Monte Carlo (mean)       : %6.0f visits\n", mc_mean))
cat(sprintf("  Monte Carlo (median)     : %6.0f visits\n\n", mc_median))
cat(sprintf("  TARGET                   :  2,445 visits\n"))
cat(sprintf("  P(reach target)          :  %.1f%%\n", mc_prob_target*100))
cat("══════════════════════════════════════════════════════\n")
