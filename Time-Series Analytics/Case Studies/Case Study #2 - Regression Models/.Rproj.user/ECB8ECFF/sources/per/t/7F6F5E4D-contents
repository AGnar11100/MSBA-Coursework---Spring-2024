install.packages("forecast")
library(forecast)
library(zoo)
revenue.data <- read.csv('673_case2.csv')
revenue.data

# 1. Plot the data and visualize time series components.
# a. Create time series data set in R using the ts() function. 
revenue.ts <- ts(revenue.data$Revenue, start=c(2005, 1), end=c(2023, 4), frequency = 4)
revenue.ts

# b. Apply the plot() function to create a data plot with the historical data. 
# Provide it in your report, and explain what time series components can be visualized in this plot.
# Plot the time series data
plot(revenue.ts,
     xlab = "Time",
     ylab = "Quarterly revenues (in $million)",
     main = "Walmart Revenue",
     xaxt = 'n'
)
axis(1, at = seq(2005, 2023, 1), labels = format(seq(2005, 2023, 1)))


# --------------------------------------------------------------------------------------------------------------
# 2. Apply five regression models using data partition.

# a. Develop data partition with the validation partition of 16 periods and the rest for the training partition.
# Define the number of periods for validation
nValid <- 16
nTrain <- length(revenue.ts) - nValid

train.ts <- window(revenue.ts, start = c(2005, 1), end = c(2005, nTrain))
valid.ts <- window(revenue.ts, start = c(2005, nTrain + 1), end = c(2005, nTrain + nValid))


# b. Use the tslm() function for the training partition to develop each of the 5 regression

# i. Regression model with linear trend
# Apply Summary() and show summary, model equation, predictors. Explain if good fit, stat signif.
# Forecast revenues for the validation period using the forecast() function, and present forecast.
train.lin <- tslm(train.ts ~ trend)
summary(train.lin)
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)

plot(revenue.ts, 
     xlab = "Time", ylab = "Quarterly revenues (in $million)", xaxt = "n",
     ylim = c (50000,300000), xlim = c(2005, 2023), bty = "l", lwd = 1.5,
     main = "Revenue with Linear Trend Forecast")
axis(1, at = seq(2005, 2023, 1), labels = format(seq(2005, 2023, 1)))
lines(train.lin$fitted, lwd = 2, col="blue")
lines(train.lin.pred$mean, lwd = 2, col="brown")

train.lin.pred$mean


# ii. Regression mode with quadratic trend
# Apply Summary() and show summary, model equation, predictors. Explain if good fit, stat signif.
# Forecast revenues for the validation period using the forecast() function, and present forecast. 
train.quad <- tslm(train.ts ~ trend + I(trend^2))
summary(train.quad)
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)

plot(revenue.ts, 
     xlab = "Time", ylab = "Quarterly revenues (in $million)", xaxt = "n",
     ylim = c (50000,300000), xlim = c(2005, 2023), bty = "l", lwd = 1.5,
     main = "Revenue with Quadratic Trend Forecast")
axis(1, at = seq(2005, 2023, 1), labels = format(seq(2005, 2023, 1)))
lines(train.quad$fitted, lwd = 2, col="blue")
lines(train.quad.pred$mean, lwd = 2, col="brown")

train.quad.pred$mean


# iii. Regression model with seasonality
# Apply Summary() and show summary, model equation, predictors. Explain if good fit, stat signif.
# Forecast revenues for the validation period using the forecast() function, and present forecast. 
train.seas <- tslm(train.ts ~ season)
summary(train.seas)
train.seas.pred <- forecast(train.seas, h = nValid, level = 0)

plot(revenue.ts, 
     xlab = "Time", ylab = "Quarterly revenues (in $million)", xaxt = "n",
     ylim = c (50000,300000), xlim = c(2005, 2023), bty = "l", lwd = 1.5,
     main = "Revenue with Linear Seasonality Forecast")
axis(1, at = seq(2005, 2023, 1), labels = format(seq(2005, 2023, 1)))
lines(train.seas$fitted, lwd = 2, col="blue")
lines(train.seas.pred$mean, lwd = 2, col="brown")

train.seas.pred$mean


# iv. Regression model with linear trend and seasonality
# Apply Summary() and show summary, model equation, predictors. Explain if good fit, stat signif.
# Forecast revenues for the validation period using the forecast() function, and present forecast. 
train.lin.seas <- tslm(train.ts ~ trend + season)
summary(train.lin.seas)
train.lin.seas.pred <- forecast(train.lin.seas, h = nValid, level = 0)

plot(revenue.ts, 
     xlab = "Time", ylab = "Quarterly revenues (in $million)", xaxt = "n",
     ylim = c (50000,300000), xlim = c(2005, 2023), bty = "l", lwd = 1.5,
     main = "Revenue with Linear Trend and Seasonality Forecast")
axis(1, at = seq(2005, 2023, 1), labels = format(seq(2005, 2023, 1)))
lines(train.lin.seas$fitted, lwd = 2, col="blue")
lines(train.lin.seas.pred$mean, lwd = 2, col="brown")

train.lin.seas.pred$mean


# v. Regression model with quadratic trend and seasonality.
# Apply Summary() and show summary, model equation, predictors. Explain if good fit, stat signif.
# Forecast revenues for the validation period using the forecast() function, and present forecast. 
train.quad.trend.seas <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.quad.trend.seas)
train.quad.trend.seas.pred <- forecast(train.quad.trend.seas, h = nValid, level = 0)

plot(revenue.ts, 
     xlab = "Time", ylab = "Quarterly revenues (in $million)", xaxt = "n",
     ylim = c (50000,300000), xlim = c(2005, 2023), bty = "l", lwd = 1.5,
     main = "Revenue with Quadratic Trend and Seasonality Forecast")
axis(1, at = seq(2005, 2023, 1), labels = format(seq(2005, 2023, 1)))
lines(train.quad.trend.seas$fitted, lwd = 2, col="blue")
lines(train.quad.trend.seas.pred$mean, lwd = 2, col="brown")

train.quad.trend.seas.pred$mean


# c. Apply the accuracy() function to compare performance measure of the 5 forecasts you 
# developed in 2b. Present the accuracy measures in your report, compare them, and, using 
# MAPE and RMSE, identify the three most accurate regression models for forecasting. If 
# the historical data contains trend and seasonality, then in selecting the regression models 
# give a preference to those models that include trend and seasonality. 

mape_lin <- round(accuracy(train.lin.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_lin <- round(accuracy(train.lin.pred$mean, valid.ts)[, "RMSE"], 3)

mape_quad <- round(accuracy(train.quad.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_quad <- round(accuracy(train.quad.pred$mean, valid.ts)[, "RMSE"], 3)

mape_seas <- round(accuracy(train.seas.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_seas <- round(accuracy(train.seas.pred$mean, valid.ts)[, "RMSE"], 3)

mape_lin_seas <- round(accuracy(train.lin.seas.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_lin_seas <- round(accuracy(train.lin.seas.pred$mean, valid.ts)[, "RMSE"], 3)

mape_quad_trend_seas <- round(accuracy(train.quad.trend.seas.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_quad_trend_seas <- round(accuracy(train.quad.trend.seas.pred$mean, valid.ts)[, "RMSE"], 3)

accuracy_df <- data.frame(
  Model = c("Linear Trend", "Quadratic Trend", "Seasonality", "Linear trend and Seasonality", "Quadratic Trend and Seasonality"),
  MAPE = c(mape_lin, mape_quad, mape_seas, mape_lin_seas, mape_quad_trend_seas),
  RMSE = c(rmse_lin, rmse_quad, rmse_seas, rmse_lin_seas, rmse_quad_trend_seas)
)

accuracy_df

# --------------------------------------------------------------------------------------------------------------------
# 3. Employ the entire data set to make time series forecast.
# a. Apply the two most accurate regression models identified in 2c to make the forecast in 
# the four quarters (Q1-Q4) of 2024-2025. For that, use the entire data set to develop the 
# regression model using the tslm() function. Apply the summary() function to identify the 
# model structure and parameters, show them in your report, and also present the 
# respective model equation and define its predictors. Briefly explain if the model is a good 
# fit, statistically significant, and thus may be applied for forecasting. Use each model to 
# forecast Walmart’s revenue in Q1-Q4 of 2024-2025 using the forecast() function, and 
# present this forecast in your report.
tot.lin.trend <- tslm(revenue.ts ~ trend)
summary(tot.lin.trend)
tot.lin.trend.pred <- forecast(tot.lin.trend, h = nValid, level = 0)

plot(revenue.ts, 
     xlab = "Time", ylab = "Quarterly revenues (in $million)", xaxt = "n",
     ylim = c (50000,300000), xlim = c(2005, 2025), bty = "l", lwd = 1.5,
     main = "Revenue with Linear Trend Forecast")
axis(1, at = seq(2005, 2025, 1), labels = format(seq(2005, 2025, 1)))
lines(tot.lin.trend$fitted, lwd = 2, col="blue")
lines(tot.lin.trend.pred$mean, lwd = 2, col="brown")
tot.lin.trend.pred$mean
# ~~~~~~~~

tot.lin.trend.seas <- tslm(revenue.ts ~ trend + season)
summary(tot.lin.trend.seas)
tot.lin.trend.seas.pred <- forecast(tot.lin.trend.seas, h = nValid, level = 0)

plot(revenue.ts, 
     xlab = "Time", ylab = "Quarterly revenues (in $million)", xaxt = "n",
     ylim = c (50000,300000), xlim = c(2005, 2025), bty = "l", lwd = 1.5,
     main = "Revenue with Linear Trend and Seasonality Forecast")
axis(1, at = seq(2005, 2025, 1), labels = format(seq(2005, 2025, 1)))
lines(tot.lin.trend.seas.pred$fitted, lwd = 2, col="blue")
lines(tot.lin.trend.seas.pred$mean, lwd = 2, col="brown")
tot.lin.trend.seas.pred$mean


# b. Apply the accuracy() function to compare the performance measures of the regression 
# models developed in 3a with those for naïve and seasonal naïve forecasts. Present the 
# accuracy measures in your report, compare them, and identify, using MAPE and RMSE,
# which forecast is most accurate to forecast Walmart’s quarterly revenue in Q1-Q4 of 
# 2024-2025. 
# Calculate MAPE and RMSE for Linear Trend model
mape_tot_lin <- round(accuracy(tot.lin.trend.pred$fitted, revenue.ts)[, "MAPE"], 3)
rmse_tot_lin <- round(accuracy(tot.lin.trend.pred$fitted, revenue.ts)[, "RMSE"], 3)

# Calculate MAPE and RMSE for Linear Trend and Seasonality model
mape_tot_lin_seas <- round(accuracy(tot.lin.trend.seas.pred$fitted, revenue.ts)[, "MAPE"], 3)
rmse_tot_lin_seas <- round(accuracy(tot.lin.trend.seas.pred$fitted, revenue.ts)[, "RMSE"], 3)

# Calculate MAPE and RMSE for Naive model
mape_naive <- round(accuracy((naive(revenue.ts))$fitted, revenue.ts)[, "MAPE"], 3)
rmse_naive <- round(accuracy((naive(revenue.ts))$fitted, revenue.ts)[, "RMSE"], 3)

# Calculate MAPE and RMSE for Seasonal Naive model
mape_snaive <- round(accuracy((snaive(revenue.ts))$fitted, revenue.ts)[, "MAPE"], 3)
rmse_snaive <- round(accuracy((snaive(revenue.ts))$fitted, revenue.ts)[, "RMSE"], 3)

# Create dataframe for final accuracy results
final_accuracy_df <- data.frame(
  Model = c("Linear Trend", "Linear Trend and Seasonality", "Naive", "Seasonal Naive"),
  MAPE = c(mape_tot_lin, mape_tot_lin_seas, mape_naive, mape_snaive),
  RMSE = c(rmse_tot_lin, rmse_tot_lin_seas, rmse_naive, rmse_snaive)
)

final_accuracy_df
