install.packages("forecast")
library(forecast)
library(zoo)

housing.data <- read.csv('real_data.csv')
housing.data

housing.ts <- ts(housing.data$val, start=c(1963, 1), end=c(2024, 2), frequency = 12)
housing.ts

# general plot of the housing data
plot(housing.ts,
     xlab = "Time",
     ylab = "Monthly Price (in $million)",
     main = "Housing Prices",
     xaxt = 'n'
)
axis(1, at = seq(1963, 2024, 1), labels = format(seq(1963, 2024, 1)))

# seasonality, trend, level plot
housing.stl <- stl(housing.ts, s.window = "periodic")
autoplot(housing.stl, main = "Grocery Store Time Series Components")


# predictability check with AR(1) - SHOWS IS PREDICTABLE
housing.ts.ar1 <- Arima(housing.ts, order = c(1,0,0))
summary(housing.ts.ar1)

ar1 <- .9398
s.e. <- .0124
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1 - null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

# predictability check with Autocorrelation of first differenced
diff.housing.ts <- diff(housing.ts, lag = 1)
Acf(diff.housing.ts, lag.max = 12, 
    main = "Autocorrelation for Differenced Housing Data")
# 8/12 od the lags show that there is some sort of underlying information

# data partitioning, validation has 18 years + 2 months of data
nValid <- 218
nTrain <- length(housing.ts) - nValid

train.ts <- window(housing.ts, start = c(1963, 1), end = c(1963, nTrain))
valid.ts <- window(housing.ts, start = c(1963, nTrain + 1), end = c(1963, nTrain + nValid))
train.ts
valid.ts


# TRAIN ALL MODELS USING TRAIN/TEST

# Moving Average (Trailing MA)
# Two-Level Model: Regression with Linear Trend and Seasonality + Trailing MA
# for Regression Residuals
ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_6 <- rollmean(train.ts, k = 6, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

ma.trail_4.pred <- forecast(ma.trailing_4, h = nValid, level = 0)
ma.trail_6.pred <- forecast(ma.trailing_6, h = nValid, level = 0)
ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)

mape_4 <- round(accuracy(ma.trail_4.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_4 <- round(accuracy(ma.trail_4.pred$mean, valid.ts)[, "RMSE"], 3)

mape_6 <- round(accuracy(ma.trail_6.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_6 <- round(accuracy(ma.trail_6.pred$mean, valid.ts)[, "RMSE"], 3)

mape_12 <- round(accuracy(ma.trail_12.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_12 <- round(accuracy(ma.trail_12.pred$mean, valid.ts)[, "RMSE"], 3)

cat("The MAPE and RMSE accuracy measures for a trailing MA forecast in the validation period with window width 4 are respectively as follows: ", mape_4, rmse_4, '\n',
    "The MAPE and RMSE accuracy measures for a trailing MA forecast in the validation period with window width 6 are respectively as follows: ", mape_6,rmse_6, '\n',
    "The MAPE and RMSE accuracy measures for a trailing MA forecast in the validation period with window width 12 are respectively as follows: ", mape_12, rmse_12, '\n')

# Build linear model with trend and seasonality
trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)

trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.res <- trend.seas$residuals
trend.seas.res

# apply trailing MA with window width 4 for residuals using rollmean()
ma.trail.res <- rollmean(trend.seas.res, k = 4, align = "right")
ma.trail.res
summary(ma.trail.res)
# identify trailing MA forecast of these residuals in the validation period with forecast()
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)

# two level forecast
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean

# Present in your report a table that contains 
# validation data, regression forecast, trailing MA forecast for residuals, and two-level
# (combined) forecast in the validation period.
twolevel.valid.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                             ma.trail.res.pred$mean, 
                             fst.2level), 3)
names(twolevel.valid.df) <- c("Sales", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
twolevel.valid.df
summary(fst.2level)



# Exponential Smoothing
# Holt-Winter’s Model with Automatic Selection of Model Options and Parameters
hw.ZZZ <- ets(train.ts, model = "ZZZ")
summary(hw.ZZZ)
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

hw.valid.df <- round(data.frame(valid.ts, hw.ZZZ.pred$mean), 3)
names(hw.valid.df) <- c("Prices", "Automated HW Forecast")
hw.valid.df



# Autoregressive Models
# Two-Level Model: Regression with Linear Trend and Seasonality + AR(1) for
# Regression Residuals
train.lin.season <- tslm(train.ts ~ trend + season)
summary(train.lin.season)
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)

# check if residuals still hold valuable information 
Acf(train.lin.season$residuals, lag.max = 24, 
    main = "Autocorrelation with a maximum of 8 lags for Housing Prices Data training period")

# Develop an AR(1) model for the regression residuals. Use the Acf() function for the residuals of the AR(1) model 
# (residuals of residuals)
res.ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(res.ar1)
Acf(res.ar1$residuals, lag.max = 24, 
    main = "Autocorrelation of AR(1) for \"residuals of residuals\" model")
# the results of this plot ^ would usually stop this modeling from fully being built since there is lack of useful information left



# Create a two-level forecasting model (regression model with linear trend and seasonality + AR(1) model for residuals) for the validation period. 
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)

valid.two.level.pred <- train.lin.season.pred$mean + res.ar1.pred$mean
ar1.twolevel.valid.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
                             res.ar1.pred$mean, valid.two.level.pred),3)
names(ar1.twolevel.valid.df) <- c("Prices", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
ar1.twolevel.valid.df





# ARIMA Models
# Auto ARIMA Model
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

auto.valid.df <- round(data.frame(valid.ts, train.auto.arima.pred$mean), 3)
names(auto.valid.df) <- c("Prices", "Auto ARIMA Forecast")
auto.valid.df



# Naive
# snaive
train.snaive <- snaive(train.ts)
summary(train.snaive)
train.snaive.pred <- forecast(train.snaive$fitted, h = nValid, level=0)
train.snaive.pred

naive.valid.df <- round(data.frame(valid.ts, train.snaive.pred$mean), 3)
names(naive.valid.df) <- c("Prices", "Seasonal Naive Forecast")
naive.valid.df


# Compare how each model does for the validation partition, pick top 3

# Two-Level Model: Regression with Linear Trend and Seasonality + Trailing MA
# Holt-Winter’s Model with Automatic Selection of Model Options and Parameters
# Two-Level Model: Regression with Linear Trend and Seasonality + AR(1) for Regression Residuals
# Auto ARIMA Model
# SNaive


# Calculate MAPE and RMSE for each model
mape_fst_2level <- round(accuracy(trend.seas.pred$mean + ma.trail.res.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_fst_2level <- round(accuracy(trend.seas.pred$mean + ma.trail.res.pred$mean, valid.ts)[, "RMSE"], 3)

mape_hw_ZZZ <- round(accuracy(hw.ZZZ.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_hw_ZZZ <- round(accuracy(hw.ZZZ.pred$mean, valid.ts)[, "RMSE"], 3)

mape_valid_two_level <- round(accuracy(train.lin.season.pred$mean + res.ar1.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_valid_two_level <- round(accuracy(train.lin.season.pred$mean + res.ar1.pred$mean, valid.ts)[, "RMSE"], 3)

mape_train_auto_arima <- round(accuracy(train.auto.arima.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_train_auto_arima <- round(accuracy(train.auto.arima.pred$mean, valid.ts)[, "RMSE"], 3)

mape_train_snaive <- round(accuracy(train.snaive.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_train_snaive <- round(accuracy(train.snaive.pred$mean, valid.ts)[, "RMSE"], 3)

# Create a data frame to store results
accuracy_df <- data.frame(
  Model = c("Two-Level: LinReg + Trailing MA", "Holt-Winters Automated Selection", "LinReg + AR(1) for Regression Residuals", "Auto ARIMA", "Seasonal Naive"),
  MAPE = c(mape_fst_2level, mape_hw_ZZZ, mape_valid_two_level, mape_train_auto_arima, mape_train_snaive),
  RMSE = c(rmse_fst_2level, rmse_hw_ZZZ, rmse_valid_two_level, rmse_train_auto_arima, rmse_train_snaive)
)

accuracy_df


# Pick two level forecasts to train on whole dataset

# Two Level with LinReg and Trailing MA
tot.trend.seas <- tslm(housing.ts ~ trend + season)
summary(tot.trend.seas)

tot.trend.seas.pred <- forecast(trend.seas, h = 24, level = 0)
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res
# apply trailing MA with window width 4 for residuals using rollmean()
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 4, align = "right")
tot.ma.trail.res
# identify trailing MA forecast of these residuals for 2 years in the future with forecast()
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 24, level = 0)
# two level forecast
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean




# Two Level using LinReg and AR(1)
tot.lin.season <- tslm(housing.ts ~ trend + season)
summary(tot.lin.season)
tot.lin.season.pred <- forecast(tot.lin.season, h = nValid, level = 0)

# check if residuals still hold valuable information 
Acf(tot.lin.season$residuals, lag.max = 24, 
    main = "Autocorrelation with a maximum of 8 lags for Housing Prices Data training period")

tot.res.ar1 <- Arima(tot.lin.season$residuals, order = c(1,0,0))
summary(tot.res.ar1)
Acf(tot.res.ar1$residuals, lag.max = 24, 
    main = "Autocorrelation of AR(1) for \"residuals of residuals\" model")


# Create a two-level forecasting model (regression model with linear trend and seasonality + AR(1) model for residuals) for the validation period. 
tot.res.ar1.pred <- forecast(tot.res.ar1, h = 24, level = 0)
tot.two.level.pred <- tot.lin.season.pred$mean + tot.res.ar1.pred$mean
tot.two.level.pred

# Calculate MAPE and RMSE for each model
mape_tot_two_level <- round(accuracy(tot.lin.season.pred$fitted + tot.res.ar1.pred$fitted, housing.ts)[, "MAPE"], 3)
rmse_tot_two_level <- round(accuracy(tot.lin.season.pred$fitted + tot.res.ar1.pred$fitted, housing.ts)[, "RMSE"], 3)

mape_tot_fst_2level <- round(accuracy(tot.trend.seas.pred$fitted + tot.ma.trail.res.pred$fitted, housing.ts)[, "MAPE"], 3)
rmse_tot_fst_2level <- round(accuracy(tot.trend.seas.pred$fitted + tot.ma.trail.res.pred$fitted, housing.ts)[, "RMSE"], 3)

# Create a data frame to store results
accuracy_df <- data.frame(
  Model = c("Two-Level: LinReg with AR(1) for residuals", "Two-Level: LinReg with Trailing MA for residuals"),
  MAPE = c(mape_tot_two_level, mape_tot_fst_2level),
  RMSE = c(rmse_tot_two_level, rmse_tot_fst_2level)
)
accuracy_df
