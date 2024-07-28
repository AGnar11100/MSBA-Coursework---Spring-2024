
## USE FORECAST LIBRARY.

library(forecast)
library(zoo)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/misc/673_BAN/module4_arima")

# Create data frame.
Amtrak.data <- read.csv("Amtrak_comp.csv")

# See the first 6 records of the file.
head(Amtrak.data)


## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION.

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
ridership.ts <- ts(Amtrak.data$Ridership, 
            start = c(1991, 1), end = c(2018, 12), freq = 12)

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
Acf(ridership.ts, lag.max = 12, main = "Autocorrelation for Amtrak Ridership")

## CREATE TIME SERIES PARTITION.

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
# Total number of period length(ridership.ts) = 159.
# nvalid = 60 months for the last 12 months (January 2014 to December 2018).
# nTrain = 276 months, from January 1991 to December 2013.
nValid <- 60 
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), 
                   end = c(1991, nTrain + nValid))


## USE TWO-LEVEL MODEL, REGRESSION WITH LINEAR TREND AND 
## SEASONALITY AND TRAILING MA FOR RESIDUALS FOR ENTIRE 
## DATA SET. USE TWO-LEVEL (COMBINED) FORECAST TO FORECAST 
## 12 FUTURE PERIODS IN 2019.

# Fit a regression model with linear trend and seasonality for
# entire data set.
tot.trend.seas <- tslm(ridership.ts ~ trend  + season)
summary(tot.trend.seas)

# Create regression forecast for future 12 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, 
                       level = 0)
tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 4, 
                    align = "right")
tot.ma.trail.res

# Identify two-level forecast, regression with linear trend and 
# seasonality and trailing MA for regression residuals, for 
# entire data set (training period).
train.fst.2level <- tot.trend.seas.pred$fitted+tot.ma.trail.res
train.fst.2level

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, 
                         level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + 
                  tot.ma.trail.res.pred$mean
tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df <- round(data.frame(tot.trend.seas.pred$mean, 
               tot.ma.trail.res.pred$mean, tot.fst.2level), 3)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", 
                        "Combined.Fst")
future12.df

# Plot historical data set and two-level model's forecast for
# entire data set and future 12 monthly periods in 2019. 
plot(ridership.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xlim = c(1991, 2020.25), lwd =1, xaxt = "n",
     main = "Two-Level Model: Regression with Trend and Seasonality + Trailing MA for Residuals") 
axis(1, at = seq(1991, 2020.25, 1), labels = format(seq(1991, 2020.25, 1)))
lines(train.fst.2level, col = "blue", lwd = 2)
lines(tot.fst.2level, col = "blue", lty =5, lwd = 2)
legend(1992,3000, legend = c("Ridership", "Two-Level Model for Entire Data Set",
                             "Two-Level Model for Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2019, 2019), c(0, 3500))
text(2005, 3400, "Data Set")
text(2020.2, 3400, "Future")
arrows(1991, 3300, 2018.9, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 3300, 2021.3, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to create autocorrelation chart of 
# two-level model's residuals. 
Acf((ridership.ts - train.fst.2level), lag.max = 12, 
    main = "Autocorrelations of Two-Level Model's Residuals")


## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 MONTHLY PERIODS IN 2019.

# Create Holt-Winter's (HW) exponential smoothing for 
# entire data set. Use ets() function with model = "ZZZ" to 
# identify the best HW options and optimal alpha, beta, & gamma 
# to fit HW for the entire data set.
HW.ZZZ <- ets(ridership.ts, model = "ZZZ")
HW.ZZZ # Model appears to be (M, Ad, M), with alpha = 0.5334, 
# beta = 0.0014, gamma = 0.1441, and phi = 0.9698.

# Use forecast() function to make predictions using this 
# HW model for 12 months in 2019.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

# Plot HW model's predictions for historical data set and
# future 12 months in 2019.
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = "n",
     main = "Holt-Winter's Automatic Model for Entire Data Set and Forecast for Future 12 Periods", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(ridership.ts)
legend(1991,3100, 
       legend = c("Ridership", 
                  "Holt-Winter'sModel for Entire Data Set",
                  "Holt-Winter's Model Forecast, Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2019, 2019), c(0, 3500))
text(2005, 3400, "Data Set")
text(2020.2, 3400, "Future")
arrows(1991, 3300, 2018.9, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 3300, 2021.3, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Use Acf() function to create autocorrelation chart of 
# residuals of HW model's with automated options and parameters. 
Acf(HW.ZZZ.pred$residuals, lag.max = 12, 
  main = "Autocorrelations of Residuals of HW Model with Automatic Options")


## FIT TWO-LEVEL MODEL, LINEAR TREND AND SEASONALITY REGRESSION 
## AND AR(1) FOR REGRESSION RESIDUALS, FOR ENTIRE DATA SET. 


# Use tslm() function to create linear trend and seasonality model.
lin.season <- tslm(ridership.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions with linear trend and seasonal 
# model into the future 12 months of 2019.  
lin.season.pred <- forecast(lin.season, h = 12, level = 0)
lin.season.pred

# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into 
# the future 12 months of 2019.
residual.ar1 <- Arima(lin.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 12, level = 0)
residual.ar1.pred

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Create two-level model's forecast for the entire data set (training
# period).
train.lin.season.ar1.pred <- lin.season$fitted + residual.ar1$fitted
train.lin.season.ar1.pred

# Identify forecast for the future 12 periods of 2019 as sum of 
# linear trend and seasonality model and AR(1) model for residuals.
lin.season.ar1.pred <- lin.season.pred$mean + residual.ar1.pred$mean
lin.season.ar1.pred


# Create a data table with linear trend and seasonal forecast 
# for 12 future periods,
# AR(1) model for residuals for 12 future periods, and combined 
# two-level forecast for 12 future periods. 
table.df <- round(data.frame(lin.season.pred$mean, 
                             residual.ar1.pred$mean, lin.season.ar1.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df


# Plot historical data, predictions for historical data, and forecast 
# for 12 future periods.
plot(ridership.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(1300, 3500), xaxt = "n",
     bty = "l", xlim = c(1991, 2020.25), lwd = 2,
     main = "Two-Level Forecast: Regression with Trend and Seasonlity + AR(1)
     for Residuals") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(lin.season$fitted + residual.ar1$fitted, col = "blue", lwd = 2)
lines(lin.season.ar1.pred, col = "blue", lty = 5, lwd = 2)
legend(1992,3200, legend = c("Ridership Series for Training and Valiadaton Periods", 
                             "Two-Level Forecast for Training and Valiadtion Periods", 
                             "Two-Level Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2019, 2019), c(0, 3500))
text(2005, 3400, "Data Set")
text(2020.2, 3400, "Future")
arrows(1991, 3300, 2018.9, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 3300, 2021.3, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for residuals of
# two-level model for different lags (up to maximum of 12).
Acf((ridership.ts - train.lin.season.ar1.pred), lag.max = 12, 
    main = "Autocorrelation for Two-Level Model's Residuals for Entire Data Set")


## FIT AUTO ARIMA MODEL FOR ENTIRE DATA SET. 
## FORECAST AND PLOT DATA. 

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters 
# for entire data set.
auto.arima <- auto.arima(ridership.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods in 2019. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 12 future periods.
plot(ridership.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(1300, 3500), xaxt = "n", 
     bty = "l", xlim = c(1991, 2020.25), lwd = 2,
     main = "Auto ARIMA Model for Entire Dataset") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1992,2900, legend = c("Ridership Series", 
                             "Auto ARIMA Forecast", 
                             "Auto ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))
lines(c(2019, 2019), c(0, 3500))
text(2005, 3400, "Data Set")
text(2020.2, 3400, "Future")
arrows(1991, 3300, 2018.9, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 3300, 2021.3, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model's Residuals")


# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET USING VARIOUS METHODS.

# Use accuracy() function to identify common accuracy measures for:
# (1) Two-Level Model with Linear Trend & Seasonality Regression and 
#     Trailing MA for Regression Residuals,
# (2) Holt-Winter's Model with Automatic Selection of Model Options
#     and Parameters, 
# (3) Two-Level Model with Linear Trend & Seasonality Regression and 
#     Ar(1) Model for Regression Residuals,
# (4) Auto ARIMA Model,
# (5) Seasonal Naive forecast, and
# (6) Naive forecast.
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, ridership.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, ridership.ts), 3)
round(accuracy(lin.season$fitted + residual.ar1$fitted, ridership.ts), 3)
round(accuracy(auto.arima.pred$fitted, ridership.ts), 3)
round(accuracy((snaive(ridership.ts))$fitted, ridership.ts), 3)
round(accuracy((naive(ridership.ts))$fitted, ridership.ts), 3)

## CREATE DATA FRAME FOR AMTRAK DATA SET IN 2019.
## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET
## FOR AMTRAK DATA SET IN 2019.

# Create data frame.
Amtrak2019.data <- read.csv("Amtrak2019.csv")
Amtrak2019.data


## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION.

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a 
# season (year) is 12. With quarterly data, frequency in 
# a season (year) is equal to 4. Arguments start and end are 
# pairs: (season number, period number).
ridership2019.ts <- ts(Amtrak2019.data$Ridership2019, 
          start = c(2019, 1), end = c(2019, 12), freq = 12)
ridership2019.ts

# Point forecast in 12 months of 2019 based on two-level model 
# with linear trend and seasonality regression and trailing MA 
# for regression residuals.
tot.fst.2level

# Develop point forecast for 12 months of 2019 based on 
# Holt-Winter's model with automatic selection for model 
# options and parameters. 
HW.ZZZ.pred$mean

# Point forecast in 12 months of 2019 based on two-level model 
# with Linear trend and seasonality regression and AR(1) model 
# for regression residuals.
lin.season.ar1.pred

# Develop point forecast for 2019 based on auto-ARIMA model. 
auto.arima.pred$mean

# naive forecast for 2019. 
(naive(ridership2019.ts))$fitted


# MEASURE FORECAST ACCURACY FOR AMTRAK DATA IN 2019.

# Use accuracy() function to identify common accuracy measures for 2019:
# (1) Two-Level Model with Linear Trend & Seasonality Regression and 
#     Trailing MA for Regression Residuals,
# (2) Holt-Winter's Model with Automatic Selection of Model Options
#     and Parameters, 
# (3) Two-Level Model with Linear Trend & Seasonality Regression and 
#     Ar(1) Model for Regression Residuals,
# (4) Auto ARIMA Model,
# (5) Naive forecast.
round(accuracy(tot.fst.2level, ridership2019.ts), 3)
round(accuracy(HW.ZZZ.pred$mean, ridership2019.ts), 3)
round(accuracy(lin.season.ar1.pred, ridership2019.ts), 3)
round(accuracy(auto.arima.pred$mean, ridership2019.ts), 3)
round(accuracy((naive(ridership2019.ts))$fitted, ridership2019.ts), 3)
