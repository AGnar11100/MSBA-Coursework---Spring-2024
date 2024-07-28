
## USE FORECAST AND ZOO LIBRARIES.

library(forecast)
library(zoo)


## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/misc/673_BAN/module2_smoothing")

# Create data frame.
Amtrak.data <- read.csv("Amtrak_comp.csv")

# See the first 6 records of the file.
head(Amtrak.data)

## CREATE TIME SERIES DATA SET.

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
ridership.ts <- ts(Amtrak.data$Ridership, 
                   start = c(1991, 1), end = c(2018, 12), freq = 12)
head(ridership.ts)


## CREATE DATA PARTITION.
## PLOT PARTITION DATA.

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 60
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), 
                   end = c(1991, nTrain + nValid))

# Plot the time series data and visualize partitions. 
plot(train.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), bty = "l",
     xaxt = "n", xlim = c(1991, 2020.25), 
     main = "Radership Data: Training and Validation Partitions", lwd = 2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(valid.ts, col = "black", lty = 1, lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2014, 2014), c(0, 3500))
lines(c(2019, 2019), c(0, 3500))
text(2002, 3400, "Training")
text(2016.5, 3400, "Validation")
text(2020.2, 3400, "Future")
arrows(1991, 3300, 2013.9, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.1, 3300, 2018.9, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 3300, 2021.3, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## USE REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY 
## FOR TRAINING PARTITION.
## IDENTIFY REGRESSION RESIDUALS FOR TRAINING PARTITION. 
## CREATE TRAILING MA USING REGRESSION RESIDUALS FOR TRAINING PARTITION. 
## PLOT REGRESSION RESIDUALS AND TRAILING MA FOR RESIDUALS IN TRAINING
## PARTITION.

# Fit a regression model with linear trend and seasonality for
# training partition. 
trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)

# Identify and display regression residuals for training
# partition (differences between actual and regression values 
# in the same periods).
trend.seas.res <- trend.seas$residuals
trend.seas.res

# Apply trailing MA for residuals with window width k = 4
# for training partition.
ma.trail.res <- rollmean(trend.seas.res, k = 4, align = "right")
ma.trail.res

# Plot residuals in training partition. 
plot(trend.seas.res, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(-600, 650), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = "n",
     main = "Regression Residuals and Trailing MA for Training Partition", 
     col = "brown", lwd =2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(ma.trail.res, col = "blue", lwd = 2, lty = 1)
legend(1994,300, legend = c("Regression Residuals, Training Partition", 
                            "Trailing MA, k=4, Training Partition"), 
       col = c("brown", "blue"), 
       lty = c(1, 1, 1, 2), lwd =c(2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2014, 2014), c(-650, 650))
lines(c(2019, 2019), c(-650, 650))
text(2002, 640, "Training")
text(2016.5, 640, "Validation")
text(2020.2, 640, "Future")
arrows(1991, 600, 2013.9, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.1, 600, 2018.9, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 600, 2021.3, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## FORECAST USING REGRESSION AND TRAILING MA FOR VALIDATION PERIOD.
## PLOT REGRESSION FORECAST AND TRAILING MA FOR RESIDUALS IN 
## TRAINING AND VALIDATION PERIODS.
## MEASURE ACCURACY OF REGRESSION AND TWO-LEVEL FORECASTS.

# Create regression forecast with trend and seasonality for 
# validation period.
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred

# Plot original data and regression forecast for training and 
# validation partitions.
plot(ridership.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = "n",
     main = "Regression Forecast in Training and Validation Partitions ") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(trend.seas$fitted, col = "blue", lwd = 2, lty = 1)
lines(trend.seas.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1992,3200, legend = c("Ridership Data", 
                             "Regression Forecast, Training Partition", 
                             "Regression Forecast, Validation Partition"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2014, 2014), c(0, 3500))
lines(c(2019, 2019), c(0, 3500))
text(2002, 3400, "Training")
text(2016.5, 3400, "Validation")
text(2020.2, 3400, "Future")
arrows(1991, 3300, 2013.9, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.1, 3300, 2018.9, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 3300, 2021.3, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Regression residuals in validation period.
trend.seas.res.valid <- valid.ts - trend.seas.pred$mean
trend.seas.res.valid

# Create residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

# Plot residuals and MA residuals forecast in training and validation partitions. 
plot(trend.seas.res, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(-600, 650), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = "n",
     main = "Regression Residuals and Trailing MA for Residuals", 
     col = "brown", lwd =2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(trend.seas.res.valid, col = "brown", lwd = 2, lty = 2)
lines(ma.trail.res, col = "blue", lwd = 2, lty = 1)
lines(ma.trail.res.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1994,550, legend = c("Regression Residuals, Training Partition", 
                             "Regression Residuals, Validation Partition",
                             "MA Forecast (k=4), Training Partition", 
                             "MA forecast (k=4), Validation Partition"), 
       col = c("brown", "brown", "blue", "blue"), 
       lty = c(1, 2, 1, 2), lwd =c(2, 2, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2014, 2014), c(-650, 650))
lines(c(2019, 2019), c(-650, 650))
text(2002, 640, "Training")
text(2016.5, 640, "Validation")
text(2020.2, 640, "Future")
arrows(2013.9, 600, 1991, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.1, 600, 2018.9, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 600, 2021.3, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level

# Create a table for validation period: validation data, regression 
# forecast, trailing MA for residuals and total forecast.
valid.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                       ma.trail.res.pred$mean, 
                       fst.2level), 3)
names(valid.df) <- c("Ridership", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(fst.2level, valid.ts), 3)


## USE REGRESSION AND TRAILING MA FORECASTS FOR ENTIRE DATA SET. 
## USE 2-LEVEL (COMBINED) FORECAST TO FORECAST 12 FUTURE PERIODS.
## MEASURE ACCURACY OF REGRESSION AND 2-LEVEL FORECASTS FOR
## ENTIRE DATA SET.

# Fit a regression model with linear trend and seasonality for
# entire data set.
tot.trend.seas <- tslm(ridership.ts ~ trend  + season)
summary(tot.trend.seas)

# Create regression forecast for future 12 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, level = 0)
tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 4, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df <- round(data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                       tot.fst.2level), 3)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(tot.trend.seas.pred$fitted, ridership.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, ridership.ts), 3)
round(accuracy((naive(ridership.ts))$fitted, ridership.ts), 3)
round(accuracy((snaive(ridership.ts))$fitted, ridership.ts), 3)


## GENERATE PLOT OF ORIGINAL DATA AND REGRESSION FORECAST, AND PREDICTIONS
## IN FUTURE 12 PERIODS.
## GENERATE PLOT OF REGRESSION RESIDUALS, TRAILING MA FOR RESIDUALS, AND 
## TRAILING MA FORECAST IN FUTURE 12 PERIODS.

# Plot original Ridership time series data and regression model.
plot(ridership.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xlim = c(1991, 2020.25), lwd =1, xaxt = "n",
     main = "Ridership Data and Regression with Trend and Seasonality") 
axis(1, at = seq(1991, 2020.25, 1), labels = format(seq(1991, 2020.25, 1)))
lines(tot.trend.seas$fitted, col = "blue", lwd = 2)
lines(tot.trend.seas.pred$mean, col = "blue", lty =5, lwd = 2)
legend(1992,3000, legend = c("Ridership", "Regression",
                             "Regression Forecast for Future 12 Periods"), 
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


# Plot regression residuals data and trailing MA based on residuals.
plot(tot.trend.seas.res, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(-600, 650), 
     bty = "l", xaxt = "n", xlim = c(1991, 2020.25), lwd =1, col = "brown", 
     main = "Regression Residuals and Trailing MA for Residuals") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(tot.ma.trail.res, col = "blue", lwd = 2, lty = 1)
lines(tot.ma.trail.res.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1992, 550, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=4) for Residuals", 
                             "Trailing MA Forecast (k=4) for Future 12 Periods"), 
       col = c("brown", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2019, 2019), c(-650, 650))
text(2005, 640, "Data Set")
text(2020.2, 640, "Future")
arrows(1991, 600, 2018.9, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 600, 2021.3, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## DEVELOP WEIGHTED MOVING AVERAGE (WMA).

# Use TTR library to apply WMA() - weighted moving average function.
# Need to define n = moving average window or number of periods 
# to average over, and also wts - weights for periods in
# WMA. The last weight in wts represents the most recent period in WMA,
# and the first weight - is the least recent.
library(TTR)

# Develop weighted moving average with n = 4 and n = 6
wma.trailing_4 <- WMA(ridership.ts, n = 4, 
                 wts = c(0.1, 0.2, 0.3, 0.4))
wma.trailing_6 <- WMA(ridership.ts, n = 6, 
                 wts = c(0.30, 0.25, 0.20, 0.15, 0.05, 0.05))

# Plot original data and trailing MA.
plot(ridership.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300,3500), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = "n", 
     main = "Weighted Moving Average") 
axis(1, at = seq(1991, 2020.25, 1), labels = format(seq(1991, 2020.25, 1)))
lines(wma.trailing_4, col = "brown", lwd = 2, lty = 1)
lines(wma.trailing_6, col = "blue", lwd = 2, lty = 5)
legend(1992,2800, legend = c("Ridership", "WMA, k=4", 
                             "WMA, k=6"), 
       col = c("black", "brown", "blue"), 
       lty = c(1, 1, 5), lwd =c(1, 2, 2), bty = "n")










