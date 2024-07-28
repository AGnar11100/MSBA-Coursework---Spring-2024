
## USE FORECAST LIBRARY.

library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/misc/673_BAN/module1_introduction")

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

## CREATE DATA PARTITION.
## PLOT DATA PARTITION.

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
# Total number of period length(ridership.ts) = 336.
# nvalid = 60 months (5 years), from January 2014 to December 2018.
# nTrain = 276 months (23 years), from January 1991 to December 2013.
nValid <- 60
nTrain <- length(ridership.ts) - nValid 
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), 
                   end = c(1991, nTrain + nValid))

# Plot the time series data and visualize partitions. 
plot(train.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = 'n', main = "", lwd = 2) 
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


## FIT REGRESSION MODEL TO TIME SERIES.
## FORECAST USING VALIDATION SET.
## PLOT FORECASTS.

# Use tslm() function to fit a regression model (equation) to the time series 
# with linear trend (ridership.lin) and quadratic trend model (rideship.quad).
ridership.lin <- tslm(train.ts ~ trend)
ridership.quad <- tslm(train.ts ~ trend + I(trend^2))

# Apply forecast() function to make predictions for ts data in
# training and validation sets.  
ridership.lin.pred <- forecast(ridership.lin, h = nValid, level = c(80, 95))
ridership.lin.pred

ridership.quad.pred <- forecast(ridership.quad, h = nValid, level = c(80, 95))

# Plot predictions for linear trend forecast.
plot(ridership.lin.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), bty = "l",
     xlim = c(1991, 2020.25), xaxt = 'n', main = "Linear Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)) )
lines(ridership.lin$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

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

# Plot predictions for quadratic trend forecast.
plot(ridership.quad.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), bty = "l",
     xlim = c(1991, 2020.25), xaxt = 'n', main = "Quadratic Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)) )
lines(ridership.quad$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

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


## IDENTIFY FORECAST ACCURACY

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(ridership.lin.pred$mean, valid.ts), 3)
round(accuracy(ridership.quad.pred$mean, valid.ts), 3)



## IDENTIFY NAIVE AND SEASONAL NAIVE FORECASTS.

# Use naive() to make naive forecast (ridership.naive.pred) 
# for validation data. 
# Use snaive() to make seasonal naive forecast (ridership.snaive.pred) for 
# validation data. 
ridership.naive.pred <- naive(train.ts, h = nValid)
ridership.snaive.pred <- snaive(train.ts, h = nValid)

# Plot the predictions for naive forecast.
plot(ridership.naive.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = "n",
     main = "Naive Forecast", col = "blue", lwd =2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(ridership.naive.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot the predictions for seasonal naive forecast.
plot(ridership.snaive.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xaxt = "n", xlim = c(1991, 2020.25), 
     main = "Seasonal Naive Forecast", col = "blue", lwd =2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(ridership.snaive.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

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

## IDENTIFY FORECAST ACCURACY FOR NAIVE and SEASONAL NAIVAE FORECASTS.

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(ridership.naive.pred$mean, valid.ts), 3)
round(accuracy(ridership.snaive.pred$mean, valid.ts), 3)


## IDENTIFY FORECAST ERRORS FOR LINEAR TREND FORECAST.
## PLOT FORECAST ERRORS FOR LINEAR TREND FORECAST.
## DEVELOP ERROR HISTOGRAM FOR LINEAR TREND FORECAST.

# Use $residuals element for the object ridership.lin.pred to
# identify training residuals.
ridership.train.res <- ridership.lin.pred$residuals
ridership.train.res

# Use the difference between actual and forecast validation data
# to identify validation errors (residuals).
ridership.valid.res <- valid.ts - ridership.lin.pred$mean
ridership.valid.res

# Plot forecast errors for linear trend forecast.
plot(ridership.valid.res, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(-600, 650), bty = "l",
     xlim = c(1991, 2020.25), xaxt = 'n',
     main = "Forecast Errors for Linear Trend Forecast", 
     col = "brown", lwd =2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(ridership.train.res, col = "brown", lwd = 2)

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


# Use hist() function to develop histogram for the model residuals.
hist(ridership.train.res, ylab = "Frequency", xlab = "Forecast Error",
     bty = "l", main = "Histogram of Linear Trend Forecast Errors", 
     col = "brown")

# Plot predictions for linear trend forecast with prediction interval
# for validation period.

plot(ridership.lin.pred, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), bty = "l",
     xlim = c(1991, 2020.25), xaxt = "n",
     main = "Linear Trend Forecast with Prediction Interval (80% and 95%) ", 
     col = "black", flty = 2, lwd =2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(ridership.lin$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd =2, lty = 1)

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