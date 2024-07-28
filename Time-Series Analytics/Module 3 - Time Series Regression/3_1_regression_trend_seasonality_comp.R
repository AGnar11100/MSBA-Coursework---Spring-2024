
## USE FORECAST LIBRARY.

library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/misc/673_BAN/module3_regression")

# Create data frame.
Amtrak.data <- read.csv("Amtrak_comp.csv")

# See the first 6 records of the file.
head(Amtrak.data)


## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET
## AND PARTITION DATA SET.

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
ridership.ts <- ts(Amtrak.data$Ridership, 
            start = c(1991, 1), end = c(2018, 12), freq = 12)

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 60 
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), 
                   end = c(1991, nTrain + nValid))



## FIT REGRESSION MODEL WITH LINEAR TREND: MODEL 1. 
## FORECAST AND PLOT DATA, AND MEASURE ACURACY.

# Use tslm() function (time series linear model) to create regression model with 
# linear trend.
train.lin <- tslm(train.ts ~ trend)

# See summary of linear trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make forecast for validation period.
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)

# Plot ts data, linear trend and forecast for validation period.
plot(train.lin.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = "n",
     main = "Regression Model with Linear Trend", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)) )
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(1991,3200, legend = c("Ridership Time Series", "Linear Regression for Training Data",
                             "Linear Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
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

# Use accuracy() function to identify common accuracy measures with rounded
# values to 3 decimals.
round(accuracy(train.lin.pred$mean, valid.ts), 3)



## FIT REGRESSION MODEL WITH EXPONENTIAL TREND: MODEL 2. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create regression model with exponential trend.
# If lambda = 0, tslm function applies Box-Cox transformation
# for log(y) - exponential trend.
# If lambda = 1, tslm function will just have a linear trend
# (the same as the original regression with linear trend, train.lin).
train.expo <- tslm(train.ts ~ trend, lambda = 0)

# See summary of exponential trend model and associated parameters.
summary(train.expo)

# Apply forecast() function to make forecast using exponential  
# trend for validation period.  
train.expo.pred <- forecast(train.expo, h = nValid, level = 0)

# Plot ts data, exponential and linear trends, and 
# respective forecasts for validation period.
plot(train.expo.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(1300, 3500), bty = "l",
     xlim = c(1991, 2020.25),xaxt = "n",
     main = "Regression Models with Linear and Exponential Trends",
     lty = 1, lwd = 2, col = "blue")
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(train.expo.pred$fitted, col = "blue", lwd = 2)
lines(train.lin.pred$fitted, col = "brown", lwd = 2, lty = 3)
lines(train.lin.pred$mean, col = "brown", lwd = 2, lty = 3)
lines(valid.ts, col = "black", lty = 1)
lines(train.ts, col = "black", lty = 1)
legend(1991,3200, legend = c("Ridership Time Series", 
                             "Exponentail Trend for Training and Validation Data", 
                             "Linear Trend for Training and Validation Data"), 
       col = c("black", "blue" , "brown"), 
       lty = c(1, 1, 3), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
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

# Use accuracy() function to identify common accuracy measures for regression model
# with linear trend and exponential trend.
round(accuracy(train.lin.pred$mean, valid.ts), 3)
round(accuracy(train.expo.pred$mean, valid.ts), 3)



## FIT REGRESSION MODEL WITH QUADRATIC (POLYNOMIAL) TREND: MODEL 3. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)

# Plot ts data, regression with quadratic trend and forecast for validation period.
plot(train.quad.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(1300, 3500), bty = "l",
     xlim = c(1991, 2020.25), xaxt = "n",
     main = "Regression Model with Quadratic Trend", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)) )
lines(train.quad.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(1991,3200, legend = c("Ridership Time Series", "Quadratic Trend for Training Data",
                             "Quadratic Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
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

# Use accuracy() function to identify common accuracy measures
# for regression models with linear trend and quadratic (polynomial) trend.
round(accuracy(train.lin.pred$mean, valid.ts), 3)
round(accuracy(train.quad.pred$mean, valid.ts), 3)



## FIT REGRESSION MODEL WITH SEASONALITY: MODEL 4. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# If necessary, run the following code to identify seasons.
train.season$data 

# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)

# Plot ts data, regression model with seasonality, and forecast for validation period.
plot(train.season.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)",
     ylim = c(1300, 3500), bty = "l",
     xlim = c(1991, 2020.25), xaxt = "n",
     main = "Regression Model with Seasonality ", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(train.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(1991,3200, legend = c("Ridership Time Series", "Seasonality Model for Training Data",
                             "Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
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

# Plot residuals of the model with seasonality.
plot(train.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-600, 850), bty = "l",
     xlim = c(1991, 2020.25), xaxt = "n", 
     main = "Residuals for the Seasonality Model", 
     col = "brown", lwd = 2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(valid.ts - train.season.pred$mean, col = "brown", lty = 1, lwd=2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2014, 2014), c(-650, 850))
lines(c(2019, 2019), c(-650, 850))
text(2002, 840, "Training")
text(2016.5, 840, "Validation")
text(2020.2, 840, "Future")
arrows(2013.9,800, 1991, 800, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.1, 800, 2018.9, 800, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 800, 2021.3, 800, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures
# for regression models with (1) linear trend, (2) quadratic (polynomial) trend,
# and (3) seasonality.
round(accuracy(train.lin.pred$mean, valid.ts), 3)
round(accuracy(train.quad.pred$mean, valid.ts), 3)
round(accuracy(train.season.pred$mean, valid.ts), 3)

## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY: MODEL 5. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)

# Plot ts data, linear trend and seasonality data, and predictions for validation period.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(1300, 3500), bty = "l",
     xlim = c(1991, 2020.25), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)
legend(1991,3200, legend = c("Ridership Time Series", 
                             "Linear Trend and Seasonality Model for Training Data",
                             "Linear Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
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

# Plot residuals of predictions with linear trend and seasonality.
plot(train.lin.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-600, 650), bty = "l",
     xlim = c(1991, 2020.25), xaxt = "n",
     main = "Residuals for Linear Trend and Seasonality Model", 
     col = "brown", lwd = 2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(valid.ts - train.lin.season.pred$mean, col = "brown", lty = 1, lwd=2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
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

# Use accuracy() function to identify common accuracy measures
# for various regression models: (1)linear trend, (2) quadratic  
# (polynomial) trend, (3) seasonality, and (4) linear trend and seasonality.
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.season.pred$mean, valid.ts),3)

## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY: MODEL 6. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create quadratic trend and seasonal model.
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.quad.season.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(1300, 3500), bty = "l",
     xlim = c(1991, 2020.25), xaxt = "n", 
     main = "Regression Model with Quadratic Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(train.quad.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)
legend(1991,3200, legend = c("Ridership Time Series", 
                             "Quadratic Trend and Seasonality Model for Training Data",
                             "Quadratic Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
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

# Plot residuals of predictions with trend and seasonality.
plot(train.quad.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-750, 650), bty = "l",
     xlim = c(1991, 2020.25), xaxt = "n",
     main = "Residuals for Quadratic Trend and Seasonality Model", 
     col = "brown", lwd = 2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(valid.ts - train.quad.season.pred$mean, col = "brown", lty = 1, lwd=2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2014, 2014), c(-800, 650))
lines(c(2019, 2019), c(-800, 650))
text(2002, 640, "Training")
text(2016.5, 640, "Validation")
text(2020.2, 640, "Future")
arrows(2013.9, 600, 1991, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.1, 600, 2018.9, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 600, 2021.3, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures
# for various regression models: (1) linear trend, (2) exponential
# trend, (3) quadratic (polynomial) trend, (4) seasonality, 
# (5) linear trend and seasonality, and (6) quadratic trend 
# and seasonality. 
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.expo.pred$mean, valid.ts), 3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.season.pred$mean, valid.ts),3)


## FIT REGRESSION MODELS WITH LINEAR TREND, AND LINEAR TREND 
## AND SEASONALITY FOR ENTIRE DATASET. FORECAST AND PLOT 
## DATA, AND MEASURE ACCURACY.

# Use tslm() function to create linear trend model.
lin.trend <- tslm(ridership.ts ~ trend)

# See summary of linear trend equation and associated parameters.
summary(lin.trend)

# Apply forecast() function to make predictions for ts with 
# linear trend  data in 12 future periods.
lin.trend.pred <- forecast(lin.trend, h = 12, level = 0)

# Plot ts data, regression model with linear trend data, 
# and predictions for future 12 periods.
plot(lin.trend.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(1300, 3500), bty = "l",
     xlim = c(1991, 2020.25), xaxt = "n",
     main = "Regression Model with Linear Trend and Forecast for Future Periods", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(lin.trend.pred$fitted, col = "blue", lwd = 2)
lines(ridership.ts)
legend(1991,3200, legend = c("Ridership Time Series", 
                             "Linear Trend  for Entire Data",
                             "Linear Trend for Future 12 Periods"), 
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


# Use tslm() function to create regression model with linear trend 
# and seasonality.
lin.season <- tslm(ridership.ts ~ trend + season)

# See summary of linear trend and seasonality equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in 12 future periods.
lin.season.pred <- forecast(lin.season, h = 12, level = 0)

# Plot ts data, regression model with linear trend and seasonality data, 
# and predictions for future 12 periods.
plot(lin.season.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(1300, 3500), bty = "l",
     xlim = c(1991, 2020.25), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality and Forecast for Future Periods", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(lin.season.pred$fitted, col = "blue", lwd = 2)
lines(ridership.ts)
legend(1991,3200, legend = c("Ridership Time Series", 
                             "Linear Trend and Seasonality Model for Entire Data",
                             "Linear and Seasonality Forecast for Future 12 Periods"), 
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


## COMPARE ACCURACY MEASURES OF REGRESSION FORECAST WITH LINEAR AND 
## QUANDRATIC TREND AND SEASONALITY FOR THE ENTIRE DATA SET WITH 
## ACCURACY MEASURES OF NAIVE FORECAST AND SEASONAL NAIVE FORECAST 
## FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures
# for naive model, seasonal naive, and regression model with quadratic trend and seasonality.
round(accuracy(lin.trend.pred$fitted, ridership.ts),3)
round(accuracy(lin.season.pred$fitted, ridership.ts),3)
round(accuracy((naive(ridership.ts))$fitted, ridership.ts), 3)
round(accuracy((snaive(ridership.ts))$fitted, ridership.ts), 3)


## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY: ALTERNATIVE MODELS.
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Develop seas object with January being the first month 
# in the model.
seas <- seasonaldummy(train.ts)
seas

# Create linear trend and seasonality model with January as
# the first month in the model.
train.lin.season1 <- tslm(train.ts ~ trend + seas)
summary(train.lin.season1)

# Develop data frame for validation period utilizing this model. 
forecast.param_1 <- data.frame(trend = c(277:336), 
  Jan = c(1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
          1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
  Feb = c(0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
          0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0), 
  Mar = c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
          0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0), 
  Apr = c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
          0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0), 
  May = c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
          0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0), 
  Jun = c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
          0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0), 
  Jul = c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,
          0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),
  Aug = c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
          0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0), 
  Sep = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
          0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),
  Oct = c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
          0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0),
  Nov = c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,
          0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0))

forecast.param_1

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.lin.season1.pred <- forecast(train.lin.season1, 
                         newdata = forecast.param_1, level = 0)
train.lin.season1.pred


# Created linear trend and seasonality model with only specific 
# months (January-February, May, and July-September).
seas[ ,c(1:2, 5, 7:9)]
      
train.lin.season2 <- tslm(train.ts ~ trend + seas[ ,c(1:2, 5, 7:9)])
summary(train.lin.season2)

# Develop data frame for validation period utilizing this model. 
forecast.param_2 <- data.frame(trend = c(277:336), 
Jan = c(1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
        1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
Feb = c(0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
        0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0), 
May = c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
        0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0), 
Jul = c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,
        0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),
Aug = c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
        0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0), 
Sep = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
        0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0))
                               
forecast.param_2

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.lin.season2.pred <- forecast(train.lin.season2, 
                            newdata = forecast.param_2, level = 0)
train.lin.season2.pred

# Use accuracy() function to identify common accuracy measures
# for various regression models: original quadratic trend and 
# seasonality and two alternative models for trend and seasonality. 
round(accuracy(train.lin.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.season1.pred$mean, valid.ts),3)
round(accuracy(train.lin.season2.pred$mean, valid.ts),3)






