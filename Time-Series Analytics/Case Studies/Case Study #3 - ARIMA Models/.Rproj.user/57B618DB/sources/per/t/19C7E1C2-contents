
## USE FORECAST LIBRARY.

library(forecast)


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
nValid <- 60 
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), 
                   end = c(1991, nTrain + nValid))

# Use Acf() function to identify autocorrelation for training and validation
# data sets, and plot autocorrelation for different lags (up to maximum of 12)
Acf(train.ts, lag.max = 12, main = "Autocorrelation for Amtrak Training Data Set")
Acf(valid.ts, lag.max = 12, main = "Autocorrelation for Amtrak Validation Data Set")


## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY. 
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION FOR RESIDUALS.
## PLOT RESIDUALS.

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonal model in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

# Plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = "n",
     main = "Regression with Linear Trend and Seasonality", 
     lwd = 2, lty = 2, col = "blue") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lwd = 2, lty = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1992,3200, legend = c("Ridership Time Series", "Regression for Training Data",
                             "Forecast for Validation Data"), 
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

# Plot residuals of the predictions with trend and seasonality.
plot(train.lin.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-600, 650), bty = "l",
     xlim = c(1991, 2020.25), xaxt = "n",
     main = "Regresssion Residuals for Training and Validation Data", 
     col = "brown", lwd = 2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(valid.ts - train.lin.season.pred$mean, col = "brown", lwd = 2, lty = 1)

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

# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(train.lin.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Amtrak Training Residuals")
Acf(valid.ts - train.lin.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Amtrak Validation Residuals")


## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
## CREATE TWO-LEVEL MODEL WITH LINEAR TREND AND SEASONALITY MODEL 
## AND AR(1) RESIDUALS.
## PLOT DATA AND IDENTIFY ACCURACY MEASURES.

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- round(data.frame(train.ts, train.lin.season$fitted, 
            train.lin.season$residuals, res.ar1$fitted, res.ar1$residuals), 3)
names(train.df) <- c("Ridership", "Regression", "Residuals",
                     "AR.Model", "AR.Model.Residuals")
train.df

# Plot residuals of the predictions for training data before AR(1).
plot(train.lin.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-600, 650), bty = "l",
     xlim = c(1991, 2014), xaxt = "n", 
     main = "Regresssion Residuals for Training Data before AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(1991, 2014, 1), labels = format(seq(1991, 2014, 1)))

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2014, 2014), c(-650, 650))
text(2002, 640, "Training")
arrows(2013.9, 600, 1991, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Plot residuals of the residuals for training data after AR(1).
plot(res.ar1$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-600, 650), bty = "l",
     xlim = c(1991, 2014), xaxt = "n",
     main = "Residuals of Residuals for Training Data after AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(1991, 2014, 1), labels = format(seq(1991, 2014, 1)))

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2014, 2014), c(-650, 650))
text(2002, 640, "Training")
arrows(2013.9, 600, 1991, 600, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Amtrak Training Residuals of Residuals")

# Create two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for validation period.

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
valid.two.level.pred <- train.lin.season.pred$mean + res.ar1.pred$mean

valid.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
            res.ar1.pred$mean, valid.two.level.pred),3)
names(valid.df) <- c("Ridership", "Reg.Forecast", 
                  "AR(1)Forecast", "Combined.Forecast")
valid.df

# plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
plot(valid.two.level.pred, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = "n",
     main = "Two-Level Forecast: Regression with Trend
             and Seasonlity + AR(1) for Residuals", lwd = 2,
             col = "blue", lty = 2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lwd = 2, lty = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1992,3200, legend = c("Ridership Time Series", "Regression for Training Data",
                             "Two Level Forecast for Validation Data"), 
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

# Use accuracy() function to identify common accuracy measures for validation period forecast:
# (1) two-level model (linear trend and seasonal model + AR(1) model for residuals),
# (2) linear trend and seasonality model only.
round(accuracy(valid.two.level.pred, valid.ts), 3)
round(accuracy(train.lin.season.pred$mean, valid.ts), 3)



## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY 
## FOR ENTIRE DATASET. FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create linear trend and seasonality model.
lin.season <- tslm(ridership.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions with linear trend and seasonal 
# model into the future 12 months.  
lin.season.pred <- forecast(lin.season, h = 12, level = 0)
lin.season.pred

# Use Acf() function to identify autocorrelation for the model residuals 
# for entire data set, and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(lin.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation of Regression Residuals for Entire Data Set")


# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future 12 months.
residual.ar1 <- Arima(lin.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 12, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Use Acf() function to identify autocorrelation for the residuals of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")


# Identify forecast for the future 12 periods as sum of linear trend and 
# seasonal model and AR(1) model for residuals.
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

# plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))
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


# Use accuracy() function to identify common accuracy measures for:
# (1) two-level model (linear trend and seasonality model 
#     + AR(1) model for residuals),
# (2) linear trend and seasonality model only, and
# (3) seasonal naive forecast. 
round(accuracy(lin.season$fitted + residual.ar1$fitted, ridership.ts), 3)
round(accuracy(lin.season$fitted, ridership.ts), 3)
round(accuracy((snaive(ridership.ts))$fitted, ridership.ts), 3)
