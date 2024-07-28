
## USE FORECAST LIBRARY.

library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/misc/673_BAN/module3_regression")

# Create data frame.
delin.data <- read.csv("delinquency_rate_comp.csv")

# See the first 6 records of the file.
head(delin.data)
tail(delin.data)

## Columns in delin.data:
# time_period - quarterly period from Q1 of 2000 
#               to Q4 of 2022.
# delin - delinquency rate on credit card loans for all 
#         commercial banks, %
# ccredit - consumer credit (non-revolving) including 
#           all credits/loans in banks, % of change  
# unemploy - overall unemployment, % 


## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
## PARTITION DATA SET.

# Function ts() takes three arguments: start, end, and freq.
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
delin.ts <- ts(delin.data$delin, 
            start = c(2000, 1), end = c(2022, 4), freq = 4)
ccredit.ts <- ts(delin.data$ccredit, 
            start = c(2000, 1), end = c(2022, 4), freq = 4)
unemploy.ts <- ts(delin.data$unemploy, 
            start = c(2000, 1), end = c(2022, 4), freq = 4)

# Define the numbers of months in the training and validation sets for
# delin.ts, nTrain and nValid, respectively.
nValid <- 20 
nTrain <- length(delin.ts) - nValid
train.ts <- window(delin.ts, start = c(2000, 1), end = c(2000, nTrain))
valid.ts <- window(delin.ts, start = c(2000, nTrain + 1), 
                   end = c(2000, nTrain + nValid))

## FIT VARIOUS REGRESSION MODELS FOR TRAINING DATA SET. 
## FORECAST IN VALIDATION PERIOD AND PLOT DATA AND 
## REGRESSION MODELS.
## PROVIDE ACCURACY MEASURES FOR THESE MODELS.

# Use tslm() function to create linear trend (delin.lin) 
# model for training partition. Forecast for the validation 
# partition periods (delin.lin.pred).
delin.lin <- tslm(train.ts ~ trend)
summary(delin.lin)
delin.lin.pred <- forecast(delin.lin, h = nValid, level = 0)
delin.lin.pred

# Use plot() function to create plot with linear trend. 
plot(delin.ts, 
     xlab = "Time", ylab = "Delinquency (in %)", xaxt = "n",
     ylim = c (1, 8), xlim = c(2000, 2026.25), bty = "l", lwd = 1.5,
     main = "Delinquency with Linear Trend Forecast")
axis(1, at = seq(2000, 2026, 1), labels = format(seq(2000, 2026, 1)))
lines(delin.lin$fitted, lwd = 2, col="blue")
lines(delin.lin.pred$mean, lwd = 2, col="brown")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2018, 2018), c(0, 8))
lines(c(2023, 2023), c(0, 8))
text(2009, 8, "Training")
text(2020.5, 8, "Validation")
text(2024.5, 8, "Future")
arrows(2000, 7.6, 2017.9, 7.6, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2018.1, 7.6, 2022.9, 7.6, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 7.6, 2026, 7.6, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use tslm() function to create quadratic trend (delin.quad)
# model for time series data. Forecast for the validation 
# partition periods (delin.quad.pred).
delin.quad <- tslm(train.ts ~ trend + I(trend^2))
summary(delin.quad)
delin.quad.pred <- forecast(delin.quad, h = nValid, level = 0)
delin.lin.pred

# Use plot() function to create plot with quadratic trend. 
plot(delin.ts, 
     xlab = "Time", ylab = "Delinquency (in %)", xaxt = "n",
     ylim = c (-2, 8), xlim = c(2000, 2026.25), bty = "l", lwd = 1.5,
     main = "Delinquency with Quadratic Trend Forecast")
axis(1, at = seq(2000, 2026, 1), labels = format(seq(2000, 2026, 1)))
lines(delin.quad$fitted, lwd = 2, col="blue")
lines(delin.quad.pred$mean, lwd = 2, col="brown")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2018, 2018), c(-2.5, 8))
lines(c(2023, 2023), c(-2.5, 8))
text(2009, 8, "Training")
text(2020.5, 8, "Validation")
text(2024.5, 8, "Future")
arrows(2000, 7.6, 2017.9, 7.6, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2018.1, 7.6, 2022.9, 7.6, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 7.6, 2026, 7.6, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use tslm() function to create linear trend and seasonality
# model (delin.lin.seas) for time series data. Forecast for 
# the validation partition periods (delin.lin.seas.pred).
delin.lin.seas <- tslm(train.ts ~ trend + season)
summary(delin.lin.seas)
delin.lin.seas.pred <- forecast(delin.lin.seas, h = nValid, level = 0)
delin.lin.seas.pred

# Use plot() function to create plot with linear trend and
# seasonality.
plot(delin.ts, 
     xlab = "Time", ylab = "Delinquency (in %)", xaxt = "n",
     ylim = c (1, 8), xlim = c(2000, 2026.25), bty = "l", lwd = 1.5,
     main = "Delinquency with Linear Trend and Seasonality Forecast")
axis(1, at = seq(2000, 2026, 1), labels = format(seq(2000, 2026, 1)))
lines(delin.lin.seas$fitted, lwd = 2, col="blue")
lines(delin.lin.seas.pred$mean, lwd = 2, col="brown")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2018, 2018), c(0, 8))
lines(c(2023, 2023), c(0, 8))
text(2009, 8, "Training")
text(2020.5, 8, "Validation")
text(2024.5, 8, "Future")
arrows(2000, 7.6, 2017.9, 7.6, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2018.1, 7.6, 2022.9, 7.6, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 7.6, 2026, 7.6, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Accuracy measures for the three forecasts: linear trend, 
# quadratic trend, and linear trend with seasonality. 
round(accuracy(delin.lin.pred$mean, valid.ts),3)
round(accuracy(delin.quad.pred$mean, valid.ts),3)
round(accuracy(delin.lin.seas.pred$mean, valid.ts),3)

#---------------------------------------------------------------- RESUME HERE ----------------------------------
## FIT LINEAR TREND REGRESSION MODEL FOR ENTIRE DATA SET. 
## FORECAST AND PLOT DATA AND REGRESSION MODEL.

# Use tslm() function to create linear trend (delin.lin.tot) 
# model for entire data set. Forecast for the future 8 
# periods (delin.lin.tot.pred).
delin.lin.tot <- tslm(delin.ts ~ trend)
summary(delin.lin.tot)
delin.lin.tot.pred <- forecast(delin.lin.tot, h = 8, level = 0)
delin.lin.tot.pred

# Use plot() function to create plot with linear trend for entire
# data set. 
plot(delin.ts, 
     xlab = "Time", ylab = "Delinquency (in %)", xaxt = "n",
     ylim = c (1, 8), bty = "l", lwd = 1.5, xlim = c(2000, 2026.25),
     main = "Delinquency with Linear Trend Forecast for Entire Data Set", 
     )
axis(1, at = seq(2000, 2026, 1), labels = format(seq(2000, 2026, 1)))
lines(delin.lin.tot$fitted, lwd = 2, col="blue")
lines(delin.lin.tot.pred$mean, lwd = 2, col="brown")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2023, 2023), c(0, 8))
text(2009, 8, "Training")
text(2024.5, 8, "Future")
arrows(2000, 7.6, 2022.9, 7.6, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 7.6, 2026, 7.6, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## FIT REGRESSION MODEL WITH LINEAR TREND AND TWO OR
## ONE EXTERNAL VARIABLES FOR ENTIRE DATA SET. 
## FORECAST DATA AND MEASURE FORECAST ACCURACY.

# Check correlation between the delin variable and 
# external variables, ccredit and unemploy. 

# The null hypothesis - correlation coefficient is equal 
# to 0 (zero). With 95% of confidence level, p-value 
# should be equal to or greater than 0.05. 
# The alternative hypothesis - correlation coefficient is
# not equal to 0 (zero). With 95% of confidence level, p-value 
# should be less than 0.05.
corr_ccredit <- cor.test(delin.data$delin, delin.data$ccredit, 
                   method = "pearson")
corr_ccredit

corr_unemploy <- cor.test(delin.data$delin, delin.data$unemploy, 
                   method = "pearson")
corr_unemploy

# Use tslm() function to create linear trend with two 
# external variables.
delin.external_2 <- tslm(delin.ts ~ trend + 
                    ccredit.ts + unemploy.ts)

# See summary of the model with linear trend and two external 
# variables.
summary(delin.external_2)

# To forecast 4 quarters of 2023, develop the values of the 
# variables for those periods with two external variables. 
# Apply forecast() function to make predictions for ts with 
# trend and two external variables in 4 quarters of 2023.  
forecast_param_2 <- data.frame(trend = c(93:96), 
                    ccredit.ts = c(2.2, 2.8, 3.9, 3.8),
                    unemploy.ts = c(3.6, 3.7, 4.1, 4.3))
forecast_param_2

delin.external_2.pred <- forecast(delin.external_2, 
                     newdata = forecast_param_2, level = 0)

delin.external_2.pred


# Use tslm() function to create linear trend with one 
# external variable.
delin.external_1 <- tslm(delin.ts ~ trend + ccredit.ts)

# See summary of the model with linear trend and one
# external variable.
summary(delin.external_1)

# To forecast 4 quarters of 2023, develop the values of the 
# variables for those periods with one external variable. 
# Apply forecast() function to make predictions for ts with 
# trend and one external variables in 4 quarters of 2023.  
forecast_param_1 <- data.frame(trend = c(93:96), 
                    ccredit.ts = c(2.2, 2.8, 3.9, 3.8))
forecast_param_1

delin.external_1.pred <- forecast(delin.external_1, 
                newdata = forecast_param_1, level = 0)

delin.external_1.pred


# Measure accuracy of the forecast with linear trend and external 
# variable(s), forecast with linear trend, naive and seasonal
# naive forecasts.
round(accuracy(delin.external_2.pred$fitted, delin.ts),3)
round(accuracy(delin.external_1.pred$fitted, delin.ts),3)
round(accuracy(delin.lin.tot.pred$fitted, delin.ts),3)
