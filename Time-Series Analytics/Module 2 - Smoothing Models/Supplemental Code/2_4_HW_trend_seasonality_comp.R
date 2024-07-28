
## LOAD LIBRARIES AND INITIAL DATA INPUT.

# Use forecast and zoo libraries
library(forecast)


# Create time series data in R.
# set working directory for locating files.
setwd("C:/misc/673_BAN/module2_smoothing")

# Create data frame.
Amtrak.data <- read.csv("Amtrak_comp.csv")

# See the first 6 records of the file.
head(Amtrak.data)

# Create time series data set using ts() function. 
# Takes three arguments: start, end, and freq.
# With monthly data, the frequency of periods per season is 12 per year. 
# Arguments start and end are pairs (season number, period number).
ridership.ts <- ts(Amtrak.data$Ridership, 
                   start = c(1991, 1), end = c(2018, 12), freq = 12)

# Create data partitioning for Amtrak Ridership data.
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 60
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), 
                   end = c(1991, nTrain + nValid))


## SIMPLE EXPONENTIAL SMOOTHING (SES) WITH PARTITIONED DATA, ALPHA = 0.2.

# Create simple exponential smoothing (SES) for training data.
# Use ets() function with model = "ANN", i.e., additive error(A), 
# no trend (N) & no seasonality (N). 
# Use alpha = 0.2 to fit SES over the training period.
ses.orig <- ets(train.ts, model = "ANN", alpha = 0.2)
ses.orig

# Use forecast() function to make predictions using this SES model 
# validation period (nValid). 
# Show predictions in tabular format.
ses.orig.pred <- forecast(ses.orig, h = nValid, level = 0)
ses.orig.pred


## HOLT'S EXPONENTIAL SMOOTHING WITH PARTITIONED DATA.

# Use ets() function with model = "AAN", i.e., additive error(A), 
# additive trend (A), & no seasonality (N). 
h.AAN <- ets(train.ts, model = "AAN", alpha = 0.1, beta = 0.1)
h.AAN

# Use forecast() function to make predictions using this HW model for 
# validation period (nValid). 
# Show predictions in tabular format.
h.AAN.pred <- forecast(h.AAN, h = nValid, level = 0)
h.AAN.pred

# Holt's model with optimal smoothing parameters.
# Use ets() function with model = "AAN", i.e., additive error(A), 
# additive trend (A), & no seasonality (N). 
h.AAN.opt <- ets(train.ts, model = "AAN")
h.AAN.opt

# Use forecast() function to make predictions using this HW model for 
# validation period (nValid). 
# Show predictions in tabular format.
h.AAN.opt.pred <- forecast(h.AAN.opt, h = nValid, level = 0)
h.AAN.opt.pred

# Plot Holt's model predictions with optimal smoothing parameters.
plot(h.AAN.opt.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = "n",
     main = "Holt's Additive Model with Optimal Smoothing Parameters", 
     col = "blue", lty = 2, lwd = 2, ) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(h.AAN.opt.pred$fitted, col = "blue", lwd = 2)
lines(ridership.ts)
legend(1992,2700, legend = c("Ridership", 
                             "Holt's Additive Model for Training Partition",
                             "Holt's Additive Model for Validation Partition"), 
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


## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA. 
## OPTIMAL PARAMETERS FOR ALPHA, BETA, AND GAMMA.

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "AAA", i.e., additive error(A), 
# additive trend (A), & additive seasonality (A). 
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.AAA <- ets(train.ts, model = "AAA")
hw.AAA

# Use forecast() function to make predictions using this HW model for 
# validation period (nValid). 
# Show predictions in tabular format.
hw.AAA.pred <- forecast(hw.AAA, h = nValid, level = 0)
hw.AAA.pred

# Plot HW predictions for HW additive model (AAA) optimal smoothing parameters.
plot(hw.AAA.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = "n",
     main = "Holt-Winter's Additive Model with Optimal Smoothing Parameters", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(hw.AAA.pred$fitted, col = "blue", lwd = 2)
lines(ridership.ts)
legend(1991,2900, 
       legend = c("Ridership", 
                  "Holt-Winter's Additive Model for Training Partition",
                  "Holt-Winter's Additive Model for Validation Partition"), 
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


## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATIC
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA.

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ", i.e., automatic selection of
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ # Model appears to be (M, N, M), with alpha = 0.5156 and gamma = 0.158.

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = "n",
     main = "Holt-Winter's Model with Automatic Selection of Model Options", 
     lty = 5, col = "blue", lwd = 2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(ridership.ts)
legend(1991,2900, 
       legend = c("Ridership", 
                  "Holt-Winter's Model for Training Partition",
                  "Holt-Winter's Model for Validation Partition"), 
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


## COMPARE ACCURACY OF THREE MODELS: SES WITH ALPHA = 0.2, HW
## ADDITIVE MODEL WITH OPTIMAL PARAMETERS, AND HW MODEL WITH
## AUTOMATED SELECTION OF MODEL OPTIONS.
round(accuracy(ses.orig.pred$mean, valid.ts), 3)
round(accuracy(hw.AAA.pred$mean, valid.ts), 3)
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)


## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full Amtrak data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(ridership.ts, model = "ZZZ")
HW.ZZZ # Model appears to be (M, Ad, M), with alpha = 0.5334, beta = 0.0014,
       # gamma = 0.1441, and phi = 0.9698.

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

# plot HW predictions for original data, optimal smoothing parameters.
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xlim = c(1991, 2020.25), xaxt = "n",
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
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

# Use ets() function with alternative model = "AAA". 
# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.AAA <- ets(ridership.ts, model = "AAA")
HW.AAA # Model appears to be (A, A, A), with alpha = 0.4233, 
# beta = 0.0001, and gamma = 0.2655.

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.AAA.pred <- forecast(HW.AAA, h = 12 , level = 0)
HW.AAA.pred

# Identify performance measures for HW forecast.
round(accuracy(HW.ZZZ.pred$fitted, ridership.ts), 3)
round(accuracy(HW.AAA.pred$fitted, ridership.ts), 3)
round(accuracy((naive(ridership.ts))$fitted, ridership.ts), 3)
round(accuracy((snaive(ridership.ts))$fitted, ridership.ts), 3)
