
## LOAD LIBRARIES AND INITIAL DATA INPUT

#Use forecast and zoo libraries
library(forecast)

# Create time series data in R.
# set working directory for locating files.
setwd("C:/misc/673_BAN/module2_smoothing")

# create data frame.
Amtrak.data <- read.csv("Amtrak_comp.csv")

# See the first 6 records of the file.
head(Amtrak.data)

# Create time series data set using ts() function. 
# Takes three arguments: start, end, and freq.
# With monthly data, the frequency of periods per season is 12 per year. 
# Arguments start and end are pairs (season number, period number).
ridership.ts <- ts(Amtrak.data$Ridership, 
                   start = c(1991, 1), end = c(2018, 12), freq = 12)


## SIMPLE EXPONENTIAL SMOOTHING (SES) WITH ORIGINAL DATA, ALPHA = 0.2.

# Create simple exponential smoothing (SES) for Amtrak data with alpha = 0.2.
# Use ets() function with model = "ANN", i.e., additive error(A), no trend (N),
# & no seasonality (N). Use alpha = 0.2 to fit SES over the original data.
ses.orig <- ets(ridership.ts, model = "ANN", alpha = 0.2)
ses.orig

# Use forecast() function to make predictions using this SES model with alpha = 0.2 
# and 12 periods into the future. 
# Show predictions in tabular format.
ses.orig.pred <- forecast(ses.orig, h = 12, level = 0)
ses.orig.pred


## SIMPLE EXPONENTIAL SMOOTHING WITH ORIGINAL DATA AND OPTIMAL ALPHA.

# Create simple exponential smoothing (SES) for Amtrak data with optimal alpha.
# Use ets() function with model = "ANN", i.e., additive error(A), no trend (N),
# & no seasonality (N). Use optimal alpha to fit SES over the original data.
ses.opt <- ets(ridership.ts, model = "ANN")
ses.opt

# Use forecast() function to make predictions using this SES model with optimal alpha
# and 12 periods into the future.
# Show predictions in tabular format
ses.opt.pred <- forecast(ses.opt, h = 12, level = 0)
ses.opt.pred


## PLOT DATA SET AND SES FORECAST: ORIGINAL AND OPTIMAL.

# Plot ses predictions for original data and alpha = 0.2.
plot(ses.orig.pred, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xaxt = "n", xlim = c(1991, 2020.25), lwd = 2, 
     main = "Original Data and SES Forecast, Alpha = 0.2", 
     flty = 5) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(ses.orig.pred$fitted, col = "blue", lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2019, 2019), c(0, 3500))
text(2005, 3400, "Data Set")
text(2020.2, 3400, "Future")
arrows(1991, 3300, 2018.9, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 3300, 2021.3, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot ses predictions for original data and optimal alpha.
plot(ses.opt.pred, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 3500), 
     bty = "l", xaxt = "n", xlim = c(1991, 2020.25), lwd = 2,
     main = "Original Data and SES Optimal Forecast, Alpha = 0.5405", 
     flty = 5) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(ses.opt.pred$fitted, col = "blue", lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2019, 2019), c(0, 3500))
text(2005, 3400, "Data Set")
text(2020.2, 3400, "Future")
arrows(1991, 3300, 2018.9, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 3300, 2021.3, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## COMPARE ACCURACY OF THE TWO SES WITH ALPHA = 0.2 AND OPTIMAL ALPHA. 
round(accuracy(ses.orig.pred$fitted, ridership.ts), 3)
round(accuracy(ses.opt.pred$fitted, ridership.ts),3)

















