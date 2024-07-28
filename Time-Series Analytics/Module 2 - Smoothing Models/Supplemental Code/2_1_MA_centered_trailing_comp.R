
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


## CREATE CENTERED MA FOR VARIOUS WINDOWS (NUMBER OF PERIODS).
## GENERATE PLOT FOR ORIGINAL DATA AND CENTERED MA.

# Create centered moving average with window k = 4, 5, and 12.
ma.centered_4 <- ma(ridership.ts, order = 4)
ma.centered_5 <- ma(ridership.ts, order = 5)
ma.centered_12 <- ma(ridership.ts, order = 12)

# Plot original data and centered MA for window widths of k= 4 and 12. 
plot(ridership.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)", xaxt = "n",
     ylim = c(1300, 3000), bty = "l",
     xlim = c(1991, 2020.25), main = "Centered Moving Average") 
axis(1, at = seq(1991, 2020.25, 1), labels = format(seq(1991, 2020.25, 1)))
lines(ma.centered_4, col = "brown", lwd = 2)
lines(ma.centered_12, col = "blue", lwd = 2)
legend(1992,3000, legend = c("Ridership", "Centered MA, k=4",
                             "Centered MA, k=12"), 
       col = c("black", "brown" , "blue"), 
       lty = c(1, 1, 1), lwd =c(1, 2, 2), bty = "n")


## CREATE DATA PARTITION.

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 60
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), 
                   end = c(1991, nTrain + nValid))

## CREATE TRAILING MA FOR VARIOUS WINDOWS (NUMBER OF PERIODS).
## SHOW FIRST SIX AND LAST SIX VALUES OF TRAILING MA.
## IDENTIFY FORECAST ACCURACY FOR TRAILING MA FORECASTS.
## FORECAST USING TRAILING MA.

# Create trailing MA with window widths (number of periods) 
# of k = 4, 5, and 12.
# In rollmean(), use argument align = "right" to calculate a trailing MA.
ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_5 <- rollmean(train.ts, k = 5, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

# Use head() function to show training MA (windows width k=12 
# for the first 6 MA results and tail() function to show the 
# last 6 MA results for MA. 
head(ma.trailing_12)
tail(ma.trailing_12)

# Create forecast for the validation data for the window widths 
# of k = 4, 5, and 12. 
ma.trail_4.pred <- forecast(ma.trailing_4, h = nValid, level = 0)
ma.trail_4.pred
ma.trail_5.pred <- forecast(ma.trailing_5, h = nValid, level = 0)
ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_12.pred

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(ma.trail_4.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_5.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3)


## GENERATE PLOT FOR PARTITION DATA AND TRAILING MA.

# Plot original data and forecast for training and validation partitions
# using trailing MA with window widths of k = 4 and k = 12.
plot(ridership.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(1300, 3500), bty = "l", xaxt = "n",
     xlim = c(1991, 2020.25), main = "Trailing Moving Average") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)) )
lines(ma.trailing_4, col = "brown", lwd = 2, lty = 1)
lines(ma.trail_4.pred$mean, col = "brown", lwd = 2, lty = 2)
lines(ma.trailing_12, col = "blue", lwd = 2, lty = 1)
lines(ma.trail_12.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1992,3400, legend = c("Ridership Data", 
                             "Trailing MA, k=4, Training Partition", 
                             "Trailing MA, k=4, Validation Partition", 
                             "Trailing MA, k=12, Training Partition", 
                             "Trailing MA, k=12, Validation Partition"), 
       col = c("black", "brown", "brown", "blue", "blue"), 
       lty = c(1, 1, 2, 1, 2), lwd =c(1, 2, 2, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2014, 2014), c(0, 3500))
lines(c(2019, 2019), c(0, 3500))
text(2002, 3500, "Training")
text(2016.5, 3500, "Validation")
text(2020.2, 3500, "Future")
arrows(1991, 3400, 2013.9, 3400, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.1, 3400, 2018.9, 3400, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 3400, 2021.3, 3400, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot original data, centered and trailing MA.
plot(ridership.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)",
     ylim = c(1300, 3500), bty = "l",  xaxt = "n",
     xlim = c(1991, 2020.25), main = "Centered and Trailing Moving Averages") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(ma.centered_12, col = "brown", lwd = 2)
lines(ma.trailing_12, col = "blue", lwd = 2, lty = 1)
lines(ma.trail_12.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1992,3400, legend = c("Ridership Data", 
                             "Centered MA, k=12",
                             "Trailing MA, k=12, Training Partition", 
                             "Trailing MA, k=12, Validation Partition"), 
       col = c("black", "brown", "blue", "blue"), 
       lty = c(1, 1, 1, 2), lwd =c(1, 2, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2014, 2014), c(0, 3500))
lines(c(2019, 2019), c(0, 3500))
text(2002, 3500, "Training")
text(2016.5, 3500, "Validation")
text(2020.2, 3500, "Future")
arrows(1991, 3400, 2013.9, 3400, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.1, 3400, 2018.9, 3400, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 3400, 2021.3, 3400, code = 3, length = 0.1,
       lwd = 1, angle = 30)