
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
## PLOT TIME SERIES DATA. 

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
ridership.ts <- ts(Amtrak.data$Ridership, 
                   start = c(1991, 1), end = c(2018, 12), freq = 12)

## Use plot() to plot time series data  
plot(ridership.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(1300, 3000), xaxt = 'n',
     main = "Amtrak Ridership")

# Establish x-axis scale interval for time in months.
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))

## DEVELOP REGRESSION MODELS FOR TIME SERIES DATA.
## PLOT TIMES SERIES DATA WITH REGRESSION TRENDLINES.

# Use tslm() function to create linear trend (rideship.lin) and 
# quadratic trend (rideship.quad) for time series data. 
ridership.lin <- tslm(ridership.ts ~ trend)
ridership.quad <- tslm(ridership.ts ~ trend + I(trend^2))

# Use plot() function to create plot with linear trend. 
plot(ridership.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)",
     ylim = c (1300, 3000), xaxt = 'n',
     main = "Amtrak Ridership with Linear Trend")
lines(ridership.lin$fitted, lwd = 2, col = "blue")
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))

# Use plot() function to create plot with quadratic trend. 
plot(ridership.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)",
     ylim = c (1300, 3000), xaxt = 'n',
     main = "Amtrak Ridership with Quadratic Trend")
lines(ridership.quad$fitted, lwd = 2, col = "blue")
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))


## ZOOM-IN PLOT OF TIME SERIES DATA.

# Create zoom-in plot for 3 years from 2014 through 2016.
ridership.ts.3yrs <- window(ridership.ts, 
                   start = c(2014, 1), end = c(2016, 12))

plot(ridership.ts.3yrs, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c (2100, 3000), xaxt = 'n',
     main = "Amtrak Ridership for 3 Years", 
     lwd = 2, col = "blue")
axis(1, at = seq(2014, 2016, 1), labels = format(seq(2014, 2016, 1)))

autoplot(ridership.ts.3yrs, ylab = "Ridership (in 000s)", 
         main = "Amtrak Ridership for 3 Years", lwd = 1, col = "blue")

