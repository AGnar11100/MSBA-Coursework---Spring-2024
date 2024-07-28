
## USE FORECAST LIBRARY.

install.packages("forecast")
library(forecast)


## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/Users/naran/OneDrive/Desktop/Spring-2024/Time-Series Analytics/Module 1 - Intro to Time Series Analytics/Supplemental Code/Autocorrelation & Plotting/R")

# Create data frame.
Amtrak.data <- read.csv("Amtrak_comp.csv")

# See the first 6 records of the file.
head(Amtrak.data)
tail(Amtrak.data)

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
## USE stl() FUNCTION TO PLOT TIME SERIES COMPONENTS 
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
ridership.ts <- ts(Amtrak.data$Ridership, 
            start = c(1991, 1), end = c(2018, 12), freq = 12)
ridership.ts

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
ridership.stl <- stl(ridership.ts, s.window = "periodic")
autoplot(ridership.stl, main = "Amtrak Time Series Components")

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags.
autocor <- Acf(ridership.ts, lag.max = 12, 
          main = "Autocorrelation for Amtrak Ridership")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)


