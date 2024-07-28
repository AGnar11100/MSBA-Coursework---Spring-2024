
## USE FORECAST LIBRARY.

library(forecast)

## CREATE DATA FRAME FOR S&P500 STOCK PRICES. 

# Set working directory for locating files.
setwd("C:/misc/673_BAN/module4_arima")

# Create data frame.
SP500.data <- read.csv("S&P500prices_23.csv")

# See the first and last 6 records of the file for S&P500 data.
head(SP500.data)
tail(SP500.data)

## USE ts() FUNCTION TO CREATE TIME SERIES DATASET FOR S&P500 CLOSE STOCK PRICES.

# Create time series data for daily close stock prices, consider frequency 
# equal to 1. 
close.price.ts <- ts(SP500.data$ClosePrice, start = 1, freq = 1)
close.price.ts
length(close.price.ts)

# Use plot() function to create plot For Close Price. 
plot(close.price.ts, 
     xlab = "Time", ylab = "Price, $", xaxt = "n",
     ylim = c (3000, 5000), main = "S&P 500 Close Price", 
     bty = "l", lwd = 2, col="blue")
axis(1, at = seq(1, 251), labels = format(seq(1, 251)))


## TEST PREDICTABILITY OF S&P500 CLOSE STOCK PRICES.

# Use Arima() function to fit AR(1) model for S&P500 close prices.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
close.price.ar1<- Arima(close.price.ts, order = c(1,0,0))
summary(close.price.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.9954
s.e. <- 0.0054
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
    "Reject null hypothesis"
} else {
    "Accept null hypothesis"
}

# Create first difference of ClosePrice data using diff() function.
diff.close.price <- diff(close.price.ts, lag = 1)
diff.close.price

# Another way to identify first difference. 
diff.close.price_lag1 <- close.price.ts[2:250] - close.price.ts[1:249]
diff.close.price_lag1

# Use plot() function to create plot for first differenced data. 
plot(diff.close.price, 
     xlab = "Time", ylab = "Price, $", xaxt = "n",
     ylim = c (-150, 100), main = "First Differencing of Stock Close Price", 
     bty = "l", lty = 5, lwd = 2, col="orange")
axis(1, at = seq(1, 250), labels = format(seq(1, 250)))

# Develop data frame with Close Price, Close Price lag 1, and first
# differenced data.
diff.df <- data.frame(close.price.ts, c("", round(close.price.ts[1:249],2)), 
                      c("", round(diff.close.price,2)))

names(diff.df) <- c("ClosePrice", "ClosePrice Lag-1", 
                     "First Difference")
diff.df 

# Use Acf() function to identify autocorrealtion for first differenced
# ClosePrice and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(diff.close.price, lag.max = 12, 
    main = "Autocorrelation for S&P500 Differenced Close Prices")


## TEST PREDICTABILITY OF AMTRAK RIDERSHIP.

# Create data frame.
Amtrak.data <- read.csv("Amtrak_comp.csv")

# Use ts() function to create time series set for Amtrak ridership.
ridership.ts <- ts(Amtrak.data$Ridership, 
                   start = c(1991, 1), end = c(2018, 12), freq = 12)
ridership.ts

# Use Arima() function to fit AR(1) model for Amtrak Ridership.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
ridership.ar1<- Arima(ridership.ts, order = c(1,0,0))
summary(ridership.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.8826
s.e. <- 0.0256
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
    "Reject null hypothesis"
} else {
    "Accept null hypothesis"
}

# Create first differenced Amtrak Ridership data using lag1.
diff.ridership.ts <- diff(ridership.ts, lag = 1)
diff.ridership.ts

# Use Acf() function to identify autocorrealtion for first differenced 
# Amtrak Ridership, and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(diff.ridership.ts, lag.max = 12, 
    main = "Autocorrelation for Differenced Amtrak Ridership Data")

