install.packages("forecast")
library(forecast)
library(zoo)
sales.data <- read.csv('673_case1.csv')


# -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Identify time series components and plot the data.

# a. Create time series data set sales.ts in R using the ts() function.\
sales.ts <- ts(sales.data$Sales,
               start = c(2015, 1), 
               end = c(2023, 12), 
               frequency = 12)
sales.ts

# b. Employ the plot() function to create a data plot of the historical data, provide it in your 
# report, and explain what data patterns can be visualized in this plot.
plot(sales.ts,
     xlab= "Time",
     ylab= "Sales (in Millions)",
     main= "Grocery Store Sales",
     ylim = c(100, 600),
     xaxt = 'n'
)
axis(1, at = seq(2015, 2024, 1), labels = format(seq(2015, 2024, 1)))
# Explain what data patterns can be visualized in this plot: In this plot for Grocery Store Sales,
# we can notice that over the years there is a gradual increase in sales showing that an upward (likely linear) trend 
# exists in this time series. We can also notice that at the start of each year, sales drop, and at 
# the end of each year, sales dramatically increase. Since this pattern repeats consistently over the years,
# we can conclude that there is also seasonality in this time series for grocery store sales. Since the amplitude of 
# the seasonal fluctuations remain relatively constant over time and the variance of the series is also relatively constant over time,
# we can conclude that this time series exhibits an Upward Linear Trend with Additive Seasonality.  


# c. Apply the Acf() function to identify possible time series components. Provide in the report 
# the autocorrelation chart
sales.stl <- stl(sales.ts, s.window = "periodic")
autoplot(sales.stl, main = "Grocery Store Time Series Components")

autocor <- Acf(sales.ts, lag.max = 12, 
               main = "Autocorrelation for Grocery Store sales")
# Explain time series components existing in the historical data: In the ACF plot for grocery store sales, three key components are observed:
# Stationarity: In an ACF plot, a stationary time series would typically show autocorrelation values that quickly drop to zero as the lag increases.
# Here, aside from a spike at lag 12, the autocorrelations are relatively low, suggesting potential stationarity.
# 
# Trend: A trend exists when there is a long-term increase or decrease in the data. In the AFC plot, there isn’t a clear indication of a 
# trend since we do not see a gradual change in the correlation as the lags increase. The absence of a slowly diminishing autocorrelation pattern suggests 
# there may not be a trend in the data. Despite this, if we use autoplot on the seasonal-trend decomposition (stl()), the trend plot suggests that there actually does 
# exist an upward trend in this Grocery store time series.
# 
# Seasonality: There is a pronounced spike at lag 12, which indicates a seasonal pattern. This suggests an annual seasonality component exists, as the sales appear to 
# correlate with their values 12 months prior. Additionally, if we use autoplot on the seasonal-trend decomposition (stl()), the seasonal plot shows repetitive patterns that 
# occur at regular intervals which is a key sign that there is strong seasonality in the sales data. This is typical for many businesses that have annual cycles influenced by factors such as holidays or events. More
# people tend to buy groceries as the holiday season progresses. 

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2. Use trailing MA for forecasting time series.

# a. Develop data partition with the validation partition of 24 monthly periods (2 years) and 
# training partition of 84 monthly periods (7 years). Provide the data partition’s R code in 
# your report.
nValid <- 24
nTrain <- length(sales.ts) - nValid
train.ts <- window(sales.ts, start=c(2015, 1), end=c(2015, nTrain))
valid.ts <- window(sales.ts, start=c(2015, nTrain + 1), end=c(2015, nTrain + nValid))

# b. Use the rollmean() function to develop 3 trailing MAs with the window width of 4, 6, and 
# 12 for the training partition. Present the R code for these MAs in your report.
ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_6 <- rollmean(train.ts, k = 6, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

# c. Use the forecast() function to create a trailing MA forecast for each window width from 
# question 2b in the validation period, and present one of them, e.g., with window width of 
# 4, in your report.
ma.trail_4.pred <- forecast(ma.trailing_4, h = nValid, level = 0)
ma.trail_6.pred <- forecast(ma.trailing_6, h = nValid, level = 0)
ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_4.pred

# d. Apply the accuracy() function to compare accuracy of the three trailing MA forecasts in 
# the validation period. Present the accuracy measures in your report, compare MAPE and 
# RMSE of these forecasts, and identify the best trailing MA forecast.
mape_4 <- round(accuracy(ma.trail_4.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_4 <- round(accuracy(ma.trail_4.pred$mean, valid.ts)[, "RMSE"], 3)

mape_6 <- round(accuracy(ma.trail_6.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_6 <- round(accuracy(ma.trail_6.pred$mean, valid.ts)[, "RMSE"], 3)

mape_12 <- round(accuracy(ma.trail_12.pred$mean, valid.ts)[, "MAPE"], 3)
rmse_12 <- round(accuracy(ma.trail_12.pred$mean, valid.ts)[, "RMSE"], 3)

cat("The MAPE and RMSE accuracy measures for a trailing MA forecast in the validation period with window width 4 are respectively as follows: ", mape_4, rmse_4, '\n',
    "The MAPE and RMSE accuracy measures for a trailing MA forecast in the validation period with window width 6 are respectively as follows: ", mape_6,rmse_6, '\n',
    "The MAPE and RMSE accuracy measures for a trailing MA forecast in the validation period with window width 12 are respectively as follows: ", mape_12, rmse_12, '\n')
# Explanation: Based on MAPE and RMSE, the trailing MA that performs the best (exhibits the 
# least error) is the trailing MA forecast with window width of 4. 

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 3. Apply the two-level forecast with regression and trailing MA for residuals.

# a. Develop using the tslm() function a regression model with linear trend and seasonality. 
# Present the model summary in your report. Present and briefly explain the model 
# equation in your report. 
trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)

# Model equation
# y = 191.4617 + 1.7910*Trend - 40.1767*Season2 - 72.7106*Season3 - 67.4016*Season4 
#     - 73.9783*Season5 - 40.8121*Season6 - 33.2746*Season7 - 9.6084*Season8 
#     + 47.7720*Season9 + 65.7953*Season10 + 105.9900*Season11 + 158.7133*Season12
# Using this model, forecast monthly sales in the validation period 
# with the forecast() function. Present the forecast in your report.
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred

plot(sales.ts, 
     xlab = "Time", ylab = "Sales (in Millions)", ylim = c(100, 600), 
     bty = "l", xlim = c(2015, 2024), xaxt = "n",
     main = "Regression Forecast for Validation Partition") 
axis(1, at = seq(2015, 2023, 1), labels = format(seq(2015, 2023, 1)))
lines(trend.seas$fitted, col = "blue", lwd = 2, lty = 1)
lines(trend.seas.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1992,3200, legend = c("Sales Data", 
                             "Regression Forecast, Training Partition", 
                             "Regression Forecast, Validation Partition"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")
lines(c(2022, 2023), c(0, 3500))

# b. Identify regression residuals in the training period, apply a trailing MA (window width of 
# 3) for these residuals using the rollmean() function, and identify trailing MA forecast of 
# these residuals in the validation period (use the forecast() function). Provide the trailing 
# MA forecast for residuals in the validation period in your report. 

# Regression residuals in the training period
trend.seas.res <- trend.seas$residuals
trend.seas.res

# apply trailing MA with window width 3 for residuals using rollmean()
ma.trail.res <- rollmean(trend.seas.res, k = 3, align = "right")
ma.trail.res

# identify trailing MA forecast of these residuals in the validation period with forecast()
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

# Provide the trailing MA forecast for residuals in the validation period in your report. 
plot(sales.ts, 
     xlab = "Time", ylab = "Sales (in Millions)", ylim = c(100, 600), 
     bty = "l", xlim = c(2015, 2024), xaxt = "n",
     main = "Regression Forecast Validation Partitions ") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(trend.seas$fitted, col = "blue", lwd = 2, lty = 1)
lines(trend.seas.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1992,3200, legend = c("Sales Data", 
                             "Regression Forecast, Training Partition", 
                             "Regression Forecast, Validation Partition"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")
lines(c(2022, 2023), c(0, 3500))

# c. Develop two-level forecast for the validation period by combining the regression forecast 
# and trailing MA forecast for residuals. Present in your report a table that contains 
# validation data, regression forecast, trailing MA forecast for residuals, and two-level
# (combined) forecast in the validation period. Apply the accuracy() function to compare 
# accuracy of the regression model with linear trend and seasonality and the two-level 
# (combined) model with the regression and trailing MA for residuals. Present the accuracy
# measures in your report, compare MAPE and RMSE of these forecasts, and identify the 
# best forecasting model for the validation period.

# Develop two-level forecast for the validation period by combining the regression forecast 
# and trailing MA forecast for residuals.
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level

# Present in your report a table that contains 
# validation data, regression forecast, trailing MA forecast for residuals, and two-level
# (combined) forecast in the validation period.
valid.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                             ma.trail.res.pred$mean, 
                             fst.2level), 3)
names(valid.df) <- c("Sales", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

# Apply the accuracy() function to compare accuracy of the regression model with linear 
# trend and seasonality and the two-level (combined) model with the regression and trailing MA for residuals.
# Present the accuracy measures in your report, compare MAPE and RMSE of these forecasts, and identify the 
# best forecasting model for the validation period.
trend.seas.MAPE <- round(accuracy(trend.seas.pred$mean, valid.ts)[, "MAPE"], 3)
trend.seas.RMSE <- round(accuracy(trend.seas.pred$mean, valid.ts)[, "RMSE"], 3)
fst.2level.MAPE <- round(accuracy(fst.2level, valid.ts)[, "MAPE"], 3)
fst.2level.RMSE <- round(accuracy(fst.2level, valid.ts)[, "RMSE"], 3)
cat("The MAPE and RMSE accuracy measures for the regression model with linear trend and seasonality are respectively as follows: ", trend.seas.MAPE, trend.seas.RMSE, '\n',
    "The MAPE and RMSE accuracy measures for the two-level (combined) model with the regression and trailing MA for residuals are respectively as follows: ", fst.2level.MAPE, fst.2level.RMSE, '\n')

# Explanation: The two-level (combined) model with regression and trailing MA for residuals performs better in forecasting for the validation period compared to the regression model with linear 
# trend and seasonality, as it has lower MAPE (5.313 vs 8.638) and RMSE (22.907 vs 32.526) values. Thus, the two-level (combined) model with regression and trailing MA for residuals is the best 
# forecasting model for the validation period.

# d. For the entire data set, identify the regression model with linear trend and seasonality 
# and trailing MA with the window width of 3 for the regression residuals. Use these models 
# to forecast the 12 months of 2024 and develop a two-level forecast for the 12 future 
# months as a combination of the specified forecasts. Present in your report a table that 
# contains the regression forecast, trailing MA forecast for residuals, and two-level 
# (combined) forecast in the 12 months of 2024.
tot.trend.seas <- tslm(sales.ts ~ trend  + season)
tot.trend.seas.res <- tot.trend.seas$residuals
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 3, align = "right")
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean

# Present in your report a table that contains the regression forecast, trailing MA forecast 
# for residuals, and two-level (combined) forecast in the 12 months of 2024.
future12.df <- round(data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                                tot.fst.2level), 3)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

# e. Develop a seasonal naïve forecast for the entire historical data set and apply the
# accuracy() function to compare accuracy of the three forecasting models: seasonal naïve 
# forecast, regression model with linear trend and seasonality, and two-level (combined) 
# model with the regression and trailing MA for residuals. Present the accuracy measures 
# in your report, compare MAPE and RMSE of these forecasts, and identify the best 
# forecasting model for forecasting monthly sales in 2024.
sales.snaive <- snaive(sales.ts, h = nValid + nTrain)
sales.snaive.MAPE <- round(accuracy(sales.snaive$fitted, sales.ts)[, "MAPE"], 3)
sales.snaive.RMSE <- round(accuracy(sales.snaive$fitted, sales.ts)[, "RMSE"], 3)
tot.trend.seas.MAPE <- round(accuracy(tot.trend.seas$fitted, sales.ts)[, "MAPE"], 3)
tot.trend.seas.RMSE <- round(accuracy(tot.trend.seas$fitted, sales.ts)[, "RMSE"], 3)
tot.fst.2level.MAPE <- round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, sales.ts)[, "MAPE"], 3)
tot.fst.2level.RMSE <- round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, sales.ts)[, "RMSE"], 3)
cat("The MAPE and RMSE accuracy measures for the Seasonal Naïve forecast for the entire dataset are respectively as follows: ", sales.snaive.MAPE, sales.snaive.RMSE, "\n",
    "The MAPE and RMSE accuracy measures for the Regression model with linear trend and seasonality for the entire dataset are respectively as follows: ", tot.trend.seas.MAPE, tot.trend.seas.RMSE, "\n",
    "The MAPE and RMSE accuracy measures for the Two-level (combined) model with the regression and trailing MA for residuals for the entire dataset are respectively as follows: ", tot.fst.2level.MAPE, tot.fst.2level.RMSE, '\n')
# Explanation: The Two-level (combined) model with regression and trailing MA for residuals performs the best in forecasting monthly sales in 2024 based on the lowest MAPE and RMSE values. Specifically, it has the lowest MAPE 
# of 5.227 and the lowest RMSE of 17.232 compared to the other forecasting models.

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 4. Use advanced exponential smoothing methods.
# a. For the training partition (from question 2a), use the ets() function to develop a HoltWinter’s (HW) model with automated selection of error, trend, and seasonality options, 
# and automated selection of smoothing parameters for the training partition. Present the 
# model summary (output) and explain the model in your report. Use the model to forecast 
# monthly sales for the validation period using the forecast() function, and present this
# forecast in your report.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
summary(hw.ZZZ)
# Explanation: The Holt-Winters (HW) model with automated selection was developed using the ets() function in R to forecast the sales time series data. The model uses the following parameters: Alpha (Level Smoothing) of 0.1951, 
# Beta (Trend Smoothing) of 1e-04, and Gamma (Seasonal Smoothing) of 1e-04. The initial states of the model are Level (l) at 201.4349, Trend (b) at 1.569, and Seasonal Components (s) ranging from 157.8048 to -7.2129. The model 
# has an error standard deviation (Sigma) of 27.9821 and information criteria of AIC 948.1414, AICc 957.4142, and BIC 989.4653. The model's performance on the training set shows a Mean Error (ME) of -0.2771, Root Mean Squared Error (RMSE) 
# of 25.17647, and Mean Absolute Percentage Error (MAPE) of 8.377149.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Sales (in Millions)", ylim = c(100, 600), 
     bty = "l", xlim = c(2015, 2024), xaxt = "n",
     main = "Holt-Winter's Automated Selection Model with Optimal Smoothing Parameters", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(sales.ts)

# b. To make a forecast in the 12 months of 2024, use the entire data set (no partitioning) to 
# develop the HW model using the ets() function for the model with the automated 
# selection of error, trend, and seasonality options, and automated selection of smoothing 
# parameters. Present the model summary (output) and explain this model in your report. 
# Use the model to forecast monthly sales in the 12 months of 2024 using the forecast()
# function, and present the forecast in your report.
tot.hw.ZZZ <- ets(sales.ts, model = "ZZZ")
summary(tot.hw.ZZZ)
# Explanation: The Holt-Winters (HW) model with automated selection was developed using the ets() function in R to forecast the sales time series data. The model's smoothing parameters are Alpha (Level Smoothing) at 0.2017, 
# Beta (Trend Smoothing) at 1e-04, and Gamma (Seasonal Smoothing) at 1e-04. The initial states of the model are Level (l) at 202.496, Trend (b) at 1.3772, and Seasonal Components (s) ranging from 156.7496 to -3.3328. The 
# model has an error standard deviation (Sigma) of 25.1588 and information criteria of AIC 1218.998, AICc 1225.798, and BIC 1264.594. The model's performance on the training set shows a Mean Error (ME) of -0.2093662, Root 
# Mean Squared Error (RMSE) of 23.2205, and Mean Absolute Percentage Error (MAPE) of 7.241739.
tot.hw.ZZZ.pred <- forecast(tot.hw.ZZZ, h = 12, level=0)
tot.hw.ZZZ.pred
plot(tot.hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Sales (in Millions)", ylim = c(100, 600), 
     bty = "l", xlim = c(2015, 2024), xaxt = "n",
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(2015, 2024, 1), labels = format(seq(2015, 2024, 1)))
lines(tot.hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(sales.ts)
legend(2015,2024, 
       legend = c("Sales", 
                  "Holt-Winter'sModel for Entire Data Set",
                  "Holt-Winter's Model Forecast, Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# c. Apply the accuracy() function to compare the two models: seasonal naïve forecast
# (applied in question 3e) and the HW model developed in question 4b. Present the 
# accuracy measures in your report, compare MAPE and RMSE of these forecasts, and 
# identify the best forecasting model.
tot.hw.ZZZ.MAPE <- round(accuracy(tot.hw.ZZZ$fitted, sales.ts)[, "MAPE"], 3)
tot.hw.ZZZ.RMSE <- round(accuracy(tot.hw.ZZZ$fitted, sales.ts)[, "RMSE"], 3)
cat("The MAPE and RMSE accuracy measures for the Seasonal Naïve forecast for the entire dataset are respectively as follows: ", sales.snaive.MAPE, sales.snaive.RMSE, "\n",
    "The MAPE and RMSE accuracy measures for the Holt-Winters (HW) model with automated selection forecast for the entire dataset are respectively as follows: ", tot.hw.ZZZ.MAPE, tot.hw.ZZZ.RMSE, "\n")
# The Holt-Winters (HW) model with automated selection outperforms the Seasonal Naïve forecast in terms of forecasting accuracy for the entire dataset, with a lower MAPE of 7.242 compared to 8.9 and a lower RMSE 
# of 23.221 compared to 36.324. Thus, the Holt-Winters (HW) model with automated selection is the better forecasting model for the entire dataset.

# d. Compare the best forecasts identified in questions 3e and 4c. Explain what your final 
# choice of the forecasting model in this case will be.
# The Two-level (combined) model with regression and trailing MA for residuals remains the best forecasting model for the entire dataset, with the lowest MAPE of 5.227 and the lowest RMSE of 17.232. While the Holt-Winters (HW) 
# model with automated selection showed improved performance over the Seasonal Naïve forecast with a MAPE of 7.242 and an RMSE of 23.221 compared to a MAPE of 8.9 and an RMSE of 36.324, it was outperformed by the Two-level model. 
# Given that the dataset had a significant trend over a short period, the Two-level model's ability to capture both trend and seasonality likely contributed to its superior performance. Therefore, the final choice for the forecasting 
# model in this case would still be the Two-level (combined) model with regression and trailing MA for residuals due to its superior accuracy in predicting the monthly sales for the entire dataset, especially when dealing with a significant trend.