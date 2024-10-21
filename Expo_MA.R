# Exponential and Moving Average smoothing – Holt winters smoothing 
# Define the data
time <- 1996:2019
observation <- c(150.3, 150.9, 151.4, 151.9, 152.5, 152.9, 153.2, 153.7, 153.6, 
                 153.5, 154.4, 154.9, 155.7, 156.3, 156.6, 156.7, 157, 157.3, 
                 157.8, 158.3, 158.6, 158.6, 159.1, 159.3)

# Create a time series object
ts_data <- ts(observation, start = 1996, end = 2019)

# Simple Exponential Smoothing (alpha = 0.3)
ses_model <- HoltWinters(ts_data, alpha = 0.3, beta = FALSE, gamma = FALSE)
plot(ts_data, col="blue")
lines(fitted(ses_model)[,1], col="red")

# Holt's Exponential Smoothing (alpha = 0.3, beta = 0.2)
holt_model <- HoltWinters(ts_data, alpha = 0.3, beta = 0.2, gamma = FALSE)
lines(fitted(holt_model)[,1], col="green")

# Add legend
legend("topleft", legend=c("Original", "Simple Exp. Smoothing", "Holt's Exp. Smoothing"),
       col=c("blue", "red", "green"), lty=1)


#Apply the Holt-Winters method to AirPassengers data and forecast next 12 months data.
#(Use multiplicative model)

model=HoltWinters(x=AirPassengers, alpha = 0.2, beta = 0.03, gamma = 0.8,
                  seasonal = c("multiplicative"))
plot(model)

## S3 method for class 'HoltWinters'
P=predict(model, n.ahead = 12, prediction.interval = FALSE,)

plot(model,P)


#CO2 Moving Average
# Define the data
year <- 1991:2003
co2 <- c(355.62, 356.36, 357.1, 358.86, 360.9, 362.58, 363.84, 366.58, 368.3, 
         369.47, 371.03, 373.61, 357.61)
ts_co2 <- ts(co2, start = 1991, end = 2003)

# a) Time series plot
plot(ts_co2, type="o", col="blue", ylab="CO2 Concentration", main="CO2 Concentration from 1991-2003")

# b) 3-year moving average for forecasting
moving_avg <- filter(ts_co2, rep(1/3, 3), sides=2)
plot(moving_avg,,type="o",col="blue",)
forecast_2004 <- mean(tail(ts_co2, 3))  # Forecast for 2004 using the last 3 values
forecast_2004

