library(zoo)
library(tseries)
library(urca)
library(forecast)

df <- read.csv("valeurs_mensuelles_parfum.csv", sep = ";", skip=3)
df <- df[, 1:2]
colnames(df) <- c("DATE", "VALUES")

df$DATE <- as.Date(paste(df$DATE, "-01", sep=""), format="%Y-%m-%d")
time_series_zoo <- zoo(df$VALUES, order.by = df$DATE)
head(time_series_zoo)

plot(time_series_zoo, type = "l", col = "blue", lwd = 2, 
     xlab = "Date", ylab = "Values", 
     main = "Time Series Plot of values")

times_series=ts(time_series_zoo, frequency = 12)
head(times_series)


time_series_zoo_diff <- diff(times_series)
plot(time_series_zoo_diff, type = "l", col = "blue", lwd = 2, 
     xlab = "Time", ylab = "Differenced Values", 
     main = "Differenced Time Series")

acf(time_series_zoo_diff)
pacf(time_series_zoo_diff)

adf_test_result <- adf.test(time_series_zoo_diff, alternative = "stationary")
adf_test_result

best_model_auto <- auto.arima(time_series_zoo_diff, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(best_model_auto)
AIC(best_model_auto)
BIC(best_model_auto)

checkresiduals(best_model_auto)


best_aic <- Inf
best_bic <- Inf
best_model_aic <- NULL
best_model_bic <- NULL

for (p in 0:8) {
  for (q in 0:8) {
    try({
      model <- Arima(time_series_zoo_diff, order = c(p, 1, q))
      if (AIC(model) < best_aic) {
        best_aic <- AIC(model)
        best_model_aic <- model
      }
      if (BIC(model) < best_bic) {
        best_bic <- BIC(model)
        best_model_bic <- model
      }
    }, silent = TRUE)
  }
}

cat("Best model by AIC:\n")
summary(best_model_aic)

cat("\nBest model by BIC:\n")
summary(best_model_bic)

forecast_result <- forecast(best_model_aic, h = 12, level = 95)
plot(forecast_result, main = "Prévisions avec IC à 95%")

