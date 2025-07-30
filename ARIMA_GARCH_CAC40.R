remove(list=ls())
graphics.off()

require(tseries)
require(quantmod)
require(forecast)
require(fGarch)
require(rugarch)
require(TSA)
require(urca)

# Retrieving data from Yahoo Finance
getSymbols('^FCHI', from='2023-02-01', to='2025-06-13')

# Separating the retrieved data into df_train and df_test 
df_train <- window(FCHI$FCHI.Close, start='2023-02-01', end='2025-05-26')
df_test <- window(FCHI$FCHI.Close, start='2025-05-27', end='2025-06-13')


# Data Analysis on the raw time series
#windows()
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(df_train, main = "CAC40 Closing Prices", xlab = "Date", ylab ="Price")
acf(df_train, lag.max=50, main="Sample ACF")
pacf(df_train, lag.max=50, main="Sample PACF")
qqnorm(df_train, main="QQ Plot")
qqline(df_train, col = "red")

# Augmented Dickey-Fuller test
adf <- ur.df(df_train, type="none", selectlags = "BIC")
summary(adf)
# Fail to reject H0: the time series is not stationary, so I have to differentiate it

# Transformations of the original time series
df_train_diff <- na.omit(diff(df_train))
df_test_diff <- na.omit(diff(df_test))
df_train_log_diff <- na.omit(diff(log(df_train)))
df_test_log_diff <- na.omit(diff(log(df_test)))

# Analysis of the diff time series
#windows()
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(df_train_diff, main = "CAC40 Diff Data", xlab = "Date", ylab ="Diff")
acf(df_train_diff, lag.max=50, main="Sample ACF")
pacf(df_train_diff, lag.max=50, main="Sample PACF")
qqnorm(df_train_diff, main="QQ Plot")
qqline(df_train_diff, col = "red")

# Analysis of the log diff time series
#windows()
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(df_train_log_diff, main = "CAC40 Log Diff Data", xlab = "Date", ylab ="Diff")
acf(df_train_log_diff, lag.max=50, main="Sample ACF")
pacf(df_train_log_diff, lag.max=50, main="Sample PACF")
qqnorm(df_train_log_diff, main="QQ Plot")
qqline(df_train_log_diff, col = "red")

# Augmented Dickey-Fuller test for the two transformed time series
adf_diff <- ur.df(df_train_diff, type="none", selectlags = "BIC")
summary(adf_diff)
adf_log_diff <- ur.df(df_train_log_diff, type="none", selectlags = "BIC")
summary(adf_log_diff)
# For both time series we reject H0: the series are stationary


# Fit ARIMA models using AIC and BIC
model_1_aic <- auto.arima(df_train_diff, max.p=10, max.q=10, 
                         stationary=TRUE, seasonal=FALSE, 
                         ic='aic', stepwise=FALSE)

model_1_bic <- auto.arima(df_train_diff, max.p=10, max.q=10, 
                         stationary=TRUE, seasonal=FALSE, 
                         ic='bic', stepwise=FALSE)

model_2_aic <- auto.arima(df_train_log_diff, max.p=10, max.q=10, 
                          stationary=TRUE, seasonal=FALSE, 
                          ic='aic', stepwise=FALSE)

model_2_bic <- auto.arima(df_train_log_diff, max.p=10, max.q=10, 
                          stationary=TRUE, seasonal=FALSE, 
                          ic='bic', stepwise=FALSE)

#Check results on raw data
model_1bis_aic <- auto.arima(df_train, max.p=10, max.q=10,
                               stationary=FALSE, seasonal=FALSE,
                               ic='aic', stepwise=FALSE)

model_1bis_bic <- auto.arima(df_train, max.p=10, max.q=10,
                               stationary=FALSE, seasonal=FALSE,
                               ic='bic', stepwise=FALSE)

print(model_1_aic) #ARMA(3,0)
print(model_1_bic) #ARMA(0,0)
print(model_2_aic) #ARMA(3,0)
print(model_2_bic) #ARMA(0,0)
print(model_1bis_aic) #ARMA(3,1,0)
print(model_1bis_bic) #ARMA(0,1,0)

#Significance ar2, ar3 of model_2_aic
print(0.0814/0.0412) #significant at 5%
print(-0.0732/0.0414) #significant at 10%

# Check if the fit is satisfactory
#windows()
tsdiag(model_1_aic) #satisfactory
#windows()
tsdiag(model_1_bic) #semi-satisfactory
#windows()
tsdiag(model_2_aic) #satisfactory
#windows()
tsdiag(model_2_bic) #not satisfactory

tsdiag(model_1bis_aic) #satisfactory
tsdiag(model_1bis_bic) #semi-satisfactory


# Ljung-Box test, significance level 10%
p_model_1_bic <- sapply(1:10, function(lag) {
  Box.test(residuals(model_1_bic), lag = lag, 
           type = "Ljung-Box", fitdf = length(model_1_bic$coef))$p.value
})
p_model_2_bic <- sapply(1:10, function(lag) {
  Box.test(residuals(model_2_bic), lag = lag, 
           type = "Ljung-Box", fitdf = length(model_2_bic$coef))$p.value
})

#windows()
plot(1:10, p_model_1_bic, type = "p", pch = 1,
     ylim = c(0, 1), xlab = "Lag", ylab = "Ljung-Box p-value",
     main = "Ljung-Box P-values (5% and 10% Significance Level)")
abline(h = 0.10, col = "red", lty = 2)
abline(h = 0.05, col = 'red', lty = 2)
points(1:10, p_model_2_bic, pch = 1, col = "blue")
legend("topright", legend = c("model_1_bic", "model_2_bic"),
       pch = c(1, 2), col = c("black", "blue"))


# Creation of model 3: AR(2)
model_3_aic <- Arima(df_train_log_diff, order=c(2,0,0), include.mean=FALSE)

# I continue the analysis with model_2_aic and model_3_aic
print(model_2_aic)
print(model_3_aic)

#windows()
tsdiag(model_2_aic) #satisfactory
#windows()
tsdiag(model_3_aic) #satisfactory


# Forecast model_2_aic, model_3_aic
forecast_2_aic <- forecast(model_2_aic, h=12, level=c(0.9, 0.95))
forecast_3_aic <- forecast(model_3_aic, h=12, level=c(0.9, 0.95))

actual <- ts(df_test_log_diff, start=588, end=600)

#windows()
layout(matrix(1:2, nrow = 2))
plot(forecast_2_aic, include=0, col="blue", 
     main="Forecasted Values model_2_aic VS Actual Values", 
     ylab="diff_log_data", xlab="Time")
lines(actual, col="red", lwd=2)
legend("topleft", col=c("red", "blue"), 
       legend=c("Actual values", "Predicted values"), 
       lty=c(1, 1), 
       lwd=c(2, 2), 
       cex=0.8)

plot(forecast_3_aic, include=0, col="blue", 
     main="Forecasted Values model_3_aic VS Actual Values", 
     ylab="diff_log_data", xlab="Time")
lines(actual, col="red", lwd=2)
legend("topleft", col=c("red", "blue"), 
       legend=c("Actual values", "Predicted values"), 
       lty=c(1, 1), 
       lwd=c(2, 2), 
       cex=0.8)


# Accuracy
accuracy_2_aic <- accuracy(forecast_2_aic, df_test_log_diff)
accuracy_3_aic <- accuracy(forecast_3_aic, df_test_log_diff)

print(accuracy_2_aic)
print(accuracy_3_aic)


# McLeod Li charts (checking if it is possible to fit a GARCH)
#windows()
par(mfrow=c(3,2))
plot(model_2_aic$residuals, main="Residuals model_2_aic")
plot(model_3_aic$residuals, main="Residuals model_3_aic")

plot(model_2_aic$residuals^2, main="Squared Residuals model_2_aic")
plot(model_3_aic$residuals^2, main="Squared Residuals model_3_aic")

McLeod.Li.test(model_2_aic)
McLeod.Li.test(model_3_aic)


# I choose to proceed only with model_3_aic because of the principle of parsimony 
#and because of the better results on accuracy

windows()
par(mfrow=c(3,1))
plot(model_3_aic$residuals, main="Residuals AR(2)")
plot(model_3_aic$residuals^2, main="Squared Residuals AR(2)")
McLeod.Li.test(model_3_aic)


residuals_3_aic <- residuals(model_3_aic)

garch_3_aic_1 <- garchFit(formula=~garch(1,1), data=residuals_3_aic, 
                          cond.dist='sstd', trace=F)
garch_3_aic_2 <- garchFit(formula=~garch(2,1), data=residuals_3_aic, 
                          cond.dist='sstd', trace=F)
garch_3_aic_3 <- garchFit(formula=~garch(1,2), data=residuals_3_aic, 
                          cond.dist='sstd', trace=F)
garch_3_aic_4 <- garchFit(formula=~garch(2,2), data=residuals_3_aic, 
                          cond.dist='sstd', trace=F)
garch_3_aic_5 <- garchFit(formula=~garch(1,0), data=residuals_3_aic, 
                          cond.dist='sstd', trace=F)
garch_3_aic_6 <- garchFit(formula=~garch(2,0), data=residuals_3_aic, 
                          cond.dist='sstd', trace=F)

print(summary(garch_3_aic_1))
print(summary(garch_3_aic_2))
print(summary(garch_3_aic_3))
print(summary(garch_3_aic_4))
print(summary(garch_3_aic_5))
print(summary(garch_3_aic_6))

# Because of the significance of the parameters, the results of Ljung-Box tests
#and better LogLik and AIC, I choose to go ahead with model garch_3_aic_1 (AR(2)+GARCH(1,1))

#AR(2)+GARCH(1,1) Diagnostics
windows()
layout(matrix(c(1,1,2:5), 3, 2, byrow=T))
plot(garch_3_aic_1, which=3)
plot(garch_3_aic_1, which=9)
plot(garch_3_aic_1, which=10)
plot(garch_3_aic_1, which=11)
plot(garch_3_aic_1, which=13)


# Prediction using ARMA(2,0)+GARCH(1,1) model
windows()
par(mfrow=c(1,1))
garch_final <- garchFit(formula=~arma(2,0)+garch(1,1), data=df_train_log_diff, 
                        cond.dist='sstd', trace=F)
preds_garch_3_aic_1 <- predict(garch_final, nx=12, plot=T, mse='cond', 
                      n.ahead=length(actual))
points(as.numeric(c(rep(NA,12),actual)))



