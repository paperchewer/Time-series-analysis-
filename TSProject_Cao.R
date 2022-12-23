
#----------------------------------------------------------
#        Advanced Time Series Project 
#         Academic year: 2019-2020 
#         Student: Tinh Cao, r0689222
#----------------------------------------------------------

# 1. Data Collection and Processing 
#---------------------------------------------------------- 

setwd("F:/0_MASE/STATISTICS 2018-19/Advanced time series/Project/Data")
FRED.GDPC1<- read.csv("F:/0_MASE/STATISTICS 2018-19/Advanced time series/Project/Data/FRED-GDPC1.csv")
head(FRED.GDPC1)
attach(FRED.GDPC1)
mydata1 = FRED.GDPC1[order(Date),]
detach(FRED.GDPC1)
names(mydata1) = c("time", "GDP")
library(zoo)
mydata1$date <- as.yearqtr(mydata1$time, format = "%Y-%m-%d")


FRED.PCE <- read.csv("F:/0_MASE/STATISTICS 2018-19/Advanced time series/Project/Data/FRED-PCE.csv")
attach(FRED.PCE)
mydata2 = FRED.PCE[order(Date),]
detach(FRED.PCE)
names(mydata2) = c("time", "PCE")
mydata2$date <- as.yearqtr(mydata2$time, format = "%Y-%m-%d")


FRED.INDPRO <- read.csv("F:/0_MASE/STATISTICS 2018-19/Advanced time series/Project/Data/FRED-INDPRO.csv")
attach(FRED.INDPRO)
mydata3 = FRED.INDPRO[order(Date),]
detach(FRED.INDPRO)
names(mydata3) = c("time", "IP")
mydata3$date <- as.yearqtr(mydata3$time, format = "%Y-%m-%d")

mydata4 = merge(mydata1, mydata2, by.x = "date", by.y = 'date')
mydata5 = merge(mydata4, mydata3, by.x = "date", by.y = "date")
attach(mydata5)
macro = data.frame(date, GDP, PCE, IP)
detach(mydata5)
attach(macro)

# save the data frame 

write.table(macro, file=file.choose(), quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)
write.table(macro, file=file.choose(), quote = FALSE, sep = " ", row.names = FALSE, col.names = TRUE)


#  data for crude oil prices 
mydata <- read.csv("F:/0_MASE/STATISTICS 2018-19/Advanced time series/Project/Data/FRED-DCOILWTICO-month.csv")
oil_data =  mydata[order(mydata$Date),] 
oil_data$time = format(as.Date(oil_data$Date),"%Y-%m")
oilprice = data.frame(oil_data$time, oil_data$Value)
names(oilprice) = c("date", "price")
write.table(oilprice, file=file.choose(), quote = FALSE, sep = " ", row.names = FALSE, col.names = TRUE)

mydata7 <- read.csv("FRED-DCOILWTICO _Week.csv")
oil_week = mydata7[order(mydata7$Date),]
names(oil_week) = c("date", "price")

write.table(oil_week, file=file.choose(), quote = FALSE, sep = " ", row.names = FALSE, col.names = TRUE)

# 2. Univariate Analysis and Model Comparison 
#---------------------------------------------------------- 

rm(list = ls())

setwd("F:/0_MASE/STATISTICS 2018-19/Advanced time series/Project/Data")
mydata = read.table("oilprice_w.txt", header = TRUE, sep = "", dec = ".")
head(mydata)
attach(mydata)
price_ts = ts(price, frequency = 52, start = c(1986,1))

#1.1 univariate statistics 
lprice = log(price)
lprice_ts = log(price_ts)
ts.plot(lprice_ts)

# the plots show that the series seems not stationary, even though the seasonality does not have efect here
# the variances seem not constant over time, there might have heteroscedasticity 

# 1.2 Test for structural breaks 
# soure of Chow test for structural breaks 
#: https://www.r-bloggers.com/endogenously-detecting-structural-breaks-in-a-time-series-implementation-in-r/

#install.packages("strucchange")
library(strucchange)
bp_ts = breakpoints(lprice_ts ~ 1)
summary(bp_ts)
ci_ts = confint(bp_ts)
plot(lprice_ts)
lines(bp_ts)
lines(ci_ts)

#1.3 create a subsample of the price series 
lprice_cut_ts = window(lprice_ts, start = 2015 )
ts.plot(lprice_cut_ts)
# there is no clear trend in the series
acf(lprice_cut_ts)
pacf(lprice_cut_ts)


#1.4 Check for unit root test to check the series is stationary or not 
library(CADFtest)
max.lag = round(sqrt(length(lprice_cut_ts)))
CADFtest(lprice_cut_ts,type = "drift", criterion = "BIC", max.lag.y = max.lag)
# p value is large, not reject H0, so that the series has a unit root 

#1.5 Take the difference of the series 
dlprice_ts = diff(lprice_cut_ts)
CADFtest(dlprice_ts,type = "drift", criterion = "BIC", max.lag.y = max.lag)
# the p value is small, so reject H0, the series is stationary 
plot(dlprice_ts)
acf(dlprice_ts)
pacf(dlprice_ts)
# we could use model MA(1)

#1.6 Run the model ARIMA model(0,1,1)
fit1 = arima(lprice_cut_ts, order = c(0,1,1), seasonal = list(order =c(0,0,0)))
fit1
ts.plot(fit1$residuals)
acf(fit1$residuals)
pacf(fit1$residuals)
Box.test(fit1$residuals, lag = max.lag, type = "Ljung-Box")
# high p value (0.81), not reject H0, so residuals are white noise, the model is valid.  

#1.7 Making prediction for 8 weeks ahead

priceforecast = predict(fit1, n.ahead = 8)
expected = priceforecast$pred

# obtain the confidence bounds of the 95% prediction inverval: 
lower = priceforecast$pred - qnorm(0.975)*priceforecast$se
upper = priceforecast$pred + qnorm(0.975)*priceforecast$se

# plot of the forecast log of oil price and the corresponding 95% prediction interval

plot.ts(expected, col = "red", ylim = c(3.7, 4.362))
lines(lower, col = "blue")
lines(upper, col ="blue")

#1.8 comparing with ARMA(1,1,1)
fit2 = arima(lprice_cut_ts, order = c(1,1,1),seasonal = list(order =c(0,0,0)))
fit2
plot(fit2$residuals)
Box.test(fit2$residuals, type = "Ljung-Box", lag = max.lag)
# the model is still valid 
priceforecast2 = predict(fit2, n.ahead = 8)
expected2 = priceforecast2$pred
lower2 = priceforecast2$pred - qnorm(0.975)*priceforecast2$se
upper2 = priceforecast2$pred + qnorm(0.975)*priceforecast2$se


ts.plot(expected2, col = "red", ylim = c(3.7,4.36))
lines(lower2, col = "blue")
lines(upper2, col ="blue")

#1.9 Comparing the forecast between ARMA(0,1,1) and ARMA(1,1,1)
y = lprice_cut_ts
S = round(0.75* length(y))
h=1
error1.h = c()
for (i in S:(length(y)-h))
{
  mymodel.sub = arima(y[1:i], order = c(0,1,1), seasonal = c(0,0,0))
  predict.h = predict(mymodel.sub, n.ahead = h)$pred[h]
  error1.h = c(error1.h, y[i+h] - predict.h)
}
error2.h = c()
for (i in S:(length(y)-h))
{
  mymodel.sub = arima(y[1:i], order = c(1,1,1), seasonal = c(0,0,0))
  predict.h = predict(mymodel.sub, n.ahead = h)$pred[h]
  error2.h = c(error2.h, y[i+h] - predict.h)
}

MAE1 = mean(abs(error1.h))
MAE2 = mean(abs(error2.h))

# using Diebold-Mariano test
library(forecast)
dm.test(error1.h, error2.h, h = h, power =1 )
# p value is 0.1131 >5%, not reject H0, and conclude that the forecast performance of the two models is not signigicantly different.  

# 3. Multivariae Analysis 
#---------------------------------------------------------- 
rm(list = ls())


setwd("F:/0_MASE/STATISTICS 2018-19/Advanced time series/Project/Data")
mydata <- read.csv("F:/0_MASE/STATISTICS 2018-19/Advanced time series/Project/Data/macro.csv")

gdp = ts(mydata$GDP, frequency = 4, start = c(1959,1))
cons = ts(mydata$PCE, frequency = 4, start = c(1959,1))
ip = ts(mydata$IP, frequency = 4, start = c(1959,1))


#1.1 Transform data 
lgdp = log(gdp)
lcons = log(cons)
lip = log(ip)

ts.plot(lgdp,lcons, lip, col = c("black", "green", "red"))
# it seems that the three series have trends 
# data are already deseasoned 

#1.2 check whether the series are I(1)
library(CADFtest)
max.lag = round(sqrt(length(lgdp)))
CADFtest(lgdp, type = "trend", criterion = "BIC", max.lag.y = max.lag)
CADFtest(lcons, type = "trend", criterion = "BIC", max.lag.y = max.lag)
CADFtest(lip, type = "trend", criterion = "BIC", max.lag.y = max.lag)
# all p values are larger than 5%, not reject H0, so the series have a unit root 

# take the difference
dlgdp = diff(log(gdp))
dlcons = diff(log(cons))
dlip = diff(log(ip))

CADFtest(dlgdp, type = "drift", criterion = "BIC", max.lag.y = max.lag)
CADFtest(dlcons, type = "drift", criterion = "BIC", max.lag.y = max.lag)
CADFtest(dlip, type = "drift", criterion = "BIC", max.lag.y = max.lag)
# all p values are small, far less than 5%, reject H0, there are no unit roots 
# the three series are I(1)

#1.3 Test for Granger causality between dlgdp and dlcons using ADLM(2)
lag =2
n = length(dlgdp)
dlgdp.0 = dlgdp[(lag+1):n]
dlgdp.1 = dlgdp[lag:(n-1)]
dlgdp.2 = dlgdp[(lag-1):(n-2)]
dlcons.1 = dlcons[lag:(n-1)]
dlcons.2 = dlcons[(lag-1):(n-2)]


fit.dlm = lm(dlgdp.0 ~dlgdp.1 + dlgdp.2+ dlcons.1 + dlcons.2)
acf(fit.dlm$residuals)
Box.test(fit.dlm$residuals, lag = max.lag, type = "Ljung-Box")
summary(fit.dlm)
# p vlue = 0.178 larger than 5%, not reject H0, model is valid. 
# there is a Granger causality 

# formally test 
fit.dlm.nox = lm(dlgdp.0 ~ dlgdp.1 + dlgdp.2)
anova(fit.dlm, fit.dlm.nox )
# p value is smaller than 5%, reject H0 of no Granger causality. 


# 1.4 VEC model and Johansen test for cointegration between lcons and lip 
# select the optimal lag length needed in the Johansen test equation 
logdata = data.frame(lcons, lip)
names(logdata)= c("logCONS", "logIP")
library(vars)
attach(logdata)
VARselect(logdata, lag.max = 10, type = "const")
# k = 2
# Johansen's trace test 
library(urca)
trace_test = ca.jo(logdata, type = "trace", K=2, ecdet = "const", spec = "transitory")
summary(trace_test)

# Vector Error Correcting Model, VECM(1)
fit_vecm = cajorls(trace_test, r=1)
fit_vecm

# VECM(1) forecast 
fit_var2 = vec2var(trace_test, r=1)
myforecast = predict(fit_var2, n.ahead = 6)

# forecast logCONS
logCONS_forecast= ts(myforecast$fcst$logCONS[,1], frequency =4, start = c(2019,3))
logCONS_lower= ts(myforecast$fcst$logCONS[,2], frequency =4, start = c(2019,3))
logCONS_upper= ts(myforecast$fcst$logCONS[,3], frequency =4, start = c(2019,3))
ts.plot(logCONS_forecast, logCONS_lower,logCONS_upper, col = c("black", "red", "red"))

# forecast logIP
logIP_forecast= ts(myforecast$fcst$logIP[,1], frequency =4, start = c(2019,3))
logIP_lower= ts(myforecast$fcst$logIP[,2], frequency =4, start = c(2019,3))
logIP_upper= ts(myforecast$fcst$logIP[,3], frequency =4, start = c(2019,3))
ts.plot(logIP_forecast, logIP_lower,logIP_upper, col = c("black", "red", "red"))
