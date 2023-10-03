library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
X_DJI <- read_csv("^DJI.csv")
View(X_DJI)
attach(X_DJI)
ex_r=(Close-Open)/Open
ex_r=ts(ex_r,start=c(2000, 1), end=c(2020, 2),frequency = 12)
plot(Date,ex_r, main="Plot of Monthly Excess Return of Dow Jones Industrial Average(2000-2020)", ylab="Monthly Excess Return",xlab="Date")
lines(Date,ex_r, col="blue")
Box.test(ex_r, lag = 20, type = 'Ljung-Box')
adf.test(ex_r)

ex_Rcomponents =decompose(ex_r)
plot(ex_Rcomponents)

autofit=auto.arima(ex_r)
f=forecast(autofit,h=12)
autoplot(f,shadecols = c("#596DD5", "#D5DBFF"), xlab = "Time", ylab = "Monthly Excess Return", title = "Forecasting Monthly Excess Return on March and Apri
plot(f, plot.conf = TRUE, shaded = TRUE, shadecols=NULL, lambda = NULL,col = 1, fcol = 4, pi.col=1, pi.lty=2, ylim = NULL, main = NULL, ylab = "d", xlab = "s")

