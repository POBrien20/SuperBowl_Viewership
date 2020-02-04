library(readr)
library(readr)
library(readr)
library(fpp)
library(readr)
library(TSA)
library(imputeTS)
library(EnvStats)
library(astsa)
library(readxl)
Superbowl <- read_excel("C:/Users/patty/Desktop/School/Senior/Semester 1/STAT 3504/Superbowl.xlsx", 
                        range = "A3:C55", col_names = FALSE)
Superbowl
this <- Superbowl[1:50,]
that<- Superbowl[51:53,]
super <- ts(this[,3], start = 1967)
forecasts <- ts(that[,3], start = 2017)
all <- ts(Superbowl[,3], start = 1967)

super 
plot(super, type = 'o', ylab = 'Average Viewership', xlab = 'Year', main = 'Average Viewership of Superbowl over Years')

adf.test(super)
adf.test(diff(super))
adf.test(diff(diff(super)))

#Dicky-Fuller unit root test tells us that 2 order differencing will be the best, as it results
#in a stationary process for us.


acf2(super)

#ACF Decays to 0 and PACF spikes at 1 then goes to 0, suggests AR(1)

acf2(diff(super))

#Does not give much information.

acf2(diff(diff(super)))

#Possibly can suggest Arima(0,2,1)

eacf(super)

#Supports ARMA(1,1) or ARMA(2,1) process!

eacf(diff(super))

#Supports ARIMA(0,1,1), (0,1,2) or (0,1,3)

eacf(diff(diff(super)))

#Suggests Arima(0,2,1)

res = armasubsets(y=super, nar = 15, nma = 15)
plot(res)

#Suggests ARMA(1,5) or AR(3) or ARMA(3,5) or AR(1)

res = armasubsets(y=diff(super), nar = 15, nma = 15)
plot(res)

#Suggests ARIMA(3,1,0), ARIMA(3,1,5), ARIMA(4,1,0)

res = armasubsets(y=diff(diff(super)), nar = 15, nma = 15)
plot(res)

#Suggests Arima(1,2,0) or (1,2,2) or (3,2,2)

summary(auto.arima(super))

#Model 1
fit <- Arima(super, order = c(1,0,0))
summary(fit)
#Not overfit, Check!

#Model 2
fit <- Arima(super, order = c(0,2,1))
summary(fit)
#All good, check!

#Model 3
fit <- Arima(super, order = c(1,0,1))
summary(fit)
#Error, doesn't work

#Model 4
fit <- Arima(super, order = c(2,0,1))
summary(fit)
#Error, doesn't work

#Model 5
fit <- Arima(super, order = c(0,1,1))
summary(fit)
#overfit

#Model 6
fit <- Arima(super, order = c(1,1,1))
summary(fit)
#overfit

#Model 7
fit <- Arima(super, order = c(1,0,5))
summary(fit)
#Error, doesn't work

#Model 8
fit <- Arima(super, order = c(3,0,0))
summary(fit)
#Error, doesn't work

#Model 9
fit <- Arima(super, order = c(3,1,0))
summary(fit)
#overfit

#Model 10
fit <- Arima(super, order = c(3,1,5))
summary(fit)
#overfit

#Model 11
fit <- Arima(super, order = c(1,2,0))
summary(fit)
#Good!

#Model 12
fit <- Arima(super, order = c(1,2,2))
summary(fit)
#Overfit

#Model 13
fit <- Arima(super, order = c(3,1,2))
summary(fit)
#Overfit

#Model 14
fit <- Arima(super, order = c(1,2,1))
summary(fit)
#Good!
#Wanted to Try this because ARIMA(1,2,2) was only overfit on second MA term.

#Model 15
summary(auto.arima(super))
fit <- Arima(super, order = c(0,1,1), include.drift = TRUE)
summary(fit)


#1, 2, 11, 14, 15


#Model 1
fit <- Arima(super, order = c(1,0,0))
summary(fit)
qqnorm(residuals(fit))
qqline(residuals(fit))
shapiro.test(residuals(fit))
runs(residuals(fit))
quartz() # open a new graph window
tsdiag(fit,gof=6,omit.initial=F) 
acf(residuals(fit))

#Passes all but the box test. Nope.

#Model 2
fit <- Arima(super, order = c(0,2,1))
summary(fit)
qqnorm(residuals(fit))
qqline(residuals(fit))
shapiro.test(residuals(fit))
runs(residuals(fit))
quartz() # open a new graph window
tsdiag(fit,gof=6,omit.initial=F) 
acf(residuals(fit))

#Passes all but the box test. Nope.

#Model 11
fit <- Arima(super, order = c(1,2,0))
summary(fit)
qqnorm(residuals(fit))
qqline(residuals(fit))
shapiro.test(residuals(fit))
runs(residuals(fit))
quartz() # open a new graph window
tsdiag(fit,gof=6,omit.initial=F) 
acf(residuals(fit))
#Passes all but the box test. Nope.

#Model 14
fit <- Arima(super, order = c(1,2,1))
summary(fit)
qqnorm(residuals(fit))
qqline(residuals(fit))
shapiro.test(residuals(fit))
runs(residuals(fit))
quartz() # open a new graph window
tsdiag(fit,gof=6,omit.initial=F) 
acf(residuals(fit))

#Passes all tests!! Check!!

#Model 15
fit <- Arima(super, order = c(0,1,1), include.drift = TRUE)
summary(fit)
runs(residuals(fit))
acf(residuals(fit))
shapiro.test(residuals(fit))
qqnorm(residuals(fit))
quartz() # open a new graph window
tsdiag(fit,gof=6,omit.initial=F)

#Passes all tests!! Check!!


#Which is better? Compare AIC's
#Model 14
fit <- Arima(super, order = c(1,2,1))
AIC(fit)
#Model 15
fit <- Arima(super, order = c(0,1,1), include.drift = TRUE)
AIC(fit)

#Model 15 has the lowest AIC, so is therefore the strongest model!

fit <- Arima(super, order = c(1,2,1))

plot(all, type = 'o', xlab = 'Year', ylab = 'Average Viewership')
lines(as.vector(time(super)), fitted(fit), col ='red')

forecast <- sarima.for(super, 10, 1, 2, 1)
points(forecasts, col = 'blue')