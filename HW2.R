# Exercise 12.1
library(TSA)
data(CREF)
dev.new(width=4.875,height=2.5,pointsize=8)
r.cref=diff(log(CREF))*100
plot(abs(r.cref), ylab='abs(return)')

dev.new(width=4.875,height=2.5,pointsize=8)
plot(r.cref^2, ylab='return^2')

# Exercise 12.2
data("usd.hkd")
dev.new(width=4.875,height=2.5,pointsize=8)
plot(ts(abs(usd.hkd$hkrate),freq=1),type='l',xlab='day', ylab='abs(return)')
dev.new(width=4.875,height=2.5,pointsize=8)
plot(ts(usd.hkd$hkrate^2,freq=1),type='l',xlab='day', ylab='return^2')

#Exercise 12.7
set.seed(29)
garch11.sim=garch.sim(alpha=c(0.01,0.1),beta=.8,n=500)
plot(garch11.sim,type='l',ylab=expression(r[t]), xlab='t')
acf(garch11.sim)
pacf(garch11.sim)
eacf(garch11.sim)

acf(garch11.sim^2)
pacf(garch11.sim^2)
eacf(garch11.sim^2)


acf(abs(garch11.sim))
pacf(abs(garch11.sim))
eacf(abs(garch11.sim))

McLeod.Li.test(y=garch11.sim)

#First 200 obs
garch11_2.sim <- garch11.sim[1:200]
plot(garch11_2.sim,type='l',ylab=expression(r[t]), xlab='t')
acf(garch11_2.sim)
pacf(garch11_2.sim)
eacf(garch11_2.sim)

acf(garch11_2.sim^2)
pacf(garch11_2.sim^2)
eacf(garch11_2.sim^2)

acf(abs(garch11_2.sim))
pacf(abs(garch11_2.sim))
eacf(abs(garch11_2.sim))

McLeod.Li.test(y=garch11_2.sim)

#Exercise 12.8
data("cref.bond")
plot(cref.bond,type='l')

r.bond <- diff(log(cref.bond)*100)
plot(r.bond)
abline(h=0)
McLeod.Li.test(y=r.bond)

acf(r.bond)
pacf(r.bond)
acf(r.bond^2)
pacf(r.bond^2)
acf(abs(r.bond))
pacf(abs(r.bond))

#Exercise 12.9
data("google")
plot(google, type='l')

acf(google)
pacf(google)

mean(google)
t.test(google,alternative = 'greater')

McLeod.Li.test(y=google)

eacf(google^2)
eacf(abs(google))

set.seed(0)
m1 <- garch(x=google-mean(google), order=c(1,1),reltol=0.000001)
summary(m1)
AIC(m1)


plot(residuals(m1))
acf(residuals(m1)^2, na.action = na.omit)
acf(abs(residuals(m1)), na.action = na.omit)
gBox(m1, method = "squared")
gBox(m1, method = "absolute")
Box.test(x=)
( "Ljung-Box")

plot((fitted(m1)[,1])^2,type='l',ylab='Conditional Variance',
     xlab='t')

qqnorm(residuals(m1))
qqline(residuals(m1))

set.seed(29)
m = garchFit(~garch(1, 1), data = google)
summary(m)
predict(m)


#Exercise 12.10
data("oil.price")
m1.oil <- arima(log(oil.price),order=c(0,1,1))
#squared
acf((residuals(m1.oil))^2)
pacf((residuals(m1.oil))^2)
eacf((residuals(m1.oil))^2)

#absolute
acf(abs(residuals(m1.oil)))
pacf(abs(residuals(m1.oil)))
eacf(abs(residuals(m1.oil)))

m2.oil=garchFit(formula=~arma(0,1)+garch(1,1),
data=diff(log(oil.price)),include.mean=F)
summary(m2.oil)

plot((residuals(m2.oil)-mean(residuals(m2.oil)))/sd(residuals(m2.oil)), 
     ylab= 'Standardized Residuals', type='l')


m3.oil=arimax(log(oil.price),order=c(0,1,1),io=c(2,56), 
              xreg=data.frame(AO=seq(oil.price)==8)) 
plot(rstandard(m3.oil),ylab= 'Standardized Residuals', type='b' )
plot(rstandard(m3.oil)^2,ylab= 'Standardized Residuals', type='l' )
acf(rstandard(m3.oil))
acf((rstandard(m3.oil))^2)
qqnorm(rstandard(m3.oil))
qqline(rstandard(m3.oil))

shapiro.test(rstandard(m3.oil))
jarque.bera.test(rstandard(m3.oil))
