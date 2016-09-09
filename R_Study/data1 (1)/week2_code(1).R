> library(fpp)

  #This loads:
  #some data for use in examples and exercises forecast package (for forecasting functions)
  #tseries package (for a few time series functions)
  #fma package (for lots of time series data) 
  #expsmooth package (for more time series data)
  #lmtest package (for some regression functions)

plot(melsyd[,"Economy.Class"], 
            main="Economy class passengers: Melbourne-Sydney",
            xlab="Year",ylab="Thousands")

plot(log(a10), ylab="$ million", xlab="Year",
            main="Antidiabetic drug sales") 

seasonplot(a10,ylab="$ million", xlab="Year", 
                        main="Seasonal plot: antidiabetic drug sales",
                        year.labels.left=TRUE, col=1:20, pch=19)

monthplot(a10,ylab="$ million",xlab="Month",xaxt="n",
                       main="Seasonal deviation plot: antidiabetic drug sales")
axis(1,at=1:12,labels=month.abb,cex=0.8)

plot(jitter(fuel[,5]), jitter(fuel[,8]), 
            xlab="City mpg", ylab="Carbon footprint")

pairs(fuel[,-c(1:2,4,7)], pch=19)





###########Summary statistics##################

fuel2 <- fuel[fuel$Litres<2,]
summary(fuel2[,"Carbon"])
sd(fuel2[,"Carbon"])
beer2 <- window(ausbeer, start=1992, end=2006-.1)
lag.plot(beer2, lags=9, do.lines=FALSE)
Acf(beer2)
set.seed(30)
x <- ts(rnorm(50))
plot(x, main="White noise")
Acf(x)


##########Measuring performances######################
beer2 <- window(ausbeer,start=1992,end=2006-.1)
beerfit1 <- meanf(beer2, h=11)
beerfit2 <- naive(beer2, h=11)
beerfit3 <- snaive(beer2, h=11)
plot(beerfit1, plot.conf=FALSE,
     main="Forecasts for quarterly beer production")
lines(beerfit2$mean,col=2)
lines(beerfit3$mean,col=3)
lines(ausbeer)
legend("topright",lty=1,col=c(4,2,3),cex=0.6,
       legend=c("Mean function", "Naive method","Seasonal naive method"))

dj2 <- window(dj,end=250)
plot(dj2,main="Dow Jones Index (daily ending 15 Jul 94)",
     ylab="",xlab="Day",xlim=c(2,290))
lines(naive(dj2,h=42)$mean,col=4)
lines(rwf(dj2,drift=TRUE,h=42)$mean,col=3)
legend("topleft",lty=1,col=c(4,3),cex=0.5,
       legend=c("Naive method","Drift method"))
beer3 <- window(ausbeer, start=2006)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)


####Transformation and diagnosis
plot(log(elec), ylab="Transformed electricity demand",
 xlab="Year", main="Transformed monthly electricity demand")
title(main="Log",line=-1)

# The BoxCox.lambda() function chooses a value of lambda.
lambda <- BoxCox.lambda(elec) # = 0.27
plot(BoxCox(elec,lambda))

monthdays <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),14)
monthdays[26 + (4*12)*(0:2)] <- 29
par(mfrow=c(2,1))
plot(milk, main="Monthly milk production per cow",
  ylab="Pounds",xlab="Years")
plot(milk/monthdays, 
main="Average milk production per cow per day", 
  ylab="Pounds", xlab="Years")
  
  
dj2 <- window(dj, end=250)
plot(dj2, main="Dow Jones Index (daily ending 15 Jul 94)", 
  ylab="", xlab="Day")
res <- residuals(naive(dj2))
plot(res, main="Residuals from naive method", 
  ylab="", xlab="Day")
Acf(res, main="ACF of residuals")
hist(res, nclass="FD", main="Histogram of residuals")

# lag=h and fitdf=K
Box.test(res, lag=10, fitdf=0)
    
Box.test(res,lag=10, fitdf=0, type="Lj")
