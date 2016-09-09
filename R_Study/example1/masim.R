# Problem 1
masim=function(thetas, sigsq, T){
  p=length(thetas) #find the number of lags in our MA
  noise=rnorm(T+p, sd=sqrt(sigsq)) #generate the white noise plus a few to get started
  x=c(noise[1:p],rep(0,T)) #put the initial noise terms in and set the rest to zero
  for (i in (p+1):(T+p)){ #this loop generates the AR series with the recursive formula
    x[i]=thetas %*% noise[i-(1:p)] +noise[i]
  }
  x=x[(p+1):(T+p)] #throw away those initial starting points
  x #return the time series
}

x1 = masim(c(0.5,0.3),0.5,3000)
acf(x1)

x2 = masim(c(0.5,0.3),0.5,100)
acf(x2)
# When T is smaller, the blue line is narrower
# Since lag is 2, there should be cut off after lag 2.
# But there are some bars obersved under big T appearing to be beyond blue lines. 




#Problem 2
getSymbols("JPM")
JPM1 = JPM['2011-1::']
# compute log return
dr= dailyReturn(JPM1)
logRet = log(dr+1)

acf(logRet)
Box.test(logRet,lag=10,type="Ljung")
#fit MA 
h1 = arima(logRet, order=c(0,0,1))
h2 = arima(logRet, order=c(0,0,2))
Box.test(h1$residuals,lag=10,type="Ljung")
Box.test(h2$residuals,lag=10,type="Ljung")
tsdiag(h1)
tsdiag(h2)

#AR(1) model
ar = arima(logRet, order=c(1,0,0))
names(h1)
#get AIC scores  AR(1) has the lowest AIC
h1$aic # -6439.724
h2$aic # -6438.971
ar$aic # -6440.229


#Model comparison based on out of sample prediction error 
# MSE_arima is a function to use input ARIMA model to do 3 one-step forcasts and calculate the MSE.
# suppose data length is i; use data[1:i-3] to generate model and forecast i-2; then use data[2:i-2] 
# to forecast i-1... 

MSE_arima = function(x,or){  # x = data, or = orders of ARIMA model
ret = as.vector(x)  
ar1 = arima(ret[1:(length(ret)-3)], order = or)
p1 = forecast(ar1, h=1)
e1 = as.numeric(ret[length(ret)-2] - p1$mean)

ar2 = arima(ret[2:(length(ret)-2)], order = or)
p2 = forecast(ar2, h=1)
e2 = as.numeric(ret[length(ret)-1] - p2$mean)

ar3 = arima(ret[3:(length(ret)-1)], order = or)
p3 = forecast(ar3, h=1)
e3 = as.numeric(ret[length(ret)] - p3$mean)
MSE = (e1^2+e2^2+e3^2)/3
return(MSE)
}


mse_arima = function(x,or,t){
  ret = as.vector(x)
  p = rep(0,t)
  e = rep(0,t)
  for(i in 1:t){
    model = arima(ret[i:(length(ret)-t+i-1)],order = or)
    fc = forecast(model,h=1)
    p[i] = fc$mean
    e[i] = as.numeric(ret[length(ret)-t+i] - p[i])
  }
  mse = sum((e)^2)/t
  return(mse)
}

ar_mse = MSE_arima(logRet, c(1,0,0))
ar_mse #0.0002721686
h1_mse = MSE_arima(logRet, c(0,0,1))
h1_mse #0.0002727153
h2_mse = MSE_arima(logRet, c(0,0,2))
h2_mse #0.0002728383