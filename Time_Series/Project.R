### Project
####Part 1
library(fpp)
library(quantmod)
library(fGarch)
library(ggplot2)
getSymbols("JNJ")
# Get log daily return of past 3 years
dr= dailyReturn(JNJ)
dr= tail(dr,252*3)
logRet = log(dr+1)

#check the autocorrelation of ts 
Box.test(logRet) 
acf(logRet)

#take training set
train = head(logRet,756-125)
Box.test(train) #p-value is small; ARIMA needed

m1 = auto.arima(train)
m1

#2   create the function for predicting signal 
direction.p = function(x,or,t){
  ret = as.vector(x)
  p = rep(0,t)
  counter = 0
  for(i in 1:t){
    model = arima(ret[i:(length(ret)-t+i-1)],order = or,optim.control=list(maxit = 1000))
    fc = forecast(model,h=1)
    p[i] = fc$mean
    if(as.numeric(ret[length(ret)-t+i] * p[i])> 0){
      counter = counter + 1
    }
  }
  precentage = counter/t
  return(precentage)
}

direction.p(logRet,c(2,0,2),125)
#0.512

#3
acf(train)  #MA(1)
pacf(train) #AR(3)

m2 = arima(train,order=c(0,0,1))
m3= arima(train,order=c(3,0,0))

direction.p(logRet,c(0,0,1),125)  #0.544
direction.p(logRet,c(3,0,0),125)  #0.44


#4
performance = rep(0,125)
op = tail(as.vector(JNJ$JNJ.Open),125)
close = tail(as.vector(JNJ$JNJ.Close),125)
money = 100000
ret = dr
p = rep(0,125) 
for(i in 1:125){
  model = arima(ret[i:(length(ret)-125+i-1)],order = c(2,0,2),optim.control=list(maxit = 1000))
  fc = forecast(model,h=1)
  p[i] = fc$mean
  if(p[i]> 0){
    long = 0.99997*money/op[i]  
    money = 0.99997*long*close[i]
  }else{
    short = 0.99997*money/op[i]
    money = money + 0.99997*(short*(op[i]-close[i]))
  }
  performance[i]= money
}                        
money
# 109840.7
plot(performance)


performance = rep(0,125)
op = tail(as.vector(JNJ$JNJ.Open),125)
close = tail(as.vector(JNJ$JNJ.Close),125)
money = 100000
ret = dr
p = rep(0,125) 
for(i in 1:125){
  model = arima(ret[i:(length(ret)-125+i-1)],order = c(0,0,1),optim.control=list(maxit = 1000))
  fc = forecast(model,h=1)
  p[i] = fc$mean
  if(p[i]> 0){
    long = 0.99997*money/op[i]  
    money = 0.99997*long*close[i]
  }else{
    short = 0.99997*money/op[i]
    money = money + 0.99997*(short*(op[i]-close[i]))
  } 
  performance[i] = money
}                        
money
# 107324.5
plot(performance)

performance = rep(0,125)
op = tail(as.vector(JNJ$JNJ.Open),125)
close = tail(as.vector(JNJ$JNJ.Close),125)
money = 100000
ret = dr
p = rep(0,125) 
for(i in 1:125){
  model = arima(ret[i:(length(ret)-125+i-1)],order = c(3,0,0),optim.control=list(maxit = 1000))
  fc = forecast(model,h=1)
  p[i] = fc$mean
  if(p[i]> 0){
    long = 0.99997*money/op[i]  
    money = 0.99997*long*close[i] 
  }else{
    short = 0.99997*money/op[i]
    money = money + 0.99997*(short*(op[i]-close[i]))
  }
  performance[i] = money
}                        
money
# 111036.3
plot(performance)

#5
#QQ Plot
qqnorm(train)   #can see heavy tails 
qqline(train)
#check arch effect
Box.test(m1$residuals^2)
Box.test(m2$residuals^2)
Box.test(m3$residuals^2)
par(mfcol=c(2,1))
acf(m1$residuals^2)
pacf(m1$residuals^2)
acf(m2$residuals^2)
pacf(m2$residuals^2)
acf(m3$residuals^2)
pacf(m3$residuals^2)
# Above plots show that garch model should be used; apply Garch(1,1)

g1=garchFit(~arma(2,2)+garch(1,1),data=train)
summary(g1)
#Drop ar2 and ma2, run arma(1,1) + garch(1,1)
g1 = garchFit(~arma(1,1)+garch(1,1),data=train)
summary(g1)
#Drop ar1 and ma1, run purely garch(1,1)
g1 = garchFit(~garch(1,1),data=train)
summary(g1)

g2=garchFit(~arma(0,1)+garch(1,1),data=train)
summary(g2)

g3=garchFit(~arma(3,0)+garch(1,1),data=train)
summary(g3)


#Simulation for g1 
op = tail(as.vector(JNJ$JNJ.Open),125)
close = tail(as.vector(JNJ$JNJ.Close),125)
money = 100000
ret = dr
result = rep(0,10)
k_list = c( 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
for(t in 1:10){
  k = k_list[t]
  p = rep(0,125)
  avg = rep(0,125)
  sigma = rep(0,125)
  money = 100000
  for(i in 1:125){
    model <- garchFit(~garch(1,1),data=ret[i:(length(ret)-125+i-1)],trace=FALSE)
    fc = predict(model,1)  #forecast function doesn't work 
    p[i] = fc$meanForecast
    avg[i] = mean(ret[(length(ret)-146+i):(length(ret)-126+i)])
    sigma[i] = fc$standardDeviation 
    if(p[i]> avg[i]+k*sigma[i]){
      long = 0.99997*money/op[i]  
      money = 0.99997*long*close[i]
    }else if(p[i] < avg[i]-k*sigma[i]){
      short = 0.99997*money/op[i]
      money = money + 0.99997*(short*(op[i]-close[i]))
    }                      
  }                        
  result[t] = money
}
k_list[which.max(result)] #when k = 0.25, rerurn is the highest  
max(result) #[1]  87426.85  88509.08  93204.71 100341.41 107193.03 104847.99 100409.87  98647.31 100000.00 100000.00
# 107193  

#graph the perfomance of different k value 
result1=c( 87426.85,88509.08,  93204.71, 100341.41, 107193.03, 104847.99, 100409.87,  98647.31, 100000.00 ,100000.00 )
df1=data.frame(rbind(k_list,result1))

ggplot(data=df1, aes(x=k_list, y=result1)) +
  geom_bar(stat="identity",fill="white", colour="darkgreen")

#Simulation for g2
money = 100000
ret = dr
result = rep(0,10)
k_list = c( 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
for(t in 1:10){
  k = k_list[t]
  p = rep(0,125)
  avg = rep(0,125)
  sigma = rep(0,125)
  money = 100000
  for(i in 1:125){
    model <- garchFit(~arma(0,1)+garch(1,1),data=ret[i:(length(ret)-125+i-1)],trace=FALSE)
    fc = predict(model,1) 
    p[i] = fc$meanForecast
    avg[i] = mean(ret[(length(ret)-146+i):(length(ret)-126+i)])
    sigma[i] = fc$standardDeviation 
    if(p[i]> avg[i]+k*sigma[i]){
      long = 0.99997*money/op[i]  
      money = 0.99997*long*close[i]
    }else if(p[i] < avg[i]-k*sigma[i]){
      short = 0.99997*money/op[i]
      money = money + 0.99997*(short*(op[i]-close[i]))
    }                      
  }                        
  result[t] = money
}
k_list[which.max(result)] #0.25
max(result)  #[1]  91880.88  93263.72  91489.10  99048.60 103161.64 100915.15 102268.27  99697.14 100449.97 100000.00
#103161.6

result2 = c( 91880.88,93263.72,91489.10 ,99048.60,103161.64,100915.15,102268.27,99697.14,100449.97,100000.00)
df2=data.frame(rbind(k_list,result2))

ggplot(data=df2, aes(x=k_list, y=result2)) +
  geom_bar(stat="identity",fill="white", colour="darkgreen")

#Simulation for g3
money = 100000
ret = dr
result = rep(0,10)
k_list = c( 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
for(t in 1:10){
  k = k_list[t]
  p = rep(0,125)
  avg = rep(0,125)
  sigma = rep(0,125)
  money = 100000
  for(i in 1:125){
    model <- garchFit(~arma(3,0)+garch(1,1),data=ret[i:(length(ret)-125+i-1)],trace=FALSE)
    fc = predict(model,1)  
    p[i] = fc$meanForecast
    avg[i] = mean(ret[(length(ret)-146+i):(length(ret)-126+i)])
    sigma[i] = fc$standardDeviation 
    if(p[i]> avg[i]+k*sigma[i]){
      long = 0.99997*money/op[i]  
      money = 0.99997*long*close[i]
    }else if(p[i] < avg[i]-k*sigma[i]){
      short = 0.99997*money/op[i]
      money = money + 0.99997*(short*(op[i]-close[i]))
    }                      
  }                        
  result[t] = money #[1]  97678.46  97399.63  93377.15  98188.98 102888.04 102989.28  98075.82  98558.64  99592.09 100907.05
} 
max(result) 
k_list[which.max(result)] #0.3
#102989.3 

result3 = c(97678.46,97399.63 ,93377.15 ,98188.98,102888.04,102989.28,98075.82, 98558.64,  99592.09,100907.05)
df3=data.frame(rbind(k_list,result3))

ggplot(data=df2, aes(x=k_list, y=result3)) +
  geom_bar(stat="identity",fill="white", colour="darkgreen")


########################### Part 2
plot(train)
#This strategy uses Blackscholes and Implied volatility function to calculate the implied volatilitty. Then we calculate the 
#probability that stock will go up/down 2.5%. We combine the prob and forecast from garch model when decide long/short. 
#get data
op = tail(as.vector(JNJ$JNJ.Open),125)
close = tail(as.vector(JNJ$JNJ.Close),125)
ret = tail(dailyReturn(JNJ),252*3)
#Bkackscholes
blackscholes <- function(S,X,rx,T,sigma){
  
  d1 <- (log(S/X)+(rx+sigma^2/2)*T)/sigma*sqrt(T)
  d2 <- d1 - sigma*sqrt(T)
  
  values <- S*pnorm(d1) - X*exp(-rx*T)*pnorm(d2)
  
  return(values)
}
#implied volatility 
imp.vol <-
  function(S, X, rx, T, op){
    sig = 0.2
    sig.up = 2
    sig.down = 0.001
    count = 0 
    
    df = blackscholes(S, X, rx, T, sig) - op
    
    while(abs(df) > 0.001 && count<50000 ){
      if(df < 0){
        sig.down = sig
        sig = (sig.up + sig)/2
      }else{
        sig.up = sig
        sig = (sig.down + sig)/2
      }
      df = blackscholes(S, X, rx, T, sig) - op
      count = count + 1
    }
    if(count==50000){
      return(NA)
    }else{
      return(sig)
    }
  }

adj = tail(as.vector(JNJ$JNJ.Adjusted),126)

opt.chain = getOptionChain("JNJ",NULL)
std = rep(0,125)
Date = time(tail(JNJ,126))
stock = rep(0,125)
t = rep(0,125)
increase = rep(0,125)
decrease = rep(0,125)
for(i in 1:125){
  max_vol =  which.max(opt.chain[[2]]$calls$Vol)
  
  X = opt.chain[[2]]$calls$Strike[max_vol] 
  stock[i] = adj[i]
  t[i] = as.numeric(difftime(as.Date("2015-12-11"),as.Date(Date[i])),units="days")
  
  c = opt.chain[[2]]$calls$Last[max_vol]
  
  getSymbols("TB3MS",src = "FRED")
  r = as.numeric(TB3MS[nrow(TB3MS)])/100
  
  std[i] = imp.vol(stock[i],X,r,t[i]/252,c)*sqrt(t[i]/252)*stock[i] #generate the s.d of stock price 
  increase[i] = pnorm(stock[i]*1.025,stock[i],std[i],lower.tail=F)  #generate the prob that daily return > 2.5% 
  decrease[i] = pnorm(stock[i]*0.975,stock[i],std[i],lower.tail=T)    #generare the prob that daily return < -2.5%
} 


p = rep(0,125) #return forecast
avg = rep(0,125) #past 20 days average return
sigma = rep(0,125) #volatility forecast
money = 100000

#Get Dow Jone Industry Average daily prices 
getSymbols("DJIA", src='FRED')
DJIA = DJIA[complete.cases(DJIA),]
DJIA=tail(DJIA,125)
money = 100000
gainDow = rep(0,125)
gainJNJ = rep(0,125)
Dow = rep(0,125)
result = rep(NA,125)
for(i in 1:125){ 
  model <- garchFit(~garch(1,1),data=ret[i:(length(ret)-125+i-1)],trace=FALSE)#purely garch(1,1) performs the best in Q5
  fc = predict(model,1) #generate 1 day forecast 
  p[i] = mean(fc$meanForecast) #generate mean of forecasts of return 
  avg[i] = mean(ret[(length(ret)-146+i):(length(ret)-126+i)])
  sigma[i] = mean(fc$standardDeviation) #generate mean of forecasts of volatility
  
  Dow[i] = 0.99997*(100000/DJIA[1])
  #return on Dow Jones
  gainDow[i] = (Dow[i]*DJIA[i] - 100000)/100000 
  
  if(increase[i]>0.4 && p[i] > avg[i] + 0.25*sigma[i]){ #1) if the prob that daily return goes up 2.5% is bigger than 40%
    long = 0.99997*money/op[i]                        #2) if predicted ret is bigger than past 20 days avg ret + 0.25*predicted volatility from garch(1,1)
    money = 0.99997*long*close[i]     
    
  }else if(decrease[i]>0.4 && p[i] - 0.5*sigma[i]){#We choose large k-value 0.5 here because short is riskier than long
    short = 0.99997*money/op[i] #large k-value means higher bound and lower trading frequency
    money = money + 0.99997*(short*(op[i]-close[i])) 
  }
  print(money)
  result[i] = money
  gainJNJ[i] = (result[i] - 100000)/100000
  
  if(gainJNJ[i] > 2.5*gainDow[i] && gainJNJ[i] > 0 && gainDow[i] > 0){
    break    #break at day 101 
  }  
}  
money  #113076.9   
plot(result)


#### We also try another stragegy 
#This strategy uses three days forecast instead one day forecast 
money = 100000
moneyOnhand = rep(TRUE,125) 
for(i in 1:125){ 
  model <- garchFit(~garch(1,1),data=ret[i:(length(ret)-125+i-1)],trace=FALSE)#purely garch(1,1) performs the best in Q5
  fc = predict(model,3) #generate 3 days forecast 
  p[i] = mean(fc$meanForecast) #generate mean of three days forecasts of return 
  avg[i] = mean(ret[(length(ret)-146+i):(length(ret)-126+i)])
  sigma[i] = mean(fc$standardDeviation) #generate mean of three days forecasts of volatility
  if(moneyOnhand[i] == TRUE){ #Check if we have money to trade; if no money, we cannot trade  
    
    if(p[i]> avg[i]+0.25*sigma[i]){
      long = 0.99997*money/op[i]  
      money = 0.99997*long*close[i+2]     
      moneyOnhand[i+1] = FALSE #We buy at day(i) and sell at day(i+2)
      moneyOnhand[i+2]= FALSE  #That means we don't have money on hand for day(i) to day(i+2)
      
    }else if(p[i] < avg[i]-0.5*sigma[i]){#We choose large k-value 0.5 here because short is riskier than long
      short = 0.99997*money/op[i] #large k-value means higher bound and lower trading frequency
      money = money + 0.99997*(short*(op[i]-close[i+2]))
      moneyOnhand[i+1] = FALSE
      moneyOnhand[i+2]= FALSE
    }
  }
  print(money) 
}  
money  #111047.2 in return