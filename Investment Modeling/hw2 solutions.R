L1 = list(matrix(1:6,2,3),c(4,1,-9,0),matrix(c(7,3,12,44,5,7,-3,1,9),3,3))
L2 = list(4,5,7:1)
L3 = list(matrix(16:1,4,4),matrix(c(3,7,8),3,2))



A = function(x=9,y)
{
  if(x<y) 
  {
    tot = x + y
  }
  else
  {
    tot = x *y
  }
  return(tot)
  
}

hw = function(fname,lname)
{
  cat("hello",fname,lname)
}

calcProfit=function(price,rent)
{
  if(price<=100000)
  {
    return(price*(.05)+(rent*12))
  }
  else
  {
    return(price*(.1) + (rent * 12))
  }
}

realProb = data.frame(price=c(95000,100000,105000,110000),rent=c(900,1000,1150,1200),prob=c(.3,.35,.15,.2))

#for loop way
tot = 0
for(i in 1:nrow(realProb))
{
  tot = tot + calcProfit(realProb$price[i],realProb$rent[i])*realProb$prob[i]
}

#mapply
sum(mapply(calcProfit,realProb$price,realProb$rent)*realProb$prob)

stocka = data.frame(Price = c(15,10,18,11,19), Prob=c(.3,.25,.15,.2,.1))

reta = (stocka$Price - 10)/10
ERA = sum(reta*stocka$Prob)
stda = sqrt(sum(stocka$Prob*(reta - ERA)^2))

impstd = 200*.25*sqrt(30/252)
waynestock = data.frame(price = c(-3.5*impstd + 200, 3.5*impstd+ 200))


shade_limit <- function(x) { 
  y <- dnorm(x,200,impstd)
  y[x<215]<-NA 
  return(y)
}

ggplot(waynestock, aes(x=price)) + 
  stat_function(fun = dnorm, args = list(mean = 200, sd = impstd), color="red", geom="area", fill="red", alpha=0.2)  + 
  stat_function(fun = shade_limit, geom="area", fill = "purple", alpha=1)





