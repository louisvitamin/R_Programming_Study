blackscholes <- function(S, X, rf, t, sigma) {
  
  d1 <- (log(S/X)+(rf+sigma^2/2)*t)/sigma*sqrt(t)
  d2 <- d1 - sigma * sqrt(t)
  
  values <- S*pnorm(d1) - X*exp(-rf*t)*pnorm(d2)
  
  return(values)
}

imp.vol <- function(s,x,t,r,price)
{
  sigma = .2
  sigma.high = 5
  sigma.low = .001
  count = 0
  error = blackscholes(s, x, r, t, sigma) - price
  
  while(abs(error) > .01 && count < 200000)
  {
    if (error<0)   
    {
      sigma.low = sigma
      sigma = (sigma.high + sigma.low) / 2 
    }
    else 
    {
      
      sigma.high=sigma
      sigma=(sigma.low + sigma.high)/2
      
    }
    
    error = blackscholes(s, x, r, t, sigma) - price
    count = count + 1 
  }
  if (count==200000) 
  {
    return(NA)
  }
  else
  {
    return(sigma)
  }
}  




#Lab # 6
opt.tsla=getOptionChain("TSLA",NULL)
max_vol =which.max(opt.tsla[[1]]$calls$Vol)
X = opt.tsla[[1]]$calls$Strike[max_vol]
c = opt.tsla[[1]]$calls$Last[max_vol]
t_string = names(opt.tsla)[1]
t= as.numeric(difftime(as.Date(t_string,"%b.%d.%Y"),Sys.Date(),unit="days"))
getSymbols("TB3MS",src= "FRED")
S=getQuote("TSLA")$Last
r=TB3MS[nrow(TB3MS)]
r=as.vector(r)/100
std= imp.vol(S,X,(t/252),r,c)*sqrt(t/252)*S

ggplot(data.frame(x = c(min(-3.5*std+S,-3.5*std+S), max(3.5*std+S,3.5*std+S))), aes(x=x)) + 
  stat_function(fun = dnorm, args = list(mean = S, sd = std), color="red", 
                geom="area", fill="red", alpha=0.2)

pnorm(220,S,std)
1-pnorm(230,S,std)
