#part 1
sss = stockSymbols()

df = data.frame(sss$Symbol, sss$Sector)
tick = subset(df, sss.Sector == "Health Care", sss.Symbol)
tick=as.vector(tick$sss.Symbol)

tick3 = list()
runFinancial=function(t)
{
  tryCatch(
    {
      xx=getFinancials(t,auto.assign = FALSE)
      return(xx)
    },
    error=function(x)
    {
      return(NA)
    }
  )
}

ww=mapply(runFinancial,tick)
names(ww)=tick
#ll <- lapply(tick, function(x) tryCatch(getFinancials(x),error=function(e){print(paste(x,'not found'));NA}))
#tick2 = substring(rapply(ll,na.omit),1,nchar(rapply(ll,na.omit))-2)
#fin=lapply(tick2, getFinancials, auto.assign=FALSE)
#names(fin)=rapply(ll,na.omit)

fin=ww[as.logical(!is.na(ww))]

#fin=list()
#for(i in 1:NROW(ll))
#{
#  if(!is.na(ll[[i]]))
#  {
#    fin[[length(fin)+1]]=ll[[i]]
#  }
#}

tick2=names(fin)
fin2=list()
for(i in 1:NROW(fin))
{
  x=fin[[i]]
  if(length(x$IS$Q)>0 && length(x$B$Q)>0)
  {
    fin2[[length(fin2)+1]]=fin[[i]]
    if(i==1)
    {
      tick3=tick2[i]
    }
    else
    {
      tick3 = c(tick3,tick2[i])
    }
  }
}



ebit = rep(0,NROW(fin2))
ppe = rep(0,NROW(fin2))
dep = rep(0,NROW(fin2))
ca = rep(0,NROW(fin2))
cl = rep(0,NROW(fin2))
roc = rep(0,NROW(fin2))

shares = rep(0,NROW(fin2))
price = rep(0,NROW(fin2))
debt = rep(0,NROW(fin2))
cash = rep(0,NROW(fin2))
redeem = rep(0,NROW(fin2))
prefer = rep(0,NROW(fin2))
minority = rep(0,NROW(fin2))
ey = rep(0,NROW(fin2))

marketCap = rep(0,NROW(fin2))

for(i in 1:NROW(fin2))
{
  x = fin2[[i]]
  
  ebit[i]=x$IS$Q["Operating Income",1]
  ppe[i] = x$B$Q["Property/Plant/Equipment, Total - Gross",1]
  dep[i] = x$B$Q["Accumulated Depreciation, Total",1]
  ca[i] = x$B$Q["Total Current Assets",1]
  cl[i] = x$B$Q["Total Current Liabilities",1]
  
  shares[i] = x$B$Q["Total Common Shares Outstanding",1]
  debt[i] = x$B$Q["Total Debt",1]
  cash[i] = x$B$Q["Cash & Equivalents",1]
  redeem[i] = x$B$Q["Redeemable Preferred Stock, Total",1]
  prefer[i] = x$B$Q["Preferred Stock - Non Redeemable, Net",1]
  minority[i] = x$B$Q["Minority Interest",1]
  price[i]=getQuote(tick3[i])$Last
  marketCap[i]=shares[i]*as.numeric(price[i])
}

dep[is.na(dep)]=0
ppe[is.na(ppe)]=0
ca[is.na(ca)]=0
cl[is.na(cl)]=0

cash[is.na(cash)]=0
redeem[is.na(redeem)]=0
prefer[is.na(prefer)]=0
minority[is.na(minority)]=0

price = as.numeric(price)
price[is.na(price)]=0

marketCap[is.na(marketCap)] = 0

for(i in 1:NROW(fin2))
{
  if((marketCap[i] > 10000) && (marketCap[i] = 10000)){
    roc[i] = ebit[i]/((ppe[i] - dep[i]) + (ca[i] - cl[i]))
    ey[i]=ebit[i]/(marketCap[i] +debt[i]-(cash[i]+(ca[i] - cl[i]))+prefer[i]+redeem[i]+minority[i])
  }
}
rocrank = rep(0,NROW(fin2))
eyrank = rep(0,NROW(fin2))


rocrank = rank(-roc)
eyrank = rank(-ey)

totrank = rocrank + eyrank
df = data.frame(totrank,tick3)

df10 = head(df[order(totrank),],10)

#part2
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

option = list(0,0,0,0,0,0,0,0,0,0)
price = rep(0,10)
for(i in 1:10){
  option[i] = getOptionChain(df10$tick3[i],"2015-12-18")
  price[i] = getQuote(as.character(df10$tick3[i]))$Last
}


impstd = rep(0,10) # to hold the stock price and implied std

t= as.numeric(difftime(as.Date("2015-12-18"),Sys.Date(),unit="days"))
getSymbols("TB3MS",src= "FRED")
r=TB3MS[nrow(TB3MS)]
r=as.vector(r)/100


for(i in 1:10)
{
  o = option[[i]]
  max_vol =which.max(o$Vol)
  X = o$Strike[max_vol]
  c = o$Last[max_vol]
  #t_string = names(o)[1]
  #t= as.numeric(difftime(as.Date(t_string,"%b.%d.%Y"),Sys.Date(),unit="days"))
  impstd[i]= imp.vol(price[i],X,(t/252),r,c)*sqrt(t/252)*price[i]
}

fp = price*(1.05) 

prob = 1-mapply(pnorm,fp,price,impstd)

dfprob = data.frame(df10$tick3, prob)
ggplot(dfprob, aes(x = df10$tick3, y=prob)) + geom_bar(stat="identity",fill="pink")
