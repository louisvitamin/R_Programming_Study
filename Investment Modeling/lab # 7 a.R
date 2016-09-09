#part A
getFinancials("aapl")
getFinancials("fb")
getFinancials("tsla")
getFinancials("amzn")

#getSymbols("aapl")
#getSymbols("fb")
#getSymbols("tsla")
#getSymbols("amzn")

tickers = c("aapl","fb","tsla","amzn")

#I create a matrix for each variable so that I can store all 5 quarters for all 4 tickers
ebit = matrix(0,4,5)
ppe = matrix(0,4,5)
dep = matrix(0,4,5)
ca = matrix(0,4,5)
cl = matrix(0,4,5)
roc = matrix(0,4,5)

shares = matrix(0,4,5)
price = rep(0,4)
debt = matrix(0,4,5)
cash = matrix(0,4,5)
redeem = matrix(0,4,5)
prefer = matrix(0,4,5)
minority = matrix(0,4,5)
ey = matrix(0,4,5)

stock = list(aapl.f,fb.f,tsla.f,amzn.f)

#apple
for(i in 1:NROW(stock))
{
x = stock[[i]]
ebit[i,]=x$IS$Q["Operating Income",]
ppe[i,] = x$B$Q["Property/Plant/Equipment, Total - Gross",]
dep[i,] = x$B$Q["Accumulated Depreciation, Total",]
ca[i,] = x$B$Q["Total Current Assets",]
cl[i,] = x$B$Q["Total Current Liabilities",]

shares[i,] = x$B$Q["Total Common Shares Outstanding",]
debt[i,] = x$B$Q["Total Debt",]
cash[i,] = x$B$Q["Cash & Equivalents",]
redeem[i,] = x$B$Q["Redeemable Preferred Stock, Total",]
prefer[i,] = x$B$Q["Preferred Stock - Non Redeemable, Net",]
minority[i,] = x$B$Q["Minority Interest",]
price[i]=getQuote(tickers[i])$Last
}

dep[is.na(dep)]=0
ppe[is.na(ppe)]=0
ca[is.na(ca)]=0
cl[is.na(cl)]=0

cash[is.na(cash)]=0
redeem[is.na(redeem)]=0
prefer[is.na(prefer)]=0
minority[is.na(minority)]=0

roc = ebit/((ppe - dep) + (ca - cl))
ey=ebit/((shares*price)+debt-(cash+(ca - cl))+prefer+redeem+minority)

rocrank = matrix(0,4,5)
eyrank = matrix(0,4,5)

for(i in 1:5)
{
  rocrank[,i] = rank(-roc[,i])
  eyrank[,i] = rank(-ey[,i])
}

dfroc = data.frame(rank=as.vector(rocrank),qtr=c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4)),ticker = rep(tickers,5))
dfey = data.frame(rank=as.vector(eyrank),qtr=c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4)),ticker = rep(tickers,5))
dfroc$type = "roc"
dfey$type = "ey"
df = rbind(dfroc,dfey)

ggplot(df[df$qtr==1,], aes(x=ticker,y=rank,fill=type)) + geom_bar(stat="identity") + scale_fill_manual(values=c("red","blue"))
ggplot(df[df$qtr==2,], aes(x=ticker,y=rank,fill=type)) + geom_bar(stat="identity") + scale_fill_manual(values=c("red","blue"))
ggplot(df[df$qtr==3,], aes(x=ticker,y=rank,fill=type)) + geom_bar(stat="identity") + scale_fill_manual(values=c("red","blue"))
ggplot(df[df$qtr==4,], aes(x=ticker,y=rank,fill=type)) + geom_bar(stat="identity") + scale_fill_manual(values=c("red","blue"))


#part B
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

opt.aapl=getOptionChain("AAPL",NULL)
opt.tsla=getOptionChain("TSLA",NULL)
opt.amzn=getOptionChain("AMZN",NULL)
opt.fb = getOptionChain("fb","2015-12-31") #because fb is not pulling them all in

opt = list(opt.aapl[[7]],opt.fb,opt.tsla[[7]],opt.amzn[[7]])
impstd = rep(0,4) # to hold the stock price and implied std

t= as.numeric(difftime(as.Date("2015-12-31"),Sys.Date(),unit="days"))
getSymbols("TB3MS",src= "FRED")
r=TB3MS[nrow(TB3MS)]
r=as.vector(r)/100

for(i in 1:NROW(opt))
{
o = opt[[i]]
max_vol =which.max(o$calls$Vol)
X = o$calls$Strike[max_vol]
c = o$calls$Last[max_vol]
#t_string = names(o)[7]
#t= as.numeric(difftime(as.Date(t_string,"%b.%d.%Y"),Sys.Date(),unit="days"))
impstd[i]= imp.vol(price[i],X,(t/252),r,c)*sqrt(t/252)*S
}

fp = price*(1.05) #the objective is to calculate the probability if the stock will appreciate greater then 5% next quarter (12/31/2015 contracts)
#so I take today's price and I calculate how much would they be 5% greater

prob = 1-mapply(pnorm,fp,price,impstd) #i used mapply instead of a for-loop, but do what makes sense to you

dfprob = data.frame(tickers, prob)
ggplot(dfprob, aes(x = tickers, y=prob)) + geom_bar(stat="identity",fill="pink")

test=function(a,b){
  if(a > 5){
    return (a+1)
  }else{
    return (a+b)
  }
}

v= c(1,2,3,4,5,6,7,8,9,10)
test(v,5)
mapply(test,v,5)
