#####################################  Part 1 
#Apple
getFinancials("aapl")
viewFinancials(aapl.f)
viewFinancials(aapl.f,"IS","A")
aapl.f$B$A["Total Equity",]


EBIT = aapl.f$IS$Q["Operating Income",]
NetP = aapl.f$B$Q["Property/Plant/Equipment, Total - Gross",] + aapl.f$B$Q["Accumulated Depreciation, Total",]
NetWC = aapl.f$B$Q["Total Current Assets",] - aapl.f$B$Q["Total Current Liabilities",]

ROC1 = EBIT/(NetP + NetWC)
ROC1 = ROC1[1]
getSymbols("AAPL",from = as.Date("2015-09-28"),  to = as.Date("2015-09-28"))
stock = as.numeric(AAPL$AAPL.Adjusted)
MarketCap = aapl.f$B$Q["Total Common Shares Outstanding",]*stock
TotalDebt = aapl.f$B$Q["Total Debt",] 
ExcessCash = aapl.f$B$Q["Cash & Equivalents",] + NetWC
PreferredStock = aapl.f$B$Q["Redeemable Preferred Stock, Total"]+aapl.f$B$Q["Preferred Stock - Non Redeemable, Net"]
Minority = aapl.f$B$Q["Minority Interest",]
PreferredStock[is.na(PreferredStock)] <- 0
Minority[is.na(Minority)] <- 0

EY1= EBIT/(MarketCap + TotalDebt - ExcessCash + PreferredStock + Minority)
EY1 = EY1[1]


#Amazon
getFinancials("amzn")

EBIT = amzn.f$IS$Q["Operating Income",]
NetP = amzn.f$B$Q["Property/Plant/Equipment, Total - Gross",] + amzn.f$B$Q["Accumulated Depreciation, Total",]
NetWC = amzn.f$B$Q["Total Current Assets",] - amzn.f$B$Q["Total Current Liabilities",]
NetP[is.na(NetP)] <- 0
ROC2 = EBIT/(NetP + NetWC)
ROC2 = ROC2[1]
getSymbols("AMZN",from = as.Date("2015-09-28"),  to = as.Date("2015-09-28"))
stock = as.numeric(AMZN$AMZN.Adjusted)
MarketCap = amzn.f$B$Q["Total Common Shares Outstanding",]*stock
TotalDebt = amzn.f$B$Q["Total Debt",] 
ExcessCash = amzn.f$B$Q["Cash & Equivalents",] + NetWC
PreferredStock = amzn.f$B$Q["Redeemable Preferred Stock, Total"]+amzn.f$B$Q["Preferred Stock - Non Redeemable, Net"]
Minority = amzn.f$B$Q["Minority Interest",]
PreferredStock[is.na(PreferredStock)] <- 0
Minority[is.na(Minority)] <- 0

EY2= EBIT/(MarketCap + TotalDebt - ExcessCash + PreferredStock + Minority)
EY2 = EY2[1]

# Facebook
getFinancials("FB")

EBIT = FB.f$IS$Q["Operating Income",]
NetP = FB.f$B$Q["Property/Plant/Equipment, Total - Gross",] + FB.f$B$Q["Accumulated Depreciation, Total",]
NetWC = FB.f$B$Q["Total Current Assets",] - FB.f$B$Q["Total Current Liabilities",]
NetP[is.na(NetP)] <- 0
ROC3 = EBIT/(NetP + NetWC)
ROC3 = ROC3[1]
getSymbols("FB",from = as.Date("2015-09-28"),  to = as.Date("2015-09-28"))
stock = as.numeric(FB$FB.Adjusted)
MarketCap = FB.f$B$Q["Total Common Shares Outstanding",]*stock
TotalDebt = FB.f$B$Q["Total Debt",]
Cash = FB.f$B$Q["Cash & Equivalents",]
Cash[is.na(Cash)] = 0
ExcessCash = Cash + NetWC
PreferredStock = FB.f$B$Q["Redeemable Preferred Stock, Total"]+FB.f$B$Q["Preferred Stock - Non Redeemable, Net"]
Minority = FB.f$B$Q["Minority Interest",]
PreferredStock[is.na(PreferredStock)] <- 0
Minority[is.na(Minority)] <- 0

EY3= EBIT/(MarketCap + TotalDebt - ExcessCash + PreferredStock + Minority)
EY3 = EY3[1]

# Tesla
getFinancials("tsla")

EBIT = tsla.f$IS$Q["Operating Income",]
NetP = tsla.f$B$Q["Property/Plant/Equipment, Total - Gross",] + tsla.f$B$Q["Accumulated Depreciation, Total",]
NetWC = tsla.f$B$Q["Total Current Assets",] - tsla.f$B$Q["Total Current Liabilities",]
NetP[is.na(NetP)] <- 0
ROC4 = EBIT/(NetP + NetWC)
ROC4 = ROC4[1]
getSymbols("TSLA",from = as.Date("2015-09-28"),  to = as.Date("2015-09-28"))
stock = as.numeric(TSLA$TSLA.Adjusted)
MarketCap = tsla.f$B$Q["Total Common Shares Outstanding",]*stock
TotalDebt = tsla.f$B$Q["Total Debt",]
Cash = tsla.f$B$Q["Cash & Equivalents",]
Cash[is.na(Cash)] = 0
ExcessCash = Cash + NetWC
PreferredStock = tsla.f$B$Q["Redeemable Preferred Stock, Total"]+tsla.f$B$Q["Preferred Stock - Non Redeemable, Net"]
Minority = tsla.f$B$Q["Minority Interest",]
PreferredStock[is.na(PreferredStock)] <- 0
Minority[is.na(Minority)] <- 0

EY4= EBIT/(MarketCap + TotalDebt - ExcessCash + PreferredStock + Minority)
EY4 = EY4[1]


df = data.frame(category=c("ROC","EY"), analytics = c(ROC1,EY1),stock = rep("Apple",2))
df = rbind(df,data.frame(category=c("ROC","EY"),analytics = c(ROC2,EY2),stock=rep("Amazon",2)))
df = rbind(df,data.frame(category=c("ROC","EY"),analytics = c(ROC3,EY3),stock=rep("Facebook",2)))
df = rbind(df,data.frame(category=c("ROC","EY"),analytics = c(ROC4,EY4),stock=rep("Tesla",2)))


ggplot(df, aes(x=category, y=analytics, fill=stock)) + 
  geom_bar(position="dodge",stat="identity",color="black")+
  scale_fill_brewer(palette="Pastel1")

####################
blackscholes <- function(S,X,rx,T,sigma){
  
  d1 <- (log(S/X)+(rx+sigma^2/2)*T)/sigma*sqrt(T)
  d2 <- d1 - sigma*sqrt(T)
  
  values <- S*pnorm(d1) - X*exp(-rx*T)*pnorm(d2)
  
  return(values)
}

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

volume = tail(as.vector(JNJ$JNJ.Volume),125)
adj = tail(as.vector(JNJ$JNJ.Adjusted),126)

opt.chain = getOptionChain("JNJ",NULL)
std = rep(0,125)
Date = time(tail(JNJ,126))
stock = rep(0,125)
prob =  rep(0,125)
t = rep(0,125)
for(i in 1:125){
max_vol =  which.max(opt.chain[[2]]$calls$Vol)

X = opt.chain[[2]]$calls$Strike[max_vol] 
stock[i] = adj[i]
t[i] = as.numeric(difftime(as.Date("2015-12-11"),as.Date(Date[i])),units="days")

c = opt.chain[[2]]$calls$Last[max_vol]

getSymbols("TB3MS",src = "FRED")
r = as.numeric(TB3MS[nrow(TB3MS)])/100

std[i] = imp.vol(stock[i],X,r,t[i]/252,c)*sqrt(t[i]/252)*stock[i]
prob[i] = pnorm(stock[i]*1.025,stock[i],std[i],lower.tail=F)  
} 
#######################
prob =  rep(0,125)

pnorm(stock*1.05,stock,std,lower.tail=F) 
#[1] 0.4788561

#Amazon
opt.chain = getOptionChain("AMZN",NULL)

max_vol =  which.max(opt.chain[[8]]$calls$Vol)

X = opt.chain[[8]]$calls$Strike[max_vol] 
getSymbols("AMZN",from = as.Date("2015-09-28"),  to = as.Date("2015-09-28"))
stock = as.numeric(AMZN$AMZN.Adjusted)

t = as.numeric(difftime(as.Date("2016-1-15"),as.Date("2015-9-28")),units="days")

c = opt.chain[[8]]$calls$Last[max_vol]

getSymbols("TB3MS",src = "FRED")
r = as.numeric(TB3MS[nrow(TB3MS)])/100


std = imp.vol(stock,X,r,t/252,c)*sqrt(t/252)*stock



pnorm(stock*1.05,stock,std,lower.tail=F)
#[1] 0.4501809

#Facebook
opt.chain = getOptionChain("FB",NULL)

max_vol =  which.max(opt.chain[[8]]$calls$Vol)

X = opt.chain[[8]]$calls$Strike[max_vol] 
getSymbols("FB",from = as.Date("2015-09-28"),  to = as.Date("2015-09-28"))
stock = as.numeric(FB$FB.Adjusted)

t = as.numeric(difftime(as.Date("2016-1-15"),as.Date("2015-11-15")),units="days")

c = opt.chain[[8]]$calls$Last[max_vol]

getSymbols("TB3MS",src = "FRED")
r = as.numeric(TB3MS[nrow(TB3MS)])/100


std = imp.vol(stock,X,r,t/252,c)*sqrt(t/252)*stock

pnorm(stock*1.05,stock,std,lower.tail=F)
#[1] 0.4797372

#Tesla
opt.chain = getOptionChain("TSLA",NULL)

max_vol =  which.max(opt.chain[[8]]$calls$Vol)

X = opt.chain[[8]]$calls$Strike[max_vol] 
getSymbols("TSLA",from = as.Date("2015-09-28"),  to = as.Date("2015-09-28"))
stock = as.numeric(TSLA$TSLA.Adjusted)

t = as.numeric(difftime(as.Date("2016-1-15"),as.Date("2015-09-28")),units="days")

c = opt.chain[[8]]$calls$Last[max_vol]

getSymbols("TB3MS",src = "FRED")
r = as.numeric(TB3MS[nrow(TB3MS)])/100


std = imp.vol(stock,X,r,t/252,c)*sqrt(t/252)*stock

pnorm(stock*1.05,stock,std,lower.tail=F)
#[1] 0.4328298
