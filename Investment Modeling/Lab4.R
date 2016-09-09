library(quantmod)
library(ggplot2)
getSymbols("AAPL")
getSymbols("NFLX")
getSymbols("AXP")
getSymbols("UNH")

drap = tail(dailyReturn(AAPL),30)
drnf = tail(dailyReturn(NFLX),30)
dram = tail(dailyReturn(AXP),30)
drun = tail(dailyReturn(UNH),30)

drap_mean=mean(drap)
drnf_mean=mean(drnf)
dram_mean=mean(dram)
drun_mean=mean(drun)

drap_std=sd(drap)
drnf_std=sd(drnf)
dram_std=sd(dram)
drun_std=sd(drun)

drap_sharpe=drap_mean/drap_std
drnf_sharpe=drnf_mean/drnf_std
dram_sharpe=dram_mean/dram_std
drun_sharpe=drun_mean/drun_std

drap_diff = drap - drap_mean
drnf_diff = drnf - drnf_mean
dram_diff = dram - dram_mean
drun_diff = drun - drun_mean

for(i in 1:length(drap_diff))
{
  if(drap_diff[i]>0)
  {
    drap_diff[i]=0
  }
  if(drnf_diff[i]>0)
  {
    drnf_diff[i]=0
  }
  if(dram_diff[i]>0)
  {
    dram_diff[i]=0
  }
  if(drun_diff[i]>0)
  {
    drun_diff[i]=0
  }
}

drap_dd = sqrt(sum((drap_diff^2)/length(drap_diff)))
drnf_dd = sqrt(sum((drnf_diff^2)/length(drnf_diff)))
dram_dd = sqrt(sum((dram_diff^2)/length(dram_diff)))
drun_dd = sqrt(sum((drun_diff^2)/length(drun_diff)))

drap_sort=drap_mean/drap_dd
drnf_sort=drnf_mean/drnf_dd
dram_sort=dram_mean/dram_dd
drun_sort=drun_mean/drun_dd

drap_max = max(drap)
drnf_max =max(drnf)
dram_max =max(dram)
drun_max=max(drun)
drap_min = min(drap)
drnf_min = min(drnf)
dram_min = min(dram)
drun_min = min(drun)

apdf = data.frame(category = c("mean","std","sharpe","dd", "sortino","best_dr", "worst_dr"), 
                 analytics = c(drap_mean,drap_std,drap_sharpe, drap_dd, drap_sort, drap_max, drap_min))
apdf$stock = "Apple"         
nfdf = data.frame(category = c("mean","std","sharpe","dd", "sortino","best_dr", "worst_dr"), 
                  analytics = c(drnf_mean,drnf_std,drnf_sharpe, drnf_dd, drnf_sort, drnf_max, drnf_min))
nfdf$stock = "Netflix"         
amdf = data.frame(category = c("mean","std","sharpe","dd", "sortino","best_dr", "worst_dr"), 
                  analytics = c(dram_mean,dram_std,dram_sharpe, dram_dd, dram_sort, dram_max, dram_min))
amdf$stock = "American Express"  
undf = data.frame(category = c("mean","std","sharpe","dd", "sortino","best_dr", "worst_dr"), 
                  analytics = c(drun_mean,drun_std,drun_sharpe, drun_dd, drun_sort, drun_max, drun_min))
undf$stock = "UnitedHealth"     

# make bar graph
risk_stat = rbind(apdf,nfdf,amdf,undf)
ggplot(risk_stat, aes(x=category, y=analytics, fill=stock)) + 
  geom_bar(position="dodge",stat="identity",color="black")+
  scale_fill_brewer(palette="Pastel1")
################################################################
#apple
drap = 1000*drap
drap = floor(drap)
drap = drap/1000
table(drap)

freq_ap = table(drap)
dfap = data.frame(freq_ap/sum(freq_ap))
names(dfap) = c("dr","prob")

dfap$dailyReturn = as.numeric(levels(dfap$dr))
ggplot(dfap, aes(x=dr, y=prob))+
         geom_bar(stat = "identity", fill = "blue",color = "pink")+
         ggtitle("Apple")

a1=sum(dfap[dfap$dailyReturn<= -.02,2])
b1=sum(dfap[dfap$dailyReturn>-.02 & dfap$dailyReturn <= 0,2])
c1=sum(dfap[dfap$dailyReturn>0 & dfap$dailyReturn <= .02,2])
d1=sum(dfap[dfap$dailyReturn>.02 & dfap$dailyReturn <= .05,2])
e1=sum(dfap[dfap$dailyReturn>.05,2])

dpap = data.frame(c("<=-2%","-2% - 0","0 - 2%", "2% - 5%",">5%"),c(a1,b1,c1,d1,e1))
names(dpap)= c("category","prob")
dpap$stock = "Apple"

#Netflix
drnf = 1000*drnf
drnf = floor(drnf)
drnf = drnf/1000
table(drnf)

freq_nf = table(drnf)
dfnf = data.frame(freq_nf/sum(freq_nf))
names(dfnf) = c("dr","prob")

dfnf$dailyReturn = as.numeric(levels(dfnf$dr))
ggplot(dfnf, aes(x=dr, y=prob))+
  geom_bar(stat = "identity", fill = "blue",color = "pink")+
  ggtitle("Netflix")

a2=sum(dfap[dfnf$dailyReturn<= -.02,2])
b2=sum(dfap[dfnf$dailyReturn>-.02 & dfnf$dailyReturn <= 0,2])
c2=sum(dfap[dfnf$dailyReturn>0 & dfnf$dailyReturn <= .02,2])
d2=sum(dfap[dfnf$dailyReturn>.02 & dfnf$dailyReturn <= .05,2])
e2=sum(dfap[dfnf$dailyReturn>.05,2])

dpnf = data.frame(c("<=-2%","-2% - 0","0 - 2%", "2% - 5%",">5%"),c(a2,b2,c2,d2,e2))
names(dpnf)= c("category","prob")
dpnf$stock = "Netflix"

#American Express
dram = 1000*dram
dram = floor(dram)
dram = dram/1000
table(dram)

freq_am = table(dram)
dfam = data.frame(freq_ap/sum(freq_am))
names(dfam) = c("dr","prob")

dfam$dailyReturn = as.numeric(levels(dfam$dr))
ggplot(dfam, aes(x=dr, y=prob))+
  geom_bar(stat = "identity", fill = "blue",color = "pink")+
  ggtitle("American Express")

a3=sum(dfam[dfap$dailyReturn<= -.02,2])
b3=sum(dfam[dfap$dailyReturn>-.02 & dfam$dailyReturn <= 0,2])
c3=sum(dfam[dfap$dailyReturn>0 & dfam$dailyReturn <= .02,2])
d3=sum(dfam[dfap$dailyReturn>.02 & dfam$dailyReturn <= .05,2])
e3=sum(dfam[dfap$dailyReturn>.05,2])

dpam = data.frame(c("<=-2%","-2% - 0","0 - 2%", "2% - 5%",">5%"),c(a3,b3,c3,d3,e3))
names(dpam)= c("category","prob")
dpam$stock = "AmericanExpress"

#UnitedHealth
drun = 1000*drun
drun = floor(drun)
drun = drun/1000
table(drun)

freq_un = table(drun)
dfun = data.frame(freq_un/sum(freq_un))
names(dfun) = c("dr","prob")

dfun$dailyReturn = as.numeric(levels(dfun$dr))
ggplot(dfun, aes(x=dr, y=prob))+
  geom_bar(stat = "identity", fill = "blue",color = "pink")+
  ggtitle("UnitedHealth")

a4=sum(dfun[dfun$dailyReturn<= -.02,2])
b4=sum(dfun[dfun$dailyReturn>-.02 & dfun$dailyReturn <= 0,2])
c4=sum(dfun[dfun$dailyReturn>0 & dfun$dailyReturn <= .02,2])
d4=sum(dfun[dfun$dailyReturn>.02 & dfun$dailyReturn <= .05,2])
e4=sum(dfun[dfun$dailyReturn>.05,2])

dpun = data.frame(c("<=-2%","-2% - 0","0 - 2%", "2% - 5%",">5%"),c(a4,b4,c4,d4,e4))
names(dpun)= c("category","prob")
dpun$stock = "UnitedHealth"

discrete_prob = rbind(dpap,dpnf,dpam,dpun)
ggplot(discrete_prob, aes(x=category, y=prob, fill=stock)) + 
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_brewer(palette="Pastel1") 