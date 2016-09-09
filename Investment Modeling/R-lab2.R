chp_price = Chopotle$Adj.Close[1:252]
g_price = Google$Adj.Close[1:252]

g_dr = rep(0,251)
chp_dr = rep(0,251)

for(i in 1:(length(g_dr)-1))
{
  g_dr[i]=(g_price[i] - g_price[i+1])/g_price[i+1]
  chp_dr[i]=(chp_price[i] - chp_price[i+1])/chp_price[i+1]
}


g_mean=mean(g_dr)
chp_mean=mean(chp_dr)

g_std=sd(g_dr)
chp_std=sd(chp_dr)

g_sharpe=g_mean/g_std
chp_sharpe=chp_mean/chp_std

g_diff = g_dr - g_mean
chp_diff = chp_dr - chp_mean

for(i in 1:length(chp_diff))
{
  if(g_diff[i]>0)
  {
    g_diff[i]=0
  }
  if(chp_diff[i]>0)
  {
    chp_diff[i]=0
  }  
}

g_dd = sqrt(sum((g_diff^2)/length(g_diff)))
chp_dd = sqrt(sum((chp_diff^2)/length(chp_diff)))

g_sort=g_mean/g_dd
chp_sort=chp_mean/chp_dd

gdf = data.frame(category = c("mean","std","sharpe","dd", "sortino"), 
                  analytics = c(g_mean,g_std,g_sharpe, g_dd, g_sort))
gdf$stock = "g"         

chpdf = data.frame(category = c("mean","std","sharpe","dd", "sortino"), 
                   analytics = c(chp_mean,chp_std,chp_sharpe, chp_dd, chp_sort))
chpdf$stock = "chp" 

df = rbind(gdf,chpdf)

ggplot(df, aes(x=category, y=analytics, fill=stock)) + 
  geom_bar(position="dodge",stat="identity",color="black")+
  scale_fill_brewer(palette="Pastel1")
