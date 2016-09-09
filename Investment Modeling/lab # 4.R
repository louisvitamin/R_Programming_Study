getSymbols(c("nflx","aapl","axp","unh"))

aapl_dr = dailyReturn(AAPL)

axp_dr = dailyReturn(AXP)

nflx_dr = dailyReturn(NFLX)

unh_dr = dailyReturn(UNH)

#aapl_dr=last(dailyReturn(AAPL),"30 days")

#multiple ways to create matrix
#mm1 = rbind(c(1,2,3),3:5,rep(2,3))
#mm2 = cbind(c(1,2,3),3:5,rep(2,3))
#mm3 = matrix(1:9,3,3)
#mm4 = matrix(1:9,3,3,byrow = T)

mm = matrix(0,4,30)
mm[1,]=last(aapl_dr,"30 days")
mm[2,]=last(axp_dr,"30 days")
mm[3,]=last(nflx_dr,"30 days")
mm[4,]=last(unh_dr,"30 days")

result = matrix(0,4,7)
for(i in 1:4)
{
  result[i,1]= mean(mm[i,])
  result[i,2]= sd(mm[i,])
  result[i,3]= result[i,1]/result[i,2]
  ret = mm[i,]-result[i,1]
  ret = ifelse(ret>0,0,ret)
  result[i,4] = sqrt(sum(ret^2)/30)
  result[i,5] = result[i,1]/result[i,4]
  result[i,6]= max(mm[i,])
  result[i,7]= min(mm[i,])
}

df = data.frame(category=c("mean","std","sharpe","dd","sortino","max","min"),factors =result[1,], stock = rep("aapl",7))
df = rbind(df,data.frame(category=c("mean","std","sharpe","dd","sortino","max","min"),factors =result[2,], stock = rep("axp",7)))
df = rbind(df,data.frame(category=c("mean","std","sharpe","dd","sortino","max","min"),factors =result[3,], stock = rep("nflx",7)))
df = rbind(df,data.frame(category=c("mean","std","sharpe","dd","sortino","max","min"),factors =result[4,], stock = rep("unh",7)))

ggplot(df,aes(x=category,y=factors,fill=stock)) + 
  geom_bar(stat="identity",position = "dodge", color = "pink") + 
  scale_fill_manual(values = c("purple","white","orange","yellow"))

mm = floor(mm*1000)/1000

freq = table(mm[1,])
prob_apple = data.frame(freq/sum(freq))
names(prob_apple) = c("ret","prob")
prob_apple$dr = as.numeric(levels(prob_apple$ret))

freq = table(mm[2,])
prob_axp = data.frame(freq/sum(freq))
names(prob_axp) = c("ret","prob")
prob_axp$dr = as.numeric(levels(prob_axp$ret))

freq = table(mm[3,])
prob_nflx = data.frame(freq/sum(freq))
names(prob_nflx) = c("ret","prob")
prob_nflx$dr = as.numeric(levels(prob_nflx$ret))

freq = table(mm[4,])
prob_unh = data.frame(freq/sum(freq))
names(prob_unh) = c("ret","prob")
prob_unh$dr = as.numeric(levels(prob_unh$ret))

prob_mm = matrix(0,4,5)

prob_mm[1,1]=sum(subset(prob_apple,dr< -.02,prob))
prob_mm[1,2]=sum(subset(prob_apple,dr>= -.02 & dr< 0,prob))
prob_mm[1,3]=sum(subset(prob_apple,dr>= 0 & dr< .02,prob))
prob_mm[1,4]=sum(subset(prob_apple,dr>= .02 & dr< .05,prob))
prob_mm[1,5]=sum(prob_apple[prob_apple$dr>= .05,2])

prob_mm[2,1]=sum(prob_axp[prob_axp$dr< -.02,2])
prob_mm[2,2]=sum(prob_axp[prob_axp$dr>= -.02 & prob_axp$dr< 0,2])
prob_mm[2,3]=sum(prob_axp[prob_axp$dr>= 0 & prob_axp$dr< .02,2])
prob_mm[2,4]=sum(prob_axp[prob_axp$dr>= .02 & prob_axp$dr< .05,2])
prob_mm[2,5]=sum(prob_axp[prob_axp$dr>= .05,2])

prob_mm[3,1]=sum(prob_nflx[prob_nflx$dr< -.02,2])
prob_mm[3,2]=sum(prob_nflx[prob_nflx$dr>= -.02 & prob_nflx$dr< 0,2])
prob_mm[3,3]=sum(prob_nflx[prob_nflx$dr>= 0 & prob_nflx$dr< .02,2])
prob_mm[3,4]=sum(prob_nflx[prob_nflx$dr>= .02 & prob_nflx$dr< .05,2])
prob_mm[3,5]=sum(prob_nflx[prob_nflx$dr>= .05,2])

prob_mm[4,1]=sum(prob_unh[prob_unh$dr< -.02,2])
prob_mm[4,2]=sum(prob_unh[prob_unh$dr>= -.02 & prob_unh$dr< 0,2])
prob_mm[4,3]=sum(prob_unh[prob_unh$dr>= 0 & prob_unh$dr< .02,2])
prob_mm[4,4]=sum(prob_unh[prob_unh$dr>= .02 & prob_unh$dr< .05,2])
prob_mm[4,5]=sum(prob_unh[prob_unh$dr>= .05,2])

probdf = data.frame(category=c("range 1","range 2","range 3","range 4","range 5"),factors =prob_mm[1,], stock = rep("aapl",5))
probdf = rbind(probdf,data.frame(category=c("range 1","range 2","range 3","range 4","range 5"),factors =prob_mm[2,], stock = rep("axp",5)))
probdf = rbind(probdf,data.frame(category=c("range 1","range 2","range 3","range 4","range 5"),factors =prob_mm[3,], stock = rep("nflx",5)))
probdf = rbind(probdf,data.frame(category=c("range 1","range 2","range 3","range 4","range 5"),factors =prob_mm[4,], stock = rep("unh",5)))

ggplot(probdf,aes(x=category,y=factors,fill=stock)) + 
  geom_bar(stat="identity",position = "dodge", color = "red") + 
  scale_fill_manual(values = c("blue","white","green","yellow"))
