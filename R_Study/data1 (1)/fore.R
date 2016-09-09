"fore" <- function(m1,rt,orig,h,xre=NULL){
# m1: is a model object
# orig: is the forecast origin
# rt: the time series
# xre: the independent variables
# h: forecast horizon
#
# It requires the library timeSeries. 
regor=c(m1$arma[1],m1$arma[6],m1$arma[2])
seaor=list(order=c(m1$arma[3],m1$arma[7],m1$arma[4]),period=m1$arma[5])
T=length(rt)
if(orig > T)orig=T
x=rt[1:orig]
xr=xre[1:orig]
mm=arima(x,order=regor,seasonal=seaor,xreg=xr)
nxreg=xre[(orig+1):(orig+h)]
p1=predict(mm,h,newxreg=nxreg)
pred=p1$pred
se=p1$se
print(pred)
print(se)
fore <- list(origin=orig,pred=pred,se=se)
}