###generating plots in lecture note 1.

x=read.table("d-aapl0009.txt",header=T)  # Load Apple stock returns
dim(x)
x[1,]
y=ts(x[,2],frequency=252,start=c(2000,1)) # Create a time-series object in R.
plot(y,type='l',xlab='year',ylab='rtn')
title(main='Apple daily return: 2000 to 2009')

 par(mfcol=c(2,1))
 hist(y,main='Returns',nclass=50)
 d1=density(y)
 plot(d1$x,d1$y,xlab='returns',ylab='den',type='l')

 x=read.table("m-sp2609.txt",header=T)
 dim(x)
[1] 1008    3
 tdx=c(1:1008)/12+1926
 par(mfcol=c(2,1))
 plot(tdx,x[,3],xlab='year',ylab='index',type='l')
 plot(tdx,x[,2],xlab='year',ylab='return',type='l')

 x=read.table("m-tb3ms.txt",header=T)
 dim(x)
 y=read.table("m-tb6ms.txt",header=T)
 dim(y)
 dim(x)

 int=cbind(x[300:914,4],y[,4])
 tdx=(c(1:615)+10)/12+1958
 par(mfcol=c(1,1))
 max(int)
 plot(tdx,int[,1],xlab='year',ylab='rate',type='l',ylim=c(0,16.5))
 lines(tdx,int[,2],lty=2)
 
  par(mfcol=c(2,1))
 max(int)
 plot(tdx,int[,1],xlab='year',ylab='3m rate',type='l',ylim=c(0,16.5))
 plot(tdx,int[,2],xlab='year',ylab='6m rate',type='l',ylim=c(0,16.5))

 plot(tdx,int[,2]-int[,1],xlab='year',ylab='spread',type='l')
 abline(h=c(0))

 x=read.table("q-ko-earns8309.txt",header=T)
 dim(x)
 tdx=c(1:107)/4+1983
 plot(tdx,x[,3],xlab='year',ylab='earnings',type='l')
 title(main='EPS of Coca Cola: 1983-2009')
 points(tdx,x[,3])
 
 y=read.table("d-useu.txt",header=T)
 dim(y)
[1] 2807    4

 tdx=c(1:2807)/252+1999
 plot(tdx,y[,4],xlab='year',ylab='eu',type='l')
 title(main='Dollars per Euro')

 r=diff(log(y[,4]))
 plot(tdx[2:2807],r,xlab='year',ylab='rtn',type='l')
 title(main='ln-rtn: US-EU')

 hist(r,main='useu: ln-rtn',nclass=50)



### Galton height example

xx=matrix(scan(file="Galton_parent_child_height.txt"),ncol=2,byrow=T)
dim(xx)
 y=xx[,1] # child 
 x=xx[,2] # mid-parent 
 out=lm(y~x) 
  summary.lm(out)
  

  plot(x,y,ylab='child',xlab='mid-parent') 
  lines(x,x) 
  lines(x,out$fit) 
  title('Galton LS fit')
  plot(x,out$res, ylab='residual',xlab='mid-parent') 
  lines(x,x*0)
  title('Galton residual plot')
  
  ### CAPM example 
  
#par(mfrow=c(1,2))
 xx=read.table(file="SP500_and_3mTCM.txt",header=TRUE)
 yy=read.table(file="m_logret_10stocks.txt",header=TRUE)
 marketrtn=xx[,"sp500"]-0.01*xx[,"X3mTCM"]
 aaplrtn=yy[,"AAPL"]-0.01*xx[,"X3mTCM"]
 plot(marketrtn,aaplrtn,xlab='SP500',ylab='AAPL',ylim=c(-0.45,0.22))
 title('excess return: AAPL vs SP500') 
 out1=lm(aaplrtn~marketrtn)
 lines(marketrtn,out1$fit) 
 summary.lm(out1) 
 
 
 ### Fama & French example
 
ff1=read.table(file="F-F_Research_Data_Factors.txt",header=TRUE)
ff2=read.table(file="F-F_Momentum_Factor.txt",header=TRUE) 
pp=read.table(file="25_Portfolios_5x5_vw.txt",header=TRUE) 
tt=517:876 ## 1970.1 to 1999.12
y=pp[tt,8]-ff1[tt,5]	##excess return of portfolio 22
xx=as.matrix(cbind(ff1[tt,2:4],ff2[tt,2])) ## form X matrix
out1=lm(y~xx)
summary.lm(out1)