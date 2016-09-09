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
 
