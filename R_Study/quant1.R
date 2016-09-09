library(quantmod)
getSymbols("IBM")

getSymbols("IBM",from='2012-01-01',to='2014-09-01',src="yahoo")
dim(IBM)

head(IBM)

tail(IBM)

chartSeries(IBM)

chartSeries(IBM, subset='last 4 months')
chartSeries(IBM, subset='2013-01-01::2014-01-01')
chartSeries(IBM,theme="white")


getSymbols("UNRATE",src="FRED") ## download unemployment rates from FRED

head(UNRATE)

chartSeries(UNRATE,theme="white")

### now look at log returns ###

IBM.rtn=diff(log(IBM$IBM.Adjusted))

chartSeries(IBM.rtn, theme="white")

getSymbols("DEXUSEU",src="FRED") ## http://research.stlouisfed.org/fred2/categories/94?t=&at=exchange+rate&ob=pv&od=desc

head(DEXUSEU)

USEU.rtn=diff(log(DEXUSEU$DEXUSEU))

chartSeries(USEU.rtn, theme="white")

chartSeries(DEXUSEU, theme="white")

chartSeries(IBM,TA=NULL)   #no volume
chartSeries(IBM,TA=c(addVo(),addBBands()))  #add volume and Bollinger Bands from TTR

addMACD()   #  add MACD indicator to current chart
