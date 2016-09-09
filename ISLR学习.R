x=rnorm (100)
y=rnorm (100)
plot(x,y)
plot(x,y,xlab=" this is the x-axis",ylab=" this is the y-axis",
       main=" Plot of X vs Y")
x=seq(1,10)

y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
contour (x,y,f)
contour (x,y,f,nlevels =45, add=T)
fa=(f-t(f))/2
contour (x,y,fa,nlevels =15)

library(ISLR)
Auto
plot(Auto$horsepower ,Auto$mpg)
attach(Auto)
identify (horsepower ,mpg ,name)

library(MASS)
Boston


