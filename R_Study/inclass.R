4+6

x<-6
y<-4
z<-x+y

z

ls()

sqrt(16)

rm(x,y)

z<-c(5,9,1,0)

x<-c(5,9)
y<-c(1,0)
z<-c(x,y)

x<-1:10

seq(1,9,by=2)

seq(8,20,length=6)

x<-seq(1,10)

rep(0,100)

rep(1:3,6)
rep(1:3,c(6,6,6))
rep(1:3,rep(6,3))

x<-c(6,8,9)
y<-c(1,2,4)
x+y
x*y
x<-c(6,8,9)
x+2

x<-c(7.5,8.2,3.1,5.6,8.2,9.3,6.5,7.0,9.3,1.2,14.5,6.2)
mean(x)
var(x)
summary(x)
x[1:6]
x[7:12]
summary(x[1:6])
summary(x[7:12])

x[c(2,4,9)]
x[-(1:6)]

x<-c(5,7,9)
y<-c(6,3,4)
z<-cbind(x,y)
z

dim(z)
rbind(z,z)
z<-matrix(c(5,7,9,6,3,4),nrow=3)
z
z<-matrix(c(5,7,9,6,3,4),nr=3,byrow=T)
z

y<-matrix(c(1,3,0,9,5,-1),nrow=3,byrow=T)
y

y+z

y*z
x<-matrix(c(3,4,-2,6),nrow=2,byrow=T)
x
y%*%x

t(z)

solve(x)

z[1,1]
z[c(2,3),2]
z[,2]
z[1:2,]

data()
data(trees)
trees[1:5,]
attach(trees)
mean(Height)
mean(trees[,2])
trees$Height
apply(trees,2,mean)

x
dnorm(x,3,2)
dnorm(5,3,2)
x<-seq(-5,10,by=.1)
dnorm(x,3,2)
rnorm(100,3,2)

###rt, rpois

par(mfrow=c(2,2))
par(mfrow=c(2,2))
hist(Height)
boxplot(Height)
hist(Volume)
boxplot(Volume)
par(mfrow=c(1,1))
plot(Height,Volume)

pairs(trees)
data(nhtemp)
plot(nhtemp)
data(faithful)
plot(faithful)
data(HairEyeColor)
plot(HairEyeColor)


sd <- function(x) sqrt(var(x))
x<-c(9,5,2,3,7)
sd(x)

several.plots<-function(x){
  par(mfrow=c(1,1))
  hist(x[,1])
  hist(x[,2])
  plot(x[,1],x[,2])
  par(mfrow=c(1,1))
  apply(x,2,summary)
}

several.plots(faithful)


