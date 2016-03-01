# In this file we use the likelihood ratio test using the fact that the
# chi-square distribution approximates the distribution of -2 ln Lambda
# to test the hypothesis that the population average income is a given
# number (say mu0=$41000.) The data is in the file "income.txt" that
# should be accessible.


# Example 2: we wish to estimate the proportion of emails that are
# non-spam. In one day we received 20 e-mails and 17 were non-spam. Thus
# the maximum likelihood estimate is 17/20=85% (see lecture notes).  The 
# question is that does this give us information? Without any testing,
# and having no experience at all, all we could say is that ratio of
# non-spam mail is 50%. So the hypothesis testing is as follows:
#  Null H0: the ratio of non-spam is 17/20
#  Alt: H1: The ratio is 1/2
#
# The log ratio is computed is as in the following function:

binaryLogRatio<-function(k,n){
	return(-2*(-k*log(k/n)-(n-k)*log(1-k/n)-n*log(2)))
}

cat("A coin is tossed 19 times and 12 times it turns up heads. So the
maximum likelihood estimate for prob. of heads is 12/19. The likelihood
ratio test using chiSq(1) distribution gives the p-value:\n")
k<-12
n<-19
p1<-1-pchisq(binaryLogRatio(k,n), df=1)
cat("\nLikelihoood ratio p-vlaue = ", p1, "\n")
readline()
cat("\nWe can also apply the more specialized binomial test, which gives
the p-value:\n")
p2<- binom.test(k,n)
cat("\nbinomial p-value= \n")
print(p2)
readline()

cat("You can play with other values of k and n to see what the exact
binomial test, and the likelihood tests will generate. Use 0 for k to
exit.\n")
k <-as.numeric(readline("\nWrite k\n"))
while(k>0){
	# Note: when readline() reads something it interprets as a
	# string, so you must turn it into a number by using
	# as.numeric() function:
	n <-as.numeric(readline("\nWrite n\n"))
	cat("\nk = ", k, "n = ", n, "\n")
	p1<-1-pchisq(binaryLogRatio(k,n), df=1)
	cat("\nLikelihoood ratio p-vlaue = ", p1, "\n")
	readline()
	cat("\nWe can also apply the more specialized binomial test, 
	which gives the p-value:\n")
	p2<- binom.test(k,n)
	cat("\nbinomial p-value= \n")
	print(p2)
	readline()
	k <-as.numeric(readline("\nWrite k\n"))
}

