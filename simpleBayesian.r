# Known distribution and priors

# prior: Pr[fake]=0.25, Pr[real]=0.75
# Distributions:  Pr[heads|fake] = 1/3  Pr[heads|real]=1/2
# toss the coin n=20 times, observed 7 heads. 
# Compute p0(7)=Pr[fake|x=7 (in n=20 tosses)]

cat("\nwhen a coin is tossed 20 times and 7 heads come up then the 
 Probability that this coin is fake is:\n")

# The binomial distribution says, if an outcome is "success" with
# probability p, and is repeated n times, what is the probability of
# observing x successes? The answer is
#          
#                                n!      k     (n-k)
#        Pr[x success; p,n] = --------  p  (1-p)
#                             k!(n-k)!
#
# Note the function choose(n,k) = n!/(k!(n-k)!)
# Directly plugging in to the formula for binomial distribution:
cat("\nDirectly computing from our own formula:\n")
print(choose(20, 7)*2^13*.25/3^20/(choose(20, 7)*2^13/3^20*0.25+choose(20,
		7)/2^20*.75))

# Or we could write a quick function for it, using dbinom function in R:
cat("\nOr using dbinom function:\n")
p <- function(x,n,p1,p2,pi1){
	dbinom(x,n,p1)*pi1/(dbinom(x,n,p1)*pi1+dbinom(x,n,p2)*(1-pi1))}

print(p(7,20,1/3,1/2,1/4))





# prior: Pr[female]=0.5, Pr[Male]=0.5
# Distributions: height of women ~ normal(mu=64in, sigma=3.5in)
#                height of me    ~ normal(mu=69in, sigma=3.5in)
# Compute Pr[female | height is 66in]
# We can use dnorm function to compute the pdf of normal distribution.
# Use ?dnorm to see documentation


pf <- function (x,mu0, sigma0, mu1, sigma1, pi0){
		return(dnorm(x,mu0,sigma0)*pi0/
                         (dnorm(x,mu0,sigma0)*pi0+dnorm(x,mu1,sigma1)*(1-pi0))
                      )}
cat("\nThe pf function can compute probability of each class if the
single variable is normally distributed:\n")
print(pf)
readline()

cat("Using normal distribution, Prob{female| height=66] =\n")
x<-66
mu0<-64
mu1<-69
sigma<-3.5
pi0<-0.5
print(pf(x,mu0,sigma, mu1,sigma,pi0))

