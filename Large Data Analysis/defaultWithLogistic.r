# We study logistic regression to estimate the probability of default on
# loans based on several variables. The data are from the ISLR package.
# In R use Packages->Install packages and install the ISLR package. Next
# in R use packages -> load package and load the ISLR package. The ISLR
# package contains a set of data from the text: "Introduction to
# Statistical Learning with Applications in R" by G. James, D. Witten,
# T. Hatie and R. Tibshirani, Springer 2013.  We are interested in the
# Default data from:

#First some useful functions:

# the top function returns the first k rows of the data frame given to
# it. If k is not specified it return the first 10 rows.

top <- function (frame, k=10){ return(frame[1:k,])}


library("ISLR")
cat("Top 10 rows of Default:\n")

print(top(Default))
readline()

# We now make some useful plots before applying logistic regression
# First, to look at the connection between default and income:
cat("\n Hit Enter to see box plot income ~ default:\n")
readline()
boxplot(income~default, data=Default)
readline()

# Now, to look at the connection between default and balance:
cat("\n Hit Enter to see box plot balance ~ default:\n")
readline()
boxplot(balance~default, data=Default)
readline()

# We can now plot the combinations of income and balance for default (in
# red) and no default (in blue)
cat("\n Hit Enter to see the scatterplot of income vs balance for
defaulted cases:\n")
readline()
plot(Default[Default$default=="Yes","income"],
		Default[Default$default=="Yes", "balance"], col="red",pch=20)
readline()
cat("\nHit Enter to add the non-default points:\n")
readline()
points(Default[Default$default=="No","income"],
		Default[Default$default=="No", "balance"], col="blue",pch=20)
readline()

# Also, compare default for students and non-students
cat("\nHit Enter to see the default ~ student plot:\n")
readline()
plot(default ~ student, data=Default)
readline()

# Now build the logistic regression model by adding varaibles one at a
# time:

model1 <- glm(default ~ income, family=binomial, data=Default)
print(summary(model1))
readline()
# the R anova function, when applied to logistic regression, does
# "analysis of Deviance" (So anova is not a good name for it, actually).
# setting "test="LRT" instructs anova to perform the maximum likelihood
# test. Note that if you supply one model, eg. anova(model3, test="LRT")
# then anova tests with respect to all variables one by one in order of
# addition to the model. But if you only want to compare two models (the
# reduced one is say model 2 and the complete one mode3) then you write
# anova(model2, model3, test="LRT")


#1 - pchisq(null deviance - residual deviance, df =1)  => p-value 

cat("\nsignificance of the model1 p-value = \n")
print(anova(model1,test="LRT"))    #p-value of the whole model
readline()

model2 <- glm(default ~ income+student, family=binomial, data=Default)
print(summary(model2))
readline()
cat("\nsignificance of the model2 p-value = \n")
print(anova(model2,test="LRT"))
readline()
cat("\nsignificance of the model2 over model1 p-value = \n")
print(anova(model1,model2,test="LRT"))     
readline()

model3 <- glm(default ~ income+student+balance, family=binomial, data=Default)
print(summary(model3))
readline()
cat("\nsignificance of the model3 p-value = \n")
print(anova(model3,test="LRT"))
readline()

# We now use our model to predict probability of default for various
# individuals:

# We create  three new data points: Two students with incomes 11000 and
# 13000 and debts 800, and 1000 respectively, and a non-student with
# income 51000 and debt of 2500:
cat("\nHit Enter to see the new data points:\n")
readline()
newDefault <- data.frame(income=c(11000, 13000,51000), 
                         student=c("Yes", "Yes","No"), 
                         balance=c(800, 1000, 2500))
print(newDefault)
readline()
cat("\nHit Enter to see the for the following data:\n")
print(predict(model3,newDefault,type="response"))

# Now let users enter their own data:
repeat{
    cat("\nHit Enter to input your own data, enter -1 for income to exit:\n")
    newInc<-readline("\nEnter new income (-1 to exit)\n")
    if (newInc < 0) break
    newBal<-readline("\nEnter new balance\n")
    newStudent <- readline("\nStudent? Enter 'y' for yes and 'n' for no\n")
    cat("\nChance of default:\n")
    print(predict(model3,data.frame(income=as.numeric(newInc),
				student=if(newStudent=="y")"Yes" else "No",
				balance=as.numeric(newBal)), type="response"))
}
    

