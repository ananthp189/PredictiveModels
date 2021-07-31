#Separation occurs in logistic regression models when a hyperplane exists that perfectly separates responses from
#non-responses. In that case the MLE 훽ˆ does not exist. Consider the following illustration:
  dat<- read.table(url("http://www.stats.gla.ac.uk/~tereza/rp/separation.txt"), header=TRUE)
dat
mod.sep <- glm(y~x1+x2, family="binomial", data=dat)
summary(mod.sep)
#Notice the large standard errors, large 푝-values and zero deviance!
