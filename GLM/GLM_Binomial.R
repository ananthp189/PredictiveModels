#beatles vs poison dose
beetles <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/beetles.csv"))
beetles

beetles$propkilled <- beetles$killed / beetles$number
p1 <- ggplot(beetles, aes(x = dose, y = propkilled))+
  geom_point(size = 1) + xlab ("Dose") + ylab ("Proportion killed")
p1

m1 <- glm(cbind(killed, number-killed) ~ dose, family = binomial(link = 'logit'),
          data=beetles)
summary(m1)

#walds test for hyposthesis testin , z parameter compared with normal dist. This results in probability of null hypothesis being very low and so dose is significant here.

#The same conclusion can be reached if we compare the residual deviance (this is the deviance for the
#model with dose included as a predictor) and the null deviance (this is the deviance for the model with just an intercept term in it). The difference in deviances is 284.202−11.232 = 272.97 which is much larger
#than the 95th percentile of a chisq dist(7 − 6) = chisquare(1) distribution
qchisq(df=1, p=0.95)

#for a good model, null deviance is 
qchisq(df=6, p=0.95)
#compare to d0 here

p.hat <- predict(m1, type="response")
fitted <- beetles$number * p.hat
cbind(beetles$killed, round(fitted,2))

#Probit for dose response 
#We can fit the probit model by changing the link option in the glm function to probit:
  m2 <- glm(cbind(killed, number-killed) ~ dose, family = binomial(link = 'probit'),
            data=beetles)
summary(m2)
