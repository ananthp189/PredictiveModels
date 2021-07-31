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

#Finally, a third choice of link that we could consider is the complementary log-log link, with the GLM equation given by
m3 <- glm(cbind(killed, number-killed) ~ dose, family = binomial(link = 'cloglog'),
          data=beetles)

summary(m3)

#plot fitted models
beet_p <- data.frame(beetles = beetles,
                     logit = fitted(m1),
                     probit = fitted(m2),
                     cloglog = fitted(m3))
p2 <- ggplot(beet_p, aes(x = beetles$dose, y = beetles$propkilled)) +
  geom_point() + xlab("Dose") + ylab("Proportion killed") +
  geom_line(aes(x = beetles$dose, y = logit, colour = "Logit")) +
  geom_line(aes(x = beetles$dose, y = probit, colour = "Probit")) +
  geom_line(aes(x = beetles$dose, y = cloglog, colour = "C log-log")) +
  guides(colour = guide_legend("Method"))
p2

library(faraway)
head(orings)
#logit link
lmod <- glm(cbind(damage, 6-damage) ~ temp, family=binomial, data=orings)
summary(lmod)
#Probit link:
  pmod <- glm(cbind(damage, 6-damage) ~ temp, family=binomial(link="probit"),
              data=orings)
summary(pmod)
#Complementary log-log link:
  cmod <- glm(cbind(damage, 6-damage) ~ temp, family=binomial(link="cloglog"), data=orings)
summary(cmod)


pred1 <- predict(lmod, newdata=data.frame(temp=seq(25,85,le=23)), type="response")
pred2 <- predict(pmod, newdata=data.frame(temp=seq(25,85,le=23)), type="response")
pred3 <- predict(cmod, newdata=data.frame(temp=seq(25,85,le=23)), type="response")
pred <- data.frame(logit = pred1, probit= pred2, cloglog=pred3, px = seq(25,85,le=23),orings)
p1.1 <- ggplot(pred, aes(x=orings$temp, y= orings$damage/6)) +
  geom_point(size = 1)+ xlim (c(25,85)) + ylim(c(0,1)) +
  xlab ("Temperature (F)") + ylab("Probability of damage") +
  geom_line(aes(x = px, y = logit, color = "Logit")) +
  geom_line(aes(x = px, y = probit, color = "Probit"))+
  geom_line(aes(x = px, y = cloglog, color = "Complementary log-log"))
p1.1
