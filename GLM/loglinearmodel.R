#food poisioning contingency table
fp <- data.frame(potato=rep(c("yes","yes","no","no"),2),
                 crab=rep(c("yes","no"),4),
                 sick=c(rep("no",4),rep("yes",4)),
                 freq = c(80,24,31,23,120,22,4,0))

summary(glm(freq ~ crab*potato, family=poisson, data=fp))
glm(freq ~ crab*potato+sick, family=poisson, data=fp)
deviance(glm(freq ~ crab*potato+sick, family=poisson, data=fp))
deviance(glm(freq ~ crab*potato+sick*crab, family=poisson, data=fp))
l0 <-glm(freq ~ crab*potato+sick*potato, family=poisson, data=fp)
deviance(l0)
l1 <- glm(freq ~ crab*potato + crab*sick+potato*sick, family=poisson, data=fp)
summary(l1)
#We see that the interaction term between sick and crab has a 푝-value of just above 0.05, suggesting that
#the relationship between crab salad and sickness is marginally significant. The potato salad has a much
#smaller 푝-value and looks like the most likely source of the outbreak, although there is some indication
#that the crab salad may have something to do with it, too.

cbind(fp$freq, round(fitted(l0),2), round(fitted(l1),2))
qchisq(df=1,p=0.95)
anova(l0,l1)
