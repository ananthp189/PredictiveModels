gpa <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/gpa.csv"))
head(gpa)
ggplot(gpa, aes(x=occasion, y=gpa))+geom_point(alpha=0.9, color="#7a0177")
gpa_lm <- lm(gpa ~ occasion, data=gpa)
summary(gpa_lm)
gpa_fit<- data.frame(x=c(1:6),
                     y=gpa_lm$coefficients[1]+ gpa_lm$coefficients[2]*c(1:6))
ggplot(gpa, aes(x=occasion, y=gpa)) + geom_point(alpha=0.9, color="#7a0177") +
  geom_line(data = gpa_fit, aes(x, y), col="#feb24c")

#There are a few issues with this modelling approach. Firstly, it assumes that we have 1200 independent
#observations from a model of the form y = β0 + β1 x + e where the errors e are independent N(0, σ2).
#This is not quite right, as we have multiple observations per student and it would be reasonable to expect
#that they are correlated. This is why the following plot might be a better depiction of the data than the
#scatterplot we saw earlier.

ggplot(gpa, aes(x=occasion, y=gpa))+ geom_point(alpha=0.9, color="#7a0177") +
  geom_path( aes(group=student), alpha=0.3, col="#ae017e")
library(lme4)

gpa_ranint <- lmer(gpa ~ occasion + (1|student), data=gpa)
summary(gpa_ranint)
round(confint(gpa_ranint, oldNames=FALSE),3)
round(head(ranef(gpa_ranint)$student),3)
gpa_ranint@beta[1]
round(head(gpa_ranint@beta[1]+ranef(gpa_ranint)$student),3)
round(head(coef(gpa_ranint)$student),3)

p1<-predict(gpa_ranint)
p2<-predict(gpa_lm)
predictions <- data.frame(x=c(1:6), s1_m=p1[1:6], s2_m=p1[7:12], l=p2[7:12],
                          gpa_1=gpa$gpa[1:6], gpa_2=gpa$gpa[7:12])
ggplot(predictions, aes(x=x, y=gpa_1)) +
  geom_point(color="#f768a1", alpha=0.8) +
  geom_line(aes(y=s1_m), colour="#f768a1", linetype=2)+
  geom_point(aes(x=x,y=gpa_2), colour="#7a0177",alpha=0.8) +
  geom_line(aes(y=s2_m), colour="#7a0177", linetype=2) +
  geom_line(aes(y=l), colour="#feb24c") + xlab("Occasion") + ylab("GPA")

#random slope and intercept
gpa_rc<-lmer(gpa ~ occasion + (1+occasion|student), data=gpa)
summary(gpa_rc)

#compare with uncorrelated linear model:
gpa_rc_uncor <- lmer(gpa ~ occasion + (1|student)+ (0+occasion|student),
                     data=gpa)
summary(gpa_rc_uncor)
round(confint(gpa_rc, oldNames=FALSE),3)
round(head(ranef(gpa_rc)$student),3) # random effects
round(head(coef(gpa_rc)$student),3) # random coefficients
p1<-predict(gpa_rc)
p2<-predict(gpa_lm)
predictions <- data.frame(x=c(1:6), s1_m=p1[1:6], s2_m=p1[7:12], s2_l=p2[7:12],
                          gpa_1=gpa$gpa[1:6], gpa_2=gpa$gpa[7:12] )
ggplot(predictions, aes(x=x, y=gpa_1)) +
  geom_point(color="#f768a1", alpha=0.8) +
  geom_line(aes(y=s1_m), colour="#f768a1", linetype=4)+
  geom_point(aes(x=x,y=gpa_2), colour="#7a0177",alpha=0.8) +
  geom_line(aes(y=s2_m), colour="#7a0177", linetype=4) +
  geom_line(aes(y=s2_l), colour="#feb24c") + xlab("Occasion") + ylab("GPA")


p1<-predict(gpa_rc)
p2<-predict(gpa_lm)
p3 <- predict(lm(gpa~occasion, data=gpa[1:6,]))
predictions <- data.frame(x=c(1:6), s1_m=p1[1:6], s1_l=p2[1:6], s1_i=p3[1:6],
                          gpa_1=gpa$gpa[1:6])
ggplot(predictions, aes(x=x, y=gpa_1)) +
  geom_point(color="#f768a1", alpha=0.8) +
  geom_line(aes(y=s1_m), colour="#f768a1", linetype=4)+
  geom_line(aes(y=s1_l), colour="#feb24c") +
  geom_line(aes(y=s1_i), colour="#fc4e2a") + xlab("Occasion") + ylab("GPA")


#Another Example
library(faraway)
data(pulp)
head(pulp)
library(lme4)
mmod <- lmer(bright ~ 1+(1|operator), data=pulp)
summary(mmod)
#interclass correlation coefficient :
0.06808/(0.06808+0.10625)
#so correlatoin between brightness measurements from the same operator is estimated at 0.39
ggplot(pulp, aes(x=bright, y=operator))+ geom_point(alpha=0.5, color="#ec7014") +
  geom_path(aes(group=operator), alpha=0.3, col="#fec44f")
#fixed model
lmod <- lm(bright ~ operator, data=pulp)
summary(lmod)


#CI
round(confint(mmod,method="boot",oldNames=FALSE),3)

#Finally we can check the linearity and normality assumptions through the usual residual plots, e.g.
# Diagnostic plots:
par(mfrow=c(1,2))
qqnorm(resid(mmod), main="")
qqline(resid(mmod))
plot(fitted(mmod), resid(mmod), xlab="Fitted", ylab="Residuals")
abline(0,0)

#Heirarchial modelling
#Fitting the model in R is as simple as adding these terms as fixed effects:
  gpa_multil<-lmer(gpa ~ occasion + sex + highgpa + (1+occasion|student), data=gpa)
summary(gpa_multil)
round(confint(gpa_multil, oldNames=FALSE),3)

