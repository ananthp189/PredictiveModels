library(mlmRev)
head(Contraception)
library(lattice)
lattice.options(default.theme = function() standard.theme())
print(xyplot(ifelse(use == "Y", 1, 0) ~ age|urban, Contraception,
             groups = livch, type = c("g", "smooth"),
             auto.key = list(space = "top", points = FALSE,
                             lines = TRUE, columns = 4),
             ylab = "Proportion", xlab = "Centered age"))

Contraception <- within(Contraception,
                        ch <- factor(livch != 0, labels = c("N", "Y")))
head(Contraception)

#variable ch used instead of livch
print(xyplot(ifelse(use == "Y", 1, 0) ~ age|urban, Contraception,
             groups = ch, type = c("g", "smooth"),
             auto.key = list(space = "top", points = FALSE,
                             lines = TRUE, columns = 2),
             ylab = "Proportion", xlab = "Centered age"))
#Based on what we’ve seen in the exploratory plots we could fit a GLM with a binary response for use, the
#logit link and explanatory variables for urban, age ch, as well as interaction between age and ch and a
#quadratic term in age.
glm1 <- glm(use ~ 1 + urban + age*ch + I(age^2), data=Contraception,
            family=binomial)
summary(glm1)
#This model does not take into account the possibility that women from the same district may have correlated
#observations. One way to take that potential correlation into account is to fit a GLMM with district
#included as a random effect. This can be done using glmer() from library(lme4) 

library(lme4)
glmm1 <- glmer(use ~ 1 + urban + age*ch + I(age^2) + (1|district),
               data=Contraception, family=binomial)
summary(glmm1, corr=FALSE)

#GEE
library(faraway)
head(epilepsy)
tdata <- data.frame(epilepsy[epilepsy$expind==1,], week=rep(seq(2,8, by=2), 59))
ggplot(tdata, aes(x=week, y=sqrt(seizures), color=factor(treat), group=id)) +
  geom_point(alpha=0.5) + geom_path(aes(linetype=factor(treat))) +
  scale_color_manual(values=c("#fd8d3c", "#41ab5d"),
                     labels=c("Placebo","Treatment"), name="Treatment") +
  scale_linetype_manual(values=c(1,6),
                        labels=c("Placebo", "Treatment"), name="Treatment")


# mean seizures (per week) during experiment
y <- matrix( epilepsy$seizures, nrow=5)
exp <- sqrt( apply(y[-1,], 2, mean)/2)
# mean seizures (per week) during baseline period
bas <- sqrt(epilepsy$seizures[epilepsy$expind==0]/8)
d <- data.frame(exp, bas, t=epilepsy$treat[5*(1:59)]+2)
ggplot(d, aes(x=bas, y=exp, color=factor(t))) + geom_point(alpha=0.8) +
  scale_colour_manual(values=c("#fd8d3c", "#41ab5d"),labels=c("Placebo","Treatment"), name="Treatment") +
  xlab("sqrt(Baseline)") + ylab("sqrt(Experiment)") +
  geom_abline(slope=1, intercept=0)
#49 is outlier, we can try fitting all the available gee methods
library(gee)
g1 <- gee(seizures ~ offset(log(timeadj))+expind+treat+I(expind*treat), id,
          family=poisson, corstr="independence", data=epilepsy, subset=(id!=49))
summary(g1)

g2 <- gee(seizures ~ offset(log(timeadj))+expind+treat+I(expind*treat), id,
          family=poisson, corstr="exchangeable", data=epilepsy, subset=(id!=49))
summary(g2)

g3 <- gee(seizures ~ offset(log(timeadj))+expind+treat+I(expind*treat), id,
          family=poisson, corstr="AR-M", Mv=1, data=epilepsy, subset=(id!=49))
summary(g3)
g4 <- gee(seizures ~ offset(log(timeadj))+expind+treat+I(expind*treat), id,
          family=poisson, corstr="unstructured", data=epilepsy, subset=(id!=49))
