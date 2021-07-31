#college admissions
library(Stat2Data)
library(ggplot2)
data(MedGPA)

medgpa.plot <- ggplot(data = MedGPA, aes(y = Acceptance, x = GPA)) +
  geom_jitter(width =0, height =0.01, alpha =0.5, colour ="#984ea3")
medgpa.plot
#We can add the linear regression line for Acceptance as a function of GPA to the plot.
medgpa.plot + geom_smooth(method = "lm", se = FALSE,
                          fullrange = TRUE, colour = "#984ea3")
med.lm <- lm(Acceptance ~ GPA, data=MedGPA)
summary(med.lm)

#Use GLM default logit
med.glm <- glm(Acceptance ~ GPA, data = MedGPA, family = binomial)
summary(med.glm)

#vcov for covariance matrix from which standard error is derived
vcov(med.glm)
sqrt(diag(vcov(med.glm)))
#o obtain an approximate 95% confidence interval for the GPA coefficient, we take 훽ˆ푗 ± 1.96se(훽ˆ푗):
5.454-1.96*1.579
5.454+1.96*1.579

#The resulting interval is (2.36, 8.55), and since it does not include zero, we conclude that the GPA coefficient is significant