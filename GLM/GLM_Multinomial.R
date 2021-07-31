dcars <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/cars.csv"))
dcars$response <- factor(dcars$response,
                         levels = c("no/little", "important", "very important"))
dcars$age <- factor(dcars$age,
                    levels = c("18-23", "24-40", "> 40"))

p1 <- ggplot(dcars, aes(x = age, y = frequency, fill = response)) +
  geom_bar(stat = "identity", position = "dodge" )+
  xlab("Age groups" ) + ylab("Frequency" ) +
  theme(legend.position = "none")
p2 <- ggplot(dcars, aes(x = sex, y = frequency, fill = response)) +
  geom_bar(stat = "identity", position = "dodge" )+
  xlab("Sex" ) + ylab("Frequency" ) +
scale_fill_discrete(name = "Response category") +
  theme(legend.position = "bottom")
p1
p2

library(nnet)
m1 <- multinom(response ~ age + sex, weight = frequency, data = dcars)
summary(m1)

nullm <- multinom(response ~ 1, data=dcars, weights=frequency)
summary(nullm)

qchisq(df=6, p=0.95)

m2 <- multinom(response ~ age * sex, weight = frequency, data = dcars)
summary(m2)
qchisq(df=4, p=0.95)

#Ordinal
library(MASS)
m4 <- polr(response ~ sex + age, data= dcars, weight = frequency, Hess=TRUE)
summary(m4)

#Nominal Regression in Classification tool
m.iris <- multinom(Species ~ Sepal.Length, data=iris)
summary(m.iris)
#fitted probabilities
head(round(fitted(m.iris),3))
#predicted category
head(predict(m.iris))
#check accuracy of prediction
sum(iris$Species!=predict(m.iris))
