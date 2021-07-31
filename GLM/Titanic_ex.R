library(ggplot2)
library(sjPlot)

titanic <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/titanic.csv"))
titanic$passenger.class <- factor(titanic$passenger.class)
head(titanic)

plot_xtab(titanic$survived,titanic$gender, show.values = FALSE,
          show.total = FALSE, axis.labels = c("Died", "Survived"),
          legend.title = "Gender")



mod.titan <- glm(survived~gender + passenger.class + age,
                 family=binomial(link="logit"), data=titanic)
summary(mod.titan)

plot_model(mod.titan, show.values=TRUE)
