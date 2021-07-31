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

plot_model(mod.titan,type="pred",terms=c("age","passenger.class", "gender"))

#roc
library(ROCR)
titanic$Prid <- predict(mod.titan, titanic, type="response")
score <- prediction(titanic$Prid,titanic$survived)
perf <- performance(score,"tpr","fpr")
auc <- performance(score,"auc")
perfd <- data.frame(x= perf@x.values[1][[1]], y=perf@y.values[1][[1]])
p4<- ggplot(perfd, aes(x= x, y=y)) + geom_line() +
  xlab("False positive rate") + ylab("True positive rate") +
  ggtitle(paste("Area under the curve:", round(auc@y.values[[1]], 3)))
p4
cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]],
                      tpr=perf@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.2))
