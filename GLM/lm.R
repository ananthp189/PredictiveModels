#linear Model 
library(ggplot2)
bollywood <-read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/bollywood_boxoffice.csv"))
head(bollywood)
b.plot <- ggplot(data = bollywood, aes(y = Gross, x = Budget)) +
  geom_point(col = "#66a61e") +
  scale_x_continuous("Budget (crore)") + scale_y_continuous("Gross (crore)")
b.plot 
#log transform
b.plot.l <- ggplot(data = bollywood, aes(y = log10(Gross), x = log10(Budget))) +
  geom_point(col = "#1b9e77") +
  scale_x_continuous("log(Budget) (crore)") +
  scale_y_continuous("log(Gross) (crore)")
b.plot.l
#linear model
bol.lm <- lm(log10(Gross) ~ log10(Budget), data = bollywood)
summary(bol.lm)
#visualize by fitting regression line
b.plot.lm <- ggplot(data = bollywood, aes(y = log10(Gross), x = log10(Budget))) +
  geom_point(col ="#1b9e77") +
  scale_x_continuous("log(Budget) (crore)") +
scale_y_continuous("log(Gross) (crore)") +
  geom_smooth(method = lm, colour="#e7298a", se=FALSE)
b.plot.lm
