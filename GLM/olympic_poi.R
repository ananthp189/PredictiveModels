olympics0 <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/OlympicMedals2012.csv"))
olympics <- data.frame(country = olympics0$Country, medals = olympics0$Medals,
                       population = olympics0$Population,
                       gold = olympics0$Gold.Medal,
                       GDP = olympics0$GDP..US.Billion)
olympics$GDPpercapita <- olympics$GDP * 10^6 / olympics$population
head(olympics)
#log transform both
p1 <- ggplot(olympics, aes(x=log(population), y=log(medals))) +
  geom_point(col="#f46d43")
p2 <- ggplot(olympics, aes(x=log(GDPpercapita), y=log(medals))) +
  geom_point(col="#f46d43")
grid.arrange(p1,p2,nrow=1)

ol1 <- glm(medals ~ log(population) + log(GDPpercapita),
           family = poisson, data = olympics)
summary(ol1)
