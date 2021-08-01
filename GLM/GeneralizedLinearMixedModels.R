library(mlmRev)
head(Contraception)
library(lattice)
lattice.options(default.theme = function() standard.theme())
print(xyplot(ifelse(use == "Y", 1, 0) ~ age|urban, Contraception,
             groups = livch, type = c("g", "smooth"),
             auto.key = list(space = "top", points = FALSE,
                             lines = TRUE, columns = 4),
             ylab = "Proportion", xlab = "Centered age"))

