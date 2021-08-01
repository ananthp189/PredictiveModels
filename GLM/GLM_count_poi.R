
library(GGally)

cancer <- read.table(url("http://www.stats.gla.ac.uk/~tereza/rp/cancer.txt"),
                     header=TRUE)
head(cancer)

ggpairs(cancer[,c(-1,-3)],
        upper=list(continuous=wrap("points", alpha=0.4, color="#d73027")),
        lower="blank", axisLabels="none")

epid1 <- glm(Y_all ~ pm10 + smoke + ethnic + log.price + easting +
               northing+offset(log(E_all)), family = poisson, data = cancer)
summary(epid1)

sd(cancer$pm10)

exp(0.0500269*sd(cancer$pm10))

library(ggplot2)
library(faraway)
head(gala)
ggpairs(gala, upper=list(continuous=wrap("points", alpha=0.4, color="#d73027")),
        lower="blank", axisLabels="none")

gal1 <- glm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,
            family = poisson, data = gala)
summary(gal1)

resp <- resid(gal1, type = "pearson")
resd <- resid(gal1, type = "deviance")
p1<- ggplot(gal1, aes(sample = resp)) + geom_point(stat = "qq", color = "#7fc97f") +
  ylab("Pearson residuals")
p2<- ggplot(gal1, aes(sample = resd)) + geom_point(stat = "qq", color = "#7fc97f") +
  ylab("Deviance residuals")
p3<- ggplot(gal1, aes(x = predict(gal1, type="link"), y =resd))+
  geom_point(col = "#7fc97f") +
  ylab("Deviance residuals") + xlab("Linear predictor")

library(ggplot2)
library(gridExtra)
grid.arrange(p1, p2, p3, nrow = 1)


#OverDispersion
ggplot(gal1, aes(x=log(fitted(gal1)), y=log((gala$Species-fitted(gal1))^2)))+
  geom_point(col="#f46d43") +
  geom_abline(slope=1, intercept=0, col="#a6d96a", size=1) + ylab(expression((y-hat(mu))^2)) + xlab(expression(hat(mu)))

#Quasi poisson model - introduce dispersion parameter
X2 <- sum(resid(gal1, type = "pearson")^2)
dp <- X2 / gal1$df.res
dp

summary(gal1, dispersion = dp)
#With the use of the estimated dispersion parameter the Wald tests are not very reliable, so we turn to an F test to
#determine the significance of the regression coefficients:
  drop1(gal1, test = "F")
  # Residual plots vs. predicted
  pred <- predict(gal1, type = "response")
  stand.resid <- rstandard(model = gal1, type = "pearson") # Standardised Pearson residuals
  par(mfrow=c(1,2))
  plot(x = pred, y = stand.resid, xlab = "Predicted count", ylab = "Standardised Pearson residuals",
       main = "Regular likelihood", ylim = c(-5,5))
  abline(h = c(-3, -2, 0, 2, 3), lty = "dotted", col = "red")
  gal2 <- glm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,
              family = quasipoisson(link = "log"), data = gala) # Quasi-Poisson model
  pred <- predict(gal2, type = "response")
  stand.resid <- rstandard(model = gal2, type = "pearson") # Standardised Pearson residuals
  plot(x = pred, y = stand.resid, xlab = "Predicted count", ylab = "Standardised Pearson residuals",
       main = "Quasi-likelihood", ylim = c(-5,5))
  abline(h = c(-3, -2, 0, 2, 3), lty = "dotted", col = "red")
  
  #To fit a negative binomial model to the Galapagos data to account for the overdispersion, we use the
  #function glm.nb() from library(MASS):
    library(MASS)
  gal3 <- glm.nb(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,
                 data = gala)
  summary(gal3)
  #We can compare the Poisson and negative binomial models by looking at their deviances and AIC scores:
  # Poisson model
  c(gal1$deviance, gal1$aic)
  # Negative binomial model
  c(gal3$deviance, gal3$aic)
  #How to choose between quasi poi and nebi? dont matter but here is comparison
  # Plot of squared residuals v predicted values
  plot.new()  
  
  res.sq <- residuals(gal1, type = "response")^2
  set1 <- data.frame(res.sq, mu.hat = gal1$fitted.values)
  fit.lin <- lm(formula = res.sq ~ mu.hat, data = set1)
  fit.quad <- lm(formula = res.sq ~ mu.hat + I(mu.hat^2), data = set1)
  plot(set1$mu.hat, y = set1$res.sq, xlab = "Predicted count",
       ylab = "Squared Residual")
  curve(expr = predict(fit.lin, newdata = data.frame(mu.hat = x), type = "response"),
        col = "blue", add = TRUE, lty = "solid")
  curve(expr = predict(fit.quad, newdata = data.frame(mu.hat = x), type = "response"),
        col = "red", add = TRUE, lty = "dashed")
  legend("topleft", legend = c("Linear", "Quadratic"), col = c( "blue", "red"),
         lty = c("solid", "dashed"), bty = "n")
  