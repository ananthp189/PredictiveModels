anderston <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/anderstonpm10.csv"))
# Create a date variable for ggplot
anderston$Date2 <- as.Date(anderston$Date, "%d/%m/%Y")
ggplot(anderston, aes(Date2, Glasgow.Anderston)) + geom_line(color = "#41ab5d") +
  scale_x_date(date_labels = "%d-%b-%y", date_breaks = "2 week") + xlab("Date") +
  ylab("Particulate matter") + ggtitle("Pollution concentrations in Glasgow")

#AirTraffic

library(zoo)
airtraffic<-read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/airtraffic.csv"))
# Create a quarterly date variable
airtraffic$Date <- as.yearqtr(paste(airtraffic$Year, airtraffic$Quarter),
                              format="%Y %q")
ggplot(airtraffic, aes(Date, passengers)) + geom_line(color = "#41b6c4") +
  scale_x_yearqtr(format = "%Y-Q%q") + xlab("Date") + ylab("Passengers") +
  ggtitle("Number of air travellers into the UK per quarter")

x <- rnorm(100, mean=0, sd=1)
plot(x, type="l", main="", xlab="")
acf(x, main="")

xacf <- acf(x, plot = FALSE)
exRandAcf <- data.frame(lag=xacf$lag, acf=xacf$acf)
exRandData <- data.frame(t=c(1:100), d=x)
exRandp1<- ggplot(exRandData, aes(t,d)) + geom_line(color="#8dd3c7") +
  xlab("Time") + ylab("Data") + ggtitle("Original data")
exRandp2<- ggplot(exRandAcf, aes(lag, acf)) +
  geom_segment(aes(xend = lag, yend = 0)) +
  geom_hline(aes(yintercept = 0)) + xlab("Lag") + ylab("ACF") +
  geom_hline(aes(yintercept = 0.196), linetype = 2, color = 'blue') +
  geom_hline(aes(yintercept = -0.196), linetype = 2, color = 'blue') +
  ggtitle("Correlogram")
grid.arrange(exRandp1, exRandp2, nrow=2)

#we fit two separate models to illustrate one with sine cosine variation and other with splinepolinomial
library(splines)
resp <- resp <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/resp.csv"))
# convert to "Date" type variable
resp$Date <- as.Date(as.character(resp$Date), format="%Y%m%d")
ggplot(resp, aes(Date, admissions_glasgow)) + geom_line(color = "#4d4d4d") +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year") + xlab("Date") +
  ylab("Respiratory admissions") +
  ggtitle("Hospital admissions due to respiratory disease in Glasgow 2000-2007")
x <- resp[,2]
n <- length(x)
t <- 1:n
# linear trend with harmonic seasonal component:
Z.fixed <- cbind(t, sin(2*pi*t/365), cos(2*pi*t/365))
resp$trend.fixed <- lm(x~Z.fixed)$fitted.values
resp$x.fixed <- x - resp$trend.fixed
22
# natural cubic spline model:
Z.flexible <- ns(t, df=48)
resp$trend.flexible <- lm(x~Z.flexible)$fitted.values
resp$x.flexible <- x - resp$trend.flexible
p1 <- ggplot(resp, aes(Date, admissions_glasgow)) +
  geom_point(color="#4d4d4d", alpha=0.4) +
  geom_line(aes(y=trend.fixed), color = "#d73027", size=1) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "2 year") +
  xlab("Date") + ylab("Respiratory admissions") +
  ggtitle("Fitted trend from model A")
p2 <- ggplot(resp, aes(Date, x.fixed)) +
  geom_line(color = "#d73027", alpha=0.7) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "2 year") +
  xlab("Date") + ylab("Respiratory admissions") +
  ggtitle("De-trended data from model A")
p3 <- ggplot(resp, aes(Date, admissions_glasgow)) +
  geom_point(color="#4d4d4d", alpha=0.4) +
  geom_line(aes(y=trend.flexible), color = "#4575b4", size=1) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "2 year") +
  xlab("Date") + ylab("Respiratory admissions") +
  ggtitle("Fitted trend from model B")
p4 <- ggplot(resp, aes(Date, x.flexible)) +
  geom_line(color = "#4575b4", alpha=0.7) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "2 year") +
  xlab("Date") + ylab("Respiratory admissions") +
  ggtitle("De-trended data from model B")
grid.arrange(p1,p3,p2,p4, nrow=2)

#Differencing in time series
x <- resp[,2]
# we use function diff(x, lag) for the difference operator
diff.1 <- data.frame(d=diff(x, lag = 1, differences = 1),
                     ind=resp$Date[-1])
diff.365 <- data.frame(d=diff(x, lag = 365, differences = 1),
                       ind=resp$Date[-(1:365)])
p1 <- ggplot(resp, aes(Date, admissions_glasgow)) +
  geom_point(color="#b10026", alpha=0.4) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "2 year") +
  xlab("Time index") + ylab("Respiratory admissions") +
  ggtitle("Original data")
p2 <- ggplot(diff.1, aes(y=d, ind)) +
  geom_point(color="#fc4e2a", alpha=0.4) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "2 year") +
  xlab("Time index") + ylab("Lag 1 difference") +
  ggtitle("First order differences")
p3 <- ggplot(diff.365, aes(y=d, ind)) +
  geom_point(color="#feb24c", alpha=0.4) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "2 year") +
  xlab("Time index") + ylab("Lag 365 difference") +
  ggtitle("Seasonal differences: d=365")
grid.arrange(p1,p2,p3, nrow=3)
