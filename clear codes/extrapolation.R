setwd("C:\\R\\how simple are tropical food webs\\50perc\\equal")
guilds <- read.table("allguilds.txt", header = TRUE, sep = "\t")
#guilds

## MM function

x <- seq(1:38)
y <- rev(guilds[,3])

data <- as.data.frame(cbind(x,y))

library(drc)
library(AICcmodavg)
library(nls2)

mmfit <- drm(y~x, data = data, fct = MM.2())
names(mmfit)
#mmfit2 <- nls(y~)
names(mmfit)
Vmax <- mmfit$coefficients[1]
Km <- mmfit$coefficients[2]

par(mfrow=c(3,3))
#windows(400,300)
plot(y~x, xlim = c(1,227), ylim = c(0,520), axes = FALSE,
     ylab = "Interactions making up to 50% of interacions",
     xlab = "Tree species")

pred <- data.frame(x = seq(1:227))
pred$y <- (Vmax / (Km + pred$x)) * pred$x
summary(mmfit)
  
par(new = TRUE)
plot(pred$y~pred$x, type = "l", lty = 2, col = "red", xlim = c(1,227), ylim = c(0,520),
     ylab = "", xlab = "")

text(50, 500, expression(y == frac(1615.25, (630.81 + x)) %*% x))

AICc.mm <- AICc(mmfit)

class(powerfit)
#Power(?) function

x <- seq(1:38)
y <- rev(guilds[,3])
#windows(400,300)
plot(y~x, xlim = c(1,227), ylim = c(0,520), axes = FALSE,
     ylab = "Interactions making up to 50% of interacions",
     xlab = "Tree species")

a <- 1
z <- 1

powerfit <- nls(y ~ a * x^z, start=list(a = a, z = z))

coeff <- summary(powerfit)$coefficients

pred <- data.frame(x = seq(1:227))
pred$y <- coeff[1,1]*pred$x^coeff[2,1]

par(new = TRUE)

plot(pred$y~pred$x, type = "l", lty = 2, col = "red", xlim = c(1,227), ylim = c(0,520),
     ylab = "", xlab = "")
text(50, 500, expression(y == 2.7618 * x^0.9637))

AICc.power <- AICc(powerfit)

#Linear fit

x <- seq(1:38)
y <- rev(guilds[,3])
#windows(400,300)
plot(y~x, xlim = c(1,227), ylim = c(0,520), axes = FALSE,
     ylab = "Interactions making up to 50% of interacions",
     xlab = "Tree species")
a <- 2.558474
b <- 0.007835707
par(new = TRUE)
data <- curve(a/b*(1-exp(-b*x)), xlim = c(1,227), ylim = c(0,520), ylab = "", xlab = "")
a/b*(1-exp(-b*7.78))
linearfit <- nls(y~a/b*(1-exp(-b*x)), start=list(a = a, b = b))

estb <- summary(linearfit)$coefficients[1,1]
esta <- summary(linearfit)$coefficients[2,1]*estb

pred <- data.frame(x = seq(1:227))
pred$y <- a/b*(1-exp(-b*pred$x))

par(new = TRUE)

plot(pred$y~pred$x, type = "l", lty = 2, col = "red", xlim = c(1,227), ylim = c(0,520),
     ylab = "", xlab = "")

AICc.linear <- AICc(linearfit)
  









#linearDRMfit <- drm(y~a*(1-exp(-b*x)), start=list(a = 1, b = 1))
curve(esta/estb*(1-exp(-estb*x)), xlim = c(0,227))

#Logartthmic function

plot(rev(1:38),guilds[,3])
x <- seq(1:38)
y <- rev(guilds[,3])

plot(y~x, ylim = c(0, 90))
new <- data.frame(x = seq(1:38))
new$y <- 50 * log(new$x/3)
par(new = TRUE)
plot(new$y ~ new$x, ylim = c(0, 90))


new = data.frame(y = rev(guilds[,3]))
new$x <- exp((new$y/150.3387))


b <- 5
a <- 5

logfit <- nls(x ~ a * exp(y/b), start=list(a = a, b=b))
fit <- summary(logfit)
fit
fit$coefficients
#a <-fit$coefficients[1,1]
b <-fit$coefficients[1,1]
new = data.frame(xdata = seq(1,227,len=200))
new$prediction <- b * log(new[,1])

plot(new, type = "l", col = "red", lty = 2, xlim = c(new[1,1],new[dim(new)[1], 1]),
     ylim = c(new[1,2],new[dim(new)[1], 2]))
par(new = TRUE)
plot(rev(1:38),guilds[,3], xlim = c(new[1,1],new[dim(new)[1], 1]),
     ylim = c(new[1,2],new[dim(new)[1], 2]))

# LINEAR FIT

par(mfrow = c(1,1))
plot(log(rev(1:38)),log(guilds[,3]))
plot(rev(1:38),guilds[,3])
linear <- lm(guilds[,3] ~ rev(1:38))
abline(linear)
summary(linear)
names(linear)

intercept <- linear$coefficient[1]
slope <- linear$coefficient[2]

new = data.frame(xdata = seq(1,227,len=200))

new$prediction <- new[,1] * slope + intercept

plot(new, type = "l", col = "red", lty = 2, xlim = c(new[1,1],new[dim(new)[1], 1]),
     ylim = c(new[1,2],new[dim(new)[1], 2]))
par(new = TRUE)
plot(rev(1:38),guilds[,3], xlim = c(new[1,1],new[dim(new)[1], 1]),
     ylim = c(new[1,2],new[dim(new)[1], 2]))

answer = new[dim(new)[1], 2]
answer
#50pc for 227 species is exp(new$prediction[200, 2])
dim(new$prediction)
exp(new[200, 2])


#Reduced power fit for interactions
plot(rev(1:38),guilds[,3])
x <- log(seq(1:38))
y <- log(rev(guilds[,3]))

windows(400,300)
plot(x, y, xlim = c(log(1), log(250)), ylim = c(x[1], 8), ylab = "log[Interactions making up to 50 % of interactions]",
     xlab = "log[Tree species]")

predDAT <- data.frame(y = y[10:length(y)], x =  x[10:length(x)])

lm.log.fit <- lm(y~x, data = predDAT)

summary(lm.log.fit)
x.pred <- seq(log(10), log(227), length = 217)
y.pred <- lm.log.fit$coefficients[1] + lm.log.fit$coefficients[2] * x.pred

#plot(x.pred, y.pred)

par(new = TRUE)
plot(x.pred, y.pred, xlim = c(log(1), log(250)), ylim = c(x[1], 8), type = "l", lwd = 2, lty = 2, col = "red", ylab = "", xlab = "")
plot(lm.log.fit)

AIcc.lin <- AICc(lm.log.fit)
int50pc <- exp(y.pred[length(y.pred)])


#Dzialajaca predykcja!!!!!

newdata <- matrix(seq(from=log(39), to = log(227), length = 29), nrow = 29, ncol = 1)
newdata = as.data.frame(newdata)
PR <- predict(lm.log.fit, newdata = data.frame(x = log(227)), interval = "confidence", 
        se.fit = FALSE, type = "response")

# Standard error of the prediction

xtopred <- log(227)
meanx <- mean(x)
n <- length(x)
sumofsquares <- sum((x - meanx)^2)
sumofsquares2 <- n*sum(x^2) - (sum(x))^2
residualsd <- sd(lm.log.fit$residuals)

predSE <- residualsd*sqrt(1 +(1/n) + (n*(xtopred - meanx)^2)/sumofsquares2)

tval <- abs(qt(0.025, (length(x) - 2)))

pred95CI <- 4.264105 +  c(-1,1) * tval * predSE

exp(pred95CI)

#For all species

setwd("C:\\R\\data\\50perc\\equal")
guilds <- read.table("allguilds.txt", header = TRUE, sep = "\t")
#guilds
tion

x <- log(seq(1:38))
y <- log(rev(guilds[,1]))

windows(400,300)
plot(x, y, xlim = c(log(1), log(250)), ylim = c(x[1], 8), ylab = "log[Arthropods making up to 50 % of abundance]",
     xlab = "log[Tree species]")

lm.log.fit.full <- lm(y ~ x)
lm.log.fit <- lm(y[10:length(y)]~x[10:length(x)])
summary(lm.log.fit)

x.pred.full <- seq(log(1), log(227), length = 227)
y.pred.full <- lm.log.fit.full$coefficients[1] + lm.log.fit.full$coefficients[2] * x.pred.full

x.pred <- seq(log(10), log(227), length = 227)
y.pred <- lm.log.fit$coefficients[1] + lm.log.fit$coefficients[2] * x.pred

par(new = TRUE)
plot(x.pred.full, y.pred.full, xlim = c(log(1), log(250)), ylim = c(x[1], 8), type = "l", lwd = 2, lty = 2, col = "red", ylab = "", xlab = "")

#plot(x.pred, y.pred)

par(new = TRUE)
plot(x.pred, y.pred, xlim = c(log(1), log(250)), ylim = c(x[1], 8), type = "l", lwd = 2, lty = 2, col = "blue", ylab = "", xlab = "")
#plot(lm.log.fit)

AIcc.lin <- AICc(lm.log.fit)
int50pc <- exp(y.pred[length(y.pred)])

