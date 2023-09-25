library(MASS)
library(scam)
library(lmtest)
library(ggplot2)


# simulate data
set.seed(11)
N <- 100;
x <- runif(N,0,10);
sigma <- 2*x ;
mu <-  5*(x-3);
y <- rnorm(N, mean = mu, sd = sigma);
data <- data.frame(
  x = x,
  y = y
)



# LSE
fit1 <- lm(y ~ x, data)
d <- data.frame(x=x);
pred1 <- predict(fit1, d, se.fit=TRUE, type="response");


# Detecting heteroskedasticity by BP-test & residual plot
## BP-test
p.value <- bptest(fit1)
print(p.value) # heteroskedasticity appear when p-value less than alpha

## Residual plot
data$res.fit1 <- fit1$residuals
ggplot(data = data, aes(x = x, y = res.fit1)) +
  geom_point(col = 'red') +
  geom_abline(slope = 0)

## Residual absolute plot
ggplot(data, aes(x = x, y = abs(res.fit1))) +
  geom_point(col = 'blue')



# WLES
w2 <- x^-2
fit2 <- lm(y ~ x, data, weights = w2)
pred2 <- predict(fit2, d, se.fit=TRUE, type="response");


# WLSE weight = residuals(fit1)^-2
w3 <- residuals(fit1)^-2
fit3 <- lm(y ~ x, data, weights = w3)
pred3 <- predict(fit3, d, se.fit=TRUE, type="response");


# Detecting heteroskedasticity by BP-test & residual plot
p.value2 <- bptest(fit2)
p.value3 <- bptest(fit3)
print(p.value2)
print(p.value3)
AIC(fit1, fit2, fit3)

# comparing true model/wlse/lse
ggplot(data, aes(x = x, y = y)) + geom_point()+
  geom_abline(slope =  fit1$coefficients[2], intercept = fit1$coefficients[1], col = 'red')+
  geom_abline( slope = fit2$coefficients[2], intercept = fit2$coefficients[1],  col = "blue")+
  geom_abline( slope = fit2$coefficients[2], intercept = fit2$coefficients[1],  col = "green")+
  geom_abline(slope = 5, intercept = -15, col = 'black')

## Comparing residual for fit1/wlse
data$res.fit2 <- fit2$residuals
data$res.fit3 <- fit3$residuals
plot(data$x, abs(data$res.fit1), col = 'red')
points(data$x, abs(data$res.fit2), col = 'blue') # much less discrepancy
points(data$x, abs(data$res.fit3), col = 'green')



# prediction model
plot(data$x, data$y)
idx = order(data$x)
lines(data$x[idx], pred1$fit[idx], col = "red")
lines(data$x[idx], pred2$fit[idx], col = "blue")
lines(data$x[idx], pred3$fit[idx], col = "green")
abline(a = -15, b = 5)

# prediction band
## LSE
fit1.pi <- predict(fit1 , d, interval="prediction");
lines(data$x[idx], fit1.pi[ ,2][idx], col="red")
lines(data$x[idx], fit1.pi[ ,3][idx], col="red")
## WLSE
fit2.pi <- predict(fit2, d,  weights = w2, interval="prediction");
lines(data$x[idx], fit2.pi[ ,2][idx], col="blue")
lines(data$x[idx], fit2.pi[ ,3][idx], col="blue")
## WLSE -- weight = residuals(lse.fit)^-2
fit3.pi <- predict(fit3, d,  weights = w3, interval="prediction");
lines(data$x[idx], fit3.pi[ ,2][idx], col="green")
lines(data$x[idx], fit3.pi[ ,3][idx], col="green")


### Question:
# 1. which weight should be choosen in WLSE?
# 2. PI3 is not smooth


















