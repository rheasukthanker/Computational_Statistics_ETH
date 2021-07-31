


#---------------------------
# Diagnostic plots
#---------------------------

##################################

set.seed(21)
n<-1000
x <- rnorm(n, 20, 3)



# Assumptions met
set.seed(29)                        
y <- 1 + 2 * x + 5 * rnorm(n)
reg <- lm(y ~ x)
plot(reg, which=1)
plot(reg, which=2)
plot(reg, which=3)
plot(reg, which=4)
plot(reg, which=5)



# skewed error distribution
# A "harmless" deviation, where \hat{\beta} is still unbiased, the
# variance expression of \hat{\beta} is still correct and \hat{\beta}
# is asympotically normally distributed (so that p-values etc are asymptotically correct)
set.seed(29)                        
y <- 1 + 2 * x + 5 * (1 - rchisq(length(x), df = 1)) / sqrt(2)
reg <- lm(y ~ x)
plot(reg, which=1)
plot(reg, which=2)
plot(reg, which=3)
plot(reg, which=4)#left skewed data
plot(reg, which=5)


# Correlation in the Residuals
# A less harmless deviation, where \hat{\beta} is still unbiased, but the
# variance expression of \hat{\beta} is not correct anymore (and thus p-values cannot be trusted)
require(MASS)
Sigma <- matrix(0.7,n,n)
diag(Sigma) <- 1
set.seed(29)                        
y <- 1 + 2 * x + 5 * mvrnorm(n = 1, mu = rep(0, length(x)), Sigma = Sigma)
reg <- lm(y ~ x)
plot(reg, which=1)
plot(reg, which=2)
plot(reg, which=3)
plot(reg, which=4)#looks okay but errors are not iid
plot(reg, which=5)

# We can't really detect this correlation in any of the plots!

#http://www.ucd.ie/ecomodel/Resources/QQplots_WebVersion.html
# Variance of the Residual depends on x
# A more serious deviation, where \hat{\beta} is still unbiased, but the
# variance expression of \hat{\beta} is not correct anymore (and thus p-values cannot be trusted)
set.seed(29)                        
y   <- 1 + 2 * x + 5 * rnorm(length(x), mean = 0, sd = (x-15)^2 / 30)
reg <- lm(y ~ x)
plot(reg, which=1)
plot(reg, which=2)
plot(reg, which=3)
plot(reg, which=4)#right skewed
plot(reg, which=5)
plot(x,y)


# Expected value of the Residual depends on x
# A more serious deviation, where \hat{\beta} has a strong bias, though the
# variance expression of \hat{\beta} is still correct.
set.seed(29)                        
y <- 1 + 2 * x + 5 * rnorm(length(x), mean = x^2 / 5 - 1, sd = 1)
reg <- lm(y ~ x)
plot(reg, which=1)
plot(reg, which=2)
plot(reg, which=3)
plot(reg, which=4)
plot(reg, which=5)
plot(x,y)


# Outlier Example

outx<-c(100,110, 120)
## Outliers that do not matter
set.seed(29)         
epsilon<-rnorm(n+3)

xout1<-c(x, outx)

y<-c()
y <- 1 + 2 * xout1 + 2 * epsilon

plot(xout1,y)
reg<-lm(y ~ xout1)
abline(reg)

plot(reg, which=1)
plot(reg, which=2)
plot(reg, which=3)
plot(reg, which=4)
plot(reg, which=5)

## Outliers that do matter
set.seed(29)         
epsilon<-c(rnorm(n), c(-50,-60,-80))

xout1<-c(x, outx)

y<-c()
y <- 1 + 2 * xout1 + 2 * epsilon

plot(xout1,y, ylim =c(10.33,249), xlim= c(5.16,124.42))
reg<-lm(y ~ xout1)
abline(reg)

plot(reg, which=1)
plot(reg, which=2)
plot(reg, which=3)
plot(reg, which=4)
plot(reg, which=5)




#---------------------------
## Confidence and Prediction intervals
#---------------------------

# Confidence Intervals for E(y_0)

set.seed(21)
n<-1000
x <- rnorm(n, 20, 3)

y <- 1 + 2 * x + 5 * rnorm(n)

plot(y,x)
fit<-lm(y~x)
abline(fit)  
# Obtain new value:
x0<-60

# Obtain prediction
Ey0 <-unname(fit$coef[1] + fit$coef[2]*x0)
# Real value
Ey0real<-1 + 2 * x0 

# Prediction interval (from Lecture code)

(quant <- qt(.975,n-2))
# Sigma.hat value:
(sigma.hat <- sqrt(sum((fit$resid)^2)/(n-2)))
# Design matrix:
X <- as.matrix(cbind(1,x))
XtXi <- solve(t(X) %*% X)
x00 <- as.matrix(c(1,x0),nrow=2)
# Now compute estimate for sd(\hat y0):
(se <- sigma.hat * sqrt( t(x00) %*% XtXi %*% x00))
# Confidence interval:
(lower <- Ey0 - quant*se)
(upper <- Ey0 + quant*se)
pred.frame <- data.frame(x=x0)
predict(fit, pred.frame, level=.95, interval="c")
# Thus it can be seen that this CI takes into account the randomness/variance of the parameters 
# hat{\beta}_1 and hat{\beta}_2.


# Prediction interval (Confidence interval for y_0)

(se <- sigma.hat * sqrt( 1 + t(x00) %*% XtXi %*% x00))
(lower <- Ey0 - quant*se)
(upper <- Ey0 + quant*se)
predict(fit, pred.frame, level=.95, interval="p")
# Here, we do almost the same, but calculate sigma.hat  + sigma.hat *t(x00) %*% XtXi %*% x00)).
# This in fact means we also take the randomness of the residual into account (which has estimated variance of sigma.hat)

#---------------------------
# Bias-Variance tradeoff
#---------------------------

# See lecture code and Ex. 3)


