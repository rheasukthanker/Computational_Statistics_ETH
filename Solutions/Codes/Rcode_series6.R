###########################
#Doing bootstrap CI manually

set.seed(007)

df = 3

#true value
theta = median(rchisq(5000000, df))
theta

#our dataset
n = 50
dset = rchisq(n, df)
hist(dset, breaks=20, freq=FALSE)
curve(dchisq(x, df), add=TRUE, col='red')

#estimator_function
estimator <- function(data){
  return(median(data))
}

#our estimate for our dataset
theta_hat = estimator(dset)
theta_hat

# generate bootstrap sample to get estimate distribution of theta
bootstrap = function(data, B){
  theta_star = rep(0, B)
  for(i in 1:B){
    bootstrap_sample = sample(dset, n, replace=TRUE)
    theta_star[i] = estimator(bootstrap_sample) 
  }
  return(theta_star)
}
theta_star = bootstrap(dset, 10000)
#distribution of the bootstrap sample
hist(theta_star)
abline(v=theta_hat, col='blue')#estimate bootstrap centers around this value
abline(v=theta, col='red')#true value 
#notice how the bootstrapped value is centered around theta_hat and not theta

#we could change value of B to see the difference
#rerun the code (without seeding) to see that the bootstrap distribution is not stable

#true estimator distribution
estimator_sample = replicate(100000, estimator(rchisq(n, df)))#simulate from actual distribution
hist(estimator_sample)

# What can we do with the bootstrap?

#estimate the bias of our estimator
bias = mean(theta_star) - theta_hat
bias

#true bias
mean(estimator_sample) - theta
#we were unlucky with our dataset
#estimate the standard deviation of our estimator 
sd_hat = sd(theta_star)
sd_hat
#true value
sd(estimator_sample)
#original data not very representative of the true distribution
#Construct CIs
alpha = 0.05
#true CI
trueCI = c(quantile(estimator_sample, alpha/2), quantile(estimator_sample, 1-alpha/2))
trueCI
#a) quantile
quantileCI = c(quantile(theta_star, alpha/2), quantile(theta_star, 1-alpha/2))
quantileCI
#b) normal assumption
#justified? skewed but not bad
normalCI = c(theta_hat - qnorm(1-alpha/2)*sd_hat, theta_hat + qnorm(1-alpha/2)*sd_hat)
normalCI
#c) reversed quantile
reversedCI = c(theta_hat - quantile(theta_star - theta_hat, 1-alpha/2), theta_hat - quantile(theta_star - theta_hat, alpha/2))
reversedCI

#d) bootstrap T
# for this we need to sample completely different statistic
t_statistic <- function(data, theta_){#theta_ is true parameter
  thetastar = estimator(data)
  #compute our sd estimate
  C = 100
  sdhat = sd(bootstrap(data, C)) #we put small value here just for speed, this is just approximation of the true estimator
  tstar = (thetastar - theta_)/sdhat#sd estimated from data
  return (tstar)
}

#true distribution of the t statistic
hist(replicate(10000, t_statistic(rchisq(n, df), theta)), breaks=15)
#compare to the distribution of thetahat - theta; not exactly the same
hist(replicate(10000, estimator(rchisq(n, df)) - theta), breaks=15)

#find the distribution of this t statistic by using the bootstrap
B = 10000 #we do bootstrap two times once the outer loop and once for t_statistic
t_star = rep(0, B)
for(i in 1:B){
  bootstrap_sample = sample(dset, n, replace=TRUE)
  t_star[i] = t_statistic(bootstrap_sample, theta_hat) 
}

hist(t_star, breaks=10)

bootstrapTCI = c(theta_hat - quantile(t_star, 1-alpha/2)*sd_hat, theta_hat - quantile(t_star, alpha/2)*sd_hat)
bootstrapTCI
#check theoritical justification of T bootstrap
#Distribution of T is closer to the original distribution
#############################################
#parametric bootstrap by hand

n <- 100

x <- runif(n, 0, 30)
eps <- rnorm(n, 0, 6)
y <- 5 + 0.5*x + eps
dat <- data.frame(cbind(x,y)) #our dataset

par(mfrow=c(1,1))
plot(y~x,data=dat)
abline(5, 0.3) #real value
abline(coef(lm(y~x, data=dat)), col='red') #our estimate

fit1 <- lm(y~x,data=dat)
summary(fit1) 
# We get estimates of the standard error, p-values and confidence intervals
# Std. Error estimates computed as (X^TX)^{-1}_{jj}\hat{\sigma}^2

# See true value of the estimator Std. Error by simulating:
simul <- function(){
  x = dat$x #Why is it wrong to sample it?
  eps <- rnorm(n, 0, 6)
  y <- 5 + 0.5*x + eps
  data.temp<-data.frame(cbind(x, y))
  return(coef(lm(y~x,data=data.temp)))
}
res.coef <- replicate(10000, simul())
(se.int.sim <- sd(res.coef[1,]))
(se.slope.sim <- sd(res.coef[2,]))
# True values of the Std. Error

#estimated model
sigma_hat = sigma(fit1)
sigma_hat
intercept_hat = coef(fit1)[1]
intercept_hat
slope_hat = coef(fit1)[2]
slope_hat

# parametric bootstrap
B <- 10000
intercept_star = rep(0, B)
slope_star = rep(0, B)
for(i in 1:B){
  # Need to simulate from the estimated model
  x = dat$x
  eps <- rnorm(n, 0, sigma_hat)#estimated noise distribution 
  y <- intercept_hat + slope_hat*x + eps#resample y estimated slope and intercept
  bootstrap_sample <- data.frame(cbind(x, y))
  
  fit2 <- lm(y~x, data=bootstrap_sample)
  intercept_star[i] = coef(fit2)[1]
  slope_star[i] = coef(fit2)[2]
}

# bootstrap estimates of the std error
#made no asuumptions required by linear model
sd(intercept_star)
sd(slope_star)

# true values of the std error
(se.int.sim <- sd(res.coef[1,]))
(se.slope.sim <- sd(res.coef[2,]))

# close to the std. error estimates of lm, but we did not need to know the formula!
summary(fit1)


##############################################
# model misspecification for parametric bootstrap
#
n <- 100

x <- runif(n, 0, 30)
eps <- rnorm(n, 0, abs(x-15)) #heteroscedastic errors depend on position of x
y <- 5 + 0.5*x + eps
dat <- data.frame(cbind(x,y)) #our dataset

par(mfrow=c(1,1))
plot(y~x,data=dat)
abline(5, 0.3) #real value
abline(coef(lm(y~x, data=dat)), col='red') #our estimate

fit1 <- lm(y~x,data=dat)
summary(fit1) 
# We get estimates of the standard error, p-values and confidence intervals
# Std. Error estimates computed as (X^TX)^{-1}_{jj}\hat{\sigma}^2

# See true value of the estimator Std. Error by simulating:
simul <- function(){
  x = dat$x #Why is it wrong to sample it?
  eps <- rnorm(n, 0, abs(x-15))
  y <- 5 + 0.5*x + eps
  data.temp<-data.frame(cbind(x, y))
  return(coef(lm(y~x,data=data.temp)))
}
res.coef <- replicate(10000, simul())
(se.int.sim <- sd(res.coef[1,]))
(se.slope.sim <- sd(res.coef[2,]))
# True values of the Std. Error
#we see that lm underestimates it a lot
#because of model misspecification heteroskedastic errors
#estimated model
sigma_hat = sigma(fit1)
sigma_hat
intercept_hat = coef(fit1)[1]
intercept_hat
slope_hat = coef(fit1)[2]
slope_hat

# parametric bootstrap
B <- 10000
param_intercept_star = rep(0, B)
#no heteroskedastic errors

param_slope_star = rep(0, B)
for(i in 1:B){
  # Need to simulate from the estimated model
  x = dat$x
  eps <- rnorm(n, 0, sigma_hat)
  y <- intercept_hat + slope_hat*x + eps
  bootstrap_sample <- data.frame(cbind(x, y))
  
  fit2 <- lm(y~x, data=bootstrap_sample)
  param_intercept_star[i] = coef(fit2)[1]
  param_slope_star[i] = coef(fit2)[2]
}

# parametric bootstrap estimates of the std error
#both make same assumptions
sd(param_intercept_star)
sd(param_slope_star)

# non-parametric bootstrap
B <- 10000
nonparam_intercept_star = rep(0, B)
nonparam_slope_star = rep(0, B)
for(i in 1:B){
  bootstrap_sample <- dat[sample(1:n, n, replace=TRUE), ]
  fit2 <- lm(y~x, data=bootstrap_sample)
  
  nonparam_intercept_star[i] = coef(fit2)[1]
  nonparam_slope_star[i] = coef(fit2)[2]
}

# non-parametric bootstrap estimates of the std error
sd(nonparam_intercept_star)
sd(nonparam_slope_star)

# true values of the std error
(se.int.sim <- sd(res.coef[1,]))
(se.slope.sim <- sd(res.coef[2,]))
#different from the results of linear model
# non-parametric much closer since it does not make wrong assumptions about the data generating mechanism