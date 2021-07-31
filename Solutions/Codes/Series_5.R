##############################################################
# Estimating the Accuracy of a Linear Regression Model

# Simulate from model with heteroscedastic errors 
# (non-constant variance)

library(boot)
set.seed(999)

n <- 100

generate.data <- function(){
  x <- runif(n,0,100)
  eps <- rnorm(n,0,abs(x-50))
  y <- 50 + 0.3*x + eps
  return(data.frame(cbind(x,y)))
}

dat <- generate.data() #our dataset

par(mfrow=c(1,1))
plot(y~x,data=dat)
abline(50, 0.3) #real value
abline(coef(lm(y~x, data=dat)), col='red') #our estimate

fit1 <- lm(y~x,data=dat)
summary(fit1) 
# We get estimates of the standard error, p-values and confidence intervals
# based on the assumptions of linear model which are violated here
# Std. Error estimates computed as (X^TX)^{-1}_{jj}\hat{\sigma}^2

# Estimate real Std. Error by simulating from the generating model:
simul <- function(){
  data.temp <- generate.data()
  return(coef(lm(y~x,data=data.temp)))
}
simul()
res.coef <- replicate(10000, simul())
dim(res.coef)
(se.int.sim <- sd(res.coef[1,]))
(se.slope.sim <- sd(res.coef[2,]))
# True values of Std. Error

# Function used for bootstrap needs to be of this format; it accepts the array of indices which is used to sample the data.
boot.fn=function(data,index){
  return(coef(lm(y~x,data=data,subset=index)))
}

# we use this statistic function here:
?boot
boot.fit = boot(dat, statistic=boot.fn, R=20)
boot.fit$t0 #original estimate (using all data)
boot.fit$t #the outputs of statistic function

# Compare bootstrap std error with true values and those from lm
boot.fit <- boot(dat, statistic=boot.fn, R=5000)
boot.fit
se.int.sim
se.slope.sim
summary(fit1)$coef

#####
#Confidence Intervals
confint(fit1) #confidence interval from lm
?boot.ci
boot.ci(boot.fit, conf=0.90, type="basic") #bootstrap CI, conf flag is by default 0.95
boot.ci(boot.fit, index=c(2), type="basic") #index needed for other variables in output of boot

boot.ci(boot.fit, index=c(1), type="basic")
boot.ci(boot.fit, index=c(1), type="perc")
boot.ci(boot.fit, index=c(1), type="norm")
boot.ci(boot.fit, index=c(1), type="stud") #error, need to give variances for t statistics

#for "stud", our bootstrap function needs to return estimate of the variances as well
coef.fn=function(data, index){
  return (coef(lm(y~x,data=data,subset=index)))
}
boot.fn=function(data,index){
  coefs = coef.fn(data, index)
  boot.sample = boot(data[index, ], coef.fn, R=50)
  vars = c(var(boot.sample$t[,1]), var(boot.sample$t[,2]))
  return(c(coefs, vars))
}
boot.fn(dat, 1:100)

boot.fit2 <- boot(dat, boot.fn, R=300) #takes long because we have a bootstrap in a bootstrap
#index[1] tells us where is the estimator, index[2] tells us where is its variance
#if our bootstrap statistic returns two values and we do not specify index, by default first is the estimator and second is its variance
boot.ci(boot.fit2, index=c(1, 3), type="stud")
boot.ci(boot.fit2, index=c(2, 4), type="stud")

