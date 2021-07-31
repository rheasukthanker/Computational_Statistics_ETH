x=c(1,1,4.5,4.5)
y=c(1,2.5,2.5,4)
plot(x,y,xlim=c(0,5),ylim=c(0,5))
fit=lm(y~x)
abline(coefficients(fit)[1],coefficients(fit)[2])
?lines
?abline
 ## simple linear regression
set.seed(21)                            # initializes the random number generator
x   <- rnorm(40, 20, 3)                 # generates x-values
y   <- 1 + 2 * x + 5 * rnorm(length(x)) # y-values = linear function(x) + error
reg <- lm(y ~ x)
summary(reg)
plot(x, y)
# fit of the linear regression
# output of selected results
# scatter plot
abline(reg)                             # draw regression line
plot(reg, which = 1)                    # draw Tukey-Anscombe plot
#Write a sequence of R-commands which randomly generates 100 times a vector of y-values 
#ac- cording to the given model yi = β0 +β1xi +εi with β0 = 1,β1 = 2 and εi 
#∼ N(0,5^2) with the given x-values, and generates a corresponding vector of 
#estimated slopes βˆ1 of the regression lines.

#For the first three generated y-vectors, plot y against x, and add the fitted regression line and
#construct the corresponding Tukey-Anscombe plot.
set.seed(21)
x=rnorm(40,20,3)
beta_1=rep(0,100)
par(mfrow=c(3,2))
for(i in c(1:100))
{ y=1+2*x+5*rnorm(length(x),5)
  fit=lm(y~x)
  beta_1[i]=coefficients(fit)[2]
  if(i<=3)
  {
    plot(x,y)
    abline(fit)
    plot(fit,which=1)
  }
}
#c) Compute the empirical mean and standard deviation of the estimated slopes.
#d) Compute the theoretical variance of βˆ1.
mean(beta_1)
sd(beta_1)
X=cbind(rep(1,40),x)
theo_var=25*solve(t(X)%*%X)
theo_var_beta1=theo_var[2,2]
#Draw a histogram of the 100 estimated slopes and add the normal density of the 
#theoretical distribution of βˆ1 to the histogram. What do you observe? Does it fit well?
par(mfrow=c(1,1))
hist(beta_1,freq=FALSE,ylim=c(0,7),xlim=c(1,3))
lines(seq(1.3, 2.6, by = 0.01), dnorm(seq(1.3, 2.6, by = 0.01), mean= 2, sd =sqrt(0.06164328)),col="red")
#4. Repeat three with different modifications to y
#a)#
set.seed(21)
x=rnorm(40,20,3)
beta_1=rep(0,100)
par(mfrow=c(3,2))
for(i in c(1:100))
{ y <- 1 + 2 * x + 5 * (1 - rchisq(length(x), df = 1)) / sqrt(2)
fit=lm(y~x)
beta_1[i]=coefficients(fit)[2]
if(i<=3)
{
  plot(x,y)
  abline(fit)
  plot(fit,which=1)
}
}
#c) Compute the empirical mean and standard deviation of the estimated slopes.
#d) Compute the theoretical variance of βˆ1.
mean(beta_1)
sd(beta_1)
X=cbind(rep(1,40),x)
theo_var=25*solve(t(X)%*%X)
theo_var_beta1=theo_var[2,2]
#Draw a histogram of the 100 estimated slopes and add the normal density of the 
#theoretical distribution of βˆ1 to the histogram. What do you observe? Does it fit well?
par(mfrow=c(1,1))
hist(beta_1,freq=FALSE,ylim=c(0,7),xlim=c(1,3))
lines(seq(1.3, 2.6, by = 0.01), dnorm(seq(1.3, 2.6, by = 0.01), mean= 2, sd =sqrt(theo_var_beta1)),col="red")
#Error distribution
par(mfrow=c(1,1))
errors <- 5 * (1 - rchisq(40, df = 1)) / sqrt(2)
hist(errors)
mean(errors)
var(errors)
#General observations most residuals are positive, could be because of the property of the error distribution.ie thicker right tail
#Tukey anscombe plots seem fine though, beta distribution seems shifted
#The errors are independent, have zero mean and constant variance σ2 = 25, but they are not normally distributed. From Figure 3, one can see that the residuals are not symmetrically distributed (there are more positive points than negative points),
#this gives some evidence that the errors are not normally
#distributed. For the estimated slope βˆ , we have E 􏱀βˆ 􏱁 = β and Var 􏰒βˆ 􏰓 = 􏰊σ2(X⊤X)−1􏰋 , but 1 11 1 22
#β1 is only approximately normally distributed. This approximation becomes better for large n (Central Limit Theorem) and the p-values are asymptotically correct. For n = 40, we see in Figure 4 that the distribution is starting to be like a Gaussian.
#b#
set.seed(21)
x=rnorm(40,20,3)
beta_1=rep(0,100)
par(mfrow=c(3,2))
for(i in c(1:100))
{y <- 1 + 2 * x + 5 * rnorm(length(x), mean = x^2 / 5 - 1, sd = 1)
fit=lm(y~x)
beta_1[i]=coefficients(fit)[2]
if(i<=3)
{
  plot(x,y)
  abline(fit)
  plot(fit,which=1)
}
}
#c) Compute the empirical mean and standard deviation of the estimated slopes.
#d) Compute the theoretical variance of βˆ1.
mean(beta_1)
sd(beta_1)
X=cbind(rep(1,40),x)
theo_var=25*solve(t(X)%*%X)
theo_var_beta1=theo_var[2,2]
#Draw a histogram of the 100 estimated slopes and add the normal density of the 
#theoretical distribution of βˆ1 to the histogram. What do you observe? Does it fit well?
par(mfrow=c(1,1))
hist(beta_1,freq=FALSE,xlim=c(0,50),ylim=c(0,3))
lines(seq(1.3, 2.6, by = 0.01), dnorm(seq(1.3, 2.6, by = 0.01), mean= 2, sd =sqrt(theo_var_beta1)),col="red")
#Error distribution
par(mfrow=c(1,1))
errors <- 5 * rnorm(length(x), mean = x^2 / 5 - 1, sd = 1)
hist(errors)
mean(errors)
var(errors)
#Conclusion b:The mean is dependent on x.ie they are not independent Beta is severely biased. Cannot trust conf intervals, pvalues, beta estimates
#Tukey anscombe plot shows a systematic trend and could be evidence for a ponential nonlinear relationship
# The errors are independent with constant variance, but do not have mean zero: their expected value depends on x. This can be seen in Figure 5. Hence, the model is misspecified and the estimated βˆ1 are severely biased. We clearly see this in Figure 6, where the estimated slopes are around 42.3 instead of 2. In the summary table of the R output, the least square estimation of the slope and intercept, the point estimation of error variance, and the resutls that depend on the distribution of βˆ1 such as t-statistic and p-values are all not ok.
set.seed(21)
x=rnorm(40,20,3)
beta_1=rep(0,100)
par(mfrow=c(3,2))
require(MASS)
Sigma <- matrix(0.7,40,40)
diag(Sigma) <- 1
for(i in c(1:100))
{y   <- 1 + 2 * x + 5 * mvrnorm(n = 1, mu = rep(0, length(x)), Sigma = Sigma)
fit=lm(y~x)
beta_1[i]=coefficients(fit)[2]
if(i<=3)
{
  plot(x,y)
  abline(fit)
  plot(fit,which=1)
}
}
#c) Compute the empirical mean and standard deviation of the estimated slopes.
#d) Compute the theoretical variance of βˆ1.
mean(beta_1)
sd(beta_1)
X=cbind(rep(1,40),x)
theo_var=25*solve(t(X)%*%X)
theo_var_beta1=theo_var[2,2]

#Draw a histogram of the 100 estimated slopes and add the normal density of the 
#theoretical distribution of βˆ1 to the histogram. What do you observe? Does it fit well?
par(mfrow=c(1,1))
hist(beta_1,freq=FALSE,xlim=c(1,3),ylim=c(1,7))
lines(seq(1.3, 2.6, by = 0.01), dnorm(seq(1.3, 2.6, by = 0.01), mean= 2, sd =sqrt(theo_var_beta1)),col="red")
#Error distribution
par(mfrow=c(1,1))
errors <- 5 * mvrnorm(n = 1, mu = rep(0, length(x)), Sigma = Sigma)
hist(errors)
mean(errors)
var(errors)
#c#normality assumption not violated, nor is the there evidence of non linear relation between x and y (tukey anscombe plot shows no trend), but the errors are heteroskedastic, we cannot trust the pvalues, confidence intervals. We can trust the parameter estimate thorugh
#The errors are normally distributed with mean zero and constant variance σ2 = 25, but they are
#correlated. Note that we can not detect this in Tukey-Anscombe plot (Figure 7). The estimated
#slopes are unbiased, but the standard error formula that is obtained under the uncorrelated error
#assumption does not hold. We indeed clearly see in Figure 8 that the estimated slopes have a
#Gaussian shape with mean 2, but a different variance compared with the normal density line that
#corresponds to distribution N (β1, 􏰊25(X⊤X)−1􏰋 ) = N (2, 0.0616). As a result, we do not trust the 22
#outputted standard errors, p-values, or any other inference.
set.seed(21)
x=rnorm(40,20,3)
beta_1=rep(0,100)
par(mfrow=c(3,2))
require(MASS)
Sigma <- matrix(0.7,40,40)
diag(Sigma) <- 1
for(i in c(1:100))
{y <- 1 + 2 * x + 5 * rnorm(length(x), mean = 0, sd = (x-15)^2 / 30)
fit=lm(y~x)
beta_1[i]=coefficients(fit)[2]
if(i<=3)
{
  plot(x,y)
  abline(fit)
  plot(fit,which=1)
}
}
#c) Compute the empirical mean and standard deviation of the estimated slopes.
#d) Compute the theoretical variance of βˆ1.
mean(beta_1)
sd(beta_1)
X=cbind(rep(1,40),x)
theo_var=25*solve(t(X)%*%X)
theo_var_beta1=theo_var[2,2]

#Draw a histogram of the 100 estimated slopes and add the normal density of the 
#theoretical distribution of βˆ1 to the histogram. What do you observe? Does it fit well?
par(mfrow=c(1,1))
hist(beta_1,freq=FALSE,xlim=c(0,4),ylim=c(0,1))
lines(seq(1.3, 2.6, by = 0.01), dnorm(seq(1.3, 2.6, by = 0.01), mean= 2, sd =sqrt(theo_var_beta1)),col="red")
#Error distribution
par(mfrow=c(1,1))
errors <- 5 * rnorm(length(x), mean = 0, sd = (x-15)^2 / 30)
hist(errors)
mean(errors)
var(errors)
#Heteroskedastic errors this time dependent on the independent vairable. No significant trend in the tukey anscombe plot though
#The errors are independent, have mean zero, and have non constant variance. 
#This can be clearly seen in the Tukey-Anscombe plot; compare with Figure 9.
#Hence, the estimated slopes are unbiased, but the standard error formula does not hold.
#We indeed see in Figure 10 that the true variation in the estimated slopes is larger 
#than the theoretical variation computed under the assumption of independent constant 
#variance errors. As a result, we do not trust the outputted standard errors, p-values,
#or any other inference.