#Question 1
set.seed(0)
n<-100
z1<-rnorm(n)
z2<-rnorm(n)
M=matrix(c(1,1,0.1,-0.1),2,2)
X=t(M%*%rbind(z1,z2)) #matrix multiplication in R ASK
beta<-c(0.5,-1.0)
x1=X[,1]
x2=X[,2]
y=5+beta[1]*x1+beta[2]*x2 +rnorm(n)
par(mfrow=c(1,1))
#a
plot(x1,x2)
plot(X)
#b
fit1<-lm(y~x1+x2)
summary(fit1)
#c
p<-3
beta_1=fit1$coefficients[2]
beta_2=fit1$coefficients[3]
y_pred<-predict(lm(y~x1+x2))  
summary=summary(fit1)
se_beta1<-coef(summary)["x1","Std. Error"]
tval_beta1=beta_1/se_beta1
tval_beta1
#d
pval_beta1=2*pt(abs(tval_beta1), df=n-p, lower=FALSE)
pval_beta1
#P-value definition
#P values evaluate how well the sample data support the devil’s advocate argument that the null hypothesis is true. It measures how compatible your data are with the null hypothesis. How likely is the effect observed in your sample data if the null hypothesis is true?
#High P values: your data are likely with a true null.
#Low P values: your data are unlikely with a true null.
#A low P value suggests that your sample provides enough evidence that you can reject the null hypothesis for the entire population.
#e
fit2<-lm(y~1)
anova(fit2,fit1)
fit3<-lm(y~x1)
summary(fit3)
fit4<-lm(y~x2)
summary(fit4)
#adding either to the other has no benefit
#null hypothesis can be rejected at 5% level
#Compare the p-value for the F-test to your significance level. If the p-value is less than the significance level, your sample data provide sufficient evidence to conclude that your regression model fits the data better than the model with no independent variables.
"This finding is good news because it means that the independent variables in your model improve the fit!Generally speaking, if none of your independent variables are statistically significant, the overall F-test is also not statistically significant. Occasionally, the tests can produce conflicting results. This disagreement can occur because the F-test of overall significance assesses all of the coefficients jointly whereas the t-test for each coefficient examines them individually. For example, the overall F-test can find that the coefficients are significant jointly while the t-tests can fail to find significance individually.
These conflicting test results can be hard to understand, but think about it this way. The F-test sums the predictive power of all independent variables and determines that it is unlikely that all of the coefficients equal zero. However, it’s possible that each variable isn’t predictive enough on its own to be statistically significant. In other words, your sample provides sufficient evidence to conclude that your model is significant, but not enough to conclude that any individual variable is significant."
#REFER https://statisticsbyjim.com/regression/interpret-f-test-overall-significance-regression/
#g
summary(fit1)
res=residuals(fit1)
res2=y-predict(fit1)
sigma2hat<- sum((res)^2)/(n-3)
sigma2hat2<- sum((res2)^2)/(n-3)
residualStandardError= sqrt(sigma2hat)
residualStandardError
residualStandardError2= sqrt(sigma2hat2)
residualStandardError2
#h
RSquared=1-sum((residuals(fit1))^2)/ sum((y-mean(y))^2)
RSquared
#If R-squared=0.93 then it means 93% variations in dependent variable Y are explained by the independent variables present in our model.
#Here R2 is low thus only 13% of variations in dependent variable Y are explained by the independent variabeles x1,x2
#But F statistic is large so adding 
#F-statistic is a good indicator of whether there is a relationship between our predictor and the response variables.
# F tells us how much our model has improved compared to the null model
# R squared on the contrary tells us how much of the variance in the data is explained by the model
# RSS tells us how wrong our predictions are from the true dependent variables
fit3<-lm(y~x1)
summary(fit3)
summary(fit1)
#coefficient of x is close to 0.4 (positive) in the first case and -0.4 in the second. Though it may seem that x1 flips its effect, this happens because the first model includes x2 too and the inclusion of x2 causes this flip in sign (effect of x1 given x2 is negative). But when x2 is not included in the prediction then the coefficient has a positive effect indicating that the slope(trend) is positive 
# Rsquared is low
# mvtnorm package
#rmvnorm can input correlation matrix
#summary$coefficients: get summary characteristics  can index it like a dictionary
#Tukey anscode is random means statistical asuumptions are satisfied
library(ISLR)
data("Carseats")
shelveloc=Carseats$ShelveLoc
sales=Carseats$Sales
advertising=Carseats$Advertising
fit<-lm(sales~shelveloc+advertising)
summary(fit)
bad<- levels(shelveloc)[1]==shelveloc # levels(shelveloc)[1] is bad ;  bad vector is true whenever the level is bad in shelveloc
#levels arranged in alphabetical order
#2a
good<-levels(shelveloc)[2]==shelveloc
medium<-levels(shelveloc)[3]==shelveloc
a1=medium*1 #convert boolean to integer
a2=good*1
fit_a<-lm(sales~a1+a2+advertising) # note both 0 indicate bad and if good 1 then good else medium
summary(fit_a) #same result
max(abs(fitted(fit_a)-fitted(fit)))
plot(advertising,sales)
c=coef(fit_a)
points(advertising[bad],sales[bad],col="red")
abline(c["(Intercept)"],c["advertising"],col="red")
points(advertising[medium],sales[medium],col="orange")
abline(a=c["(Intercept)"]+c["a1"],b=c["advertising"],col="orange")
points(advertising[good],sales[good])
abline(a=c["(Intercept)"]+c["a2"],b=c["advertising"])
legend("bottomright", c("bad","medium","good"),col=c("red","orange","black"), lty=1)
#The summaries and the fitted values are indeed the same. The coefficient of a1 is the
#increase in the intercept for the level medium compared to the level bad and the coefficient 
#of a2 is the increase in the intercept of good compared to bad (concerning shelveloc). 
#The slopes with respect to advertising are the same for all three levels. 
#Intuitively, if we have two child car seats offered at two different locations with the 
#same amount spent on advertising, according to our model we would expect the difference in the sales
#between a seat with medium and a seat with bad shelve location to be equal to the coefficient of a1 and 
#the difference in the sales between a seat with good and a seat with bad shelve location to be equal to the
#coefficient of a2. The following R commands visualize the fitted regression lines for all three levels
#of shelveloc:For fixed advertising, the fitted number of sales is higher by 4.577 sold units for a good shelve location than for a bad shelve location. Similarly, for fixed advertising, the fitted number of sales for a medium shelve 
#location exceed the fitted number of sales for a bad shelve location by about 1.751.
#2b
b1=bad*1
b2=good*1
fit_b<-lm(sales~b1+b2+advertising)# regress on medium 
summary(fit_b)
#2c
c1=bad*1
c2=medium*1
c3=good*1
fit_c<-lm(sales~c1+c2+c3+advertising)
summary(fit_c) #c3 is na
#But we see that c3 has NA values. The reason for this is that the columns of the model 
#matrix in this case are linearly dependent beause we have too many predictors. 
#More precisely, the predictors (intercept), c1, c2 and c3 are linearly dependent
#such that there exist infinitely many possible solutions to the least squares problem.
#To avoid this, we need to fit the model without intercept.
#2d
fit_c<-lm(sales~-1+c1+c2+c3+advertising)
summary(fit_c)
#Now the problem is not present anymore. The coefficient of c1 is the intercept for the fit 
#when the level of shelveloc is bad, similarly the coefficient of c2 is the intercept 
#for the level medium and the coefficient of c3 is the intercept for the level good. 
#Note that for example the difference in intercepts for medium compared to bad is 
#about 6.648 − 4.897 which is equal the coefficient of a1 in subtask a).
#2e
max(abs(fitted(fit_a)-fitted(fit_b)))
max(abs(fitted(fit_a)-fitted(fit_c)))
max(abs(fitted(fit_c)-fitted(fit_b)))
#2g
a1=medium*1+good*1
fit_d=lm(sales~a1+advertising)
summary(fit_d)
anova(fit_d,fit_a)
#Hence, this model is clearly a submodel of the model from part a) such that we can use 
#the partial
#3a
airline <- scan("http://stat.ethz.ch/Teaching/Datasets/airline.dat") #12 months*12 years
plot(airline,type="l")
#The data show an increasing trend, which might be linear. Moreover, there are monthly 
#fluctuations, which get stronger with time. If a linear model would be fitted to these data,
#we could observe the residual variance increasing with time.
#3b
log_a<-log(airline)
plot(log_a,type="l")#the variance reduces
#With logarithmized data, the global trend remains more or less linear, while the monthly 
#fluctuations get stable. The fit of a linear model is much more reasonable here. 
#Taking logarithms or other transformations of the target variable is often a good 
#method to remove monotone trends in the variation. In terms of the original variables,
#this means that a multiplicative model is fitted instead of an additive one (see part e)).
#3c
x1<-rep(c(1,rep(0,11)),12) 
x2<-rep(c(0,1,rep(0,10)),12)
x3<-rep(c(0,0,1,rep(0,9)),12)
x4<-rep(c(0,0,0,1,rep(0,8)),12)
x5<-rep(c(0,0,0,0,1,rep(0,7)),12)
x6<-rep(c(rep(0,5),1,rep(0,6)),12)
x7<-rep(c(rep(0,6),1,rep(0,5)),12)
x8<-rep(c(rep(0,7),1,rep(0,4)),12)
x9<-rep(c(rep(0,8),1,rep(0,3)),12)
x10<-rep(c(rep(0,9),1,rep(0,2)),12)
x11<-rep(c(rep(0,10),1,0),12)
x12<-rep(c(rep(0,11),1),12)
t<-1:144.
fit_air<-lm(log_a~-1+t+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12)
summary(fit_air)
#3d
par(mfrow=c(3,2))
plot(t, airline, type="l", main="original data")
plot(t, log(airline), type="l", main="logarithmized data")
## plots of fitted values and residuals
plot(t, fitted(fit_air), type="l", main="fitted data")
plot(t, residuals(fit_air), type="l", main="residuals")
## two artificial normal datasets to compare
s=summary(fit_air)
plot(t, rnorm(144,sd=s$sigma), type="l", main="simulated normal data")
plot(t, rnorm(144,sd=s$sigma), type="l", main="more simulated normal data")
#The relevant plots are shown below. For the model assumptions we observe that 
#departures from normality, heteroscedasticity of variances or violations of 
#linearity are not clearly visible, but the residuals seem to be correlated. 
#Some experience is needed to assess such plots. One possibility to acquire such 
#experience is to take a look at artificial data generated according to the model 
#which we want to check (i.i.d. normally distributed residuals). You may compare the actual
#residuals with the two plots from artificial data. Since there seems to be serial 
#correlation (violation of model assumptions), the standard errors and p-values are not valid.
#adding 12 exp((t+12)*0.0100688)*exp(...)
#changes by 12*0.0100688 therefore as the amount of increase per year is roughly 
#12*0.0100688
t=t+12
plot(t,exp(fitted(fit_air)))
#yes the linear assumptions hold
#Encoding in the seasons
months<-rep(c(1,2,3,4,5,6,7,8,9,10,11,12),12)
s1<-(3==months)+(4==months)+(5==months)
s1
s2<-(6==months)+(7==months)+(8==months)
s3<-(9==months)+(10==months)+(11==months)
s4<-(12==months)+(1==months)+(2==months)
fit_air_season<-lm(log_a~-1+t+s1+s2+s3+s4)
anova(fit_air_season,fit_air)
#The partial F-test is significant, i.e. the larger model with all monthly 
#predictors is significantly better than the smaller 
#model with only four predictors, one for each season.