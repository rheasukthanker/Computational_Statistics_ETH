#Series 2 revision
#Question 1
set.seed(0)
n<-100
z1<-rnorm(n)
z2=rnorm(n)
M=matrix(c(1,1,0.1,-0.1),2,2)
X=t(M%*%rbind(z1,z2))
beta<-c(0.5,-1)
x1=X[,1]
x2=X[,2]
y=5+beta[1]*x1+beta[2]*x2+rnorm(n)
#a)Create a plot of the x1 and x2
par(mfrow=c(1,3))
plot(x1,x2)
plot(x1,y)
plot(x2,y)
#b)Fit a linear model 
fit1<-lm(y~x1+x2)
summary(fit1)#both the coefficients are not significant
s=summary(fit1)
#t value of beta 1
rse=sqrt(sum(s$residuals^2)/(n-3))
X_full=cbind(rep(1,n),x1,x2)
cov_beta=solve(t(X_full)%*%X_full)*(rse^2)#square because rse is estimate of the standard deviation
cov_beta
t_beta1=coefficients(fit1)[2]/sqrt(cov_beta[2,2])
t_beta1
#t value of beta0
t_beta0=coefficients(fit1)[1]/sqrt(cov_beta[1,1])
t_beta0
#t values of beta2
t_beta2=coefficients(fit1)[3]/sqrt(cov_beta[3,3])
t_beta2
#Definition of p value
#Probability of observing a value equal to a  given statistic (or greater) when the null hypothesis is actually true. Intuitively if such a probability is low we reject the null hypothesis 
#p value of beta 1
?pt
#not df=n-3
quant0<-2*pt(abs(t_beta0),df=n-3,lower.tail = FALSE)
quant0
quant2<-2*pt(abs(t_beta2),df=n-3,lower.tail = FALSE)
quant2
quant1<-2*pt(abs(t_beta1),df=n-3,lower.tail = FALSE)
quant1
#e P value of the overall F test is 6.222
#calculating f statistic by hand
TSS=sum((y-mean(y))^2)
RSS=sum(s$residuals^2)
F_stat_num=(TSS-RSS)/2
F_stats_deno=RSS/(n-3)
F_stat=F_stat_num/F_stats_deno
F_stat
# F stat by anova
fit_null=lm(y~1)
an=anova(fit_null,fit1)
an
# f refer series paper
#g  
summary(fit1)
RSE=sqrt(RSS/(n-3))
RSE
#R squared calculation
R_sq=1-(RSS/TSS)
R_sq
#Adjusted r squared
Adj_r_sq=1-(RSS/TSS)*((n-1)/(n-3))
Adj_r_sq
#i 
#Observed only x1 and y, x2 is hidden 
fit3<-lm(y~x1)
summary(fit3)
summary(fit1)
#The coefficient has a different sign than before. In both models, this is the amount by which the fitted values change if x1 is increased by 1. However, in the first model, we fix x2 whereas in the second model, there is no such second predictor. The“effect”of a certain predictor depends on the specified model.
#Question 2
library(ISLR)
data(Carseats)
?Carseats
sales=Carseats$Sales
shelveloc=Carseats$ShelveLoc
advertising=Carseats$Advertising
#fit using automatic coding by lm
fit<-lm(sales~shelveloc+advertising)
summary(fit)
#a1 is 1 when the level of shelveloc is medium and a2 is 1 if level is good
shelveloc
#a1 a2 comb 0 0 bad 1 0 medium and 0 1 Good
a1=1*(shelveloc=="Medium")
a2=1*(shelveloc=="Good")
fit_a<-lm(sales~a1+a2+advertising)
summary(fit_a)
summary(fit)#same 
max(abs(fitted(fit_a)-fitted(fit)))
#Interpretation 
#The summaries and the fitted values are indeed the same. The coefficient of a1 is the increase in the intercept for the level medium compared to the level bad and the coefficient of a2 is the increase in the intercept of good compared to bad (concerning shelveloc). The slopes with respect to advertising are the same for all three levels. Intuitively, if we have two child car seats offered at two different locations with the same amount spent on advertising, according to our model we would expect the difference in the sales between a seat with medium and a seat with bad shelve location to be equal to the coefficient of a1 and the difference in the sales between a seat with good and a seat with bad shelve location to be equal to the coefficient of a2
#.
bad=shelveloc=="Bad"
par(mfrow=c(1,1))
plot(advertising,sales)
c=coef(fit_a)
points(advertising[bad],sales[bad],col="red")
abline(a=c["(Intercept)"],b=c["advertising"],col="red")
medium=shelveloc=="Medium"
points(advertising[medium],sales[medium],col="orange")
abline(a=c["(Intercept)"]+c["a1"],b=c["advertising"],col="orange")
good=shelveloc=="Good"
points(advertising[good],sales[good])
abline(a=c["(Intercept)"]+c["a2"],b=c["advertising"])
legend("bottomright", c("bad","medium","good"),col=c("red","orange","black"), lty=1)
#levels function
levels(shelveloc)# alphabetical order
#different coding scheme
b1=1*(shelveloc=="Bad")
b2=1*(shelveloc=="Good")
fit_b=lm(sales~b1+b2+advertising)
summary(fit_b)
summary(fit_a)
#The coefficient of b1 is the increase in the intercept for the level bad compared to the level medium and the coefficient of b2 is the increase in the intercept of good compared to medium (concerning shelveloc). Again, the slopes with respect to advertising are the same for all three levels. Intuitively, if we have two child car seats offered at two different locations with the same amount spent on advertising, according to our model we would expect the difference in the sales between a seat with bad and a seat with medium shelve location to be equal to the coefficient of b1 and the difference in the sales between a seat with good and a seat with medium shelve location to be equal to the coefficient of b2.
#Constrast coding 3
c1=1*(shelveloc=="Bad")
c2=1*(shelveloc=="Medium")
c3=1*(shelveloc=="Good")
fit_c<-lm(sales~c1+c2+c3+advertising)
summary(fit_c)
#Problem because the covariates at this point become linearly dependent because bad=1 can be inferred from medium and good
#d
#Remove intercept
#int=rep(-1,dim(Carseats)[1])
fit_c<-lm(sales~-1+c1+c2+c3+advertising)
summary(fit_c)
#Now the problem is not present anymore. The coefficient of c1 is the intercept for the fit when the level of shelveloc is bad, similarly the coefficient of c2 is the intercept for the level medium and the coefficient of c3 is the intercept for the level good. Note that for example the difference in intercepts for medium compared to bad is about 6.648 − 4.897 which is equal the coefficient of a1 in subtask a).
max(abs(fitted(fit_a)-fitted(fit_b)))
max(abs(fitted(fit_a)-fitted(fit_c)))
max(abs(fitted(fit_b)-fitted(fit_c)))
summary(fit_a)
summary(fit_b)
#This can be seen in the summary of fit_b.
#If we drop predictor b2, we do not distinguish anymore
#between the levels medium and good. This means we only have to consider 
#the p-value corresponding to b2 which is smaller than 2e-16, which is smaller
#than 0.05. We conclude that we should distinguish between medium and good.
dummy_d=a1+a2
fit_d=lm(sales~dummy_d+advertising)
anova(fit_a,fit_d)
#partial f test is highly significant so we do need to distinguidh between good adn medium
#Question 3
airline<-scan("http://stat.ethz.ch/Teaching/Datasets/airline.dat")
airline
num_months=length(airline)
num_months#(60-49+1)*12
(60-49+1)*12
plot(airline,type="l")# relationship seems non linear 
#The data show an increasing trend, which might be linear. Moreover, there are monthly fluctuations, which get stronger with time. If a linear model would be fitted to these data, we could observe the residual variance increasing with time.
plot(log(airline),type="l") #relationship transforms to a linear one
#With logarithmized data, the global trend remains more or less linear, while the monthly fluctuations get stable. The fit of a linear model is much more reasonable here. Taking logarithms or other transformations of the target variable is often a good method to remove monotone trends in the variation. In terms of the original variables, this means that a multiplicative model is fitted instead of an additive one (see part e)).
predictor_matrix=matrix(rep(0,12*length(airline)),length(airline),12)
for(i in c(1:length(airline)))
{ ind=i%%12
 if (ind==1)
 {
   predictor_matrix[i,1]=1
 }
else if(ind==2)
{
  predictor_matrix[i,2]=1
}
else if(ind==3)
{
  predictor_matrix[i,3]=1
}
else if(ind==4)
{
  predictor_matrix[i,4]=1
}
else if(ind==5)
{
  predictor_matrix[i,5]=1
}
else if(ind==6)
{
  predictor_matrix[i,6]=1
}
else if(ind==7)
{
  predictor_matrix[i,7]=1
}
else if(ind==8)
{
  predictor_matrix[i,8]=1
}
else if(ind==9)
{
  predictor_matrix[i,9]=1
}
else if(ind==10)
{
  predictor_matrix[i,10]=1
}
else if(ind==11)
{
  predictor_matrix[i,11]=1
}
else
{
  predictor_matrix[i,12]=1
}
}
t<-1:144
fit_air<-lm(log(airline)~-1+t+predictor_matrix)
summary(fit_air)
residuals=summary(fit_air)$residuals
plot(residuals)
plot(fitted(fit_air))
predictor_matrix
par(mfrow=c(3,2))
plot(t, airline, type="l", main="original data")
plot(t, log(airline), type="l", main="logarithmized data")
## plots of fitted values and residuals
plot(t, fitted(fit_air), type="l", main="fitted data")
plot(t, residuals(fit_air), type="l", main="residuals")
## two artificial normal datasets to compare
s=summary(fit_months)
plot(t, rnorm(144,sd=s$sigma), type="l", main="simulated normal data")
plot(t, rnorm(144,sd=s$sigma), type="l", main="more simulated normal data")
#The relevant plots are shown below. For the model assumptions we observe that departures from normality, heteroscedasticity of variances or violations of linearity are not clearly visible, but the residuals seem to be correlated. Some experience is needed to assess such plots. One possibility to acquire such experience is to take a look at artificial data generated according to the model which we want to check (i.i.d. normally distributed residuals). You may compare the actual residuals with the two plots from artificial data. Since there seems to be serial correlation (violation of model assumptions), the standard errors and p-values are not valid.
#We have used that x(t+12)j = xtj for all j ∈ {1, · · · , 12} because if we increase the month index by 12, the same month indicator will be active (one year has 12 months). This means that if we increase
#t by 12, the fitted values are multiplied by exp(12β). Hence we have a multiplicative instead of an 􏰲
#Question f
predictor_matrix=matrix(rep(0,4*length(airline)),length(airline),4)
for(i in c(1:length(airline)))
{ ind=i%%12
if (ind==9|ind==10|ind==11)
{
  predictor_matrix[i,3]=1
}
else if(ind==6|ind==7|ind==8)
{
  predictor_matrix[i,2]=1
}
else if(ind==3|ind==4|ind==5)
{
  predictor_matrix[i,1]=1
}
else if(ind==0|ind==1|ind==2)
{
  predictor_matrix[i,4]=1
}


}
predictor_matrix
fit_season<-lm(log(airline)~-1+t+predictor_matrix)
summary(fit_season)
anova(fit_air,fit_season)
s1<-rep(c(rep(0,2),rep(1,3),rep(0,7)),12) 
s2<-rep(c(rep(0,5),rep(1,3),rep(0,4)),12) 
s3<-rep(c(rep(0,8),rep(1,3),rep(0,1)),12) 
s4<-rep(c(1,1,rep(0,9),1),12)
fit_season<-lm(log(airline)~-1+t+s1+s2+s3+s4)
summary(fit_season)
