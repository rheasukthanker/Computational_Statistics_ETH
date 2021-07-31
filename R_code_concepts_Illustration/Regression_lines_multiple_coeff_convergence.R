#Simulate random X,Y from true(linear) model,plot different regression lines,
#Does average coeff converge? Change variance of of noise ?how does this change
#the multiple lines plotted?
#sd=0.4
par(mfrow=c(1,1))
x=rnorm(50,mean=2,sd=4)
y=10+2*x
plot(x,y,type="l",col="red")
coef1=rep(0,5)
coef2=rep(0,5)
for(i in c(1:5))
{
y=10+2*x+rnorm(50,sd=0.4)
fit1=lm(y~x)
coeffs=coefficients(fit1)
coef1[i]=coeffs[1]
coef2[i]=coeffs[2]
lines(x,predict(fit1),col="blue")
}
mean(coef1)
mean(coef2)
#Coefficients convergence on an average?
?coefficients
#sd=1
par(mfrow=c(1,1))
x=rnorm(50,mean=2,sd=4)
y=10+2*x
plot(x,y,type="l",col="red")
coef1=rep(0,5)
coef2=rep(0,5)
for(i in c(1:5))
{
  y=10+2*x+rnorm(50,sd=1)
  fit1=lm(y~x)
  coeffs=coefficients(fit1)
  coef1[i]=coeffs[1]
  coef2[i]=coeffs[2]
  lines(x,predict(fit1),col="blue")
}
mean(coef1)
mean(coef2)
#sd=5
par(mfrow=c(1,1))
x=rnorm(50,mean=2,sd=4)
y=10+2*x
plot(x,y,type="l",col="red")
coef1=rep(0,5)
coef2=rep(0,5)
for(i in c(1:50))
{
  y=10+2*x+rnorm(50,sd=5)
  fit1=lm(y~x)
  coeffs=coefficients(fit1)
  coef1[i]=coeffs[1]
  coef2[i]=coeffs[2]
  lines(x,predict(fit1),col="blue")
}
mean(coef1)
mean(coef2)
#as noise variance increases the  lines deviate more from the original line, 
#also convergence rate to true coefficients is poorer, convergence rate gets better when we plot more regression line

##14)Centered x and check beta_0 should be same as mean
x=c(1,2,3,4,5)
x=x-mean(x)
x
mean(x)
y=3*x+5+rnorm(5)
fit1=lm(y~x)
coefficients(fit1)[1]
mean(y)
#2)RSS variation on adding features to the model
#Rss also tends to decrease on average on adding covariates
rssa=rep(0,47)
for(i in c(2:48))
{
  X=rnorm(50*i)
  X=matrix(X,nrow=50,ncol=i)
  y=6+2*X[,1]+5*X[,2]+rnorm(50)
  fit=lm(y~X)
  pred_y=predict(fit)
  rss=sum((y-pred_y)^2)
  rssa[i-1]=rss
}
plot(c(2:48),rssa)
rssa
#13)More spread out the x's smaller is the standard error of the coefficients
#Spread out
x=c(10,30,40,50,100,200,300,400,500,600)
y=9+6*x+rnorm(10)
fit=lm(y~x)
sum=summary(fit)
sum$coefficients
#Not spread out
x=c(10,15,20,6,21,22,23,24,25,18)
y=9+6*x+rnorm(10)
fit=lm(y~x)
sum=summary(fit)
sum$coefficients
#even more concentrated
x=c(1,1.5,1.25,1.34,1.65,1.73,1.95,1.84,1.57,1.23)
y=9+6*x+rnorm(10)
fit=lm(y~x)
sum=summary(fit)
sum$coefficients
#verifies intuition standard error of coefficient increases when x are not spread out
#27)Non constant variance of error terms/ Heteroscedacity and using log transforms to mitigate it
#25)Simulate own non linear data and check residual plots patterns
x=rnorm(100,mean=5,sd=5)
y=10+6*x+13*x^2+15*x^3+rnorm(100)
fit=lm(y~x)
plot(fit,which=1)#Tukey anscombe shows a clear pattern
#Reduce degree of polynomial
x=rnorm(100,mean=5,sd=5)
y=10+6*x+13*x^2+rnorm(100)
fit=lm(y~x)
plot(fit,which=1)#Tukey anscombe shows a clear pattern, narrower than earlier but still a bowl
#Now linear
x=rnorm(100,mean=5,sd=5)
y=10+6*x+rnorm(100)
fit=lm(y~x)
plot(fit,which=1)#approximately random
#Heteroskedastic errors
x=rnorm(100,mean=5)
dev=rep(0,100)
for (i in c(1:100))
{ dev[i]=2*abs(x[i]) }
y=4+2*x+rnorm(100,sd=dev)
fit=lm(y~x)
plot(fit,which=1)#approximately random
#Correlated covariates
#multiple simulations
for (i in c(1:10)){
X1=rnorm(100)
X2=6+2*X1+rnorm(100)
#X2=rnorm(100)
#X3=rnorm(100)
full_X=cbind(X1,X2)
y=7+2*X1+rnorm(100)
#full_X
fit_cor=lm(y~full_X)
#summary(fit_all)$coefficients
print(cor(full_X))
#Not correlated covariates
#X1=rnorm(100)
#X2=6+2*X1+rnorm(100)
X2=rnorm(100)
#X3=rnorm(100)
full_X=cbind(X1,X2)
#y=7+2*X1+rnorm(100)
#full_X
fit_nocor=lm(y~full_X)
#summary(fit_all)$coefficients
print(cor(full_X))
print(summary(fit_cor)$coefficients)
print(summary(fit_nocor)$coefficients)}
#Almost always standard error is the correlated case is larger
#12)Justify by simulation the more observations we have the smaller the standard error
for(i in c(1:10))
{
#increase n
x=rnorm(5000)
mat=matrix(x,nrow=500,ncol=10)
y=2*mat[,1]+9+rnorm(500)
fit1=lm(y[1:100]~mat[1:100,])
print(summary(fit1)$coefficients)
print(sqrt(sum((summary(fit1)$residuals)^2)/89))
fit2=lm(y~mat)
print(summary(fit2)$coefficients)
print(sqrt(sum((summary(fit2)$residuals)^2)/489))
}
#summary(fit1)
#sqrt(sum(fit1$residuals^2)/500)
#Standard error of the coefficients almost always lower when n increases
print(length(y[101:500]))
###ASK does rse always decrease when we increase the sample size
#Calculating F statistic by hand
x=rnorm(1000)
X=matrix(x,nrow=100,ncol=10)
y=4+6*X[,4]+rnorm(100)
fit1=lm(y~X)
summary(fit1)$fstatistic
#Calculation by hand
y_pred=predict(fit1)
f_num=sum((y_pred-mean(y))^2)/10
f_deno=sum((y_pred-y)^2)/89
F_stat=f_num/f_deno#matched the value returned by lm
F_stat
#Calculate predictions by hand 
X_new=cbind(rep(1,100),X)
y_pred2=X_new%*%solve(t(X_new)%*%(X_new))%*%t(X_new)%*%y
#y_pred=predict(fit1)
f_num=sum((y_pred2-mean(y))^2)/10
f_deno=sum((y_pred2-y)^2)/89
F_stat=f_num/f_deno#matched the value returned by lm
F_stat
#Calculation of predictions by hand do not forget to add the intercept column
#22)Verify intuition prediction intervals are always wider than confidence intervals
X=matrix(rnorm(500),nrow=50,ncol=10)
y=8+2*X[,1]+6*X[,3]+9*X[,4]+rnorm(50)
fit_1<-lm(y~X)
#Confidence intervals 
s=summary(fit_1)
rse=sqrt(sum(s$residuals^2)/(50-11))
rse
#Confidence intervals of y
predict.lm(fit_1,interval ="prediction",level=0.95)
y_pred_upr=predict(fit_1)-qt(0.975,39)*rse
y_pred_upr
