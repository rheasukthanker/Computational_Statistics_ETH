#Series 9
#P-value distribution under null
?seq
NCols=10
NRows=200
library(boot)
my_seq_x<-matrix(rnorm(NCols*NRows), ncol=NCols) 
?replicate
a=replicate(1000,c(NA))
#Under null uniform distribution holds for heteroskedastic errors too?
for( i in 1:1000)
{ beta0=2
  #beta1=8
  #alt
  #y=beta0+beta1*my_seq_x[,1]+rnorm(10000,0,sd=x^4)
  ##null 
  y=beta0+rnorm(200,0,sd=1)
  #null chisq
  #p-value follows uniform under null regardless of error distribution and heteroskedacity of errors?
  #y=beta0+rchisq(10000,1,mean(my_seq_x))
  linearMod=lm(y~my_seq_x)
  modelSummary <- summary(linearMod)  # capture model summary as an object
  modelCoeffs <- modelSummary$coefficients  # model coefficients
  beta.estimate <- modelCoeffs[2, "Estimate"]  # get beta estimate for speed
  std.error <- modelCoeffs[2, "Std. Error"]  # get std.error for speed
  t_value <- beta.estimate/std.error  # calc t statistic
  p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
  a[i]=p_value
}
a=unlist(a)
hist(a)
#Best subset selection
library(leaps)

NCols=10
NRows=200

my_seq_x<-matrix(rnorm(NCols*NRows), ncol=NCols) 
?replicate
a=replicate(1000,c(NA))
#Under null uniform distribution holds for heteroskedastic errors too?
for( i in 1:1000)
{ beta0=2
#beta1=8
#alt
#y=beta0+beta1*my_seq_x[,1]+rnorm(10000,0,sd=x^4)
##null 
y=beta0+rnorm(200,0,sd=1)
#null chisq
#p-value follows uniform under null regardless of error distribution and heteroskedacity of errors?
#y=beta0+rchisq(10000,1,mean(my_seq_x))
df=data.frame(my_seq_x,y)
regfit.full=regsubsets(y~.,data=df,nvmax=10)
reg.summary=summary(regfit.full)
min_cp=which.min(reg.summary$cp)
coeffs_min=coef(regfit.full,min_cp)
if ("X1" %in% names(coeffs_min))
{
subset=df[, c(names(coeffs_min)[2:length(names(coeffs_min))],"y")]
fit_sub=lm(y~.,data=subset)
modelSummary <- summary(fit_sub)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["X1", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["X1", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
a[i]=p_value}

}
par(mfrow=c(1,1))
a=unlist(a)
a <- a[!is.na(a)]
hist(a)
a
#Best subset selection with caution
library(leaps)

NCols=10
NRows=200

my_seq_x<-matrix(rnorm(NCols*NRows), ncol=NCols) 
?replicate
a=replicate(5000,c(NA))
#Under null uniform distribution holds for heteroskedastic errors too?
for( i in 1:5000)
{ beta0=2
#beta1=8
#alt
#y=beta0+beta1*my_seq_x[,1]+rnorm(10000,0,sd=x^4)
##null 
y=beta0+rnorm(200,0,sd=1)
#null chisq
#p-value follows uniform under null regardless of error distribution and heteroskedacity of errors?
#y=beta0+rchisq(10000,1,mean(my_seq_x))
df=data.frame(my_seq_x,y)
inds=sample(c(1:200),100,replace=FALSE)
df_p1=df[inds,]
df_p2=df[-inds,]
regfit.full=regsubsets(y~.,data=df_p1,nvmax=10)
reg.summary=summary(regfit.full)
min_cp=which.min(reg.summary$cp)
coeffs_min=coef(regfit.full,min_cp)
if ("X1" %in% names(coeffs_min))
{
  subset=df_p2[, c(names(coeffs_min)[2:length(names(coeffs_min))],"y")]
  fit_sub=lm(y~.,data=subset)
  modelSummary <- summary(fit_sub)  # capture model summary as an object
  modelCoeffs <- modelSummary$coefficients  # model coefficients
  beta.estimate <- modelCoeffs["X1", "Estimate"]  # get beta estimate for speed
  std.error <- modelCoeffs["X1", "Std. Error"]  # get std.error for speed
  t_value <- beta.estimate/std.error  # calc t statistic
  p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
  a[i]=p_value}

}
par(mfrow=c(1,1))
a=unlist(a)#more uniform
a <- a[!is.na(a)]
length(a)
hist(a)
a
#Question 4
#fit for cubic and plot
#(a) Use the poly() function to fit a cubic polynomial regression to predict nox using dis. Report the regression output, and plot the resulting data and polynomial fits.
library(ISLR)
library(MASS)
?Boston
data("Boston")
df_4=Boston[,c("dis","nox")]
df_4#dis predictor and nox response
?poly
attach(df_4)
polyfit=lm(nox~poly(dis,4))
summary(polyfit)
?predict
par(mfrow=c(1,2))
plot(dis,predict(polyfit))
line(dis,predict(polyfit))
plot(dis,nox)
x <- with(df_4, seq(min(dis), max(dis), length.out=2000))
y <- predict(polyfit, newdata = data.frame(dis = x))
par(mfrow=c(1,1))
plot(dis,predict(polyfit))
lines(x, y, col = "red")
#fit for all and plot
#(b) Plot the polynomial fits for a range of different polynomial degrees (say, from 1 to 10), and report the associated residual sum of squares
rss=rep(NA,10)


for( i in c(1:10))
{
  polyfit=lm(nox~poly(dis,i))
  #summary(polyfit)
  #?predict
  par(mfrow=c(1,3))
  plot(dis,predict(polyfit))
  plot(dis,nox)
  x <- with(df_4, seq(min(dis), max(dis), length.out=2000))
  y <- predict(polyfit, newdata = data.frame(dis = x))
  plot(dis,nox)
  lines(x, y, col = "red")
  k=length(polyfit$coefficients)-1 #Subtract one to ignore intercept
  SSE=sum(polyfit$residuals**2)
  n=length(polyfit$residuals)
  rss[i]=sqrt(SSE/(n-(1+k))) #Residual Standard Error
}
#Choose degree by CV
#Perform cross-validation or another approach to select the opti- mal degree for the polynomial, and explain your results.
perform_cv<-function(input)
{
  folds <- cut(seq(1,nrow(input)),breaks=10,labels=FALSE)
  shuffle=sample(c(1:n),n,replace=FALSE)
  input=input[shuffle,]
  mat_rss=matrix(nrow=10,ncol=16)
  for(i in c(1:10))
  {
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- input[testIndexes, ]
    trainData <- input[-testIndexes, ]
    for( j in c(3:16))
    { nox=trainData[,"nox"]
      dis=trainData[,"dis"]
      polyfit=lm(nox~poly(dis,j),data=trainData)#create separate nox and dis
      #summary(polyfit)
      #?predict
      #par(mfrow=c(1,3))
      #plot(dis,predict(polyfit))
      #plot(dis,nox)
      #x <- with(df_4, seq(min(dis), max(dis), length.out=2000))
      #y <- predict(polyfit, newdata = data.frame(dis = x))
      #plot(dis,nox)
      #lines(x, y, col = "red")
      k=length(polyfit$coefficients)-1 #Subtract one to ignore intercept
      #new_data_test <- data.frame(dis=testData[,"dis"])
      #m=predict(fit,newdata=test,type="response")
      residuals_fold=predict(polyfit,newdata=testData)
      SSE=sum((testData[,"nox"]-residuals_fold)**2)
      n=length(residuals_fold)
      mat_rss[i,j]=SSE#Residual Standard Error
    }
    
  }
  return(mat_rss)
}
rss_mat<-perform_cv(df_4)[,3:16]
#cm=colMeans(rss_mat)
deg_best=which.min(colMeans(rss_mat))
deg_best# 3
plot(3:16, colMeans(rss_mat), lwd = 2, type = "l", xlab = "df", ylab = "CV error")
#Use splines
#Use the bs() function to fit a regression spline to predict nox using dis. Report the output for the fit using four degrees of freedom. How did you choose the knots? Plot the resulting fit.
?bs
attach(df_4)
require(stats)
require(splines)
require(graphics)
#fit_bs=lm(nox~bs(dis,knots=c(20,30,40)))
fit_bs=lm(nox~bs(dis,df=4))
summary(fit_bs)
par(mfrow=c(1,1))
plot(dis,predict(fit_bs))
x <- with(df_4, seq(min(dis), max(dis), length.out=2000))
y <- predict(fit_bs, newdata = data.frame(dis = x))
par(mfrow=c(1,1))
lines(x,y,col='red')
#lines(dis,predict(fit_bs))
#Range of dfs
rss=rep(NA,16)

#Now fit a regression spline for a range of degrees of freedom, and plot the resulting fits and report the resulting RSS. Describe the results obtained.
for( i in c(3:16))
{
  polyfit=lm(nox~bs(dis,df=i))
  #summary(polyfit)
  #?predict
  par(mfrow=c(1,3))
  plot(dis,predict(polyfit))
  plot(dis,nox)
  x <- with(df_4, seq(min(dis), max(dis), length.out=2000))
  y <- predict(polyfit, newdata = data.frame(dis = x))
  plot(dis,nox)
  lines(x, y, col = "red")
  k=length(polyfit$coefficients)-1 #Subtract one to ignore intercept
  SSE=sum(polyfit$residuals**2)
  n=length(polyfit$residuals)
  rss[i]=SSE #Residual Standard Error
}
par(mfrow=c(1,1))#
plot(3:16, rss[3:16], lwd = 2, type = "l", xlab = "df", ylab = "Train error")
#df best cv
#Choose degree by CV
perform_cv<-function(input)
{ shuffle=sample(c(1:n),n,replace=FALSE)
  input=input[shuffle,]
  folds <- cut(seq(1,nrow(input)),breaks=10,labels=FALSE)
  mat_rss=matrix(nrow=10,ncol=16)
  for(i in c(1:10))
  {
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- input[testIndexes, ]
    trainData <- input[-testIndexes, ]
    count=0
    for( j in c(3:16))
    { nox=trainData[,"nox"]
    dis=trainData[,"dis"]
    polyfit=glm(nox~bs(dis,df=j),data=trainData)#create separate nox and dis
    #summary(polyfit)
    #count=count+1
    #?predict
    #par(mfrow=c(1,3))
    #plot(dis,predict(polyfit))
    #plot(dis,nox)
    #x <- with(df_4, seq(min(dis), max(dis), length.out=2000))
    #y <- predict(polyfit, newdata = data.frame(dis = x))
    #plot(dis,nox)
    #lines(x, y, col = "red")
    k=length(polyfit$coefficients)-1 #Subtract one to ignore intercept
    #new_data_test <- data.frame(dis=testData[,"dis"])
    #m=predict(fit,newdata=test,type="response")
    residuals_fold=predict(polyfit,newdata=testData)
    SSE=sum((testData[,"nox"]-residuals_fold)^2)
    n=length(residuals_fold)
    mat_rss[i,j]=SSE#Residual Standard Error
    }
    
  }
  return(mat_rss)
}
#rss_mat
rss_mat<-perform_cv(df_4)[,3:16]
#cm=colMeans(rss_mat)
deg_best=which.min(colMeans(rss_mat))
deg_best# 1
par(mfrow=c(1,1))
plot(3:16, colMeans(rss_mat), lwd = 2, type = "l", xlab = "df", ylab = "CV error")
all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = glm(nox ~ bs(dis, df = i), data = Boston)
  all.cv[i] = cv.glm(Boston, lm.fit, K = 10)$delta[2]
}
plot(3:16, all.cv[-c(1, 2)], lwd = 2, type = "l", xlab = "df", ylab = "CV error")
all.cv
?cv.glm
