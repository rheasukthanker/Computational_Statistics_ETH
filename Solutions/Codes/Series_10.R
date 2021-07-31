#Exercise 10
#for GAMs
install.packages("gam")
install.packages("FNN")
library(gam)
library(FNN)
set.seed(1)
xtrain <- matrix(rnorm(20*100),ncol=20, nrow = 100)
#response in training set
ytrain <- sin(2*xtrain[,1]) + 0.3*rnorm(100)#non-lenearity, only relevant predictor is first predictor
# training set
dtrain <- data.frame(xtrain,y = ytrain)
# predictors in test set
xtest <- matrix(rnorm(20*100),ncol=20, nrow = 100)
# response in test set
ytest <- sin(2*xtest[,1]) + 0.3*rnorm(100)
# test set
dtest <- data.frame(xtest,y = ytest)
#Fit on the training set each of the three models (KNN, multiple linear regression and GAM with splines)
#with only the first predictor. For the GAM you will use a spline with degree of freedom 4
#(function s(..., 4) in the formula) and for the KNN regression you will vary the parameter 
#k (number of nearest neighbors) between 1 and 10 (fit 10 KNN). Record the test mean-squared
#error for each approach and discuss your results.

#Multiple linear regression
fit_lm=lm(ytrain~xtrain[,1])
test_predictions=predict(lm(ytrain~xtrain[,1]),data.frame(xtest[,1]))# required data.frame
test_predictions
MSE_lm=sum((ytest-test_predictions)^2)/100
MSE_lm

#Using KNN
MSEs=rep(NA,10)
for (i in c(1:10))
{
dim(ytrain)
knn_fit=knn.reg(train=as.data.frame(xtrain[,1]),test=as.data.frame(xtest[,1]),ytrain, k = i, algorithm=c("kd_tree",
                                                    "cover_tree", "brute"))
knn_preds=knn_fit$pred
MSE_knn=sum((ytest-knn_preds)^2)/100
MSEs[i]=MSE_knn
}
MSEs
#Using GAMs
?glm
?gam
?s
#fit_gam=gam(ytrain~s(xtrain[,1],df=4))
test_predictions=gam.s(xtrain[,1],ytrain,df=4,spar=0.2,xeval=data.frame(xtest[,1]))
MSE_gam=sum((ytest-test_predictions)^2)/100
MSE_gam
#Apply again the same procedure as in b), but now add incrementally additional predictors 
#in the fit that are not associated with the response (columns 2 to 20 in the dataset). 
#Record the test mean-squared error for each possible number m of additional predictor 
#between 1 (one additional predictors) and 19 (all predictors used in the fit), for each 
#of the 12 modelling approaches (GAM with splines of degree 4 for each predictor, multiple 
#linear regression and 10 KNN fits with varying parameter k between 1 and 10).

#Add covariates incrementally
#Linear model
MSE_lm=rep(NA,19)
#only 2
fit_lm=lm(ytrain~xtrain[,2])
test_predictions=predict(lm(ytrain~xtrain[,2]),data.frame(xtest[,2]))# required data.frame
test_predictions
MSE_lm1=sum((ytest-test_predictions)^2)/100
MSE_lm[1]=MSE_lm1
#using 2 and 3
fit_lm=lm(ytrain~xtrain[,2]+xtrain[,3])
test_predictions=predict(lm(ytrain~xtrain[,2]+xtrain[,3]),data.frame(xtest[,2:3]))# required data.frame
test_predictions
MSE_lm2=sum((ytest-test_predictions)^2)/100
MSE_lm[2]=MSE_lm2
#using 2,3 and 4
fit_lm=lm(ytrain~xtrain[,2]+xtrain[,3]+xtrain[,4])
test_predictions=predict(lm(ytrain~xtrain[,2]+xtrain[,3]+xtrain[,4]),data.frame(xtest[,2:4]))# required data.frame
test_predictions
MSE_lm3=sum((ytest-test_predictions)^2)/100
MSE_lm[3]=MSE_lm3
MSE_lm
#using 2,3,4,5
fit_lm=lm(ytrain~xtrain[,2]+xtrain[,3]+xtrain[,4]+xtrain[,5])
test_predictions=predict(lm(ytrain~xtrain[,2]+xtrain[,3]+xtrain[,4]+xtrain[,5]),data.frame(xtest[,2:5]))# required data.frame
test_predictions
MSE_lm4=sum((ytest-test_predictions)^2)/100
MSE_lm[4]=MSE_lm4
MSE_lm
#using 2,3,4,5,6
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:6]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:6]))# required data.frame
test_predictions
MSE_lm5=sum((ytest-test_predictions)^2)/100
MSE_lm[5]=MSE_lm5
MSE_lm
# using 2,3,4,5,6,7
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:7]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:7]))# required data.frame
test_predictions
MSE_lm6=sum((ytest-test_predictions)^2)/100
MSE_lm[6]=MSE_lm6
MSE_lm
#using 2,3,4,5,6,7,8
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:8]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:8]))# required data.frame
test_predictions
MSE_lm8=sum((ytest-test_predictions)^2)/100
MSE_lm[7]=MSE_lm8
MSE_lm
#using 2,3,4,5,6,7,8,9
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:9]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:9]))# required data.frame
test_predictions
MSE_lm8=sum((ytest-test_predictions)^2)/100
MSE_lm[8]=MSE_lm8
MSE_lm
#using 2,3,4,5,6,7,8,9,10
i=10
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:i]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:i]))# required data.frame
test_predictions
MSE_lm8=sum((ytest-test_predictions)^2)/100
MSE_lm[i-1]=MSE_lm8
MSE_lm
i=i+1
#till 11
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:i]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:i]))# required data.frame
test_predictions
MSE_lm8=sum((ytest-test_predictions)^2)/100
MSE_lm[i-1]=MSE_lm8
MSE_lm
i=i+1
#till12
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:i]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:i]))# required data.frame
test_predictions
MSE_lm8=sum((ytest-test_predictions)^2)/100
MSE_lm[i-1]=MSE_lm8
MSE_lm
i=i+1
#till13
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:i]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:i]))# required data.frame
test_predictions
MSE_lm8=sum((ytest-test_predictions)^2)/100
MSE_lm[i-1]=MSE_lm8
MSE_lm
i=i+1
#till14
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:i]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:i]))# required data.frame
test_predictions
MSE_lm8=sum((ytest-test_predictions)^2)/100
MSE_lm[i-1]=MSE_lm8
MSE_lm
i=i+1
#till 15
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:i]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:i]))# required data.frame
test_predictions
MSE_lm8=sum((ytest-test_predictions)^2)/100
MSE_lm[i-1]=MSE_lm8
MSE_lm
i=i+1
#till 16
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:i]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:i]))# required data.frame
test_predictions
MSE_lm8=sum((ytest-test_predictions)^2)/100
MSE_lm[i-1]=MSE_lm8
MSE_lm
i=i+1
#till 17
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:i]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:i]))# required data.frame
test_predictions
MSE_lm8=sum((ytest-test_predictions)^2)/100
MSE_lm[i-1]=MSE_lm8
MSE_lm
i=i+1
#till 18
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:i]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:i]))# required data.frame
test_predictions
MSE_lm8=sum((ytest-test_predictions)^2)/100
MSE_lm[i-1]=MSE_lm8
MSE_lm
i=i+1
#till 19
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:i]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:i]))# required data.frame
test_predictions
MSE_lm8=sum((ytest-test_predictions)^2)/100
MSE_lm[i-1]=MSE_lm8
MSE_lm
i=i+1
#till 20
fit_lm=lm(ytrain~.,dat=as.data.frame(xtrain[,2:i]))#shortcut
test_predictions=predict(fit_lm,as.data.frame(xtest[,2:i]))# required data.frame
test_predictions
MSE_lm8=sum((ytest-test_predictions)^2)/100
MSE_lm[i-1]=MSE_lm8
MSE_lm
i=i+1
#Fit Knn
MSEs_knn=matrix(rep(NA,190),ncol=19)
for (i in c(1:10))
{ for (j in c(2:20))
  {#dim(ytrain)
  knn_fit=knn.reg(train=as.data.frame(xtrain[,2:j]),test=as.data.frame(xtest[,2:j]),ytrain, k = i, algorithm=c("kd_tree",
                                                                                                           "cover_tree", "brute"))
  knn_preds=knn_fit$pred
  MSE_knn=sum((ytest-knn_preds)^2)/100
  MSEs_knn[i,j-1]=MSE_knn
}
  }
MSEs_knn
#Fit GAM
?glm
?gam
?ns
MSEs=rep(0,19)
#only2
j=2
test_predictions=gam.s(xtrain[,2],ytrain,df=4,spar=0.2,xeval=data.frame(xtest[,2]))
MSE_gam=sum((ytest-test_predictions)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
#2,3
j=j+1
?gam
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:3]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2,3,4
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:5
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:6
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:7
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:8
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7])+s(xtrain[,8]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:9
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7])+s(xtrain[,8])+s(xtrain[,9]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:10
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7])+s(xtrain[,8])+s(xtrain[,9])+s(xtrain[,10]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:11
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7])+s(xtrain[,8])+s(xtrain[,9])+s(xtrain[,10])+s(xtrain[,11]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:12
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7])+s(xtrain[,8])+s(xtrain[,9])+s(xtrain[,10])+s(xtrain[,11])+s(xtrain[,12]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:13
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7])+s(xtrain[,8])+s(xtrain[,9])+s(xtrain[,10])+s(xtrain[,11])+s(xtrain[,12])+s(xtrain[,13]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:14
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7])+s(xtrain[,8])+s(xtrain[,9])+s(xtrain[,10])+s(xtrain[,11])+s(xtrain[,12])+s(xtrain[,13])+s(xtrain[,14]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:15
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7])+s(xtrain[,8])+s(xtrain[,9])+s(xtrain[,10])+s(xtrain[,11])+s(xtrain[,12])+s(xtrain[,13])+s(xtrain[,14])+s(xtrain[,15]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:16
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7])+s(xtrain[,8])+s(xtrain[,9])+s(xtrain[,10])+s(xtrain[,11])+s(xtrain[,12])+s(xtrain[,13])+s(xtrain[,14])+s(xtrain[,15])+s(xtrain[,16]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:17
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7])+s(xtrain[,8])+s(xtrain[,9])+s(xtrain[,10])+s(xtrain[,11])+s(xtrain[,12])+s(xtrain[,13])+s(xtrain[,14])+s(xtrain[,15])+s(xtrain[,16])+s(xtrain[,17]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:18
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7])+s(xtrain[,8])+s(xtrain[,9])+s(xtrain[,10])+s(xtrain[,11])+s(xtrain[,12])+s(xtrain[,13])+s(xtrain[,14])+s(xtrain[,15])+s(xtrain[,16])+s(xtrain[,17])+s(xtrain[,18]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:19
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7])+s(xtrain[,8])+s(xtrain[,9])+s(xtrain[,10])+s(xtrain[,11])+s(xtrain[,12])+s(xtrain[,13])+s(xtrain[,14])+s(xtrain[,15])+s(xtrain[,16])+s(xtrain[,17])+s(xtrain[,18])+s(xtrain[,19]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs
#2:20
j=j+1
gam_fit=gam(ytrain~s(xtrain[,2])+s(xtrain[,3])+s(xtrain[,4])+s(xtrain[,5])+s(xtrain[,6])+s(xtrain[,7])+s(xtrain[,8])+s(xtrain[,9])+s(xtrain[,10])+s(xtrain[,11])+s(xtrain[,12])+s(xtrain[,13])+s(xtrain[,14])+s(xtrain[,15])+s(xtrain[,16])+s(xtrain[,17])+s(xtrain[,18])+s(xtrain[,19])+s(xtrain[,20]))
gam_pred=predict(gam_fit,as.data.frame(xtest[,2:j]))
MSE_gam=sum((ytest-gam_pred)^2)/100
MSE_gam
MSEs[j-1]=MSE_gam
MSEs

#############
set.seed(1)
n <- 100
# first dataset
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
x4 <- rnorm(n)
y <- 0.7 + x1 + 2*x2 + 0.5*x3 -3*x4 + rnorm(n)
# second dataset
x1_cor <- rnorm(n)
x2_cor <- rnorm(n)
x3_cor <- rnorm(n)
x4_cor <- x1_cor + 0.4*rnorm(n)
y_cor <- x1_cor + 2*x2_cor + 0.5*x3_cor -3*x4_cor + rnorm(n)
#####
#Backfitting algorithm
backfit<-function(a1,a2,a3,a4,y1)
{ actual_coefficients=lm(y1~a1+a2+a3+a4)$coeff
  b0=actual_coefficients[1]
  b1=actual_coefficients[2]
  b2=actual_coefficients[3]
  b3=actual_coefficients[4]
  b4=actual_coefficients[5]
  mean_y=mean(y1)
  g1=rep(0,n)
  g2=rep(0,n)
  g3=rep(0,n)
  g4=rep(0,n)
  beta1=0
  for(i in c(1:10000)){
  new_response=y-mean_y-g2-g3-g4
  fit1=lm(new_response~a1)
  coeff= fit1$coeff
  alpha1=coeff[1]
  beta1=coeff[2]
  g1=alpha1+beta1*a1
  new_response=y-mean_y-g1-g3-g4
  fit2=lm(new_response~a2)
  coeff= fit2$coeff
  alpha2=coeff[1]
  beta2=coeff[2]
  g2=alpha2+beta2*a2
  new_response=y-mean_y-g1-g2-g4
  fit3=lm(new_response~a3)
  coeff= fit3$coeff
  alpha3=coeff[1]
  beta3=coeff[2]
  g3=alpha3+beta3*a3
  new_response=y-mean_y-g1-g2-g3
  fit4=lm(new_response~a4)
  coeff= fit4$coeff
  alpha4=coeff[1]
  beta4=coeff[2]
  g4=alpha4+beta4*a4
  beta0=mean_y+alpha1+alpha2+alpha3+alpha4}
  return (c(beta0,beta1,beta2,beta3,beta4))
  
}
a=backfit(x1,x2,x3,x4,y)
a
a=backfit(x1_cor,x2_cor,x3_cor,x4_cor,y_cor)
a
#Backfit_CMUalg
backfit<-function(in1,in2,in3,in4,y_out)
{
  c_y=y_out-mean(y_out)
  c_x1=in1-mean(in1)
  c_x2=in2-mean(in2)
  c_x3=in3-mean(in3)
  c_x4=in4-mean(in4)
  beta_hat=rep(0,4)
  alpha=rep(0,4)
  delta=0.0001
  for (i in c(1:1000)){
      c_y=c_y-beta_hat[2]*c_x2-beta_hat[3]*c_x3-beta_hat[4]*c_x4
      fit1=lm(c_y~c_x1)$coeff
      alpha[1]=fit1[2]
      beta_hat[1]=alpha[1]
      c_y=c_y-beta_hat[1]*c_x1-beta_hat[3]*c_x3-beta_hat[4]*c_x4
      fit1=lm(c_y~c_x2)$coeff
      alpha[2]=fit1[2]
      beta_hat[2]=alpha[2]
      c_y=c_y-beta_hat[1]*c_x1-beta_hat[2]*c_x2-beta_hat[4]*c_x4
      fit1=lm(c_y~c_x3)$coeff
      alpha[3]=fit1[2]
      beta_hat[3]=alpha[3]
      c_y=c_y-beta_hat[1]*c_x2-beta_hat[2]*c_x2-beta_hat[3]*c_x3
      fit1=lm(c_y~c_x4)$coeff
      alpha[4]=fit1[2]
      beta_hat[4]=alpha[4]
      
  }
  beta0=mean(c_y)-beta_hat[1]*mean(c_x1)-beta_hat[2]*mean(c_x2)-beta_hat[3]*mean(c_x3)-beta_hat[4]*mean(c_x4)
  return(c(beta0,beta_hat[1],beta_hat[1],beta_hat[3],beta_hat[4]))
}

set.seed(1)
n <- 100
# first dataset
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
x4 <- rnorm(n)
y <- 0.7 + x1 + 2*x2 + 0.5*x3 -3*x4 + rnorm(n)
# second dataset
x1_cor <- rnorm(n)
x2_cor <- rnorm(n)
x3_cor <- rnorm(n)
x4_cor <- x1_cor + 0.4*rnorm(n)
y_cor <- 1+x1_cor + 2*x2_cor + 0.5*x3_cor -3*x4_cor + rnorm(n)
coeffs1=backfit(x1,x2,x3,x4,y)
