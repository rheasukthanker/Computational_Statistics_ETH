#Series 8
# for replication
set.seed(1)
n <- 30
p <- 50
# relevant covariate
x_true <- sample(c(0:1),size = n,replace = T)
# noise covariates
x <- matrix(sample(c(0:1),size=n*p,replace = T),ncol=p,nrow=n)
# combination of the two
x <- cbind(x_true, x)
# response
y <- ifelse(x[,1]==0, 0, sample(c(0:1), size = n, replace = T))
#Bonferroni correction
?chisq.test
#p=50
p_bon=0.05/p
count=0
for (j in 1:10){
p_vals_ind=rep(NA,p)
for(i in 1:p)
{
  pval=chisq.test(x[,i],y)#,simulate.p.value = TRUE,B=2000) 
  p_vals_ind[i]=pval$p.value
}
print(p_vals_ind[p_vals_ind<=p_bon])
}
#westfall young permutation
min=rep(NA,1000)
for ( i in c(1:1000))
{
  sample_y=sample(y,length(y),replace=TRUE)
  p_vals_ind=rep(NA,p)
  for(j in 1:p)
  {
    pval=chisq.test(x[,j],sample_y,simulate.p.value = TRUE,B=2000) 
    p_vals_ind[j]=pval$p.value
  }
  min[i]=min(p_vals_ind)
}
?quantile
q=quantile(min,probs=0.05)
q
for (j in 1:10){
  p_vals_ind=rep(NA,p)
  for(i in 1:p)
  {
    pval=chisq.test(x[,i],y,simulate.p.value = TRUE,B=2000) 
    p_vals_ind[i]=pval$p.value
  }
  #print(p_vals_ind)
  print(p_vals_ind[p_vals_ind<q])
}
#Question 2
require(ISLR)
data("Hitters")
na.omit(Hitters)
Hitters=Hitters[complete.cases(Hitters),]
dim(Hitters)
folds <- cut(seq(1,nrow(Hitters)),breaks=10,labels=FALSE)
errar=rep(NA,10)
errcp=rep(NA,10)
errbic=rep(NA,10)
errcv=rep(NA,10)
library(leaps)
for(i in c(1:10))
{
  testIndexes <- which(folds==i,arr.ind=TRUE)
  #testData <- Hitters[testIndexes, ]
  testData= model.matrix(Salary~.,data=Hitters[testIndexes,])
  trainData <- Hitters[-testIndexes, ]
  regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
  #best model AdjR
  reg.summary=summary(regfit.full)
  min_adjr2=which.max(reg.summary$adjr2)
  coeffs_min=coef(regfit.full,min_adjr2)
  pred = testData[,names(coeffs_min)]%*%coeffs_min
  errar[i] = mean((Hitters$Salary[testIndexes]-pred)^2)
  #Cp
  #reg.summary=summary(regfit.full)
  min_cp=which.min(reg.summary$cp)
  coeffs_min=coef(regfit.full,min_cp)
  #print(names(coeffs_min))
  pred = testData[,names(coeffs_min)]%*%coeffs_min
  errcp[i] = mean((Hitters$Salary[testIndexes]-pred)^2) 
  #bic
  min_bic=which.min(reg.summary$bic)
  coeffs_min=coef(regfit.full,min_bic)
  pred = testData[,names(coeffs_min)]%*%coeffs_min
  errbic[i] = mean((Hitters$Salary[testIndexes]-pred)^2)  
  #cv
  folds <- cut(seq(1,nrow(trainData)),breaks=10,labels=FALSE)
  cv_mat=matrix(nrow=10,ncol=length(reg.summary$cp))
  for( j in c(1:10))
  {   testIndexes2 <- which(folds==j,arr.ind=TRUE)
      testData2 <- model.matrix(Salary~.,data=Hitters[testIndexes2,])
      trainData2 <- Hitters[-testIndexes2, ]
      regfit.full=regsubsets(Salary~.,data=trainData2,nvmax=19)
      reg.summary=summary(regfit.full)
      for( k in c(1:length(reg.summary$bic)))
      {
        coeffs=coef(regfit.full,k)
        pred = testData2[,names(coeffs)]%*%coeffs
        cv_mat[j,k] = mean((Hitters$Salary[testIndexes2]-pred)^2)
      }
  }
  cv_mean=colMeans(cv_mat)
  min_cv=which.min(cv_mean)
  coeffs_min=coef(regfit.full,min_cv)
  #print(min_cv)
  pred = testData[,names(coeffs_min)]%*%coeffs_min
  errcv[i] = mean((Hitters$Salary[testIndexes]-pred)^2) 
}
mean(errbic)
#Exercise 3
install.packages("hdi")
require("hdi")
?riboflavin
data(riboflavin)
head(riboflavin)
install.packages("glmnet")
grid <- 10^seq(10,-2, length = 100)
library("glmnet")
?glmnet
attach(riboflavin)
#Lasso
fit_lasso=glmnet(riboflavin$x,riboflavin$y,alpha=1,lambda=grid)
#summary(fit_lasso)
dim(coef(fit_lasso))
fit_lasso$lambda[100] # =grid[50]
coef(fit_lasso)[,50]
train=sample(1:nrow(riboflavin$x), nrow(riboflavin$x)/2) 
X=riboflavin$x
y=riboflavin$y
cv.out = cv.glmnet(X[train,],y[train],alpha=1)
par(mfrow=c(1,1))
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam  # 0.02732276
#Ridge
fit_ridge=glmnet(riboflavin$x,riboflavin$y,alpha=0,lambda=grid)
dim(coef(fit_ridge))
fit_ridge$lambda[100] # =grid[50]
coef(fit_lasso)[,50]
train=sample(1:nrow(riboflavin$x), nrow(riboflavin$x)/2) 
X=riboflavin$x
y=riboflavin$y
cv.out = cv.glmnet(X[train,],y[train],alpha=0)
par(mfrow=c(1,1))
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam  # 8.59012
#Cross validation which lasso or ridge is best in this case
n=dim(riboflavin)[1]
folds=cut(c(1:n),breaks=10,labels=FALSE)
mse_lasso=rep(NA,10)
mse_ridge=rep(NA,10)
g=sample(nrow(riboflavin))
riboflavin=riboflavin[g,]
X=riboflavin$x
y=riboflavin$y
for(i in c(1:10))
{
  test_inds=folds[folds==i]
  test_X=X[test_inds,]
  test_y=y[test_inds]
  train_X=X[-test_inds,]
  train_y=y[-test_inds]
  cv_lasso= cv.glmnet(train_X,train_y,alpha=1)
  cv_ridge= cv.glmnet(train_X,train_y,alpha=0)
  best_las=cv_lasso$lambda.min
  best_ridge=cv_ridge$lambda.min
  fit_lasso=glmnet(train_X,train_y,alpha=1,lambda=grid)
  fit_ridge=glmnet(train_X,train_y,alpha=0,lambda=grid)
  lasso.pred=predict(fit_lasso,s=best_las,newx=test_X)
  mse_lasso[i]=mean((lasso.pred-test_y)^2)
  ridge.pred=predict(fit_ridge,s=best_ridge,newx=test_X)
  mse_ridge[i]=mean((ridge.pred-test_y)^2)
}
mean(mse_lasso)
mean(mse_ridge)
