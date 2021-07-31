p=2
n=500
g1<-function(x){2*x/(1+abs(x)^1.5)}
g2<-function(x){x^3/abs(x)^1.5}
g3<-function(x){x}
x<-runif(n*p,min=-1,max=1)
Z<-matrix(x,ncol=p)
par(mfrow=c(1,3))
Xg1<-g1(Z)
Xg2<-g2(Z)
Xg3<-g3(Z)
plot(Xg1)
plot(Xg2)#centered around zero
plot(Xg3)#uniform
sampleX<-function(n=500){
x=runif(n*p,min=-1,max=1)
Z<-matrix(x,ncol=p)
Xg1<-g1(Z)
return(Xg1)}
f1dim<-function(x){ sin(8*x)/(1+(4*x)^2) }
f<-function(X){
  return(f1dim(X[,1]))
}
f(Xg1)
library("kknn")
sampleY<-function(X){ return(f(X)+rnorm(dim(X)[1],sd=0.3))}
res=rep(0,1000)
for(i in 1:1000){
Xtrain<-sampleX(500)
Ytrain<-sampleY(Xtrain)
dfTrain=data.frame(y=Ytrain,x=Xtrain)
Xtest<-sampleX(2000) 
Ytest<-sampleY(Xtest)
dfTest=data.frame(x=Xtest)
fit.kknn <- kknn(y ~., dfTrain,dfTest,k=8)
predTest=predict(fit.kknn)
res[i] <- mean((predTest-Ytest)^2)
}
act=mean(res)
par(mfrow=c(1,1))
hist(res)
abline(v=act, col="red",lwd=2)
#Validation
sampleY<-function(X){ return(f(X)+rnorm(dim(X)[1],sd=0.3))}
ValidationSet<-function(X,Y){
  n<-length(Y)
  s <- sample(1:n, size=n, replace=F)
  folds <- cut(seq(1,n), breaks=2, labels=FALSE)
  ind.test <- s[which(folds==1)]
  dfTrain=data.frame(y=Y[ind.test],x=X[ind.test,])
  dfTest=data.frame(x=X[-ind.test,])
  fit.kknn <- kknn(y ~ ., dfTrain,dfTest,k=8)
  predTest=predict(fit.kknn)
  Ytest<-Y[-ind.test]
  MSEEstimate=mean((predTest-Ytest)^2)
  return(MSEEstimate)
}
X <- sampleX()
Y <- sampleY(X)
ValidationSet(X,Y)
RepeatedValidationSet<-function(X,Y){
  MSEEstimate <- replicate(10, ValidationSet(X,Y))
  return(mean(MSEEstimate))
}
RepeatedValidationSet(X,Y)
act
#10-fold cross validation 

tencv<-function(X,Y){
  tencv<-rep(0,10)
  n<-length(Y)
  s <- sample(1:n, size=n, replace=F)
  folds <- cut(seq(1,n), breaks=10, labels=FALSE)
  for(i in 1:10 ){
  ind.test <- s[which(folds==i)]
  dfTrain=data.frame(y=Y[-ind.test],x=X[-ind.test,])
  dfTest=data.frame(x=X[ind.test,])
  fit.kknn <- kknn(y ~ ., dfTrain,dfTest,k=8)
  predTest=predict(fit.kknn)
  Ytest<-Y[ind.test]
  MSEEstimate=mean((predTest-Ytest)^2)
  tencv[i]=MSEEstimate
  }
  return(mean(tencv))}
tencv(X,Y)
RepeatedTenFoldCV<-function(X,Y){
  MSEEstimate <- replicate(10, tencv(X,Y))
  return(mean(MSEEstimate))
}
RepeatedTenFoldCV(X,Y)
LOOCV<-function(X,Y){
  n <- length(Y)
  MSEEstimate <- numeric(n)
  for (i in 1:n) {
    dfTrain=data.frame(y=Y[-i],x=X[-i,])
    dfTest=data.frame(x=matrix(X[i,],nrow=1))
    fit.kknn <- kknn(y ~ ., dfTrain,dfTest,k=8)
    predTest=predict(fit.kknn)
    Ytest<-Y[i]
    MSEEstimate[i]<-(predTest-Ytest)^2
  }
  return(mean(MSEEstimate))
}
LOOCV(X,Y)
act
EvaluateOnSimulation<-function(estimationFunction, iterations=200){
  result<-numeric(iterations)
  for (i in 1:iterations) {
    X<-sampleX()
    Y<-sampleY(X)
    result[i]= estimationFunction(X,Y)
  }
  return(result)
}
EstimatesVS <- EvaluateOnSimulation(ValidationSet)
EstimatesrepVS <- EvaluateOnSimulation(RepeatedValidationSet)
Estimatestencv <- EvaluateOnSimulation(tencv)
Estimatesreptencv <- EvaluateOnSimulation(RepeatedTenFoldCV)
Estimatesloocv <- EvaluateOnSimulation(LOOCV)
Estimates <- cbind(EstimatesVS,EstimatesrepVS,Estimatestencv,Estimatesreptencv,Estimatesloocv ) #results from the 5 CV methods
boxplot(Estimates)
abline(h=act,col="red")
bias_vs=mean(EstimatesVS)-act
bias_vs
var_vs=var(EstimatesVS)
var_vs
bias_vsrep=mean(EstimatesrepVS)-act
bias_vsrep
var_vsrep=var(EstimatesrepVS)
var_vsrep
bias_tencv=mean(Estimatestencv)-act
bias_tencv
var_tencv=var(Estimatestencv)
var_tencv
bias_reptencv=mean(Estimatesreptencv)-act
bias_reptencv
var_reptencv=var(Estimatesreptencv)
var_reptencv
bias_loocv=mean(Estimatesloocv)-act
bias_loocv
var_loocv=var(Estimatesloocv)
var_loocv
biases=c(bias_vs,bias_vsrep,bias_tencv,bias_reptencv,bias_loocv)
caption<-c("Val. Set", "Repeated Val. Set", "10 Fold CV", "Repeated 10 Fold CV","Leave-one-out")

names(biases)=caption
barplot(biases, main="Bias for estimation of expected test MSE")
variances<-c(var_vs,var_vsrep,var_tencv,var_reptencv,var_loocv)
names(variances)=caption
barplot(variances, main="Variance for estimation of expected test MSE")
msemse<-biases^2+variances
names(msemse)=caption
barplot(msemse, main="MSE for estimation of expected test MSE")
tencv_mod<-function(X,Y){
  tencv<-rep(0,10)
  n<-length(Y)
  s <- sample(1:n, size=n, replace=F)
  folds <- cut(seq(1,n), breaks=10, labels=FALSE)
  for(i in 1:10 ){
    ind.test <- s[which(folds==i)]
    dfTrain=data.frame(y=Y[-ind.test],x=X[-ind.test,])
    dfTest=data.frame(x=X[ind.test,])
    fit.kknn <- kknn(y ~ ., dfTrain,dfTest,k=8)
    predTest=predict(fit.kknn)
    Ytest<-Y[ind.test]
    MSEEstimate=mean((predTest-Ytest)^2)
    tencv[i]=MSEEstimate
  }
  return(return(var(tencv)/10))}
tencv_mod(X,Y)
