
library(kknn)

#---------------------------
# Cross-validation
#---------------------------


# Programm a complicated looking function that does cross-validation and chooses from
# a set of models:
TenFoldCV <- function(X,Y){
  MSEEstimateFolds.kknn8 <- numeric(10)
  MSEEstimateFolds.kknn6 <- numeric(10)
  MSEEstimateFolds.kknn4 <- numeric(10)
  MSEEstimateFolds.reg<-numeric(10)
  n <- length(Y)
  s <- sample(1:n, size=n, replace=F) # Randomly shuffle the data
  folds <- cut(seq(1,n), breaks=10, labels=FALSE)
  for (i in 1:10) {
    ind.test <- s[which(folds==i)]
    dfTrain=data.frame(y=Y[-ind.test],x=X[-ind.test,])
    dfTest=data.frame(x=X[ind.test,])
    fit.kknn8 <- kknn(y ~ ., dfTrain,dfTest,k=8)  
    fit.kknn6 <- kknn(y ~ ., dfTrain,dfTest,k=6)  
    fit.kknn4 <- kknn(y ~ ., dfTrain,dfTest,k=4)  
    fit.reg <- lm(y ~ ., dfTrain)  
    predTest.kknn8=predict(fit.kknn8)
    predTest.kknn6=predict(fit.kknn6)
    predTest.kknn4=predict(fit.kknn4)
    predTest.reg=predict(fit.reg, dfTest)
    Ytest <- Y[ind.test]
    MSEEstimateFolds.kknn8[i] <- mean((predTest.kknn8-Ytest)^2)
    MSEEstimateFolds.kknn6[i] <- mean((predTest.kknn6-Ytest)^2)
    MSEEstimateFolds.kknn4[i] <- mean((predTest.kknn4-Ytest)^2)
    MSEEstimateFolds.reg[i] <- mean((predTest.reg-Ytest)^2)
  } 
  
  tmp<-data.frame(kknn8=mean(MSEEstimateFolds.kknn8), 
                  kknn6=mean(MSEEstimateFolds.kknn6), 
                  kknn4=mean(MSEEstimateFolds.kknn4),
                  reg=mean(MSEEstimateFolds.reg))
  

  return(list(minMSE=min(tmp), Method=colnames(tmp)[apply(tmp,1,which.max)]))
}


# Generate data
n=1000
d=20
X<-matrix(rnorm(n*d), nrow=n,ncol = d)
Y<-rnorm(n)
# Note that in this example there is no relationship between Y and X; the best model for Y is just the mean
# (you can of course play around with different specifications here)




# choose a method via CV on all of the data
listreturn=TenFoldCV(data.frame(X),Y)




# Try to correctly to evaluate the actual MSE with the double CV.
n <- length(Y)
s <- sample(1:n, size=n, replace=F) # Randomly shuffle the data
folds <- cut(seq(1,n), breaks=10, labels=FALSE)
MSEEstimateFolds <- numeric(10)
for (i in 1:10) {
  
  
  ind.test <- s[which(folds==i)]
  dfTrain=data.frame(y=Y[-ind.test],x=X[-ind.test,])
  dfTest=data.frame(x=X[ind.test,])
  Ytest<-Y[ind.test]
  
  
  # Do model selection on the training data
  listreturn=TenFoldCV(dfTrain[,-1],dfTrain$y)
  
 
  
  # Evaluate
  
  if (listreturn$Method=="kknn8"){
    fit.kknn8 <- kknn(y ~ ., dfTrain,dfTest,k=8)
    predTest.kknn8=predict(fit.kknn8)

    MSEEstimateFolds[i] <- mean((predTest.kknn8-Ytest)^2)
  }
  
  if (listreturn$Method=="kknn6"){
    fit.kknn6 <- kknn(y ~ ., dfTrain,dfTest,k=6)
    predTest.kknn6=predict(fit.kknn6)
    
    MSEEstimateFolds[i] <- mean((predTest.kknn6-Ytest)^2)
  }
  
  if (listreturn$Method=="kknn4"){
    fit.kknn4 <- kknn(y ~ ., dfTrain,dfTest,k=4)
    predTest.kknn4=predict(fit.kknn4)
    
    MSEEstimateFolds[i] <- mean((predTest.kknn4-Ytest)^2)
  }
  
  if (listreturn$Method=="reg"){
    fit.reg <- lm(y ~ ., dfTrain)  
    predTest.reg=predict(fit.reg, dfTest)
    MSEEstimateFolds[i] <- mean((predTest.reg-Ytest)^2)
  }
  
} 

# Estimated MSE of the procedure (Use CV to get MSE for all model than choose the one with the lowest estimated MSE)
mean(MSEEstimateFolds) #(1)




# Easier, and often used in practice (e.g. in kaggle competitions)
n <- length(Y)
s <- sample(1:n, size=n, replace=F) # Randomly shuffle the data
folds <- cut(seq(1,n), breaks=2, labels=FALSE) # Split into training and testing

ind.test <- s[which(folds==2)]
dfTrain=data.frame(y=Y[-ind.test],x=X[-ind.test,])
dfTest=data.frame(x=X[ind.test,])
Ytest<-Y[ind.test]





# Find optimal model with CV on training data
listreturn=TenFoldCV(dfTrain[,-1],dfTrain$y)

# Evaluate the chosen model on test data

if (listreturn$Method=="kknn8"){
  fit.kknn8 <- kknn(y ~ ., dfTrain,dfTest,k=8)
  predTest.kknn8=predict(fit.kknn8)
  
  MSEEstimateFolds <- mean((predTest.kknn8-Ytest)^2)
}

if (listreturn$Method=="kknn6"){
  fit.kknn6 <- kknn(y ~ ., dfTrain,dfTest,k=6)
  predTest.kknn6=predict(fit.kknn6)
  
  MSEEstimateFolds <- mean((predTest.kknn6-Ytest)^2)
}

if (listreturn$Method=="kknn4"){
  fit.kknn4 <- kknn(y ~ ., dfTrain,dfTest,k=4)
  predTest.kknn4=predict(fit.kknn4)
  
  MSEEstimateFolds <- mean((predTest.kknn4-Ytest)^2)
}

if (listreturn$Method=="reg"){
  fit.reg <- lm(y ~ ., dfTrain)  
  predTest.reg=predict(fit.reg, dfTest)
  MSEEstimateFolds <- mean((predTest.reg-Ytest)^2)
}


# Estimated MSE for the final model (should be close to the one above!)
MSEEstimateFolds #(2)





