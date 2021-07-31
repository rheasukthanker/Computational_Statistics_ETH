#Bias variance tradeoff
#Actual model is linear
#Fit linear
x_train=rnorm(50)

y_train=4*x_train+rnorm(50)
x_test=rnorm(50)
y_test=4*x_test+rnorm(50)
fit_lin<-lm(y_train~x_train)
predict_train=predict(fit_lin)
mset1=mean((predict_train-y_train)^2)
predict_test<-predict(fit_lin,data.frame(x=x_test))
mse1=mean((predict_test-y_test)^2)
mse1
#Fit quadratic
fit_quad<-lm(y_train~x_train+I(x_train^2))
predict_train=predict(fit_quad)
mset2=mean((predict_train-y_train)^2)
mset2
predict_test<-predict(fit_quad,data.frame(x_test,x_test^2))
mse2=mean((predict_test-y_test)^2)
mse2
#Fit 10 degree
?poly
fit_cubic<-lm(y_train~poly(x_train,degree=10))
summary(fit_cubic)
predict_train=predict(fit_cubic)
mset3=mean((predict_train-y_train)^2)
mset3
predict_test<-predict(fit_cubic,data.frame(x_test,x_test^2,x_test^3,x_test^4,x_test^5,x_test^6,x_test^7,x_test^8,x_test^9,x_test^10))
mse3=mean((predict_test-y_test)^2)
mse3
#Plotting
par(mfrow=c(1,1))
plot(c(1,1),c(mset1,mse1),xlim=range(1:4),ylim=range(0:50),col="red")
points(c(2,2),c(mset2,mse2),col="black")
points(c(3,3),c(mset3,mse3),col="blue")
lines(c(mset1,mset2,mset3))
lines(c(mse1,mse2,mse3))
which.min(c(mse1,mse2,mse3))
##Bias variance tradeoff
#Actual model is quadratic
#Fit linear
x_train=rnorm(5000)
y_train=4*x_train+3*x_train^2+rnorm(5000)
x_test=rnorm(50)
y_test=4*x_test+3*x_test^2+rnorm(50)
fit_lin<-lm(y_train~x_train)
predict_train=predict(fit_lin)
mset1=mean((predict_train-y_train)^2)
predict_test<-predict(fit_lin,data.frame(x=x_test))
mse1=mean((predict_test-y_test)^2)
mse1
#Fit quadratic
fit_quad<-lm(y_train~x_train+I(x_train^2))
predict_train=predict(fit_quad)
mset2=mean((predict_train-y_train)^2)
mset2
predict_test<-predict(fit_quad,data.frame(x_test,x_test^2))
mse2=mean((predict_test-y_test)^2)
mse2
#Fit 10 degree
?poly
fit_cubic<-lm(y_train~poly(x_train,degree=10))
summary(fit_cubic)
predict_train=predict(fit_cubic)
mset3=mean((predict_train-y_train)^2)
mset3
predict_test<-predict(fit_cubic,data.frame(x_test,x_test^2,x_test^3,x_test^4,x_test^5,x_test^6,x_test^7,x_test^8,x_test^9,x_test^10))
mse3=mean((predict_test-y_test)^2)
mse3
#Plotting
par(mfrow=c(1,1))
plot(c(1,1),c(mset1,mse1),xlim=range(1:4),ylim=range(0:100),col="red")
points(c(2,2),c(mset2,mse2),col="black")
points(c(3,3),c(mset3,mse3),col="blue")
lines(c(mset1,mset2,mset3))
lines(c(mse1,mse2,mse3))
which.min(c(mse1,mse2,mse3))

