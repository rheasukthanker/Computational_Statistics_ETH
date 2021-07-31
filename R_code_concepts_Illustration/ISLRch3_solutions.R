library(MASS)
library(ISLR)
fix(Boston)
names(Boston)
?Boston
attach(Boston)
lm.fit=lm(medv~lstat)
#or 
lm.fit=lm(medv~lstat,data=Boston)
lm.fit
summary(lm.fit)
#What does lm.fit contain
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
#Prediction and confidence intervals
predict(lm.fit,interval = "confidence")
dim(Boston)
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="prediction")
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="confidence")
#confidence and prediction intervals are centered around the same point but the prediction intervals are wider
par(mfrow=c(1,1))
plot(lstat,medv)
abline(lm.fit)
#abline(intercept,slope)
abline(lm.fit,lwd=3)#line width
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red",pch="+")
plot(1:20,1:20,pch=1:20)
par(mfrow=c(2,2))
plot(lm.fit)
#alternatively
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
#leverage statistics
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))#which.max identify index of the largest element
which.max(c(1,2,3))
#Multiple linear regression
lm.fit<-lm(medv~lstat+age)
summary(lm.fit)
#use all variable
lm.fit<-lm(medv~.,data=Boston)
summary(lm.fit)
#Residual standard error
summary(lm.fit)$sigma
library(car)
vif(lm.fit)
#All except one 
lm.fit1<-lm(medv~.-age,data=Boston)
summary(lm.fit1)
#or
lm.fit1=update(lm.fit,~.-age)
#Interaction terms
summary(lm(medv~lstat*age,data=Boston))
#Non linear transform of predictors
lm.fit2<-lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
#Anova
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)#less discernible pattern
#polyfit
lm.fit5<-lm(medv~poly(lstat,5))
summary(lm.fit5)
plot(lm.fit5)
#log transform
summary(lm(medv~log(rm),data=Boston))
#Qualitative predictors
fix(Carseats)
names(Carseats)
attach(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
contrasts(ShelveLoc)
print("Hola Amigos")
#Ch 3 exercises
?Auto
attach(Auto)
fit1<-lm(mpg~horsepower)
summary(fit1)
out=predict(fit1,data.frame(horsepower=c(98)))
out
#confint
predict(fit1,data.frame(horsepower=c(98)),interval = "confidence")
predict(fit1,data.frame(horsepower=c(98)),interval = "prediction")
par(mfrow=c(1,1))
plot(horsepower,mpg)
abline(fit1,col="red",lwd="3")
par(mfrow=c(2,2))
plot(fit1)
#Based on the residuals plots, there is some evidence of non-linearity.
#Question 9
pairs(Auto)
head(Auto)
cor(Auto[,1:8])
fit_all<-lm(mpg~.-name,data=Auto)
summary(fit_all)
#Yes, there is a relatioship between the predictors and the response by testing the null hypothesis of whether all the regression coefficients are zero. The F -statistic is far from 1 (with a small p-value), indicating evidence against the null hypothesis.

#Looking at the p-values associated with each predictor’s t-statistic, we see that displacement, weight, year, and origin have a statistically significant relationship, while cylinders, horsepower, and acceleration do not.
#The regression coefficient for year, 0.7508, suggests that for every one year, mpg increases by the coefficient. In other words, cars become more fuel efficient every year by almost 1 mpg / year.
par(mfrow=c(2,2))
plot(fit_all)
#because there is a discernible curve pattern to the residuals plots. From the leverage plot, point 14 appears to have high leverage, although not a high magnitude residual.
plot(predict(fit_all), rstudent(fit_all))
fit_int<-lm(mpg~.-name+displacement:cylinders+displacement:weight,data=Auto)
summary(fit_int)
#From the correlation matrix, I obtained the two highest correlated pairs and used them in picking my interaction effects. From the p-values, we can see that the interaction between displacement and weight is statistically signifcant, while the interactiion between cylinders and displacement is not.
par(mfrow=c(2,2))
fit_1=lm(mpg~I(log(horsepower))+I(horsepower^2)+I(sqrt(horsepower))+I(log(weight))+I(weight^2)+I(sqrt(weight)))
summary(fit_1)
lm.fit3 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)
lm.fit2<-lm(log(mpg)~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
summary(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
#Question 10
attach(Carseats)
fit<-lm(Sales~Price+Urban+US)
summary(fit)
#Price
#The linear regression suggests a relationship between price and sales given the low p-value of the t-statistic. The coefficient states a negative relationship between Price and Sales: as Price increases, Sales decreases.

#UrbanYes
#The linear regression suggests that there isn’t a relationship between the location of the store and the number of sales based on the high p-value of the t-statistic.

#USYes
#The linear regression suggests there is a relationship between whether the store is in the US or not and the amount of sales. The coefficient states a positive relationship between USYes and Sales: if the store is in the US, the sales will increase by approximately 1201 units.
#Fit smaller
fit2<-lm(Sales~Price+US)
summary(fit2)
#Based on the RSE and R^2 of the linear regressions, they both fit the data similarly, with linear regression from (e) fitting the data slightly better.
confint(fit2)
plot(predict(lm.fit2), rstudent(lm.fit2))
par(mfrow=c(2,2))
plot(lm.fit2)
#xceed (p+1)/n (0.0076) on the leverage-statistic plot that suggest that the corresponding points have high leverage.
#Question 11
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
fit<-lm(y~-1+x)
summary(fit)
#b
fit2<-lm(x~-1+y)
summary(fit2)
#Both results in (a) and (b) reflect the same line created in 11a. In other words, y=2x+ϵ could also be written x=0.5∗(y−ϵ).
(sqrt(length(x)-1) * sum(x*y)) / (sqrt(sum(x*x) * sum(y*y) - (sum(x*y))^2))
#If you swap t(x,y) as t(y,x), then you will find t(x,y) = t(y,x)
fit<-lm(y~x)
summary(fit)
fit2<-lm(x~y)
summary(fit2)
#You can see the t-statistic is the same for the two linear regressions.
#Question 12
#When the sum of the squares of the observed y-values are equal to the sum of the squares of the observed x-values.
#12b 
x<-rnorm(100)
y<-4*x+rnorm(100)
fit1=lm(y~x)
fit2<-lm(x~y)
summary(fit1)
summary(fit2)
sum(x^2)
sum(y^2)
x<-rnorm(100)
y<--sample(x,100)
fit1<-lm(y~x)
fit2<-lm(x~y)
summary(fit1)
summary(fit2)
#Question 13
set.seed(1)
x<-rnorm(100)
eps<-rnorm(100,mean=0,sd=0.25)
y=-1+0.5*x+eps
fit<-lm(y~x)
summary(fit)
par(mfrow=c(1,1))
plot(x,y)
#and y with a positive slope, with a variance as is to be expected.
#The linear regression fits a model close to the true value of the coefficients as was constructed. The model has a large F-statistic with a near-zero p-value so the null hypothesis can be rejected.
abline(fit,lwd="2",col=1)
abline(-1,0.5,lwd="2",col=2)
legend(-1, legend = c("model fit", "pop. regression"), col=c(1,2), lwd=2)
par(mfrow=c(2,2))
plot(fit)
fit<-lm(y~x+I(x^2))
summary(fit)
confint(fit)
#There is evidence that model fit has increased over the training data given the slight increase in R2 and RSE. Although, the p-value of the t-statistic suggests that there isn’t a relationship between y and x2.
#Decreasing variance
set.seed(1)
x<-rnorm(100)
eps<-rnorm(100,mean=0,sd=0.01)
y=-1+0.5*x+eps
fit<-lm(y~x)
confint(fit)
summary(fit)
par(mfrow=c(1,1))
plot(x,y)
#and y with a positive slope, with a variance as is to be expected.
#The linear regression fits a model close to the true value of the coefficients as was constructed. The model has a large F-statistic with a near-zero p-value so the null hypothesis can be rejected.
abline(fit,lwd="2",col=1)
abline(-1,0.5,lwd="2",col=2)
legend(-1, legend = c("model fit", "pop. regression"), col=c(1,2), lwd=2)
par(mfrow=c(2,2))
plot(fit)
fit<-lm(y~x+I(x^2))
summary(fit)
confint(fit)
# As expected, the error observed in R2 and RSE decreases considerably.
#Increase noise variance
set.seed(1)
x<-rnorm(100)
eps<-rnorm(100,mean=0,sd=1)
y=-1+0.5*x+eps
fit<-lm(y~x)
summary(fit)
par(mfrow=c(1,1))
plot(x,y)
#and y with a positive slope, with a variance as is to be expected.
#The linear regression fits a model close to the true value of the coefficients as was constructed. The model has a large F-statistic with a near-zero p-value so the null hypothesis can be rejected.
abline(fit,lwd="2",col=1)
abline(-1,0.5,lwd="2",col=2)
legend(-1, legend = c("model fit", "pop. regression"), col=c(1,2), lwd=2)
par(mfrow=c(2,2))
plot(fit)
fit<-lm(y~x+I(x^2))
summary(fit)
confint(fit)
# As expected, the error observed in R2 and RSE increases considerably.
#All intervals seem to be centered on approximately 0.5, with the second fit’s interval being narrower than the first fit’s interval and the last fit’s interval being wider than the first fit’s interval.
#Collinearity problem
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
par(mfrow=c(1,1))
plot(x1,x2)#linear relation
fit<-lm(y~x1+x2)
summary(fit)
cor(x1,x2)
#The regression coefficients are close to the true coefficients, although with high standard error. We can reject the null hypothesis for β1 because its p-value is below 5%. We cannot reject the null hypothesis for β2 because its p-value is much above the 5% typical cutoff, over 60%.
fit<-lm(y~x1)
summary(fit)
#Yes, we can reject the null hypothesis for the regression coefficient given the p-value for its t-statistic is near zero.
fit<-lm(y~x2)
summary(fit)
#Yes, we can reject the null hypothesis for the regression coefficient given the p-value for its t-statistic is near zero.
#No, because x1 and x2 have collinearity, it is hard to distinguish their effects when regressed upon together. When they are regressed upon separately, the linear relationship between y and each predictor is indicated more clearly.
x1<-c(x1,0.1)
x2<-c(x2,0.8)
y=c(y,6)
par(mfrow=c(1,1))
plot(x1,x2)#linear relation
fit<-lm(y~x1+x2)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
cor(x1,x2)
#The regression coefficients are close to the true coefficients, although with high standard error. We can reject the null hypothesis for β1 because its p-value is below 5%. We cannot reject the null hypothesis for β2 because its p-value is much above the 5% typical cutoff, over 60%.
fit<-lm(y~x1)
summary(fit)
plot(fit)
#Yes, we can reject the null hypothesis for the regression coefficient given the p-value for its t-statistic is near zero.
fit<-lm(y~x2)
summary(fit)
plot(fit)
#In the first model, it shifts x1 to statistically insignificance and shifts x2 to statistiscal significance from the change in p-values between the two linear regressions.
#In the first and third models, the point becomes a high leverage point.
#outlier for model 2 y~x1
names(Boston)
summary(Boston)
attach(Boston)
lm.zn = lm(crim~zn)
summary(lm.zn) # yes
lm.indus = lm(crim~indus)
summary(lm.indus) # yes
lm.chas = lm(crim~chas) 
summary(lm.chas) # no
lm.nox = lm(crim~nox)
summary(lm.nox) # yes
lm.rm = lm(crim~rm)
summary(lm.rm) # yes
lm.age = lm(crim~age)
summary(lm.age) # yes
lm.dis = lm(crim~dis)
summary(lm.dis) # yes
lm.rad = lm(crim~rad)
summary(lm.rad) # yes
lm.tax = lm(crim~tax)
summary(lm.tax) # yes
lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio) # yes
lm.black = lm(crim~black)
summary(lm.black) # yes
lm.lstat = lm(crim~lstat)
summary(lm.lstat) # yes
lm.medv = lm(crim~medv)
summary(lm.medv) # yes
lm.all = lm(crim~., data=Boston)
summary(lm.all)
#zn, dis, rad, black, medv
par(mfrow=c(1,1))
x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.all)[2:14]
plot(x, y)
#coefficient for nox is approximately -10 in univariate model and 31 in multiple regression model.
lm.zn = lm(crim~poly(zn,3))
summary(lm.zn) # 1, 2
lm.indus = lm(crim~poly(indus,3))
summary(lm.indus) # 1, 2, 3
lm.nox = lm(crim~poly(nox,3))
summary(lm.nox) # 1, 2, 3
cor(Boston)
