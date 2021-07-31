url <- "https://ww2.amstat.org/publications/jse/datasets/fruitfly.dat.txt"
data <- read.table(url)
head(data)
data <- data[,c(-1,-6)]
names(data) <- c("partners","type","longevity","thorax")
attach(data)
# define vectors for the colors (col) and plotting characters (pch) > 
col.partn <- 1*(partners==0) + 2*(partners==1) + 3*(partners==8)
# 1=black, 2=red, 3=green
pch.type <- 1*(type==0) + 2*(type==1) + 3*(type==9)
# 1='circle', 2='triangle', 3='plus'
# col.partn
# pch.type
par(mfrow=c(1,1))
plot(thorax, longevity, pch=pch.type, col=col.partn,ylim=range(longevity),xlim=range(thorax))
col_v=c(2,3,2,3,1)
pch_v=c(1,1,2,2,3)
legend("topleft",c("1 pregnant","8 pregnant","1 virgin","8 virgin","0 partners"),pch=pch_v,col=col_v)
# 2 1 - 1 pregant
# 3 1 -8 pregnant
# 2 2 -1 virgin
# 3 2- 8 virgin
# 1 3 zero partnerts
#We can see that larger fruitflies tend to live longer. Furthermore, comparing fruitflies with similar thorax value, fruitflies with 8 virgins tend to live shorter.
#making 3 separate plots
par(mfrow=c(1,1))
plot(thorax[partners==0],longevity[partners==0],pch=3,col=1)
legend("topleft",c("Zero partners"),pch=3,col=1)
partners_1=partners[partners==1]
type_1=type[partners==1]
col_1=2*(partners_1==1)
pch_1=1*(type_1==0)+2*(type_1==1)
plot(thorax[partners==1],longevity[partners==1],pch=pch_1,col=col_1)
legend("topleft",c("1 pregnant","1 virgin"),pch=c(1,2),col=c(2,2))
partners_8=partners[partners==8]
type_8=type[partners==8]
col_1=3*(partners_8==8)
pch_1=1*(type_8==0)+2*(type_8==1)
plot(thorax[partners==8],longevity[partners==8],pch=pch_1,col=col_1)
legend("topleft",c("8 pregnant","8 virgin"),pch=c(1,2),col=c(3,3))
#It seems that male fruitflies with pregnant females tend to live 
#longer than those with virgin females. This difference in lifespan seems to be 
#larger when partners=8 compared to partners=1. Hence, there seems to be an 
#interaction effect between type and partners in their effect on longevity.

#long_g1=longevity[partners==8 & type==0]
par(mfrow=c(1,1))
thorax_g1=thorax[partners==8 & type==0 ]#8 pregnant
#boxplot(thorax_g1)
thorax_g2=thorax[partners==8 & type==1]#8 virgin
#boxplot(thorax_g2)
thorax_g3=thorax[partners==1 & type==0]#1 preg
#boxplot(thorax_g3)
thorax_g4=thorax[partners==1 & type==1]#1 virgin
#boxplot(thorax_g4)
thorax_g5=thorax[partners==0 & type==9]#no partners
#boxplot(thorax_g5)
boxplot(thorax_g1,thorax_g2,
        thorax_g3,thorax_g4,
        thorax_g5,
        main="Thorax length",
        names=c("8p","8v","1p","1v","0"),col="grey")
#Way to create dummies
dummy.1.p <- (partners==1)*(type==0)*1 
dummy.1.v <- (partners==1)*(type==1)*1 
dummy.8.p <- (partners==8)*(type==0)*1 
dummy.8.v <- (partners==8)*(type==1)*1 
dummy.0 <- (partners==0)
fitfull<-lm(thorax~dummy.1.p+dummy.1.v+dummy.8.p+dummy.8.v)
fitintercept<-lm(thorax~1)
anova(fitfull,fitintercept)
#The test is not significant. This was to be expected since the assignments to the groups were random,
#hence the distribution of thorax should be similar among the different 
fitlo<-lm(longevity[partners==1]~factor(type[partners==1]))
summary(fitlo)
fitdum<-lm(longevity[partners==1]~1)
anova(fitlo,fitdum)
fit_witht<-lm(longevity[partners==1]~factor(type[partners==1])+thorax[partners==1])
anova(fit_witht,fitdum)
anova(fitlo,fit_witht)
summary(fit_witht)
#We can see that type is much more significant in the second model which includes thorax.
#The t-value is obtained by dividing the point estimate by the estimate of the standard error.
#Note that the point estimates of the coefficient of type are slightly different in the two models,
#but we will leave this aside, and focus on the standard errors of the estimate. 
#These are 3.456 in the model with thorax and 4.326 in the model without thorax.
#The ratio is 3.456/4.326=0.80.
#The smaller standard error in the model with thorax leads to a larger t-value and
#hence more significant results. Why is the standard error smaller in the model with thorax? 
#The residual standard error is smaller in the model with thorax than in the model without 
#thorax because thorax explains a significant amount of the variation in longevity.
#Moreover, thorax is not much correlated with type, so that we don’t have to worry 
#about large variance inflation factors.
fit1<-lm(longevity~thorax+as.factor(partners)+as.factor(type)+as.factor(partners)*as.factor(type))
summary(fit1)
#We only have 5 different groups of male fruitflies but there are
#9 different combinations of the two three-level categorical predictors 
#type and partners. The combinations (1,9), (8,9), (0,0) and (0,1) 
#for (partners,type) do not appear in the dataset because they do not make sense. 
#This is why we R reports ”Coefficients: (4 not defined because of singularities)”. 
#We need to do the analysis more carefully, see the next subquestion.
t0<-c(type==0)
t1<-c(type==1)
p1<-c(partners==1)
p8<-c(partners==8)
pred_1=t0*p1
pred_2=t0*p8
pred_3=t1*p1
pred_4=t1*p8
fit1_corr<-lm(longevity~thorax+pred_1+pred_2+pred_3+pred_4)
summary(fit1_corr)
fit_gPart <- lm(longevity ~ thorax + I(pred_3 +pred_1) +
                  I(pred_2 + pred_1) +
                  I(pred_4 - pred_1))
summary(fit_gPart)
anova(fit_gPart,fit1_corr)
#The partial F-test shows that the interaction between type and partners is significant.
url <- "https://raw.githubusercontent.com/jawj/coffeestats/master/lifeexp.dat"
data <- read.table(url, sep="\t", header=T, row.names=1)
data <- data[,c("LifeExp","People.per.TV","People.per.Dr")]
attach(data)
par(mfrow=c(1,3))
hist(LifeExp)
hist(People.per.TV)
hist(People.per.Dr)
pairs(data)
data[order(data[,"LifeExp"],decreasing=T)[1:3],]
data[order(People.per.TV,decreasing=T)[1:3],]
data[order(People.per.Dr,decreasing=T)[1:3],]
#The countries with the highest life expectancy are Japan, Italy and Spain, the countries with the highest number of people per TV are Burma, Ethiopia and Bangladesh, the three countries with the highest number of people per doctor are Ethiopia, Tanzania and Zaire.
datanew <-data[complete.cases(data),]
fit<-lm(LifeExp~log2(People.per.Dr)+log2(People.per.TV))
summary(fit)
#In the original scale, the interpretation of the coefficients βtv and βdr of l2tv and l2dr is as fol- lows: If we have two countries that have the same value for People.per.TV whereas the values for People.per.Dr differ by a factor of 2 (i.e. the l2dr values differ by 1) then the predicted values for life expectancy would differ by βdr. Similarly for two countries with the same value for People.per.Dr and a difference in People.per.TV by a factor of 2 (i.e. the l2tv values differ by 1), 
#the predicted values for life expectancy would differ by βtv.
#We cannot conclude that more TVs imply a higher life expectancy because we only 
#have an observa- tional study, which generally does not allow for conclusions about causal relations. However, we can use the number of people per TV to predict life expectancy for a new observation, i.e. country.
t<-order(cooks.distance(fit),decreasing = T)[1:2]
data[t,]
par(mfrow=c(1,1))
plot(fit,which=4)
new_x=c(1,50,300)
pred<-fit$coefficients[1]*new_x[1]+fit$coefficients[2]*new_x[2]+fit$coefficients[3]*new_x[3]
pred
pred_frame=data.frame(People.per.TV=50,People.per.Dr=3000)
predict(fit,pred_frame,level=0.95,interval = 'c')
predict(fit,pred_frame,level=0.95,interval = 'p')
par(mfrow=c(1,1))
plot(fit,which =c(1,2,3,4,5))
#In the Tukey-Anscombe plot, we do not see any clear model violations.
#The QQ plot shows that the distribution of the residuals is somewhat left-skewed, but this is not very severe.
#The scale-location plot shows that the variance of the residuals does not depend on the fitted values. There are no indications of heteroscedasticity (= non-equal error variance).
rownames(data)[c(17,30)]
#There are two countries which have clearly larger Cook’s distance (i.e. have a high impact on the fitted regression plane) than the others: North Korea and Sudan.
#In the above residuals versus leverage plot the red dotted lines indicate the levels 0.5 and 1 of the Cook’s distance. We can observe that North Korea has a very large standardized residual, leverage and Cook’s distance, such that we should analyze how the confidence and prediction intervals change if we exclude this observation.
fit<-lm(LifeExp[-c(17,30)]~log2(People.per.Dr[-c(17,30)])+log2(People.per.TV[-c(17,30)]))
predict(fit,pred_frame,level=0.95,interval = 'c')
predict(fit,pred_frame,level=0.95,interval = 'p')
#We see that the values for the confidence and prediction intervals do not differ too much 
#from the ones in part d). This is a good sign, since we do not want our results to depend 
#heavily on only two observations.
f <- function(x){ .3* x - 0.2*x^2 + 0.1*x^3 + sin(2*x) }
span <- c(0.1,0.2,0.3,0.45,0.7,1)
sigma <- 1.5
n <- 100
grid<-seq(-5,5,length=100)
x <- seq(from=-5,to=5, length=n)
xtest<-2
par(mfrow=c(2,3))
for(i in 1:length(span)){
  plot(x,f(x), type="l", lwd=2, main=paste("alpha=",span[i]))
  for (j in 1:25){
  y <- f(x) + rnorm(n=length(x),mean=0,sd=sigma)
  lo <- loess(y ~ x, span=span[i])
  lines(x, predict(object=lo, x),col="gray")}
  abline(v=xtest, lty=3)
}
#We can observe that for small values of span (complex models), there is more variance in the fitted values at xtest. This is because small alpha (bandwidth), only a close neighbourhood of samples with x-values around alpha are involved in the predicted value of the loess smoother at xtest. Therefore, the noise plays a greater role such that the variance of the predictions is larger. The larger the value of span the more data points in the x-neighbourhood of xtest are involved in the predicted value of the loess smoother such that the noise is not as important as for smaller values of span. At the same time, we have a positive bias at x=xtest because the smoother uses points with x-values in the neighbourhood that have mainly larger function values f(x) (i.e. larger expectation for y). This is the bias-variance trade-off. If we increase the span for loess, the variance decreases 
#but at the same time the squared bias increases.
sigma<-1.5
n<-20
x <- seq(from=-5,to=5, length=n)
par(mfrow=c(2,3))
for(i in 1:length(span)){
  plot(x,f(x), type="l", lwd=2, main=paste("alpha=",span[i]))
  for (j in 1:25){
    y <- f(x) + rnorm(n=length(x),mean=0,sd=sigma)
    lo <- loess(y ~ x, span=span[i])
    lines(x, predict(object=lo, x),col="gray")}
  abline(v=xtest, lty=3)
}
#increase sigma
sigma<-4
n<-100
x <- seq(from=-5,to=5, length=n)
par(mfrow=c(2,3))
for(i in 1:length(span)){
  plot(x,f(x), type="l", lwd=2, main=paste("alpha=",span[i]))
  for (j in 1:25){
    y <- f(x) + rnorm(n=length(x),mean=0,sd=sigma)
    lo <- loess(y ~ x, span=span[i])
    lines(x, predict(object=lo, x),col="gray")}
  abline(v=xtest, lty=3)
}
#increase both
sigma<-4
n<-1000
x <- seq(from=-5,to=5, length=n)
par(mfrow=c(2,3))
for(i in 1:length(span)){
  plot(x,f(x), type="l", lwd=2, main=paste("alpha=",span[i]))
  for (j in 1:25){
    y <- f(x) + rnorm(n=length(x),mean=0,sd=sigma)
    lo <- loess(y ~ x, span=span[i])
    lines(x, predict(object=lo, x),col="gray")}
  abline(v=xtest, lty=3)
}
#LOESS uses local polynomial regression based on αn observations. A larger n leads to more precise estimates. Increasing sigma leads of course to a larger variation in the fitted values.
nsim <- 1000
xtest <- 2
sigma <- 1.5
n <- 100
# number of simulations
# x-value of test point
grid<-seq(-5,5,length=n)
x <- seq(from=-5,to=5, length=n)
fit.test <- matrix(rep(NA,nsim*length(span)),nrow=nsim)
#   rows correspond to different simulations, 
#   columns correspond to different smoothing parameters

# create object to store true y value at xtest:
y.test <- rep(NA,nsim)

for (i in 1:nsim){
  # generate new training data:
  y <- f(x) + rnorm(n=length(x),mean=0,sd=sigma)
  # generate new test data and store it in ith element of y.test
  y.test[i] <- f(xtest) + rnorm(n=1,mean=0,sd=sigma)
  
  # fit loess curves with different smoothing levels
  for (j in 1:length(span)){ 
    lo <- loess(y ~ x, span=span[j])
    # store fitted value at xtest: 
    fit.test[i,j] <- predict(object=lo, xtest)
  }
} 

# plot histograms of fitted values at xtest:
par(mfrow=c(2,3))
for (i in 1:length(span)){
  hist(fit.test[,i],xlim=range(fit.test),freq=F,
       main=paste("alpha=",span[i]),xlab=paste("fitted value at x=",xtest))
  lines(density(fit.test[,i]))
  abline(v=f(xtest))
}
f(xtest)
#The histograms show that the fitted values at xtest are more concentrated for larger span values (there is a smaller variance) but at the same time, they have a systematic error (f(xtest)=-0.157), i.e. there is a larger bias. In other words, we observe the bias-variance trade-off.
(ExpTestMSE <- apply((fit.test-y.test)^2,2,mean))
(Bias2 <- (apply(fit.test,2,mean)-f(xtest))^2)
(VarY <- var(y.test))
(VarEps <- var(y.test))
Bias2+Var+VarY - ExpTestMSE
par(mfrow=c(1,1))
plot(span,ExpTestMSE,ylim=range(0,max(ExpTestMSE)), 
     type="l",lwd=2,col="purple", xlab="smoothing parameter alpha (small values indicate a flexible model)")
lines(span, Bias2,lwd=2,col="red")
lines(span,Var,lwd=2,col="blue")
abline(h=VarEps,lty=2,lwd=2)
legend("topleft", c("Expected test MSE at xtest", "Bias^2", "Variance", "Irreducible error"),lwd=2, lty=c(1,1,1,2),col=c("purple", "red", "blue","black"))


