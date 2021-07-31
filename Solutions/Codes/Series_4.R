#Question 1
url <- "https://ww2.amstat.org/publications/jse/datasets/fruitfly.dat.txt"
data <- read.table(url)
data <- data[,c(-1,-6)] # remove id and sleep
names(data) <- c("partners","type","longevity","thorax")
pairs(data)
#From the pairs plot we see that there is a relation between longevity and type.
#Furthermore, thorax length is positively correlated with longevity.
#The database is attached to the R search path. This means that the database is searched by R when evaluating a 
#variable, so objects in the database can be accessed by simply giving their names.
?attach
attach(data)
col.partn <- 1*(partners==0) + 2*(partners==1) + 3*(partners==8)
pch.type <- 1*(type==0) + 2*(type==1) + 3*(type==9)
#partners
#type
par(mfrow=c(1,1))
plot(thorax, longevity, pch=pch.type, col=col.partn,
     ylim=range(longevity),xlim=range(thorax))
legend("topleft",c("1 pregnant","8 pregnant","1 virgin",
                   "8 virgin","0 partners"),
       pch=c(1,2,1,2,3),col=c(2,2,3,3,1))
#We can see that larger fruitflies tend to live longer. 
#Furthermore, comparing fruitflies with similar thorax value, 
#fruitflies with 8 virgins tend to live shorter.
par(mfrow=c(1,3))
plot(thorax[partners==0],longevity[partners==0],pch=pch.type[partners==0],col=1,
    main="0 partners",ylim=range(longevity),xlim=range(thorax),
    xlab="thorax length", ylab="longevity")
plot(thorax[partners==1],longevity[partners==1],
     pch=pch.type[partners==1],col=2,main="1 partners",
     ylim=range(longevity),xlim=range(thorax),
     xlab="thorax length", ylab="longevity")
legend("topleft",c("pregnant","virgin"),pch=c(1,2),col=2)
plot(thorax[partners==8],longevity[partners==8],
    pch=pch.type[partners==8],col=3,main="8 partners",
    ylim=range(longevity),xlim=range(thorax),
    xlab="thorax length", ylab="longevity")
legend("topleft",c("pregnant","virgin"),pch=c(1,2),col=3)
#t seems that male fruitflies with pregnant females tend to live longer 
#than those with virgin females. This difference in lifespan seems to be 
#larger when partners=8 compared to partners=1. Hence, there seems to be 
#an interaction effect between type and partners in their effect on longevity.
par(mfrow=c(1,1))
dummy.1.p <- (partners==1)*(type==0)*1 
dummy.1.v <- (partners==1)*(type==1)*1 
dummy.8.p <- (partners==8)*(type==0)*1  
dummy.8.v <- (partners==8)*(type==1)*1  
dummy.0 <- (partners==0)
boxplot(thorax[dummy.1.p==1],thorax[dummy.1.v==1],
          thorax[dummy.8.p==1],thorax[dummy.8.v==1],
          thorax[dummy.0==1],
          main="Thorax length",
          names=c("1p","8p","1v","8v","0"),col="grey")
fitfull<-lm(thorax~dummy.1.p+dummy.1.v+dummy.8.p+dummy.8.v)
fitintercept<-lm(thorax~1)
anova(fitintercept,fitfull)
#The test is not significant. This was to be expected since the assignments 
#to the groups were random,
#hence the distribution of thorax should be similar among the different groups.
fit_e1 <- lm(longevity[partners==1] ~ factor(type[partners==1]))
summary(fit_e1)
fit_e2 <- lm((longevity)[partners==1] ~ thorax[partners==1] +
               factor(type[partners==1]))
summary(fit_e2)
#We can see that type is much more significant in 
#the second model which includes thorax. The t-value is 
#obtained by dividing the point estimate by the estimate of the
#standard error. Note that the point estimates of the coefficient of 
#type are slightly different in the two models, but we will leave this 
#aside, and focus on the standard errors of the estimate. 
#These are 3.456 in the model with thorax and 4.326 in the model
#without thorax. The ratio is 3.456/4.326=0.80. The smaller standard 
#error in the model with thorax leads to a larger 
#t-value and hence more significant results. 
#Why is the standard error smaller in the model with thorax? 
#The residual standard error is smaller in the model with thorax 
#than in the model without thorax because thorax explains a significant
#amount of the variation in longevity. Moreover, thorax is not much correlated with type,
#so that we don’t have to worry about large variance inflation factors.
partners.f <- as.factor(partners)
type.f <- as.factor(type)
fit_f1 <- lm(longevity ~ thorax + partners.f + type.f + partners.f*type.f) 
summary(fit_f1)
#We only have 5 different groups of male fruitflies but 
#there are 9 different combinations of the two three-level 
#categorical predictors type and partners. 
#The combinations (1,9), (8,9), (0,0) and (0,1) for (partners,type)
#do not appear in the dataset because they do not make sense.
#This is why we R reports ”Coefficients: (4 not defined because of singularities)”.
#We need to do the analysis more carefully, see the next subquestion.
fit_gFull <- lm((longevity) ~ thorax + dummy.1.p + dummy.1.v + dummy.8.p + dummy.8.v)
summary(fit_gFull)


