#bonferonni correction
n=c(70,75,80,85,90,95,100,125,130,145,150)
#j=5
#n=n[j]
p=68
nsim=500

alpha_bf=0.05/p
pval=rep(0,nsim)
power2=rep(0,length(n))

for(j in 1:length(n)){
  n_now=n[j]
  X=matrix(rnorm(n_now*p),nrow=n_now,ncol=p)
for(i in 1:nsim){
y=rnorm(n_now)+X[,1]
fit<-summary(lm(y~X))
pval[i]=coef(fit)[2,4]
#print(pval[i])
}
par(mfrow=c(1,1))
(power2[j]=sum(pval<=alpha_bf)/nsim)}
plot(n,power2)
power2
#bonf loss in power more when number of samples is small, alpha is small or m is large
#loss in power is more for smaller number of samples and smaller alpha in bonferonni
X=matrix(rnorm(100*5), nrow=100,ncol=5)
y=rnorm(100)+X[,1]*3+X[,2]*4+X[,3]*3
fit1<-lm(y~X)
summary(fit1)
tss1=sum((y-mean(y))^2)
rss1=sum((y-predict(fit1))^2)
num=(tss1-rss1)/5
deno=rss1/(100-6)
(fstat1<-num/deno)
X_2=X[,1:3]
fit2<-lm(y~X_2)
summary(fit2)
tss2=sum((y-mean(y))^2)
rss2=sum((y-predict(fit2))^2)
num=(tss2-rss2)/3
deno=rss2/(100-4)
(fstat2<-num/deno)
(f_relative=((rss2-rss1)/(2))/(rss/(100-6)))
anova(fit1,fit2)
