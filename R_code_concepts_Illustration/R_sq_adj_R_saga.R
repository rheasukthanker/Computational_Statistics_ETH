#RSS R squared and adjusted R squared
#R^2 vs Adjusted R squared
?rnorm
n=500
R_sq=rep(0,n-3)
Adj_Rsq=rep(0,n-3)
for (i in c(2:498))
{
X=rnorm(n*i)
X=matrix(X,nrow=n,ncol=i)
X
y=2*X[,1]+4*X[,2]+rnorm(n)
fit_diffnp<-lm(y~X)
R_sq[i-1]=summary(fit_diffnp)$r.squared
Adj_Rsq[i-1]=summary(fit_diffnp)$adj.r.squared
}
par(mfrow=c(1,2))
plot(c(1:(n-3)),R_sq)
plot(c(1:(n-3)),Adj_Rsq)
length(c(1:(n-3)))
#See the significant drop in adj_Rsq when p very large justfies intuition
#ask when in general can you trust a high adj-R squared value