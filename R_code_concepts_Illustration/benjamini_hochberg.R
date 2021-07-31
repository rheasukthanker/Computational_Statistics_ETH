#Benjamini Hochberg
X=rnorm(200*20)
X=matrix(X,nrow=200,ncol=20)
y=rnorm(200)+X[,1]+X[,20]+X[,7]
fit<-lm(y~X)
s=summary(fit)
(pvals=coef(s)[,4])
?sort
pvals_sorted=sort(pvals,decreasing = FALSE)
rank_p=rank(pvals_sorted)
j=rep(0,21)
m=21
#FDR Q
Q=0.05
for(i in 1:21)
{
  j[i]=(rank_p[i]/m)*Q
}
j
#Compare your original p-values to the critical B-H from Step 3; find the largest p value that is smaller than the critical value.
plot(rank_p,pvals_sorted)
?abline
abline(0,(Q/m))
which.max(pvals_sorted[pvals_sorted<=j])
names(pvals[pvals<=j])
