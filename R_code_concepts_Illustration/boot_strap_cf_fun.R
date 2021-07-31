#Distribution of mean
y=rnorm(200)
return_median<-function(x)
{
  return(median(x))
}
med=rep(0,100000)
for(i in 1:100000)
{ y=rnorm(200)
  med[i]=quantile(y,0.75)
}
hist(med)
#bootstrap T confidence interval
boot_main=rep(0,1000)
sd_sub=rep(0,1000)
for(i in 1:1000)
{
  new_y=y[sample(1:200,200,replace = TRUE)]
  boot_main[i]=return_median(new_y)
  print(return_median(new_y))
  boot_sub=rep(0,1000)
  for(j in 1:1000)
  {
    newy=unique(new_y)
    newy=newy[sample(1:length(newy),length(newy),replace=TRUE)]
    boot_sub[j]=return_median(newy)
  }
  sd_sub[i]=sd(boot_sub)
  
}
act<-return_median(y)
t_stat=(boot_main-act)/sd_sub
(lower_cf=act-quantile(t_stat,0.975)*sd(boot_main))
(upper_cf=act-quantile(t_stat,0.025)*sd(boot_main))

# Perc, narrower than t
lower=quantile(boot_main,0.025)
lower
upper=quantile(boot_main,0.975)
upper
#basic, bit wider than t 
mean_centered=boot_main-act
mean_centered
(lower=act-quantile(mean_centered,0.975))
(upper=act-quantile(mean_centered,0.025))
#normal, wider than t bootstrap 
(lower=act-qnorm(0.975)*sd(boot_main))
(upper=act+qnorm(0.975)*sd(boot_main))
