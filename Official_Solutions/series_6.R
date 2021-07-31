 # The values are rounded to minutes (from 2000 to 2018).
boogg <- c(17, 26, 12, 6, 12, 18, 10, 12, 26, 13, 13, 11, 12, 35, 7, 21, 44,
               10, 21)
stripchart(boogg, method = "stack")
# Alternatively
stem(boogg)
require("MASS")
(fit.gamma <- fitdistr(boogg, "gamma"))
# Plot the density on top of the histogram
hist(boogg, freq = FALSE, breaks = 50)
lines(x=seq(from=0,to=max(boogg),by=0.4),y = dgamma(x = seq(from = 0, to = max(boogg),by=0.4),shape = fit.gamma$estimate["shape"],rate = fit.gamma$estimate["rate"]),col=2)
R <- 1000
len.b <- length(boogg) 
set.seed(987)
res <- rep(NA, R)
for (i in 1:R) {
  x <- rgamma(n = len.b, shape = fit.gamma$estimate["shape"],
              rate = fit.gamma$estimate["rate"])
  res[i] <- quantile(x, probs = 0.75)#quantity of interest
}
# Plot theta*
?quantile
hist(res, breaks = 20)
abline(v = quantile(boogg, probs = 0.75), col = 2, lwd = 2)
# theta hat
boogg.75quant <- quantile(boogg, probs = 0.75)
names(boogg.75quant) <- NULL
# quantile CI
(CI.q1 <- quantile(res, probs = c(0.025, 0.975)))
# reversed CI
CI.r1 <- boogg.75quant - quantile(res - boogg.75quant, probs = c(0.975, 0.025))
names(CI.r1) <- NULL
CI.r1
# normal approx CI
(CI.n1 <- c(boogg.75quant - qnorm(0.975) * sd(res), boogg.75quant + qnorm(0.975) * sd(res)))
require("boot")
fun.theta <- function(x, ind) {quantile(x[ind], probs = 0.75)} 
fun.gen <- function(x, mle) {
  rgamma(length(x), shape = mle[1], rate = mle[2])
}
res.boot <- boot(boogg, fun.theta, R = 1000, sim = "parametric",
                   ran.gen = fun.gen, mle = fit.gamma$estimate)
res.boot
# Plot theta*
hist(res.boot$t, breaks = 20)
abline(v = boogg.75quant, col = 2, lwd = 2)
# Calculate CIs
(res.boot.ci <- boot.ci(res.boot, type = c("norm", "basic", "perc")))
R <- 1000
res2 <- rep(NA, R) 
for (i in 1:R) {
  ind <- sample(1:len.b, len.b, replace = TRUE)
  res2[i] <- quantile(boogg[ind], probs = 0.75)
}
hist(res2, breaks = 20)
abline(v = boogg.75quant, col = 2, lwd = 2)
(CI.q2 <- quantile(res2, probs = c(0.025, 0.975)))
CI.r2 <- boogg.75quant - quantile(res2 - boogg.75quant, probs = c(0.975, 0.025))
names(CI.r2) 
CI.r2
(CI.n2 <- c(boogg.75quant - qnorm(0.975) * sd(res2),
                boogg.75quant + qnorm(0.975) * sd(res2)))
CI.q1.boot <- res.boot.ci$percent[4:5]
CI.r1.boot <- res.boot.ci$basic[4:5]
CI.n1.boot <- res.boot.ci$normal[2:3]
ylim.max <- max(c(CI.q1, CI.q1.boot, CI.q2, CI.r1, CI.r1.boot, CI.r2, CI.n1,
                    CI.n1.boot, CI.n2))
ylim.min <- min(c(CI.q1, CI.q1.boot, CI.q2, CI.r1, CI.r1.boot, CI.r2, CI.n1,
                    CI.n1.boot, CI.n2))
plot(x = NA, xlim =c(0, 0.6), ylim = c(ylim.min - 1, ylim.max + 3), ylab = "CI",
       xlab = "", xaxt='n')
axis(side = 1, at = c(0.05, 0.3, 0.55),
       labels = c("quantile", "reversed", "normal"))
legend("top", c("param. hand","param. boot","non-param. hand"), lty = 1:3,
         lwd = 2, col = 1:3, ncol = 3, bty ="n")
# quantile
lines(x = rep(0, times = 2), y = c(CI.q1[1], CI.q1[2]), lwd = 2)
lines(x = rep(0.05, times = 2), y = c(CI.q1.boot[1], CI.q1.boot[2]), col = 2,
        lty = 2, lwd = 2)
lines(x = rep(0.1, times = 2), y = c(CI.q2[1], CI.q2[2]), col = 3, lty = 3,
        lwd = 2)
lines(x = rep(0.25, times = 2), y = c(CI.r1[1], CI.r1[2]), lwd = 2)
lines(x = rep(0.3, times = 2), y = c(CI.r1.boot[1], CI.r1.boot[2]), col = 2,
        lty = 2, lwd = 2)
lines(x = rep(0.35, times = 2), y = c(CI.r2[1], CI.r2[2]), col = 3, lty = 3,
        lwd = 2)
# normal
lines(x = rep(0.5, times = 2), y = c(CI.n1[1], CI.n1[2]), lwd = 2)
lines(x = rep(0.55, times = 2), y = c(CI.n1.boot[1], CI.n1.boot[2]), col = 2,
        lty = 2, lwd = 2)
lines(x = rep(0.6, times = 2), y = c(CI.n2[1], CI.n2[2]), col = 3, lty = 3,
        lwd = 2)
#The bootstrap CIs based on parametric bootstrap (by hand and using the package boot) are similar, but obviously they do not have to be exactly the same since they are based on different bootstrap samples. The normal approximation CI based on parametric bootstrap using the package boot is shifted compared to the normal approximation CI calculated by hand because the function boot.ci corrects for the bias.
#As we would expect for such a small sample size, the confidence intervals for non-parametric bootstrap are wider.
#Panini problem
n.npack=c(25, 30, 35, 40,50)
cards_per_pack=5
nr.cards=682
alpha=0.05
nsimul=100000
get_duplicates<-function(sample)
{
  return(length(sample[duplicated(sample)]))
}

simulate.duplicate.k <- function(k, npack){
  probs <- c(rep(5, k), rep(1, nr.cards - k))
  probs <- probs/sum(probs)
  res <- sample(1:nr.cards, npack*5, prob=probs, replace = TRUE)
  return(5 * npack - length(unique(res)))
}
set.seed(456)
n.npack <- c(25, 30,31,32,33, 35, 40)
resNULL <- matrix(data = NA, nrow = length(n.npack),ncol = nsimul)
resALT <- matrix(data = NA, nrow = length(n.npack),ncol = nsimul)
for (i in 1:length(n.npack)) {
  npack <- n.npack[i]
  for (k in 1:nsimul) {
    # Note that p = 0 corresponds to the H0
    #10 15 20 25 30
    #CI
    #Computational Statistics (SS 2019) — Solution to Series 6 — 7
    resNULL[i, k] <- simulate.duplicate.k(k = 0, npack = npack)
    resALT[i, k] <- simulate.duplicate.k(k = 100, npack = npack)
  }
}
par(mfrow=c(1,1))
res.max <- max(c(resNULL[i, ], resALT[i, ]))
for (i in 1:length(n.npack)) {
  npack <- n.npack[i]
  # boundary of rejection region at alpha = 0.05
  (rej <- quantile(resNULL[i, ], 0.95)+1)
  # power given the rejection boundary
  power <- sum(resALT[i, ] >= rej) / nsimul
  # plot
  res.range <- range(c(resNULL[i, ], resALT[i, ]))
  p1 <- hist(resNULL[i, ], plot = FALSE,
             breaks = seq(from = res.range[1], to = res.range[2],
                          by = 1))
  p2 <- hist(resALT[i, ], plot = FALSE,
             breaks = seq(from = res.range[1], to = res.range[2],
                          by = 1))
  plot(p1, col = rgb(0, 0, 1, 1/4), xlim = c(0, res.max),
       ylim = c(0, max(c(p1$counts, p2$counts))),  xlab = "",
       main = paste("Power for p = ", round(2/3, 2), " and npack = ", npack,
                    " equals", round(power, 2)))
  plot(p2, col = rgb(1, 0, 0, 1/4), xlab = "", add = TRUE)
  abline(v = rej, col = 4, lwd = 2)
}
