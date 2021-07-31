####################################### Series 1
######################## Problem 1
############ a)
Data <- matrix(c(1,1,1,2.5,4.5,2.5,4.5,4), nrow=4, byrow=T)
colnames(Data) <- c("X", "Y")
Data

X <- Data[,1]
Y <- Data[,2]

fit_YX <- lm(Y~X)
fit_XY <- lm(X~Y)
plot(X, Y, xlim=c(0,5.5), ylim=c(0,5))
abline(fit_YX)                        
abline(fit_XY, lty = 3)                                 # wrong!

ab <- fit_XY$coefficients
abline(-ab[1]/ab[2], 1/ab[2], lwd=2, lty=2) ## regression line is X = a + bY

############ b), c) (No need to use R)

######################## Problem 2 (No need to use R)

######################## Problem 3, 4 (Only need to change one lien of R code)
set.seed(21)                              ## initializes the rng
nrep <- 100                               ## number of repetitions
slope <- numeric(nrep)                    ## initialization of vector
x <- rnorm(40, 20, 3)                     ## x-values
for (i in 1:nrep){
  
  ### Problem 3
  y <- 1 + 2 * x + 5 * rnorm(length(x))  ## simulation of y-values
  
  ### Problem 4 (a)
  # y   <- 1 + 2 * x + 5 * (1 - rchisq(length(x), df = 1)) / sqrt(2)
  
  ### Problem 4 (b)
  # y   <- 1 + 2 * x + 5 * rnorm(length(x), mean = x^2 / 5 - 1, sd = 1)
  
  ### Problem 4 (c)
  # library(MASS)
  # Sigma <- matrix(0.7,40,40)
  # diag(Sigma) <- 1
  # y   <- 1 + 2 * x + 5 * mvrnorm(n = 1, mu = rep(0, length(x)), Sigma = Sigma)
  
  ### Problem 4 (d)
  # y <- 1 + 2 * x + 5 * rnorm(length(x), mean = 0, sd = (x-15)^2 / 30) 
  
  reg <- lm(y ~ x)                       ## least squares regression
  slope[i] <- coefficients(reg)[2]       ## saves the slope
}

par(mfrow = c(3, 2))                  ## display 6 figures as 3 rows and 2 columns
set.seed(21)                               ## initializes the rng
x <- rnorm(40, 20, 3)                      ## x-values
for (i in 1:3){                      ## now we only look at the first three simulations
  
  ### Problem 3
   y <- 1 + 2 * x + 5 * rnorm(length(x))  ## simulation of y-values
  
  ### Problem 4 (a)
  # y   <- 1 + 2 * x + 5 * (1 - rchisq(length(x), df = 1)) / sqrt(2)
  
  ### Problem 4 (b)
  # y   <- 1 + 2 * x + 5 * rnorm(length(x), mean = x^2 / 5 - 1, sd = 1)
  
  ### Problem 4 (c)
  # Sigma <- matrix(0.7,40,40)
  # diag(Sigma) <- 1
  # y   <- 1 + 2 * x + 5 * mvrnorm(n = 1, mu = rep(0, length(x)), Sigma = Sigma)
  
  ### Problem 4 (d)
  # y <- 1 + 2 * x + 5 * rnorm(length(x), mean = 0, sd = (x-15)^2 / 30) 
  
  reg <- lm(y ~ x)                       ## least squares regression
  
  plot(y ~ x)           
  abline(reg)
  plot(reg, which = 1)                   ## Tukey-Anscombe plot
}

mean(slope)                            ## mean and other information
sd(slope)                              ## empirical standard deviation

X <- cbind(1, x)                       ## design matrix
XtXinv <- solve(crossprod(X))          ## crossprod(X) <=> t(X) %*% X  = X'X
tvar <- 5^2 * XtXinv[2, 2]  

par(mfrow = c(1, 1))                       ## now display only one figure
hist(slope, breaks = 15, freq = FALSE)    ## histogram, you may want to change the range of x-axis for problem 4 b)
lines(seq(1.3, 2.6, by = 0.01),                     ## add theoretical density
      dnorm(seq(1.3, 2.6, by = 0.01), mean = 2, sd = sqrt(tvar) ) )
