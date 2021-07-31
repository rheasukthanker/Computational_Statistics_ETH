#Illustration of QQ plots and what they mean 
# Draw two plots next to each other
par(mfrow = c(1, 2))

# normal_density are the y-values for the normal curve
# zs are the x-values for the normal curve
n <- 1000
normal_density <- dnorm(seq(-4, 4, 0.01))
zs <- seq(-4, 4, 0.01)

# Add some spice to the default histogram function
hist_ <- function(x, ...){
  hist(x, breaks = 30, xlab = "Z", ylab = "",  yaxt='n', freq = FALSE, ...)
  lines(zs, normal_density, type = "l", col = "red", lwd = 2)
}

# Gaussian Normal
# rnorm() generates random numbers from a normal distribution
# gaussian_rv is the dataset that will be compared to the Gaussian distribution
gaussian_rv <- rnorm(n)

# Draw the histogram
hist_(gaussian_rv, main = "Gaussian Distribution")

# Draw the Q-Q plot
qqnorm(gaussian_rv)
qqline(gaussian_rv, col = "blue", lwd = 2)
# Skewed Right
# skew_right is the dataset that will be compared to the Gaussian distribution
skew_right <- c(gaussian_rv[gaussian_rv > 0] * 2.5, gaussian_rv)#more positive

hist_(skew_right, main = "Skewed Right", ylim = c(0, max(normal_density)))

qqnorm(skew_right)
qqline(skew_right, col = "blue", lwd = 2)

# Skewed Left
# skew_left is the dataset that will be compared to the Gaussian distribution
skew_left <- c(gaussian_rv[gaussian_rv < 0]*2.5, gaussian_rv)#more negative

hist_(skew_left, main = "Skewed Left", ylim = c(0, max(normal_density)))

qqnorm(skew_left)
qqline(skew_left, col = "blue", lwd = 2)
# Fat Tails
fat_tails <- c(gaussian_rv*2.5, gaussian_rv)

hist_(fat_tails, main = "Fat Tails", ylim = c(0, max(normal_density)), xlim = c(-10, 10))

qqnorm(fat_tails)
qqline(fat_tails, col = "blue", lwd = 2)

# Thin Tails
thin_tails <- rnorm(n, sd = .8)

hist_(thin_tails, main = "Thin Tails")

qqnorm(thin_tails)
qqline(thin_tails, col = "blue", lwd = 2)
