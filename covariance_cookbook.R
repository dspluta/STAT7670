# Covariance Cookbook

# White noise

Sigma <- diag(1, nrow = 100, ncol = 100)
dat <- rmvnorm(1, 100, Sigma = Sigma)

dat
plot(dat$X, ty = 'l')

acf(dat$X)

# AR(1)

eps <- rnorm(100, 0, 0.1)
y <- rep(0, 100)

y[1] <- eps[1]
for (i in 2:100) {
  y[i] <- 0.7 * y[i - 1] + eps[i]
}

plot(y, ty = "l")
acf(y)

# Compound
p <- 100
Sigma <- matrix(0.2, nrow = p, ncol = p)
Sigma <- Sigma + diag(0.8, nrow = p, ncol = p)

dat <- rmvnorm(1, p, Sigma = Sigma)
plot(dat$X, ty = "l")
