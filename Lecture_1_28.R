library(MASS)
library(ggplot2)

# More info at: https://www.digilab.co.uk/posts/the-kernel-cookbook

# Sampling MV Normal
n <- 1000
p <- 2
mu <- rep(0, p)
Sigma <- diag(1, p, p)
X <- MASS::mvrnorm(n, mu, Sigma)
dat <- data.frame(X)

rmvnorm <- function(n, p, mu = rep(0, p), Sigma = diag(1, p, p)) {
  X <- MASS::mvrnorm(n, mu, Sigma)
  dat <- data.frame(X)
  return(dat)
}

dat <- rmvnorm(1000, 2)

ggplot(dat) + 
  geom_point(aes(x = X1, y = X2)) + 
  xlim(-4, 4) + 
  ylim(-4, 4)

#### Sampling MV Normal with correlation

Sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)
dat <- rmvnorm(1000, 2, Sigma = Sigma)
ggplot(dat) + 
  geom_point(aes(x = X1, y = X2)) + 
  geom_segment()
  xlim(-4, 4) + 
  ylim(-4, 4)

# Performing spectral decomposition
results <- svd(Sigma)
U <- -1 * results$u
Lambda <- diag(results$d)

# Plotting directions of greatest variation (PCs)
ggplot(dat) + 
  geom_point(aes(x = X1, y = X2)) + 
  geom_segment(x = 0, y = 0, xend = U[1, 1], yend = U[2, 1], 
               color = "red", linewidth = 2) +
  geom_segment(x = 0, y = 0, xend = U[2, 2], yend = U[1, 2], 
               color = "blue", linewidth = 2) +
  xlim(-4, 4) + 
  ylim(-4, 4)
  

# Estimating Sigma
X <- as.matrix(dat)
X <- scale(X, center = TRUE, scale = FALSE)
S <- t(X) %*% X / (n - p + 1)

cov(X)

# Question: How does the estimation error 
# of Sigma scale with n and p?

# Theoretical PCs vs Empirical PCs
set.seed(123)
n <- 100
p <- 2
mu <- rep(0, p)
Sigma <- diag(1, p, p)
X <- MASS::mvrnorm(n, mu, Sigma)
dat <- data.frame(X)

ggplot(dat) + 
  geom_point(aes(x = X1, y = X2)) + 
  xlim(-4, 4) + 
  ylim(-4, 4)

# estimate covariance matrix
S <- cov(X)

S_decomp <- svd(S)
U <- -1 * S_decomp$u

ggplot(dat) + 
  geom_point(aes(x = X1, y = X2)) + 
  geom_segment(x = 0, y = 0, xend = sqrt(2)/2, yend = sqrt(2)/2, 
               color = "red", linewidth = 2) +
  geom_segment(x = 0, y = 0, xend = -sqrt(2)/2, yend = sqrt(2)/2, 
               color = "blue", linewidth = 2) +
  geom_segment(x = 0, y = 0, xend = U[1, 1], yend = U[1, 2],
               color = "green", linewidth = 2) +
  xlim(-4, 4) + 
  ylim(-4, 4)


