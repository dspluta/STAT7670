data(attitude)

head(attitude)

X <- attitude[, 2:7]

# EDA

hist(X[, 1])
hist(X[, 2])
hist(X[, 3])
hist(X[, 4])
hist(X[, 5])
hist(X[, 6], breaks = 30)

ggplot(attitude) +
  geom_point(aes(x = complaints, y = privileges))

ggplot(attitude) +
  geom_point(aes(x = complaints, y = raises))

pairs(attitude)

# Fit a linear regression
fit <- lm(rating ~ ., data = attitude)
summary(fit)

plot(fit) #diagnostics

# Principal Components Analysis

X <- scale(X, center = TRUE, scale = TRUE)

X_pc <- prcomp(X)

var_explained <- X_pc$sdev^2 / sum(X_pc$sdev^2)

qplot(c(1:6), var_explained) + 
  geom_line()

# Fit linear regression on the first PC
pc1 <- X_pc$x[, 1]

fit <- lm(attitude$rating ~ pc1)
summary(fit)
plot(fit)

pc2 <- X_pc$x[, 2]
fit2 <- lm(attitude$rating ~ pc1 + pc2)

summary(fit2)

##
fit_all <- lm(attitude$rating ~ X_pc$x)
summary(fit_all)

# Fit a linear regression
fit <- lm(rating ~ ., data = attitude)
summary(fit)
