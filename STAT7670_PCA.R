library(ggplot2)
data("USArrests")

dat <- USArrests

dat
cov(dat)

results <- prcomp(USArrests, scale = TRUE)

#transformation matrix
results$rotation <- -1*results$rotation

# Data after transformation
results$x

# Visualization

biplot(results, scale = 0)

# Find variance explained by each component

var_explained <- results$sdev^2 / sum(results$sdev^2)

qplot(c(1:4), var_explained) + 
  geom_line()
