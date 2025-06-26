## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------

install.packages("MASS")  
library(MASS)

# Read the data
data <- read.csv("Berkeley.csv")

# Compute Mahalanobis distances for a multivariate normality check
mahalanobis_distances <- mahalanobis(data, colMeans(data), cov(data))

# Q-Q Plot 
qqplot(qchisq(ppoints(nrow(data)), df = ncol(data)), mahalanobis_distances, 
       main = "Chi-Square Q-Q Plot for Multivariate Normality",
       xlab = "Theoretical Quantiles", ylab = "Mahalanobis Distances")
abline(0, 1, col = "red")


## -----------------------------------------------------------------------------
par(mfrow = c(2, 2)) 
for (i in 1:4) { 
  qqnorm(data[, i], main = paste("Q-Q Plot Age", i), col = "blue", pch = 16, cex = 1)
  qqline(data[, i], col = "red", lwd = 2)
}
for (i in 4:8) { 
  qqnorm(data[, i], main = paste("Q-Q Plot Age", i), col = "blue", pch = 16, cex = 1)
  qqline(data[, i], col = "red", lwd = 2)
}
for (i in 8:12) { 
  qqnorm(data[, i], main = paste("Q-Q Plot Age", i), col = "blue", pch = 16, cex = 1)
  qqline(data[, i], col = "red", lwd = 2)
}


