## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------
# Load the dataset
dat1 <- read.csv("dat1.csv")

# Check structure
head(dat1)

# Check for ellipticity and examine the correlaton matrix
pairs(dat1)

## examine the correlation matrix
library(corrplot)

corrplot(cor(dat1), type="lower", method="ellipse")



## -----------------------------------------------------------------------------
library(kernlab)

# MDS
mds <- cmdscale(dist(dat1), k = 2)

# Kernel PCA with median-based sigma
euclidean_dists <- dist(scale(dat1), method = "euclidean")
median_sigma <- median(as.vector(euclidean_dists))

manhattan_dists <- dist(scale(dat1), method = "manhattan")
median_sigma_l1 <- median(as.vector(manhattan_dists))

kpc_poly <- kpca(~., data = dat1, kernel = "polydot", kpar = list(degree = 3), features = 2)
kpc_laplace <- kpca(~., data = dat1, kernel = "laplacedot", kpar = list(sigma = median_sigma_l1), features = 2)
kpc_rbf <- kpca(~., data = dat1, kernel = "rbfdot", kpar = list(sigma = median_sigma), features = 2)

# Plot all
par(mfrow = c(2, 2))
plot(mds, main = "MDS", xlab = "Dim1", ylab = "Dim2", pch = 19)
plot(rotated(kpc_poly), main = "Kernel PCA (Polydot)", xlab = "PC1", ylab = "PC2", pch = 19)
plot(rotated(kpc_laplace), main = "Kernel PCA (Laplacedot)", xlab = "PC1", ylab = "PC2", pch = 19)
plot(rotated(kpc_rbf), main = "Kernel PCA (RBF)", xlab = "PC1", ylab = "PC2", pch = 19)




## -----------------------------------------------------------------------------
# Use elbow method or silhouette
## elbow plot of Within cluster sum of squares
library(factoextra)
library(ggplot2)
fviz_nbclust(dat1, kmeans, method="wss")
# apply kmeans to original data with appropriate number of clusters in the dataset
km_dat1 <- kmeans(dat1,center=4)
pairs(dat1, col=km_dat1$cluster)



## -----------------------------------------------------------------------------
library(kernlab)

# Kernel PCA with median-based sigma
euclidean_dists <- dist(scale(dat1), method = "euclidean")
median_sigma <- median(as.vector(euclidean_dists))

manhattan_dists <- dist(scale(dat1), method = "manhattan")
median_sigma_l1 <- median(as.vector(manhattan_dists))

kpc_poly <- kpca(~., data = dat1, kernel = "polydot", kpar = list(degree = 3), features = 2)
kpc_laplace <- kpca(~., data = dat1, kernel = "laplacedot", kpar = list(sigma = median_sigma_l1), features = 2)
kpc_rbf <- kpca(~., data = dat1, kernel = "rbfdot", kpar = list(sigma = median_sigma), features = 2)

# WSS plots with titles
fviz_nbclust(rotated(kpc_poly), kmeans, method = "wss") + ggtitle("Kernel PCA (Polydot) - WSS Elbow")
fviz_nbclust(rotated(kpc_laplace), kmeans, method = "wss") + ggtitle("Kernel PCA (Laplacedot) - WSS Elbow")
fviz_nbclust(rotated(kpc_rbf), kmeans, method = "wss") + ggtitle("Kernel PCA (RBF) - WSS Elbow")





## -----------------------------------------------------------------------------
library(kernlab)
library(ggplot2)

# Kernel PCA with median-based sigma
euclidean_dists <- dist(scale(dat1), method = "euclidean")
median_sigma <- median(as.vector(euclidean_dists))

km_dat1<- kmeans(dat1, center=4)

kpc_poly <- kpca(~., data = dat1, kernel = "polydot", kpar = list(degree = 3), features = 2)
kpc_laplace <- kpca(~., data = dat1, kernel = "laplacedot", kpar = list(sigma =median_sigma_l1), features = 2)

kpc_poly_pc <- rotated(kpc_poly)
kpc_laplace_pc <- rotated(kpc_laplace)

df_dat1_pc_poly <- data.frame(PC1=kpc_poly_pc[,1],PC2=kpc_poly_pc[,2],
                         clus4=as.factor(km_dat1$cluster) )
df_dat1_pc_laplace <- data.frame(PC1=kpc_laplace_pc[,1],PC2=kpc_laplace_pc[,2],
                         clus4=as.factor(km_dat1$cluster) )

g4_poly <- ggplot(df_dat1_pc_poly, aes(x=PC1, y=PC2, color=clus4)) + geom_point() +
  theme_bw() + labs(title="Poly Kernel K-Means clustering for K=4")

g4_laplace <- ggplot(df_dat1_pc_laplace, aes(x=PC1, y=PC2, color=clus4)) + geom_point() +
  theme_bw() + labs(title="Laplace K-Means clustering for K=4")

print(g4_poly)
print(g4_laplace)



## -----------------------------------------------------------------------------
# Load the dataset
glass <- read.csv("glass.csv")

# Check structure
head(glass)

# Check for ellipticity and examine the correlaton matrix
pairs(glass)

## examine the correlation matrix
library(corrplot)

corrplot(cor(glass), type="lower", method="ellipse")




## -----------------------------------------------------------------------------
# Scatter plot of first two PCs
pca <- prcomp(glass, center = TRUE, scale = TRUE)$x[,1:2]
plot(pca, main="PCA", xlab="PC1",ylab="PC2")



## -----------------------------------------------------------------------------
# Use elbow method or silhouette
# Use elbow method or silhouette
## elbow plot of Within cluster sum of squares
library(factoextra)
library(ggplot2)
fviz_nbclust(glass, kmeans, method="wss")
# apply kmeans to original data with appropriate number of clusters in the dataset
km_glass <- kmeans(glass,center=4)
pairs(glass, col=km_glass$cluster)



## -----------------------------------------------------------------------------
glass_pca <- prcomp(glass, center = TRUE, scale = TRUE)$x[,1:2]
fviz_nbclust(glass_pca, kmeans, method="wss")



## -----------------------------------------------------------------------------
glass_pca <- prcomp(glass, center = TRUE, scale = TRUE)
km_glass <- kmeans(glass,center=3)
df_glass_pc <- data.frame(PC1=glass_pca$x[,1], PC2=glass_pca$x[,2],
                          clus3=as.factor(km_glass$cluster)) 
glass_g3 <-  ggplot(df_glass_pc, aes(x=PC1, y=PC2, color=clus3)) + geom_point() +
  theme_bw() + labs(title="PCA K-Means clustering for K=3")
print(glass_g3)


