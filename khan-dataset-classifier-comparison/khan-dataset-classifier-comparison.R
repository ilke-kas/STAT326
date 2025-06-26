## -----------------------------------------------------------------------------
library(ISLR2)
str(Khan)



## -----------------------------------------------------------------------------
library(MASS)


class_labels <- sort(unique(Khan$ytrain))  

# class-specific covariance matrices computation
cov_list <- lapply(class_labels, function(cls) {
  cov(Khan$xtrain[Khan$ytrain == cls, ])
})

norm_diff_12 <- norm(cov_list[[1]] - cov_list[[2]], type = "F")
norm_diff_13 <- norm(cov_list[[1]] - cov_list[[3]], type = "F")
norm_diff_14 <- norm(cov_list[[1]] - cov_list[[4]], type = "F")
norm_diff_23 <- norm(cov_list[[2]] - cov_list[[3]], type = "F")
norm_diff_24 <- norm(cov_list[[2]] - cov_list[[4]], type = "F")
norm_diff_34 <- norm(cov_list[[3]] - cov_list[[4]], type = "F")

cat("Frobenius norms of covariance differences:\n")
cat("Class 1 vs 2:", norm_diff_12, "\n")
cat("Class 1 vs 3:", norm_diff_13, "\n")
cat("Class 1 vs 4:", norm_diff_14, "\n")
cat("Class 2 vs 3:", norm_diff_23, "\n")
cat("Class 2 vs 4:", norm_diff_24, "\n")
cat("Class 3 vs 4:", norm_diff_34, "\n")




## -----------------------------------------------------------------------------
library(klaR)
library(ISLR2)

results <- data.frame(lambda = numeric(), gamma = numeric(), accuracy = numeric())

model <- lda(Khan$xtrain, grouping = Khan$ytrain)
pred <- predict(model, Khan$xtest)
acc <- mean(pred$class == Khan$ytest)
      
results <- rbind(results, data.frame( accuracy = acc))
cat( "accuracy:", acc, "\n")



## -----------------------------------------------------------------------------
library(klaR)
library(ISLR2)

data(Khan)
xtrain_df <- as.data.frame(Khan$xtrain)
xtest_df  <- as.data.frame(Khan$xtest)
colnames(xtest_df) <- colnames(xtrain_df)

ytrain_vec <- as.factor(Khan$ytrain)
ytest_vec  <- Khan$ytest

colnames(xtest_df) <- colnames(xtrain_df)

nb_model <- NaiveBayes(xtrain_df, as.factor(ytrain_vec))

nb_pred <- predict(nb_model, xtest_df)

mean(nb_pred$class == ytest_vec)




## -----------------------------------------------------------------------------
library(e1071)
library(ISLR2)

data(Khan)

# convert to data frames 
xtrain_df <- as.data.frame(Khan$xtrain)
xtest_df  <- as.data.frame(Khan$xtest)

# scale training data
xtrain_scaled <- scale(xtrain_df)

#  same scaling parameters for test data
xtest_scaled <- scale(xtest_df, 
                      center = attr(xtrain_scaled, "scaled:center"), 
                      scale  = attr(xtrain_scaled, "scaled:scale"))

xtrain_scaled_df <- as.data.frame(xtrain_scaled)
xtest_scaled_df  <- as.data.frame(xtest_scaled)
colnames(xtest_df) <- colnames(xtrain_df)

ytrain_vec <- as.factor(Khan$ytrain)
ytest_vec  <- Khan$ytest

# train SVM
svm_model <- svm(x = xtrain_df, y = ytrain_vec, kernel = "linear", cost = 1)

# predict
svm_pred <- predict(svm_model, xtest_df)

accuracy <- mean(svm_pred == ytest_vec)
print(paste("Test Accuracy:", round(accuracy * 100, 2), "%"))



## -----------------------------------------------------------------------------
library(ISLR2)
library(class)

# Load Khan data
data(Khan)

# Convert data to data frames
xtrain_df <- as.data.frame(Khan$xtrain)
xtest_df  <- as.data.frame(Khan$xtest)
colnames(xtest_df) <- colnames(xtrain_df)

# Prepare labels
ytrain_vec <- as.factor(Khan$ytrain)
ytest_vec  <- Khan$ytest

for (k in c(1, 3, 5, 7)) {
  knn_pred <- knn(train = xtrain_df, test = xtest_df, cl = ytrain_vec, k = k)
  acc <- mean(knn_pred == ytest_vec)
  print(paste("Accuracy for k =", k, ":", round(acc * 100, 2), "%"))
}



