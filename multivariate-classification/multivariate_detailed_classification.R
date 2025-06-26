## -----------------------------------------------------------------------------
library(ggplot2)
library(GGally)
library(patchwork)
library(heplots)
library(biotools)
library(car)
library(caret)
library(pROC)
library(e1071)
library(ggcorrplot)


## -----------------------------------------------------------------------------
# Read the dataset
diabetes_df <- read.csv("diabetes.csv")
# there are some na values drop them 
diabetes_df <-  na.omit(diabetes_df)
# see the summary 
str(diabetes_df)
summary(diabetes_df)



## -----------------------------------------------------------------------------
# make it factor 
diabetes_df$T2DM <- factor(diabetes_df$T2DM)

# GGally to plot key variables
ggpairs(diabetes_df, columns = c("glycaemia", "HbA1c", "BMI", "age", "followUp", "sex"),
        aes(color = T2DM, alpha = 0.6),
        title = "Distributions of Predictors by T2DM Diagnosis")



## -----------------------------------------------------------------------------
# predictor variables
predictors <- c("glycaemia", "HbA1c", "BMI", "age", "followUp", "sex")

# individual boxplots and store them in a list
plots <- lapply(predictors, function(var) {
  ggplot(diabetes_df, aes(x = T2DM, y = .data[[var]], fill = T2DM)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", var, "by T2DM"),
         x = NULL, y = var) +
    theme_minimal() +
    theme(legend.position = "none")
})

# Combine plots using patchwork library
(plots[[1]] | plots[[2]] | plots[[3]]) /
(plots[[4]] | plots[[5]] | plots[[6]])



## -----------------------------------------------------------------------------
## check for overall normality of features
heplots::cqplot(diabetes_df[,1:6])  
par(mfrow=c(1,2))
## check for normality of features within class
heplots::cqplot(subset(diabetes_df,T2DM=="No")[,1:6])
heplots::cqplot(subset(diabetes_df,T2DM=="Yes")[,1:6])



## -----------------------------------------------------------------------------
boxM(diabetes_df[, c("glycaemia", "HbA1c", "BMI", "age", "followUp", "sex")], group = diabetes_df$T2DM)


## -----------------------------------------------------------------------------
diabetes_df$T2DM_numeric <- ifelse(tolower(trimws(diabetes_df$T2DM)) == "yes", 1, 0)
full_model <- lm(T2DM_numeric~ glycaemia + HbA1c + BMI + age + followUp + sex, data = diabetes_df)
vif(full_model)



## -----------------------------------------------------------------------------
print("########### LDA Part ########################")
# fit LDA model (exclude T2DM_numeric)
fit.lda <- lda(T2DM ~ . - T2DM_numeric, data = diabetes_df)
pred_lda <- predict(fit.lda, diabetes_df)
lda_class <- pred_lda$class

# confusion matrix
conf_matrix_lda <- table(Predicted = lda_class, Actual = diabetes_df$T2DM)
print(conf_matrix_lda)

# accuracy
lda_acc <- mean(lda_class == diabetes_df$T2DM)
cat("LDA Accuracy:", round(lda_acc, 4), "\n")

print("############# LDA with t test ###################")
# fit LDA model (exclude T2DM_numeric) with t test
predictors <- setdiff(names(diabetes_df), c("T2DM", "T2DM_numeric"))
ttest_pvals <- sapply(predictors, function(var) {
  t.test(diabetes_df[[var]] ~ diabetes_df$T2DM)$p.value
})

significant_vars <- names(ttest_pvals[ttest_pvals < 0.05])

lda_formula <- as.formula(paste("T2DM ~", paste(significant_vars, collapse = " + ")))
fit.lda_ttest <- lda(lda_formula, data = diabetes_df)

pred_lda_ttest <- predict(fit.lda_ttest, diabetes_df)
lda_ttest_class <- pred_lda_ttest$class

# confusion matrix
conf_matrix <- table(Predicted = lda_ttest_class, Actual = diabetes_df$T2DM)
print(conf_matrix)

# Accuracy
lda_ttest_acc <- mean(lda_ttest_class == diabetes_df$T2DM)
cat("LDA (t-test) Accuracy:", round(lda_ttest_acc, 4), "\n")

print("########### QDA Part ##############################")
# Fit QDA model (exclude T2DM_numeric)
fit.qda <- qda(T2DM ~ . - T2DM_numeric, data = diabetes_df)
pred_qda <- predict(fit.qda, diabetes_df)
qda_class <- pred_qda$class

# confusion matrix
conf_matrix_qda <- table(Predicted = qda_class, Actual = diabetes_df$T2DM)
print(conf_matrix_qda)
qda_acc <- mean(qda_class == diabetes_df$T2DM)
cat("QDA Accuracy:", round(qda_acc, 4), "\n")
print("########### RDA Part ########################")
cv5 <- trainControl(method = "cv", number = 5)

# Fit RDA model (excluding T2DM_numeric)
fit.rda <- train(T2DM ~ . - T2DM_numeric, 
                 data = diabetes_df,
                 method = "rda",
                 trControl = cv5)

# Print model summary
print(fit.rda)
# Predict on training data using the best RDA model
rda_pred <- predict(fit.rda, newdata = diabetes_df)

# confusion matrix
rda_cm <- confusionMatrix(rda_pred, diabetes_df$T2DM)
print(rda_cm)
cat("RDA Best Accuracy:", round(rda_cm$overall["Accuracy"], 4), "\n")

print("########### Naive Bayes Part ########################")
# Fit Naive Bayes model excluding T2DM_numeric
fit.nb <- naiveBayes(T2DM ~ . - T2DM_numeric, data = diabetes_df)
pred_nb <- predict(fit.nb, diabetes_df)

#confusion matrix
conf_matrix_nb <- table(Predicted = pred_nb, Actual = diabetes_df$T2DM)
print(conf_matrix_nb)
accuracy_nb <- mean(pred_nb == diabetes_df$T2DM)
cat("Naive Bayes Accuracy:", round(accuracy_nb, 4), "\n")
#####################################################



## -----------------------------------------------------------------------------


# Ensure binary outcome for ROC
diabetes_df$T2DM_numeric <- ifelse(diabetes_df$T2DM == "Yes", 1, 0)

# Probabilities for each model
lda_probs <- pred_lda$posterior[, "Yes"]
lda_ttest_probs <- pred_lda_ttest$posterior[, "Yes"]
qda_probs <- pred_qda$posterior[, "Yes"]
nb_probs <- predict(fit.nb, diabetes_df, type = "raw")[, "Yes"]
rda_probs <- predict(fit.rda, newdata = diabetes_df, type = "prob")[, "Yes"]

# Compute ROC curves
roc_lda <- roc(diabetes_df$T2DM_numeric, lda_probs)
roc_lda_ttest <- roc(diabetes_df$T2DM_numeric, lda_ttest_probs)
roc_qda <- roc(diabetes_df$T2DM_numeric, qda_probs)
roc_nb <- roc(diabetes_df$T2DM_numeric, nb_probs)
roc_rda <- roc(diabetes_df$T2DM_numeric, rda_probs)

# Plot all ROC curves
plot(roc_lda, col = "blue", lwd = 2, main = "ROC Curves for All Models")
lines(roc_lda_ttest, col = "skyblue", lwd = 2)
lines(roc_qda, col = "red", lwd = 2)
lines(roc_nb, col = "darkgreen", lwd = 2)
lines(roc_rda, col = "purple", lwd = 2)

legend("bottomright",
       legend = c(
         paste("LDA AUC:", round(auc(roc_lda), 3)),
         paste("LDA (t-test) AUC:", round(auc(roc_lda_ttest), 3)),
         paste("QDA AUC:", round(auc(roc_qda), 3)),
         paste("Naive Bayes AUC:", round(auc(roc_nb), 3)),
         paste("RDA AUC:", round(auc(roc_rda), 3))
       ),
       col = c("blue", "skyblue", "red", "darkgreen", "purple"),
       lwd = 2)




## -----------------------------------------------------------------------------
library(MASS)

# Fit QDA with custom priors to handle imbalance
qda_model <- qda(T2DM ~ . - T2DM_numeric, data = diabetes_df, 
                 prior = c("No" = 0.70, "Yes" = 0.3))

# Predict on test set
qda_pred <- predict(qda_model, diabetes_df)
qda_class <- qda_pred$class
qda_probs <- qda_pred$posterior[, "Yes"]

# Confusion Matrix
conf_matrix <- table(Predicted = qda_class, Actual = diabetes_df$T2DM)
print(conf_matrix)
cat("Accuracy:", mean(qda_class == diabetes_df$T2DM), "\n")



## -----------------------------------------------------------------------------
set.seed(238)
print("##################### SVM with radial kernel##########################")
# Fit SVM
svm.fit1 <- svm(T2DM ~ . - T2DM_numeric, data = diabetes_df, kernel = "radial")

# Predict
svm.pred1 <- predict(svm.fit1, newdata = diabetes_df)

# Confusion Matrix
svm_cm <- table(Predicted = svm.pred1, Actual = diabetes_df$T2DM)
print(svm_cm)

# Accuracy
svm_acc <- mean(svm.pred1 == diabetes_df$T2DM)
cat("SVM (radial) Accuracy:", round(svm_acc, 4), "\n")
print("################### Radial SVM with tunning ###############################")
## choosing the optimal cost
tune.cost <- tune(svm, T2DM ~ . - T2DM_numeric,
                  data = diabetes_df,
                  kernel = "radial",
                  ranges = list(cost = 10^(-2:2), gamma = 10^(-2:2) ))
# Extract the best cost from tuning
best_params <- tune.cost$best.parameters
best_cost <- tune.cost$best.parameters$cost
best_gamma <- best_params$gamma

# Fit SVM with best cost on diabetes data
svm.fit2 <- svm(T2DM ~ . - T2DM_numeric, data = diabetes_df,
                cost = best_cost, kernel = "radial", gamma=best_gamma, probability=TRUE)

# Predict on the full dataset
svm.pred2 <- predict(svm.fit2, newdata = diabetes_df)

# Confusion matrix
svm_cm2 <- table(Predicted = svm.pred2, Actual = diabetes_df$T2DM)
print(svm_cm2)

# Accuracy
svm_acc2 <- mean(svm.pred2 == diabetes_df$T2DM)
cat("Final SVM Accuracy (with best cost):", round(svm_acc2, 4), "\n")

print("##################### kNN ##########################")

# Fit model with k = 5
knn.fit2 <- gknn(T2DM ~ . - T2DM_numeric, data = diabetes_df, k = 3)
pred.knn2 <- predict(knn.fit2, diabetes_df)

# Confusion matrix
cf.knn2 <- table(Predicted = pred.knn2, Actual = diabetes_df$T2DM)
print(cf.knn2)

# Accuracy
knn2_acc <- sum(diag(cf.knn2)) / nrow(diabetes_df)
cat("gKNN Accuracy (k=5):", round(knn2_acc, 4), "\n")

print("###################### Logistic Regression ############################")

# Required libraries
library(ResourceSelection)
library(ROCR)
# Step 2: Fit logistic model
fit_logit <- glm(T2DM_numeric ~ poly(HbA1c, 2) + poly(BMI, 2) +
                  glycaemia + age + followUp + sex + HbA1c:BMI,
                data = diabetes_df, family = binomial(link = "logit"))

# Step 3–5: Model summary, odds ratios, fit diagnostics
#summary(fit_logit)
exp(cbind(OddsRatio = coef(fit_logit), confint(fit_logit)))
logLik(fit_logit); deviance(fit_logit); AIC(fit_logit)

# Step 6: Hosmer-Lemeshow test
hoslem.test(diabetes_df$T2DM_numeric, fitted(fit_logit), g = 10)

# Step 8: Optimal cutoff using ROCR
pred_rocr <- prediction(fitted(fit_logit), diabetes_df$T2DM_numeric)
perf_cost <- performance(pred_rocr, "cost")
optimal_cutoff <- pred_rocr@cutoffs[[1]][which.min(perf_cost@y.values[[1]])]
cat("Optimal cutoff:", round(optimal_cutoff, 3), "\n")

# Step 9: Confusion matrix at optimal cutoff
pred_class_opt <- ifelse(fitted(fit_logit) > optimal_cutoff, 1, 0)
conf_mat_opt <- table(Predicted = pred_class_opt, Actual = diabetes_df$T2DM_numeric)
print("here")
print(conf_mat_opt)

# Step 10: Accuracy at optimal cutoff
accuracy_opt <- sum(diag(conf_mat_opt)) / sum(conf_mat_opt)
cat("Accuracy at optimal cutoff:", round(accuracy_opt, 4), "\n")

print("############# Weighted Logistic Regression ###############")
# Assign weights: 7 for positive class (Yes = 1), 3 for negative class (No = 0)
diabetes_df$weights <- ifelse(diabetes_df$T2DM_numeric == 1, 7, 3)

# Fit weighted logistic regression model
fit_weighted <- glm(T2DM_numeric ~ glycaemia + HbA1c + BMI + age + followUp + sex,
                    data = diabetes_df, 
                    family = binomial(link = "logit"), 
                    weights = weights)

# Summary of the model
#summary(fit_weighted)

# Odds Ratios and Confidence Intervals
exp(cbind(OddsRatio = coef(fit_weighted), confint(fit_weighted)))

# Confusion matrix at 0.5 cutoff
pred_class <- ifelse(fitted(fit_weighted) > 0.5, 1, 0)
conf_mat <- table(Predicted = pred_class, Actual = diabetes_df$T2DM_numeric)
print(conf_mat)

# Accuracy
acc <- sum(diag(conf_mat)) / sum(conf_mat)
cat("Accuracy:", round(acc, 4), "\n")




## -----------------------------------------------------------------------------
# Logistic regression
logit_probs <- fitted(fit_logit)
roc_logit <- roc(diabetes_df$T2DM_numeric, logit_probs)

# SVM (must use probability=TRUE during training)
svm_probs <- attr(predict(svm.fit2, newdata = diabetes_df, probability = TRUE), "probabilities")[, "Yes"]
roc_svm <- roc(diabetes_df$T2DM_numeric, svm_probs)

# gKNN (return class probabilities)
knn_probs <- predict(knn.fit2, diabetes_df, type = "prob")[, "Yes"]
roc_knn <- roc(diabetes_df$T2DM_numeric, knn_probs)

# Weighted logistic
wlogit_probs <- fitted(fit_weighted)
roc_wlogit <- roc(diabetes_df$T2DM_numeric, wlogit_probs)

# -----------------------------
# Plot all ROC curves
# -----------------------------
plot(roc_logit, col = "blue", lwd = 2, main = "ROC Curves for All Models")
lines(roc_svm, col = "red", lwd = 2)
lines(roc_knn, col = "green", lwd = 2)
lines(roc_wlogit, col = "purple", lwd = 2)

legend("bottomright",
       legend = c(
         paste("Logistic AUC:", round(auc(roc_logit), 3)),
         paste("SVM AUC:", round(auc(roc_svm), 3)),
         paste("gKNN AUC:", round(auc(roc_knn), 3)),
         paste("Weighted Logistic AUC:", round(auc(roc_wlogit), 3))
       ),
       col = c("blue", "red", "green", "purple"),
       lwd = 2)



## -----------------------------------------------------------------------------
set.seed(238)  # replace with your own NetID numeric part
library(caret)

train_index <- createDataPartition(diabetes_df$T2DM, p = 0.7, list=TRUE,times=1)$Resample1
train_data <- diabetes_df[train_index, ]
test_data <- diabetes_df[-train_index, ]
train_data$T2DM_numeric <- ifelse(tolower(trimws(train_data$T2DM)) == "yes", 1, 0)
test_data$T2DM_numeric  <- ifelse(tolower(trimws(test_data$T2DM)) == "yes", 1, 0)


cat("Train set:\n"); print(table(train_data$T2DM))
cat("Test set:\n"); print(table(test_data$T2DM))



## -----------------------------------------------------------------------------
library(MASS)

# Fit QDA with custom priors to handle imbalance
qda_model <- qda(T2DM ~ . - T2DM_numeric - weights, data = train_data, 
                 prior = c("No" = 0.70, "Yes" = 0.3))

# Predict on test set
qda_pred <- predict(qda_model, test_data)
qda_class <- qda_pred$class
qda_probs <- qda_pred$posterior[, "Yes"]
roc_qda <- roc(test_data$T2DM_numeric, qda_probs)

# Confusion Matrix
conf_matrix <- table(Predicted = qda_class, Actual = test_data$T2DM)
print(conf_matrix)
cat("Accuracy:", mean(qda_class == diabetes_df$T2DM), "\n")


## -----------------------------------------------------------------------------
library(e1071)

print("##################### SVM with radial kernel (Train/Test) ##########################")

# Fit SVM on training data
svm.fit1 <- svm(T2DM ~ . - T2DM_numeric, data = train_data, kernel = "radial")

# Predict on test data
svm.pred1 <- predict(svm.fit1, newdata = test_data)

# Confusion Matrix
svm_cm <- table(Predicted = svm.pred1, Actual = test_data$T2DM)
print(svm_cm)

# Accuracy
svm_acc <- mean(svm.pred1 == test_data$T2DM)
cat("SVM (radial) Accuracy (Test Set):", round(svm_acc, 4), "\n")

print("################### Radial SVM with tuning (Train/Test) ###############################")

# Tune cost and gamma on training data
tune.cost <- tune(svm, T2DM ~ . - T2DM_numeric,
                  data = train_data,
                  kernel = "radial",
                  ranges = list(cost = 10^(-2:2), gamma = 10^(-2:2)))

# Extract best cost and gamma
best_params <- tune.cost$best.parameters
best_cost <- best_params$cost
best_gamma <- best_params$gamma

# Fit tuned SVM on training data
svm.fit2 <- svm(T2DM ~ . - T2DM_numeric,
                data = train_data,
                cost = best_cost, kernel = "radial", gamma = best_gamma,
                probability = TRUE)

# Predict on test data
svm.pred2 <- predict(svm.fit2, newdata = test_data)

# Confusion matrix
svm_cm2 <- table(Predicted = svm.pred2, Actual = test_data$T2DM)
print(svm_cm2)

# Accuracy
svm_acc2 <- mean(svm.pred2 == test_data$T2DM)
cat("Final SVM Accuracy with Tuning (Test Set):", round(svm_acc2, 4), "\n")


## -----------------------------------------------------------------------------
train_data$weights <- ifelse(train_data$T2DM_numeric == 1, 7, 3)
test_data$weights <- ifelse(test_data$T2DM_numeric == 1, 7, 3)
# Fit weighted logistic regression model
fit_weighted <- glm(T2DM_numeric ~ glycaemia + HbA1c + BMI + age + followUp + sex,
                    data = diabetes_df, 
                    family = binomial(link = "logit"), 
                    weights = weights)

# Summary of the model
#summary(fit_weighted)

# Odds Ratios and Confidence Intervals
exp(cbind(OddsRatio = coef(fit_weighted), confint(fit_weighted)))

# Confusion matrix at 0.5 cutoff

pred_probs <- predict(fit_weighted, newdata = test_data, type = "response")
roc_logit <- roc(test_data$T2DM_numeric, pred_probs)
pred_class <- ifelse(pred_probs > 0.5, 1, 0)
conf_mat <- table(Predicted = pred_class, Actual = test_data$T2DM_numeric)
print(conf_mat)

# Accuracy
acc <- sum(diag(conf_mat)) / sum(conf_mat)
cat("Weighted Logistic Regression Accuracy (Test Set):", round(acc, 4), "\n")




## -----------------------------------------------------------------------------
# Initialize plot with logistic regression ROC
plot(roc_logit, col = "blue", lwd = 2, main = "ROC Curves (Test Set)")

# Add QDA ROC curve
lines(roc_qda, col = "red", lwd = 2)

# Add legend with AUCs and labels
legend("bottomright",
       legend = c(paste("Logistic Regression AUC =", round(auc(roc_logit), 2)),
                  paste("QDA AUC =", round(auc(roc_qda), 2))),
       col = c("blue", "red"),
       lwd = 2)




## -----------------------------------------------------------------------------
# Read the dataset
energy_df <- read.csv("energy-1.csv")
# there are some na values drop them 
energy_df <-  na.omit(energy_df)
# see the summary 
str(energy_df)
summary(energy_df)



## -----------------------------------------------------------------------------

# Select only numeric columns

pairs(energy_df,
      main = "Pairwise Scatterplots of Numerical Features",
      pch = 21, bg = "lightblue")

# Compute the correlation matrix for numeric features
cor_matrix <- cor(energy_df[, sapply(energy_df, is.numeric)], use = "complete.obs")

# Plot correlation heatmap
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           title = "Correlation Heatmap of Numerical Features",
           colors = c("red", "white", "blue"))





## -----------------------------------------------------------------------------
# need to convert categorical variables as factors
energy_df$overall_height <- as.factor(energy_df$overall_height)
energy_df$orientation <- as.factor(energy_df$orientation)
energy_dfglazing_distribution<- as.factor(energy_df$glazing_distribution)

lm_model <- lm(HeatingLoad ~ ., data=energy_df)
summary(lm_model)


## -----------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(lm_model, 1:2)


## -----------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(lm_model, 3:4)


## -----------------------------------------------------------------------------
trial <-lm(HeatingLoad ~ (relative_compactness + surface_area + wall_area + glazing_area  + overall_height )^2 + orientation+ glazing_distribution, data = energy_df)
summary(trial)


## -----------------------------------------------------------------------------
library(factoextra)
lm_model_remedy <- lm(HeatingLoad ~ . -roof_area, data=energy_df)
resid_trial <- residuals(lm_model_remedy)
fitted_trial <- fitted(lm_model_remedy)
# Create matrix for clustering
resid_fit_matrix <- cbind(resid_trial, fitted_trial)
fviz_nbclust(resid_fit_matrix, kmeans, method="wss")

## -----------------------------------------------------------------------------
set.seed(238)
kmeans_result <- kmeans(resid_fit_matrix, centers = 4)
# Add cluster labels to your dataset
energy_df$cluster <- as.factor(kmeans_result$cluster)
table(energy_df$cluster)


## -----------------------------------------------------------------------------
trial_clustered <- lm(HeatingLoad ~ relative_compactness + surface_area + wall_area + glazing_area  + overall_height  + orientation+ glazing_distribution + cluster,
data = energy_df)
summary(trial_clustered)


## -----------------------------------------------------------------------------
# Stepwise selection
library(MASS)
step_model <- stepAIC(lm_model, direction = "both", trace = FALSE)

# View selected model
summary(step_model)



## -----------------------------------------------------------------------------
library(glmnet)

# Prepare the data
X <- model.matrix(lm_model)[, -1]  # Remove the first column (intercept)
y <- energy_df$HeatingLoad

# Fit LASSO model using cross-validation
cv_lasso <- cv.glmnet(X, y, alpha = 1)

# Get best lambda
best_lambda <- cv_lasso$lambda.min
print(best_lambda)
# Fit final LASSO model
lasso_model <- glmnet(X, y, alpha = 1, lambda = best_lambda)

# Show coefficients
coef(lasso_model)
# Predict using the LASSO model
lasso_preds <- predict(lasso_model, s = best_lambda, newx = X)

# Compute SSE and SST
sse <- sum((y - lasso_preds)^2)
sst <- sum((y - mean(y))^2)

# Compute R-squared
r_squared <- 1 - (sse / sst)

# Count the number of non-zero coefficients (excluding the intercept)
p <- sum(coef(lasso_model)[-1] != 0)
n <- length(y)

# Compute adjusted R-squared
adj_r_squared <- 1 - (1 - r_squared) * (n - 1) / (n - p - 1)

# Print adjusted R-squared
cat("Adjusted R-squared for LASSO:", round(adj_r_squared, 4), "\n")



## -----------------------------------------------------------------------------
library(MASS)
step_model_clustered <- stepAIC(trial_clustered, direction = "both", trace = FALSE)

# View selected model
summary(step_model_clustered)


## -----------------------------------------------------------------------------
library(glmnet)

# Prepare the data
X <- model.matrix(trial_clustered)[, -1]  # Remove the first column (intercept)
y <- energy_df$HeatingLoad

# Fit LASSO model using cross-validation
cv_lasso <- cv.glmnet(X, y, alpha = 1)

# Get best lambda
best_lambda <- cv_lasso$lambda.min
print(best_lambda)
# Fit final LASSO model
lasso_model <- glmnet(X, y, alpha = 1, lambda = best_lambda)

# Show coefficients
coef(lasso_model)
# Predict using the LASSO model
lasso_preds <- predict(lasso_model, s = best_lambda, newx = X)

# Compute SSE and SST
sse <- sum((y - lasso_preds)^2)
sst <- sum((y - mean(y))^2)

# Compute R-squared
r_squared <- 1 - (sse / sst)

# Count the number of non-zero coefficients (excluding the intercept)
p <- sum(coef(lasso_model)[-1] != 0)
n <- length(y)

# Compute adjusted R-squared
adj_r_squared <- 1 - (1 - r_squared) * (n - 1) / (n - p - 1)

# Print adjusted R-squared
cat("Adjusted R-squared for LASSO:", round(adj_r_squared, 4), "\n")

