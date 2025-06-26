## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(ISLR2)
library(caret)
library(leaps)
library(glmnet)


## -----------------------------------------------------------------------------

set.seed(238)
data(Carseats)

# Example: create partition
 train_id <- createDataPartition(Carseats$Sales, p = 0.5, list = FALSE)
 train_data <- Carseats[train_id, ]
 test_data <- Carseats[-train_id, ]
 # Train verisinin başı
head(train_data)

# Test verisinin başı
head(test_data)



## -----------------------------------------------------------------------------
# Example:
lm_model <- lm(Sales ~ ., data = train_data)
summary(lm_model)


## -----------------------------------------------------------------------------
# Example residual plots:
par(mfrow=c(1,2))
plot(lm_model,1:2)


## -----------------------------------------------------------------------------
library(pracma)
# Predict Sales using the trained first-order model
sales_pred <- predict(lm_model, newdata = test_data)

# Assess model accuracy (RMSE)
do.call(cbind, pracma::rmserr(test_data$Sales, sales_pred))



## -----------------------------------------------------------------------------
# Second-order linear model with interactions and squared terms
lm_model2 <- lm(Sales ~ (.)^2 + I(CompPrice^2) + I(Income^2) + I(Advertising^2) + 
                          I(Population^2) + I(Price^2) + I(Age^2) + I(Education^2), 
                data = train_data)

summary(lm_model2)
par(mfrow=c(1,2))
plot(lm_model2,1:2)



## -----------------------------------------------------------------------------
library(pracma)
# Predict Sales using the trained first-order model
sales_pred <- predict(lm_model2, newdata = test_data)

# Assess model accuracy (RMSE)
do.call(cbind, pracma::rmserr(test_data$Sales, sales_pred))



## -----------------------------------------------------------------------------
library(MASS)
print("backward")
full_model <- lm(Sales ~ ., data = train_data)
backward_model <- stepAIC(full_model, direction = "backward")
summary(backward_model)
print("=========================================================================")
print("forward")
null_model <- lm(Sales ~ 1, data = train_data)
forward_model <- stepAIC(null_model,
                         direction = "forward",
                         scope = list(lower = null_model, upper = full_model))
summary(forward_model)
print("=========================================================================")
print("both")
stepwise_model <- stepAIC(full_model, direction = "both")
summary(stepwise_model)



## -----------------------------------------------------------------------------
# Predict on test set
pred_backward <- predict(backward_model, newdata = test_data)
pred_forward <- predict(forward_model, newdata = test_data)
pred_stepwise <- predict(stepwise_model, newdata = test_data)

# Evaluate accuracy (using pracma::rmserr)
print("backward")
do.call(cbind, rmserr(test_data$Sales, pred_backward))
print("forward")
do.call(cbind, rmserr(test_data$Sales, pred_forward))
print("both")
do.call(cbind, rmserr(test_data$Sales, pred_stepwise))



## -----------------------------------------------------------------------------
library(glmnet)

# Prepare the data
x_train <- model.matrix(Sales ~ ., data = train_data)[, -1]  # Remove intercept
y_train <- train_data$Sales

x_test <- model.matrix(Sales ~ ., data = test_data)[, -1]
y_test <- test_data$Sales

# Fit LASSO using cross-validation
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)

# Plot the cross-validated MSE
plot(cv_lasso)

# Best lambda (minimizes error)
best_lambda <- cv_lasso$lambda.min
best_lambda

# Fit LASSO with the best lambda
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

# Coefficients of selected features
coef(lasso_model)

# Predict on test data
lasso_pred <- predict(lasso_model, s = best_lambda, newx = x_test)

# Calculate prediction accuracy
library(pracma)
do.call(cbind, rmserr(y_test, lasso_pred))


