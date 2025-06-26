## -----------------------------------------------------------------------------
# lets read the data first
insurance_data <- read.csv("insurance.csv")
head(insurance_data)


## -----------------------------------------------------------------------------
# get the summary of the dataset
library(skimr)
skim(insurance_data)


## -----------------------------------------------------------------------------
library(dplyr)

#lets check the categorical data values 
categorical_data <- insurance_data %>%
  select(where(~ is.character(.x) || is.factor(.x)))



for (col in names(categorical_data)) {
  cat("\n---", col, "---\n")
  print(table(categorical_data[[col]]))
  print(prop.table(table(categorical_data[[col]])))
}




## -----------------------------------------------------------------------------
# Lets check the histogram of the continuos variables
library(ggplot2)

# Example: Histogram for 'age'
ggplot(insurance_data, aes(x = age)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 20) +
  labs(title = "Age Distribution", x = "Age", y = "Count")

# Repeat for 'bmi' and 'charges'
ggplot(insurance_data, aes(x = bmi)) +
  geom_histogram(fill = "lightgreen", color = "white", bins = 20) +
  labs(title = "BMI Distribution", x = "BMI", y = "Count")

ggplot(insurance_data, aes(x = children)) +
  geom_histogram(fill = "gray", color = "white", bins = 20) +
  labs(title = "Children Distribution", x = "Children", y = "Count")

ggplot(insurance_data, aes(x = charges)) +
  geom_histogram(fill = "tomato", color = "white", bins = 20) +
  labs(title = "Charges Distribution", x = "Charges", y = "Count")



## -----------------------------------------------------------------------------
boxplot(charges ~ smoker, data = insurance_data,
        main = "Charges by Smoking Status", ylab = "Charges", col = c("lightblue", "salmon"))
boxplot(charges ~ sex, data = insurance_data,
        main = "Charges by Sex", ylab = "Charges", col = c("lightgreen", "plum"))
boxplot(charges ~ region, data = insurance_data,
        main = "Charges by Region", ylab = "Charges", col = "lightgray")
boxplot(charges ~ children, data = insurance_data,
        main = "Charges by Number of Children", ylab = "Charges", col = "lightpink")



## -----------------------------------------------------------------------------
numeric_data <- insurance_data[, sapply(insurance_data, is.numeric)]
cor_matrix <- cor(numeric_data)
print(cor_matrix)

library(corrplot)

corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", addCoef.col = "black")



## -----------------------------------------------------------------------------
pairs(insurance_data[, c("age", "bmi", "children", "charges")])


## -----------------------------------------------------------------------------
library(dplyr)

insurance_data %>%
  group_by(sex, region,smoker) %>%
  summarize(mean_charges = mean(charges), .groups = "drop")%>%
  arrange(mean_charges)



## -----------------------------------------------------------------------------
basic_lm_model <- lm(charges ~ age + bmi + children + sex + smoker + region, data = insurance_data)
summary(basic_lm_model)
plot(basic_lm_model)



## -----------------------------------------------------------------------------
interaction_lm <- lm(charges ~ age + bmi + children + sex + smoker + region + bmi*smoker, data = insurance_data)
summary(interaction_lm )
plot(interaction_lm ) 



## -----------------------------------------------------------------------------
squared_lm <- lm(charges ~ (age + bmi + children + sex + smoker + region)^2, data = insurance_data)
summary(squared_lm)
plot(squared_lm)


## -----------------------------------------------------------------------------
my_model <- lm(charges ~  children * region + sex + (age+smoker+bmi)^2 , data = insurance_data)
summary(my_model)
plot(my_model)



## -----------------------------------------------------------------------------
AIC(basic_lm_model,interaction_lm,squared_lm,my_model)


## -----------------------------------------------------------------------------
summary(basic_lm_model)


## -----------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(basic_lm_model,1:2)


## -----------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(basic_lm_model,3:4)


## -----------------------------------------------------------------------------
# extract the residuals
residuals <- resid(basic_lm_model)
head(residuals)



## -----------------------------------------------------------------------------
# extract the model matrix X without the intercept of my_Model in q2
X <- model.matrix(basic_lm_model)[, -1]  
head(X)


## -----------------------------------------------------------------------------
#  find squared residuals
squared_residuals <- residuals^2

# fit linear model  with ϵ̂² (squared residuals) as the response and X as the predictor
squared_model <- lm(squared_residuals ~ X)
summary(squared_model)



## -----------------------------------------------------------------------------
library(factoextra)

# find the fitted values fitted values
fitted_vals <- fitted(basic_lm_model)

# recreate the matrix  [residuals, fitted values]
resid_fit_matrix <- cbind(residuals, fitted_vals)
fviz_nbclust(resid_fit_matrix, kmeans, method="wss")


## -----------------------------------------------------------------------------
#  k-means clustering 
set.seed(42) 
kmeans_result <- kmeans(resid_fit_matrix, centers = 3)

# cluster frequencies
table(kmeans_result$cluster)


## -----------------------------------------------------------------------------
library(ggplot2)

# data frame with fitted values, residuals, and cluster assignments
resid_df <- data.frame(
  Fitted = fitted(basic_lm_model),
  Residuals = resid(basic_lm_model),
  Cluster = as.factor(kmeans_result$cluster)
)
ggplot(resid_df, aes(x = Fitted, y = Residuals, color = Cluster)) +
  geom_point(alpha = 0.7) +
  labs(title = "Residuals vs Fitted (Clustered)", x = "Fitted Values", y = "Residuals") +
  theme_minimal()


## -----------------------------------------------------------------------------
# need to get standardized residuals
standardized_resid <- rstandard(basic_lm_model)

# theoretical quantiles from qqnorm()
qq_data <- qqnorm(standardized_resid, plot.it = FALSE)

# into a data frame
resid_df <- data.frame(
  Theoretical = qq_data$x,
  Sample = qq_data$y,
  Cluster = as.factor(kmeans_result$cluster)
)

# with labels and line
library(ggplot2)
ggplot(resid_df, aes(x = Theoretical, y = Sample, color = Cluster)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Normal Q-Q Plot (Standardized Residuals by Cluster)",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()




## -----------------------------------------------------------------------------

resid_df <- data.frame(
  Fitted = fitted(basic_lm_model),
  Std_Resid = rstandard(basic_lm_model),
  Cluster = as.factor(kmeans_result$cluster)
)

#square root of absolute standardized residuals
resid_df$Sqrt_Abs_Std_Resid <- sqrt(abs(resid_df$Std_Resid))

ggplot(resid_df, aes(x = Fitted, y = Sqrt_Abs_Std_Resid, color = Cluster)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.6) +
  labs(title = "Scale-Location Plot (by Cluster)",
       x = "Fitted Values",
       y = "√|Standardized Residuals|") +
  theme_minimal()




## -----------------------------------------------------------------------------
# Cook's distances
cooks <- cooks.distance(basic_lm_model)

# Create a data frame with observation index
cooks_df <- data.frame(
  Obs = 1:length(cooks),
  Cook = cooks,
  Cluster = as.factor(kmeans_result$cluster)
)

# Plot
ggplot(cooks_df, aes(x = Obs, y = Cook, color = Cluster)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Cook's Distance by Observation (Clustered)",
       x = "Observation Number",
       y = "Cook's Distance") +
  theme_minimal()



## -----------------------------------------------------------------------------
# add cluster to dataset
insurance_data$cluster <- as.factor(kmeans_result$cluster)

# refit the model from (ii)
model_with_cluster <- lm(charges ~ age + bmi + children + sex + smoker + region + cluster, data = insurance_data)

summary(model_with_cluster)



## -----------------------------------------------------------------------------

# see why cluster 3 dropped
model.matrix(~ cluster, data = insurance_data)[1:5, ]
insurance_data %>%
  group_by(cluster) %>%
  summarise(
    avg_age = mean(age),
    avg_bmi = mean(bmi),
    avg_charges = mean(charges),
    smoker_rate = mean(smoker == "yes"),
    male_rate = mean(sex == "male"),
    .groups = "drop"
  )
X <- model.matrix(charges ~ age + bmi + children + sex + smoker + region + cluster, data = insurance_data)

qr(X)$rank # actual rank
ncol(X)         # total number of columns in the design matrix






## -----------------------------------------------------------------------------
library(ggplot2)

# Matrix for clustering
resid_fit_matrix <- cbind(resid(basic_lm_model), fitted(basic_lm_model))

# by looking at the graph in part (vii), optimal number of clusters, decided to test clusters from 4 to 6
for (k in 4:6) {
  cat("\nFitting model with", k, "clusters...\n")

  # perform k-means
  set.seed(42)
  kmeans_result <- kmeans(resid_fit_matrix, centers = k)
  insurance_data$cluster <- as.factor(kmeans_result$cluster)

  # fit model with clusters
  model_k <- lm(charges ~ age + bmi + children + sex + smoker + region + cluster, data = insurance_data)
  summary_stats <- summary(model_k)
  
  # print summary info
  cat("Adjusted R-squared:", summary_stats$adj.r.squared, "\n")
  cat("Residual Std. Error:", summary_stats$sigma, "\n")
  print(summary(model_k))

  # plot Residuals vs Fitted
  resid_df <- data.frame(
    Fitted = fitted(model_k),
    Residuals = resid(model_k),
    Cluster = insurance_data$cluster
  )
  
  print(
    ggplot(resid_df, aes(x = Fitted, y = Residuals, color = Cluster)) +
      geom_point(alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = paste("Residuals vs Fitted -", k, "Clusters")) +
      theme_minimal()
  )
}



## -----------------------------------------------------------------------------
library(Metrics)

# data split into train and test functions
set.seed(42)
sample_idx <- sample(nrow(insurance_data), 0.8 * nrow(insurance_data))
train_data <- insurance_data[sample_idx, ]
test_data <- insurance_data[-sample_idx, ]

# get residuals and fitted values for training set (basic model)
basic_model_train <- lm(charges ~ age + bmi + children + sex + smoker + region, data = train_data)
resid_fit_matrix <- cbind(resid(basic_model_train), fitted(basic_model_train))

# perform k-means clustering on training residuals and fitted values
set.seed(42)
kmeans_result <- kmeans(resid_fit_matrix, centers = 6)
train_data$cluster <- as.factor(kmeans_result$cluster)

#train both models
model_basic <- lm(charges ~ age + bmi + children + sex + smoker + region, data = train_data)
model_clustered <- lm(charges ~ age + bmi + children + sex + smoker + region + cluster, data = train_data)

# assign clusters to test set based on nearest center (this is approximate)
basic_model_test <- lm(charges ~ age + bmi + children + sex + smoker + region, data = test_data)
test_resid_fit <- cbind(resid(basic_model_test), fitted(basic_model_test))

# assign clusters to test set (find nearest cluster center)
assign_clusters <- function(points, centers) {
  apply(points, 1, function(p) {
    which.min(colSums((t(centers) - p)^2))
  })
}
test_data$cluster <- as.factor(assign_clusters(test_resid_fit, kmeans_result$centers))

# predict on test set
pred_basic <- predict(model_basic, newdata = test_data)
pred_clustered <- predict(model_clustered, newdata = test_data)

print("Basic Linear Model (ii)")
do.call(cbind, pracma::rmserr(test_data$charges, pred_basic))
print("Clustered Model k = 6 Model (iv)")
do.call(cbind, pracma::rmserr(test_data$charges, pred_clustered))




## -----------------------------------------------------------------------------
# lets read the data first
energy_data <- read.csv("energy.csv")
head(energy_data)


## -----------------------------------------------------------------------------
# get the summary of the dataset
library(skimr)
skim(energy_data)


## -----------------------------------------------------------------------------
# Lets check the histogram of the continuos variables
library(ggplot2)

# Example: Histogram for 'age'
ggplot(energy_data, aes(x = relative_compactness)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 20) +
  labs(title = "relative_compactness Distribution", x = "relative_compactness", y = "Count")

# Repeat for 'bmi' and 'charges'
ggplot(energy_data, aes(x = surface_area)) +
  geom_histogram(fill = "lightgreen", color = "white", bins = 20) +
  labs(title = "surface_area Distribution", x = "surface_area", y = "Count")

ggplot(energy_data, aes(x = wall_area)) +
  geom_histogram(fill = "gray", color = "white", bins = 20) +
  labs(title = "wall_area Distribution", x = "wall_area", y = "Count")

ggplot(energy_data, aes(x = roof_area)) +
  geom_histogram(fill = "tomato", color = "white", bins = 20) +
  labs(title = "roof_area Distribution", x = "roof_area", y = "Count")

ggplot(energy_data, aes(x = overall_height)) +
  geom_histogram(fill = "pink", color = "white", bins = 20) +
  labs(title = "overall_height Distribution", x = "overall_height", y = "Count")

ggplot(energy_data, aes(x = orientation)) +
  geom_histogram(fill = "black", color = "white", bins = 20) +
  labs(title = "orientation Distribution", x = "orientation", y = "Count")

ggplot(energy_data, aes(x = glazing_area)) +
  geom_histogram(fill = "yellow", color = "white", bins = 20) +
  labs(title = "glazing_area Distribution", x = "glazing_area", y = "Count")

ggplot(energy_data, aes(x = glazing_distribution)) +
  geom_histogram(fill = "orange", color = "white", bins = 20) +
  labs(title = "glazing_distribution Distribution", x = "glazing_distribution", y = "Count")



## -----------------------------------------------------------------------------
# Convert to factors
energy_data$orientation <- as.factor(energy_data$orientation)
energy_data$glazing_distribution <- as.factor(energy_data$glazing_distribution)
energy_data$overall_height <- as.factor(energy_data$overall_height)

# Check structure to confirm
str(energy_data)




## -----------------------------------------------------------------------------
numeric_data <- energy_data[, sapply(energy_data, is.numeric)]
cor_matrix <- cor(numeric_data)
print(cor_matrix)

library(corrplot)
# Bigger and clearer corrplot
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.cex = 0.9,          # text label size
         number.cex = 0.7,      # correlation coefficient size
         addCoef.col = "black",
         mar = c(0, 0, 1, 0))   # reduce margins if needed



## -----------------------------------------------------------------------------
pairs(energy_data)


## -----------------------------------------------------------------------------
# Orientation vs Heating Load
boxplot(HeatingLoad ~ orientation, data = energy_data,
        main = "Heating Load by Orientation", 
        ylab = "Heating Load", xlab = "Orientation",
        col = "lightblue")

# Glazing Distribution vs Heating Load
boxplot(HeatingLoad ~ glazing_distribution, data = energy_data,
        main = "Heating Load by Glazing Distribution", 
        ylab = "Heating Load", xlab = "Glazing Distribution",
        col = "lightgreen")

# Overall Height vs Heating Load
boxplot(HeatingLoad ~ overall_height, data = energy_data,
        main = "Heating Load by Overall Height", 
        ylab = "Heating Load", xlab = "Overall Height",
        col = "lightpink")




## -----------------------------------------------------------------------------
basic_lm_model  <- lm(HeatingLoad ~ ., data = energy_data)
summary(basic_lm_model)
plot(basic_lm_model)



## -----------------------------------------------------------------------------
numeric_data <- energy_data[, sapply(energy_data, is.numeric)]
cor_matrix <- cor(numeric_data)
print(cor_matrix)

library(corrplot)
# Bigger and clearer corrplot
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.cex = 0.9,          # text label size
         number.cex = 0.7,      # correlation coefficient size
         addCoef.col = "black",
         mar = c(0, 0, 1, 0))   # reduce margins if needed



## -----------------------------------------------------------------------------
# Get the model matrix
X <- model.matrix(basic_lm_model)

# Compute the rank
qr(X)$rank

ncol(X)



## -----------------------------------------------------------------------------
alias(basic_lm_model)


## -----------------------------------------------------------------------------
# Refit the model without roof_area
model_refit <- lm(HeatingLoad ~ . -roof_area,
                  data = energy_data)

# View summary
summary(model_refit)



## -----------------------------------------------------------------------------
plot(model_refit)


## -----------------------------------------------------------------------------
trial <-lm(HeatingLoad ~ (relative_compactness + surface_area + wall_area + glazing_area )^2 +
   overall_height + orientation+ glazing_distribution, data = energy_data)
summary(trial)



## -----------------------------------------------------------------------------
library(factoextra)
resid_trial <- residuals(model_refit)
fitted_trial <- fitted(model_refit)

# Create matrix for clustering
resid_fit_matrix <- cbind(resid_trial, fitted_trial)
fviz_nbclust(resid_fit_matrix, kmeans, method="wss")


## -----------------------------------------------------------------------------
set.seed(42)
kmeans_result <- kmeans(resid_fit_matrix, centers = 3)

# Add cluster labels to your dataset
energy_data$cluster <- as.factor(kmeans_result$cluster)
table(energy_data$cluster)



## -----------------------------------------------------------------------------
trial_clustered <- lm(HeatingLoad ~ relative_compactness + surface_area + wall_area + glazing_area +
                        overall_height + orientation + glazing_distribution + cluster,
                      data = energy_data)

summary(trial_clustered)




## -----------------------------------------------------------------------------
# Stepwise selection
library(MASS)
step_model <- stepAIC(model_refit, direction = "both", trace = FALSE)

# View selected model
summary(step_model)



## -----------------------------------------------------------------------------
library(glmnet)

# Prepare the data
X <- model.matrix(model_refit)[, -1]  # Remove the first column (intercept)
y <- energy_data$HeatingLoad

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
energy_data <- read.csv("energy.csv")
energy_data$orientation <- as.factor(energy_data$orientation)
energy_data$glazing_distribution <- as.factor(energy_data$glazing_distribution)
energy_data$overall_height <- as.factor(energy_data$overall_height)
set.seed(42)
sample_idx <- sample(1:nrow(energy_data), size = 0.8 * nrow(energy_data))
train_data <- energy_data[sample_idx, ]
test_data <- energy_data[-sample_idx, ]
step_model <- stepAIC(lm(HeatingLoad ~ relative_compactness + surface_area + wall_area + glazing_area +
                           overall_height + orientation + glazing_distribution,
                         data = train_data),
                      direction = "both", trace = FALSE)

step_preds <- predict(step_model, newdata = test_data)
library(glmnet)

# Model matrix for training
X_train <- model.matrix(HeatingLoad ~ relative_compactness + surface_area + wall_area + glazing_area +
                          overall_height + orientation + glazing_distribution, data = train_data)[, -1]
y_train <- train_data$HeatingLoad

# Model matrix for test
X_test <- model.matrix(HeatingLoad ~ relative_compactness + surface_area + wall_area + glazing_area +
                         overall_height + orientation + glazing_distribution, data = test_data)[, -1]
y_test <- test_data$HeatingLoad

# Cross-validation for LASSO
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1)
best_lambda <- cv_lasso$lambda.min

# Final LASSO model and prediction
lasso_model <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda)
lasso_preds <- predict(lasso_model, newx = X_test)
library(Metrics)

print("LASSO")
do.call(cbind, pracma::rmserr(y_test, lasso_preds))
print("Stepwise")
do.call(cbind, pracma::rmserr(y_test, step_preds))



## -----------------------------------------------------------------------------
library(glmnet)

# Prepare data
X <- model.matrix(HeatingLoad ~ ., data = energy_data)[, -1]  # remove intercept
y <- energy_data$HeatingLoad

set.seed(42)
n <- nrow(energy_data)
n_train <- floor(0.6 * n)

rmse_list <- c()
mae_list <- c()

for (i in 1:200) {
  train_idx <- sample(1:n, n_train)
  test_idx <- setdiff(1:n, train_idx)
  
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test <- X[test_idx, ]
  y_test <- y[test_idx]
  
  # Cross-validated LASSO on training set
  cv_fit <- cv.glmnet(X_train, y_train, alpha = 1)
  best_lambda <- cv_fit$lambda.min
  
  # Predict 
  pred <- predict(cv_fit, s = best_lambda, newx = X_test)
  
  # metrics
  rmse_list[i] <- sqrt(mean((y_test - pred)^2))
  mae_list[i] <- mean(abs(y_test - pred))
}

#  average metrics
mean_rmse <- mean(rmse_list)
mean_mae <- mean(mae_list)

cat("Average RMSE over 200 splits:", round(mean_rmse, 4), "\n")
cat("Average MAE over 200 splits:", round(mean_mae, 4), "\n")


