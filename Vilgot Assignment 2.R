library(tidyverse)
library(MASS)
library(glmnet)

house <- read.csv("house_prices.csv")                 # Importing the data

train <- house %>%                                    # Training set
  filter(train == TRUE)
test <- house %>%                                     # Test set
  filter(train == FALSE)

train <- train[1:21]                                  # Removing train/test column
test <- test[1:21]

models_rmse <- matrix(nrow = 1, ncol = 7)             # Preparing a table
models_rmse <- as.data.frame(models_rmse)
colnames(models_rmse) <- 
  c("lr_all", "lr_step", "ridge", "LASSO", 
    "bagging", "feature bagging", "stacking")


# Linear regression, all covariats included
lr_all <- lm(SalePrice ~ . , data = train)            # Linear model
summary(lr_all)
lr_all_pred <- predict.lm(lr_all, test, 
                          type = "response")          # Predicting test set
lr_all_mse <- (test$SalePrice - lr_all_pred)^2        # Squared error

models_rmse[1, 1] <- mean(sqrt(lr_all_mse))           # RMSE for lr_all

# Linear regression, using step
lr_all <- lm(SalePrice ~ ., data = train)       
lr_step <- step(lr_all, direction = "both")           # Letting R choose variables

lr_step_pred <- predict.lm(lr_step, test, 
                           type = "response")         # Predicting test set
lr_step_mse <- (test$SalePrice - lr_step_pred)^2      # Squared error

models_rmse[1, 2] <- mean(sqrt(lr_step_mse))          # RMSE for lr_step

# Ridge
set.seed(1)
x <- model.matrix(SalePrice ~ ., train)               # Preparing the training set
y <- train$SalePrice                                  # Response variable
xtest <- model.matrix(SalePrice ~ ., test)            # Preparing the test set
ytest <- test$SalePrice                               # Response variable to predict

cv_out <- cv.glmnet(x, y, alpha = 0)                  # Cross validation, ridge
plot(cv_out)                                          # Plot MSE and lambda

ridge_pred <- predict(cv_out, xtest,                  # Predicting, lambda = value
                      lambda = cv_out$cv.min)         # that minimize MSE from CV

ridge <- (test$SalePrice - ridge_pred)^2              # Squared error

print(models_rmse[1, 3] <- mean(sqrt(ridge)))         # RMSE for ridge

# Lasso
set.seed(1)
cv_out_lasso <- cv.glmnet(x, y, alpha = 1)            # Cross validation, LASSO
plot(cv_out_lasso)                                    # Plot MSE and lambda

lasso_pred <- predict(cv_out_lasso, xtest,            # Predicting, lambda = value
                      lambda = cv_out_lasso$cv.min)   # that minimize MSE from CV

lasso <- (ytest - lasso_pred)^2                       # Squared error
models_rmse[1, 4] <- mean(sqrt(lasso))                # RMSE for LASSO



# Stacking
lr_all_pred_train <- 
  predict.lm(lr_all, 
             test, type = "response")     # Predicting train set
lr_step_pred_train <- 
  predict.lm(lr_step, 
             train, type = "response")    # Predicting train set
ridge_pred_train <- 
  predict(cv_out, xtest, 
          lambda = cv_out$cv.min)         # Predicting train set
lasso_pred_train <- 
  predict(cv_out_lasso, x, 
          lambda = cv_out_lasso$cv.min)   # Predicting train set
second_layer_train <- 
  as.data.frame(cbind(y, 
                      lasso_pred_train, ridge_pred_train, 
                      lr_all_pred_train, lr_step_pred_train)) # Column bind with true values
colnames(second_layer_train)[2:5] <- 
  c("lasso_pred", "ridge_pred", 
    "lr_all_pred", "lr_step_pred")        # Setting column names
stacking_pred_mod <- 
  lm(y ~ ., data = second_layer_train)    # Regressing true value on predicted
second_layer_test <- 
  as.data.frame(cbind(lasso_pred, 
                      ridge_pred, lr_all_pred, lr_step_pred)) # Column bind test predictions
colnames(second_layer_test)[1:2] <- 
  c("lasso_pred", "ridge_pred")           # Setting column names
pred_stacking <- 
  predict.lm(stacking_pred_mod, 
             second_layer_test, type = "response")   # Predicting
stacking <- (ytest - pred_stacking)^2     # Squared error
print(models_rmse[1, 7] <- 
        mean(sqrt(stacking)))             # RMSE for stacking

rmse_final$names <- factor(rmse_final$names, levels = rmse_final$names[order(rmse_final$RMSE)])
ggplot(rmse_final, aes(x = RMSE, y = names)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmax = upper, xmin = lower))
