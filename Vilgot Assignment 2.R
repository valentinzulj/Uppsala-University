# Assignment 2
# Problem 1
library(tidyverse)
library(MASS)
library(glmnet)
load("prostate.RData")
house <- read.csv("house_prices.csv")

# Ridgefunktionen
ridge <- function(data, dep, indep, lambda) {
  # Your code
  x <- data %>%
    select(indep) %>%
    mutate(intercept = 1) %>%
    mutate_all(as.numeric) %>%
    select(intercept, everything())
  x <- as.matrix(x)
  y <- data %>%
    select(dep)
  y <- as.matrix(y)
  I <- diag(ncol(x))
  beta_ridge <- (solve((t(x) %*% x) + (lambda*I))) %*% (t(x) %*% y)
  return(beta_ridge)
}

# Setting the dependent and independent variable
dep <- "lpsa"
indep <- c("lcavol", "lweight", "age",
           "lbph", "svi", "lcp", "gleason",
           "pgg45")

beta_hat <- as.matrix(ridge(prostate, dep, indep, 10))
remove(beta_hat)
pred <- function(data, indep, beta_hat) {
  # Your code
  k <- length(indep) + 1
  x <- data %>%
    select(indep) %>%
    as.matrix()
  y <- as.tibble(x %*% beta_hat[2:k, 1])
  preds <- y %>%
    mutate(pred = rowSums(y) + beta_hat[1, 1]) %>%
    select(pred)
  return(preds)
}

ridge(prostate, dep, indep, 10)
pred(prostate, indep, beta_hat)

# CV
cv <- function(data, dep, indep, lambda) {
  k <- length(unique(data$group))
  msevektor <- numeric(k)
  for (i in 1:k) {
    x <- data                         %>%
      filter(group != i)              %>%
      mutate(intercept = 1)           %>%
      mutate_all(as.numeric)          %>%
      select(intercept, everything()) %>%
      as.tibble()
    beta_hat <- ridge(x, dep, indep, lambda)
    x <- data                         %>%
      filter(group == i)              %>%
      as.tibble()
    
    prediktioner <- pred(x, indep, beta_hat)
    
    x <- cbind(prediktioner, x)
    x <- as.tibble(x)
    x <- x                            %>%
      mutate(squareerror = (lpsa - pred)^2)
    msevektor[i] <- mean(x$squareerror)
    
  }
  mse <- mean(msevektor)
  return(mse)
}
cv(prostate, dep, indep, 10)

l <- seq(0, 50, length.out = 51)
mse <- numeric(length(l))
for(i in 1:length(l)){
  mse[i] <- cv(prostate, "lpsa", indep, l[i])
}

mse %>%
  enframe() %>%
  ggplot() +
  geom_line(aes(x = name, y = value, color = "red")) +
  labs(x = expression(lambda),
       y = "MSE",
       title = expression(MSE~plotted~against~lambda)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold",              
                                  hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 15),
        panel.grid.major = element_line(color = "gray90")
  ) + 
  guides(color = FALSE)


# Uppgift 2, house prices
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
colnames(models_rmse) <- c("lr_all", "lr_step", 
                           "ridge", "LASSO", 
                           "bagging", "feature bagging", 
                           "stacking")


# Linear regression, all covariats included
lr_all <- lm(SalePrice ~ . , data = train)            # Linear model
summary(lr_all)
lr_all_pred <- predict.lm(lr_all, test, 
                          type = "response")          # Predicting test set
lr_all_mse <- (test$SalePrice - lr_all_pred)^2        # Squared error

models_rmse[1, 1] <- mean(sqrt(lr_all_mse))           # RMSE for lr_all

# Linear regression, using step
summary(lr_all <- lm(SalePrice ~ ., data = train))         
lr_step <- step(lr_all)                               # Letting R choose variables
summary(lr_step)

lr_step_pred <- predict.lm(lr_step, test, 
                           type = "response")         # Predicting test set
lr_step_mse <- (test$SalePrice - lr_step_pred)^2      # Squared error

models_rmse[1, 2] <- mean(sqrt(lr_step_mse))          # RMSE for lr_step

# Ridge
x <- model.matrix(SalePrice ~ ., train)               # Preparing the training set
y <- train$SalePrice                                  # Response variable
xtest <- model.matrix(SalePrice ~ ., test)            # Preparing the test set
ytest <- test$SalePrice                               # Response variable to predict

cv_out <- cv.glmnet(x, y, alpha = 0)                  # Cross validation, ridge
plot(cv_out)                                          # Plot MSE and lambda

ridge_pred <- predict(cv_out, xtest,                  # Predicting, lambda = value
                      lambda = cv_out$cv.min)         # that minimize MSE from CV

ridge <- (test$SalePrice - ridge_pred)^2              # Squared error

models_rmse[1, 3] <- mean(sqrt(ridge))                # RMSE for ridge

# Lasso
cv_out_lasso <- cv.glmnet(x, y, alpha = 1)            # Cross validation, LASSO
plot(cv_out_lasso)                                    # Plot MSE and lambda

lasso_pred <- predict(cv_out_lasso, xtest,            # Predicting, lambda = value
                      lambda = cv_out_lasso$cv.min)   # that minimize MSE from CV

lasso <- (ytest - lasso_pred)^2                       # Squared error
models_rmse[1, 4] <- mean(sqrt(lasso))                # RMSE for LASSO


# Stacking
