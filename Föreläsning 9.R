library(ISLR)
library(modelr)
library(MASS)
data("Auto")

set.seed(100)
s <- sample(1:392, 196, replace = FALSE)
train <- Auto[s, ]
test <- Auto[-s, ]

mod1 <- lm(mpg ~ horsepower, data = train)
mod2 <- lm(mpg ~ poly(horsepower, 2), data = train)

train %>%
  add_predictions(mod1, var = "mod1") %>%
  add_predictions(mod2, var = "mod2") %>%
  summarize(mod1 = mean((mpg-mod1)^2), # Training MSE
            mod2 = mean((mpg-mod2)^2))

train %>%
  add_predictions(mod1, var = "mod1") %>%
  add_predictions(mod2, var = "mod2") %>%
  summarize(mod1 = mean((mpg-mod1)^2), # Test MSE
            mod2 = mean((mpg-mod2)^2))

#### Leave one out cross validation ####
n <- nrow(Auto)
y_hat <- numeric(n)
for(i in 1:n){
  train <- Auto[-i, ]
  test <- Auto[i, ]
  mod <- lm(mpg ~ horsepower, data = train)
  y_hat[i] <- predict(mod, test)
}

mean((Auto$mpg - y_hat)^2)

#### Exercise ####

y_hat <- matrix(nrow = n, ncol = 10)
for(i in 1:n){
  for(j in 1:deg){
  train <- Auto[-i, ]
  test <- Auto[i, ]
  mod <- lm(mpg ~ poly(horsepower, j), data = train)
  y_hat[i,j] <- predict(mod, test)
  
  }
}

colMeans((Auto$mpg - y_hat)^2) %>%
  enframe() %>%
  ggplot(aes(x = name, y = value)) +
  geom_line()


#### K-fold cross validation ####

n <- nrow(Auto)
y_hat <- matrix(nrow = n, ncol = 10)
folds <- sample(rep(1:8, 392/8), n, replace = FALSE)

for(i in 1:8){
  for(j in 1:deg){
    train <- Auto[folds != i, ]
    test <- Auto[folds == i, ]
    mod <- lm(mpg ~ poly(horsepower, j), data = train)
    y_hat[folds == i,j] <- predict(mod, test)
    
  }
}

colMeans((Auto$mpg - y_hat)^2) %>%
  enframe() %>%
  ggplot(aes(x = name, y = value)) +
  geom_line()


#### Bootstrapping ####
sig2_x <- 2
sig2_y <- 1.5
sig_xy <- 0.75
alpha <- (sig2_y - sig_xy)/(sig2_x + sig2_y - 2*sig_xy)

mu <- c(0,0)
Sigma <- matrix(c(sig2_x, sig_xy, sig_xy, sig2_y), nrow = 2, ncol = 2)
n <- 100

R <- 1000
alpha_hat <- numeric(R)

for (i in 1:R) {
  XY <- mvrnorm(n, mu, Sigma)
  sig2_X_hat <- var(XY[, 1])
  sig2_Y_hat <- var(XY[, 2])
  sig_XY_hat <- cov(XY[, 1], XY[, 2])
  alpha_hat[i] <- (sig2_Y_hat - sig_XY_hat)/
    (sig2_X_hat + sig2_Y_hat - 2*sig_XY_hat)
}

enframe(alpha_hat) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 50)

mean(alpha_hat)
sd(alpha_hat)

R <- 1000
alpha_hat <- numeric(R)

set.seed(100)
XY_true <- mvrnorm(n, mu, Sigma)


for (i in 1:R) {
  samples <- sample(1:n, n, replace = TRUE) # Bootstrap sample
  XY <- XY_true[samples, ]                  # Bootstrap sample  
  sig2_X_hat <- var(XY[, 1])
  sig2_Y_hat <- var(XY[, 2])
  sig_XY_hat <- cov(XY[, 1], XY[, 2])
  alpha_hat[i] <- (sig2_Y_hat - sig_XY_hat)/
    (sig2_X_hat + sig2_Y_hat - 2*sig_XY_hat)
}

sd(alpha_hat)


#### Parallel computing ####
library(parallel)
library(microbenchmark)

x <- mean(rnorm(100000))

mean_fun <- function(R){
  return(mean(rnorm(R)))
}

mean_fun(100000)

mean(sapply(X = c(25000, 25000, 25000, 25000), FUN = mean_fun))

cl <- makeCluster(4) #Setting up workers
x <- parSapply(cl = cl, X = c(25000, 25000, 25000, 25000), FUN = mean_fun) #Parallel

microbenchmark(serial = mean_fun(100000), #timing the different options
               parallel = parSapply(cl = cl, X = c(25000, 25000, 25000, 25000),
                                   FUN = mean_fun))



set.seed(100)
XY_true <- mvrnorm(n, mu, Sigma)

boot_fun <- function(R, XY_true){
  n <- nrow(XY_true)
  alpha_hat <- numeric(R)
  for (i in 1:R) {
    samples <- sample(1:n, n, replace = TRUE) # Bootstrap sample
    XY <- XY_true[samples, ]                  # Bootstrap sample  
    sig2_X_hat <- var(XY[, 1])
    sig2_Y_hat <- var(XY[, 2])
    sig_XY_hat <- cov(XY[, 1], XY[, 2])
    alpha_hat[i] <- (sig2_Y_hat - sig_XY_hat)/
    (sig2_X_hat + sig2_Y_hat - 2*sig_XY_hat)
  }
  return(alpha_hat)
}  
serial <- boot_fun(1000, XY_true)
serial <- sapply(X = c(250, 250, 250, 250), FUN = boot_fun, XY_true = XY_true)
parallel <- parSapply(cl = cl, X = c(250, 250, 250, 250), FUN = boot_fun, XY_true = XY_true)

microbenchmark(serial = boot_fun(1000, XY_true),
               parallel = parSapply(cl = cl, X = c(250, 250, 250, 250), 
                                    FUN = boot_fun, XY_true = XY_true))
stopCluster(cl)

