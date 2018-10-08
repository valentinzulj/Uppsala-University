# Assignment 2
# Problem 1
library(tidyverse)
load("prostate.RData")

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
  x <- data %>%
    select(indep) %>%
    as.matrix()
  y <- as.tibble(x %*% beta_hat[2:9, 1])
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
cv(prostate, dep, indep, 2)

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
