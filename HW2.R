library(tidyverse)
a <- combn(20,10)

Y <- matrix(c(rnorm(20, m = 10, sd = 1)), ncol = 1)
t_hat <- numeric(ncol(a))

for(k in 1:ncol(a)){
  
  W <- matrix(1, nrow = 20)
  s <- a[, k]
  W[s, ] <- 0
  
  B <- matrix(NA, nrow = 20, ncol = 1)
  
  for(i in 1:20){
    if(W[i, 1] == 0){
      B[i, 1] = Y[i, 1]
    }
    else{
      B[i, 1] = Y[i, 1] + rnorm(1, m = 0, sd = 1)
    }
  }
  
  tib <- as.tibble(cbind(W[, 1], Y[, 1]))
  
  t_tib <- tib %>% group_by(V1) %>% summarize(m = mean(V2)) %>% pull(m)
  t_hat[k] <- t_tib[2] - t_tib[1]
  print(k)
}

hist(t_hat)

q <- quantile(t_hat, probs = c(0.9, 0.975, 0.99))


#### ####

Y <- matrix(c(rnorm(20, m = 10, sd = 1)))

tau <- numeric(1000)
f <- matrix(0, ncol = 2, nrow = 1000)

for(k in 1:1000){
  X <- matrix(1, nrow = 20, ncol = 2 )
  B <- matrix(NA, nrow = 20, ncol = 1)
  s <- sample(1:20, 10, replace = FALSE)
  X[s, 2] <- 0
  
  for(i in 1:20){
    if(X[i, 2] == 1){
      B[i, 1] <- Y[i, 1] + rnorm(1)
    } else {
      B[i, 1] <- Y[i, 1]
    }
  }
  
  tib <- as.tibble(cbind(X[, 2], B[, 1]))
  
  tib <- tib %>% 
    group_by(V1) %>%
    summarize(tau = mean(V2)) %>%
    pull(tau)
  
  tau <- tib[2] - tib[1]
  if(abs(tau) > 0.8506509){
    f[k, 1] <- 1
  }
  
  t <- as.tibble(cbind(X[, 2], B[, 1]))
  x <- t %>% filter(V1 == 1) %>% pull(V2)
  y <- t %>% filter(V1 == 0) %>% pull(V2)
  
  if(t.test(x, y)$p.value < 0.05){
    f[k, 2] <- 1
  }
  print(k)
}

colMeans(f)
