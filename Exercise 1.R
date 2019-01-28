library(tidyverse)

t <- numeric(10000)

for(k in 1:10){
X <- matrix(1, nrow = 100)
s <- sample(1:100, 50, replace = FALSE)
for (i in s){
  X[i, ] = 0
}
X <- cbind(X, NA)
for(i in 1:nrow(X)){
  
  if(X[i, 1] == 0){
    X[i, 2] <- rnorm(1, m = 10, sd = 1)
  }
  else {
    X[i, 2] <- rnorm(1, m = 10, sd = 1) + 2
  }
}
tau <- as.tibble(X)
tau <- tau %>%
  group_by(V1) %>%
  summarize(tau = mean(V2)) %>%
  pull(tau)
tau <- tau[2] - tau[1]

t[k] <- tau
print(k)
}

hist(t)
