library(tidyverse)
library(gridExtra)

set.seed(314)

#### SATE ####

sate <- function(n, homogenous = TRUE){
  tau <- numeric(10000)
  tau_reg <- numeric(10000)
  Y <- matrix(c(rnorm(n, m = 10, sd = 1)))
  if(homogenous == TRUE){
    
    for(k in 1:10000){
    X <- matrix(1, nrow = n, ncol = 2 )
    B <- matrix(NA, nrow = n, ncol = 1)
    s <- sample(1:n, n/2, replace = FALSE)
    X[s, 2] <- 0
    
    for(i in 1:n){
      if(X[i, 2] == 1){
        B[i, 1] <- Y[i, 1] + 2
      } else {
        B[i, 1] <- Y[i, 1]
      }
    }
  
  tib <- as.tibble(cbind(X[, 2], B[, 1]))
  
  tib <- tib %>% 
    group_by(V1) %>%
    summarize(tau = mean(V2)) %>%
    pull(tau)
  
  tau[k] <- tib[2] - tib[1]
  tau_reg[k] <- (solve(t(X) %*% X) %*% t(X) %*% B)[2,]
  print(k)
  }
  } else {
    for(k in 1:10000){
      X <- matrix(1, nrow = n, ncol = 2 )
      B <- matrix(NA, nrow = n, ncol = 1)
      s <- sample(1:n, n/2, replace = FALSE)
      X[s, 2] <- 0
      
      for(i in 1:n){
        if(X[i, 2] == 1){
          B[i, 1] <- Y[i, 1] + 2 + rnorm(1, m = 0, sd = 2)
        } else {
          B[i, 1] <- Y[i, 1]
        }
      }
      
      
      tib <- as.tibble(cbind(X[, 2], B[, 1]))
      
      tib <- tib %>% 
        group_by(V1) %>%
        summarize(tau = mean(V2)) %>%
        pull(tau)
      
      tau[k] <- tib[2] - tib[1]
      tau_reg[k] <- (solve(t(X) %*% X) %*% t(X) %*% B)[2,]
      print(k)
    }
    }
  out <- cbind(as.matrix(tau), as.matrix(tau_reg))
  return(out)
}

a <- sate(100)

hist(a[,1])
mean(a[,1])
sd((a[,1]))
hist(a[,2])
mean((a[,2]))
sd((a[,2]))

b <- sate(200)

hist(b[,1])
mean(b[,1])
sd((b[,1]))
hist(b[,2])
mean(b[,2])
sd((b[,2]))

c <- sate(300)

hist(c[,1])
mean(c[,1])
sd((c[,1]))
hist(c[,2])
mean(c[,2])
sd((c[,2]))

sate_homo <- cbind(a,b,c)
write.csv(sate_homo, "sate_homo.csv")

a1 <- sate(100, homogenous = FALSE)

hist(a1[,1])
mean(a1[,1])
sd((a1[,1]))
hist(a1[,2])
mean(a1[,2])
sd((a1[,2]))

b1 <- sate(200, homogenous = FALSE)

hist(b1[,1])
mean(b1[,1])
sd((b1[,1]))
hist(b1[,2])
mean(b1[,2])
sd((b1[,2]))

c1 <- sate(300, homogenous = FALSE)

hist(c1[,1])
mean(c1[,1])
sd((c1[,1]))
hist(c1[,2])
mean(c1[,2])
sd((c1[,2]))

sate_hetero <- cbind(a1,b1,c1)
write.csv(sate_hetero, "sate_hetero.csv")



#### PATE ####

pate <- function(n, homogenous = TRUE){
  tau <- numeric(10000)
  tau_reg <- numeric(10000)
  
  if(homogenous == TRUE){
    for(k in 1:10000){
      X <- matrix(1, nrow = n, ncol = 2)
      s <- sample(1:n, n/2, replace = FALSE)
      X[s, 2] <- 0
      
      Y <- matrix(1, nrow = n)
      
      for(i in 1:n){
        if(X[i, 2] == 0){
          Y[i, 1] = rnorm(1, m = 10, sd = 1)
        } else {
          Y[i, 1] = rnorm(1, m = 10, sd = 1) + 2
        }
      }
      tib <- as.tibble(cbind(X[, 2], Y[, 1]))
      
      tib <- tib %>% 
        group_by(V1) %>%
        summarize(tau = mean(V2)) %>%
        pull(tau)
      
      tau[k] <- tib[2]- tib[1]
      tau_reg[k] <- (solve(t(X) %*% X) %*% t(X) %*% Y)[2,]
    print(k)}
  }
  else{
    for(k in 1:10000){
      X <- matrix(1, nrow = n, ncol = 2)
      s <- sample(1:n, n/2, replace = FALSE)
      X[s, 2] <- 0
      
      Y <- matrix(1, nrow = n)
      
      for(i in 1:n){
        if(X[i, 2] == 0){
          Y[i, 1] = rnorm(1, m = 10, sd = 1)
        } else {
          Y[i, 1] = rnorm(1, m = 10, sd = 1) + 2 + rnorm(1, m = 0, sd = 2) 
        }
      }
      tib <- as.tibble(cbind(X[, 2], Y[, 1]))
      
      tib <- tib %>% 
        group_by(V1) %>%
        summarize(tau = mean(V2)) %>%
        pull(tau)
      
      tau[k] <- tib[2]- tib[1]
      tau_reg[k] <- (solve(t(X) %*% X) %*% t(X) %*% Y)[2,]
    print(k)}
  }
  out <- cbind(as.matrix(tau), as.matrix(tau_reg))
  return(out)
}

pa <- pate(100)

hist(pa[,1])
mean(pa[,1])
sd((pa[,1]))
hist(pa[,2])
mean(pa[,2])
sd((pa[,2]))

pb <- pate(200)

hist(pb[,1])
mean(pb[,1])
sd((pb[,1]))
hist(pb[,2])
mean(pb[,2])
sd((pb[,2]))

pc <- pate(300)

hist(pc[,1])
mean(pc[,2])
sd((pc[,1]))
hist(c[,2])
mean(pc[,2])
sd((c[,2]))

pate_homo <- cbind(pa,pb,pc)
write.csv(pate_homo, "pate_homo.csv")

pa1 <- pate(100, homogenous = FALSE)

hist(pa1[,1])
mean(pa1[,1])
sd((pa1[,1]))
hist(pa1[,2])
mean(pa1[,2])
sd((pa1[,2]))

pb1 <- pate(200, homogenous = FALSE)

hist(pb1[,1])
mean(pb1[,1])
sd((pb1[,1]))
hist(pb1[,2])
mean(pb1[,2])
sd((pb1[,2]))

pc1 <- pate(300, homogenous = FALSE)

hist(pc1[,1])
mean(pc1[,1])
sd((pc1[,1]))
hist(pc1[,2])
mean(pc1[,1])
sd((pc1[,2]))

pate_hetero <- read.csv("pate_hetero.csv")

pate_hetero_tau <- ggplot(data = pate_hetero, mapping = aes(alpha = 0.3)) +
  geom_density(mapping = aes(x = V5, fill = "300")) +
  geom_density(mapping = aes(x = V3, fill = "200")) +
  geom_density(mapping = aes(x = V1, fill = "100")) +
  labs(y = "Density", x = expression(hat(tau))) +
  theme_classic() +
  scale_fill_manual(name = "Sample Size",values = c("100" = "red","200" = "blue", "300" = "green")) +
  guides(alpha = FALSE)

pate_hetero_beta <- ggplot(data = pate_hetero, mapping = aes(alpha = 0.3)) +
  geom_density(mapping = aes(x = V6, fill = "300")) +
  geom_density(mapping = aes(x = V4, fill = "200")) +
  geom_density(mapping = aes(x = V2, fill = "100")) +
  labs(y = "Density", x = expression(hat(beta))) +
  theme_classic() +
  scale_fill_manual(name = "Sample Size",values = c("100" = "red","200" = "blue", "300" = "green")) +
  guides(alpha = FALSE)


pate_homo <- read.csv("pate_homo.csv")

pate_homo_tau <- ggplot(data = pate_homo, mapping = aes(alpha = 0.3)) +
  geom_density(mapping = aes(x = V5, fill = "300")) +
  geom_density(mapping = aes(x = V3, fill = "200")) +
  geom_density(mapping = aes(x = V1, fill = "100")) +
  labs(y = "Density", x = expression(hat(tau))) +
  theme_classic() +
  scale_fill_manual(name = "Sample Size",values = c("100" = "red","200" = "blue", "300" = "green")) +
  guides(alpha = FALSE)


pate_homo_beta <- ggplot(data = pate_homo, mapping = aes(alpha = 0.3)) +
  geom_density(mapping = aes(x = V6, fill = "300")) +
  geom_density(mapping = aes(x = V4, fill = "200")) +
  geom_density(mapping = aes(x = V2, fill = "100")) +
  labs(y = "Density", x = expression(hat(beta))) +
  theme_classic() +
  scale_fill_manual(name = "Sample Size",values = c("100" = "red","200" = "blue", "300" = "green")) +
  guides(alpha = FALSE)


sate_hetero <- read.csv("sate_hetero.csv")

sate_hetero_tau <- ggplot(data = sate_hetero, mapping = aes(alpha = 0.3)) +
  geom_density(mapping = aes(x = V5, fill = "300")) +
  geom_density(mapping = aes(x = V3, fill = "200")) +
  geom_density(mapping = aes(x = V1, fill = "100")) +
  labs(y = "Density", x = expression(hat(tau))) +
  theme_classic() +
  scale_fill_manual(name = "Sample Size",values = c("100" = "red","200" = "blue", "300" = "green")) +
  guides(alpha = FALSE)

sate_hetero_beta <- ggplot(data = sate_hetero, mapping = aes(alpha = 0.3)) +
  geom_density(mapping = aes(x = V6, fill = "300")) +
  geom_density(mapping = aes(x = V4, fill = "200")) +
  geom_density(mapping = aes(x = V2, fill = "100")) +
  labs(y = "Density", x = expression(hat(beta))) +
  theme_classic() +
  scale_fill_manual(name = "Sample Size",values = c("100" = "red","200" = "blue", "300" = "green")) +
  guides(alpha = FALSE)

sate_homo <- read.csv("sate_homo.csv")

sate_homo_tau <- ggplot(data = sate_homo, mapping = aes(alpha = 0.3)) +
  geom_density(mapping = aes(x = V5, fill = "300")) +
  geom_density(mapping = aes(x = V3, fill = "200")) +
  geom_density(mapping = aes(x = V1, fill = "100")) +
  labs(y = "Density", x = expression(hat(tau))) +
  theme_classic() +
  scale_fill_manual(name = "Sample Size",values = c("100" = "red","200" = "blue", "300" = "green")) +
  guides(alpha = FALSE)


sate_homo_beta <- ggplot(data = sate_homo, mapping = aes(alpha = 0.3)) +
  geom_density(mapping = aes(x = V6, fill = "300")) +
  geom_density(mapping = aes(x = V4, fill = "200")) +
  geom_density(mapping = aes(x = V2, fill = "100")) +
  labs(y = "Density", x = expression(hat(beta))) +
  theme_classic() +
  scale_fill_manual(name = "Sample Size",values = c("100" = "red","200" = "blue", "300" = "green")) +
  guides(alpha = FALSE)

Y <- matrix(c(rnorm(300, m = 10, sd = 1)))
st <- numeric(10000)

for(k in 1:10000){
  X <- matrix(1, nrow = 300, ncol = 2 )
  B <- matrix(NA, nrow = 300, ncol = 1)
  s <- sample(1:300, 150, replace = FALSE)
  X[s, 2] <- 0
  
  for(i in 1:300){
    if(X[i, 2] == 1){
      B[i, 1] <- Y[i, 1] + 2
    } else {
      B[i, 1] <- Y[i, 1]
    }
  }
  
  tib <- as.tibble(cbind(X[, 2], B[, 1]))
  v <- tib %>% group_by(V1) %>% summarize(s = (sd(V2))^2) %>% pull(s)
  st[k] <- sqrt(v[1]/150 + v[2]/150)
  print(k)
}

mean(st)

