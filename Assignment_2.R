library(tidyverse)
library(MASS)
library(pscl)
library(mediation)
library(broom)
library(xtable)
library(VIM)
library(naniar)
library(gridExtra)
library(mice)


#################################
#### Apply and Interpret GLM ####
#################################
count <- as.tibble(read.table("count_dat.txt", header = TRUE))

#### Logit Model ####
count <- count %>%
  mutate(binary = ifelse(Count == 0, 0, 1))

binary <- glm(binary ~ X1 + X2, family = binomial(link = "logit"), data = count)
summary(binary)

n <- 1000
a <- matrix(1, nrow = n)
b <- matrix(c(seq(-10, 10, length.out = 100)), nrow = n)
c <- matrix(mean(count$X2), nrow = n)
A <- cbind(a,b,c)

B <- matrix(c(binary$coefficients), nrow = 3)

probs <- c(exp(A %*% B))/(1 + c(exp(A %*% B)))


bin_plot <- ggplot(mapping = aes(x = b, y = probs)) + 
  geom_line(color = "blue") +
  labs(x = expression(X[1]),
       y = "Probability")+
  theme_classic()
bin_plot

a1 <- matrix(1, nrow = 2) 
b1 <- matrix(c(mean(count$X1),mean(count$X1) + sd(count$X1)), nrow = 2)
c1 <- c[1:2, ]
A1 <- cbind(a1, b1, c1)

OR <- exp(A1[2, ] %*% B - A1[1, ] %*% B)

#### Categorical ####
categorical <- polr(as.factor(Count) ~ X1 + X2, data = count, Hess = TRUE)
summary(categorical)
confint(categorical)

ctable <- coef(summary(categorical))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable         

xtable(ctable, digits = c(5, 5, 5, 5, 5))
#### Count ####
count_mod <- glm(Count ~ X1 + X2, 
                 family = poisson(link = "log"), data = count) # Poisson Regression
summary(count_mod)


quasi_count_mod <- glm(Count ~ X1 + X2, 
                       family = quasipoisson, data = count) # Poisson With Overdispersion
summary(quasi_count_mod)

zi_count_mod <- zeroinfl(Count ~ X1 + X2|X1 + X2, 
                         data = count, dist = "poisson", EM = TRUE) # Zero Inflation Poissin
summary(zi_count_mod)

zi_beta <- matrix(c(zi_count_mod$coefficients$zero), ncol = 1)
x1 <- matrix(c(1, mean(count$X1), mean(count$X2)), nrow = 1)
x2 <- matrix(c(1, mean(count$X1) + sd(count$X1), mean(count$X2)), nrow = 1)

exp(x1 %*% zi_beta)/(1+exp(x1 %*% zi_beta))
exp(x2 %*% zi_beta)/(1+exp(x2 %*% zi_beta))

count %>%
  ggplot(mapping = aes(x = Count, y = X1)) +
  geom_point(mapping = aes(alpha = 3), color = "blue") + 
  labs(y = expression(X[1])) + 
  guides(alpha = FALSE) +
  theme_classic()



#################################
####  Mediation & Moderation ####
#################################
med <- as.tibble(read.table("med_dat.txt", header = TRUE))

#### Moderation ####
moderator <- lm(perform ~ negexp*negtone + dysfunc, data = med)

basic <-  lm(perform ~ negexp + negtone + dysfunc, data = med)
n <- seq(min(med$negtone), max(med$negtone), length.out = 100)
y1 <- matrix(mean(med$negexp), nrow = 100)
y2 <- matrix(n, nrow = 100)
y3 <- matrix(mean(med$dysfunc), nrow = 100)
y4 <- matrix(y1*y2, nrow = 100)
Y <- cbind(1,y1,y2,y3,y4)

b1 <- matrix(basic$coefficients, nrow = 4)
b2 <- matrix(moderator$coefficients, nrow = 5)

u <- Y[,1:4] %*% b1
v <- Y %*% b2

ggplot() +
  geom_line(mapping = aes(x = n, y = c(u), color = "Ordinary")) +
  geom_line(mapping = aes(x = n, y = c(v), color = "Moderated")) +
  labs(y = "Fitted Value", x = "Negtone") +
  theme_classic()


#### Mediation ####
med_mod <- lm(negtone ~ dysfunc, data = med)
out_mod <- lm(perform ~ negtone + dysfunc, data = med)
mediator <- mediate(med_mod, out_mod, treat = "dysfunc", mediator = "negtone")
summary(mediator)

#################################
####     Missing Data and    ####   
####   Multiple Imputations  ####
#################################

#### Applying mice ####
count_missing <- as.tibble(read.table("count_dat_missing.txt", header = TRUE))

na_count <- count_missing[!complete.cases(count_missing), ]
ok_count <- count_missing[complete.cases(count_missing), ]

na_outcome <- ggplot(data = na_count, mapping = aes(x = Count)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") + 
  labs(title = "Missing", y = NULL) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

ok_outcome <- ggplot(data = ok_count, mapping = aes(x = Count)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") + 
  labs(y = "Frequency", title = "Full") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

na_x2 <- ggplot(data = na_count, mapping = 
                  aes(x = Count, y = X2))+
  geom_point() +
  labs(x = "Count", y = NULL) +
  theme_classic()

ok_x2 <- ggplot(data = ok_count, mapping = 
                  aes(x = Count, y = X2))+
  geom_point() +
  labs(y = expression(X[2]), x = "Count") +
  theme_classic()

grid.arrange(ok_outcome, na_outcome, ok_x2, na_x2, ncol = 2)

imps <- mice(count_missing, seed = 4419, printFlag = FALSE)
summary(imps)

fit <- with(imps, glm(Count ~ X1 + X2, family = poisson(link = log)))
summary(pool(fit))


#### Multiple Imputation for Regression ####
missing_sex <- as.tibble(read.table("SEXDISC_missing.txt"
                                    , header = TRUE))
imp_sex <- mice(missing_sex, seed = 666, printFlag = FALSE)
imp_sex_long <- complete(imp_sex, action = "long")

rubin <- function(data){
  data <- subset(data, select = -.id)                            # Removing id col
  v_cov <- array(NA, dim = c((ncol(data) - 1), (ncol(data) - 1),
                             length(unique(data$.imp))))         # Preallocation
  beta <- matrix(NA, ncol = length(unique(data$.imp)),
                 nrow = (ncol(data) - 1))                        # Preallocation
  for(i in unique(data$.imp)){
    d <- data %>%
      filter(.imp == i)
    mod <- lm(liking ~ sexism + approp + factor(protest), data = d)
    v_cov[, , i] <- diag(vcov(mod))
    beta[, i] <- mod$coefficients
  }                               # Beta & vcov
  beta_bar_m <- rowMeans(beta)                                   # Beta mean
  B_mat <- array(NA, dim = c((ncol(data) - 1), (ncol(data) - 1), 
                             length(unique(data$.imp))))         # Preallocation
  B_m <- matrix(0, nrow = (ncol(data) - 1), 
                ncol = (ncol(data) - 1))                         # Preallocation
  v_sum <- matrix(0, nrow = (ncol(data) - 1), 
                  ncol = (ncol(data) - 1))                       # Preallocation  
  for(i in unique(data$.imp)){                                   # Variance loop  
    B_mat[, , i] <- crossprod(t((beta[, i] - beta_bar_m)))
    B_m <- B_m + B_mat[, , i]
    v_sum <- v_sum + v_cov[, , i]
  }
  B <- (1/(ncol(data) - 1))*B_m                                  # B matrix
  V <- (1/length(unique(data$.imp)))*v_sum                       # Sigma sum
  sigma_b_bar <- V + (1+(1/length(unique(data$.imp))))*B         # Pooled var
  s_err <- sqrt(diag(sigma_b_bar))                               # Std. Errors
  out <- cbind(beta_bar_m, s_err)                                # Output matrix
  colnames(out) <- c("Estimate", "Std. Error")
  return(out)
} 
# Note that imput needs to be the "long" output of complete(<mice object>)
rubin(imp_sex_long) # Estimates

f <- with(imp_sex, lm(liking ~ sexism + approp + factor(protest)))
summary(pool(f))    # Comparing with mice 

