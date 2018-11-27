library(tidyverse)
library(MASS)
library(pscl)
library(mediation)
library(broom)
library(xtable)

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
            
probs <- numeric(n)

for(i in 1:n){
  probs[i] <- (exp(A[i, ] %*% B)/(1 + exp(A[i, ] %*% B)))
}



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




                    #################################
                    ####  Mediation & Moderation ####
                    #################################
med <- as.tibble(read.table("med_dat.txt", header = TRUE))
                 
#### Moderation ####
moderator <- lm(negtone ~ negexp, data = med)
lm_mod <- lm(perform ~ dysfunc + negtone + negexp + negtone*negexp, data = med)
moderator_model <- mediate(moderator, lm_mod, sims=500, treat="negexp", mediator="negtone")
summary(moderator_model)
