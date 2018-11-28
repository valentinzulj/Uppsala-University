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


bin_plot <- ggplot(mapping = aes(x = b, y = p1)) + 
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
print(zi_count_mod)
exp(coef(zi_count_mod)[2])/(1+exp(coef(zi_count_mod)[2]))

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
