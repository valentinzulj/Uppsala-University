library(tidyverse)
library(StatProg)
library(stringr)
library(forcats)
library(modelr)
library(devtools)
library(MASS)
library(broom)

######### Modelling #########
data("birpanel")
birpanel

birpanel <- birpanel %>%
  mutate(LBW = ifelse(dbirwt < 2500, TRUE, FALSE))

#### Q1 ####
birpanel %>%
  count(LBW)
#### Answer: 11439 ####

birpanel <- birpanel %>%
  mutate(cigar = na_if(cigar, 99))

birpanel %>%
  summarise_at(c("dbirwt", "LBW", "gestat", "smoke", "dmage", "dmeduc",
                 "married", "black", "novisit",
                 "pretri2", "pretri3"), mean)


birpanel <- birpanel %>% 
  mutate(somecoll = 2*somecoll,
         collgrad = 3*collgrad) %>%
  mutate(educ = factor(hsgrad + somecoll + collgrad),
         educ = fct_recode(educ,
                           "No_HS" = "0",
                           "HS_Grad" = "1",
                           "Some_Coll" = "2",
                           "Coll_Grad" = "3"))



birpanel <- birpanel %>%
  mutate(adeqcode2 = 2*adeqcode2,
         adeqcode3 = 3*adeqcode3, 
         kessner = factor(adeqcode2 + adeqcode3),
         kessner = fct_recode(kessner,
                              "1" = "0"))


birpanel <- birpanel %>%
  mutate(pretri2 = 2*pretri2,
         pretri3 = 3*pretri3,
         prenatal = factor(novisit + pretri2 + pretri3),
         prenatal = fct_recode(prenatal,
                               "No_Visit" = "1",
                               "First_Tri" = "0",
                               "Second_Tri" = "2",
                               "Third_Tri" = "3")) 


lm_birwt <- lm(dbirwt ~ smoke + male + dmage + agesq + educ + 
       married + black + kessner + kessner + prenatal + 
         factor(stateres) + factor(year) + factor(idx), data = birpanel)
lm_birwt
summary(lm_birwt)

lm_birwt_reduced <- lm(dbirwt ~ smoke + male + dmage + agesq + educ + 
                         married + black + kessner + kessner + prenatal, data = birpanel)


######################
####  Question 2  ####
#### Ans: 514.8^2 ####
######################


############### Simulation #################

sigma <- matrix(c(1, 0.75, 0.75, 1), # Variance covariance
                nrow = 2)
n <- 50
mu <- c(0, 0)
x <- mvrnorm(n, mu = mu, Sigma = sigma)
e <- rnorm(50, 0, 1)
b <- matrix(c(0, 0.5), nrow = 2)
y1 <-  x%*%b + e


R <- 10000
t_val <- numeric(R)

for(i in 1:R){
  x <- mvrnorm(n, mu = mu, Sigma = sigma)
  e <- rnorm(50, 0, 1)
  y <- x %*% b + e
  mod <- lm(y[, 1] ~ x[, 1] + x[, 2])
  if(as.numeric(tidy(mod)[3,5]) < 0.05){               # If significant
    t_val[i] <- as.numeric(tidy(mod)[2,4])             # Save p-value
  } else {                                             # If not significant
    mod_red <- lm(y[, 1] ~ x[, 1])                     # Run reduced
    t_val[i] <- as.numeric(tidy(mod_red)[2,4])         # Save p-vale
  }
}


ggplot(mapping = aes(x = t_val, alpha = 0.5)) +
  geom_density(fill = "blue", color = "blue") +
  theme_classic(base_family = "Optima") +
  labs(x = expression(t[hat(beta)[1]]),
       y = "Density",
       title = expression(Density~of~t[hat(beta)[1]])) +
  theme(plot.title = element_text(face = "bold",              
                                 hjust = 0.5, size = 15),    
        plot.subtitle = element_text(colour = "gray35", 
                                     hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 15)) +
  scale_alpha(guide = "none")


t_abs <- abs(t_val)  
for(i in 1:length(t_abs)){
  if(t_abs[i] <= 1.96){
    t_abs[i] = TRUE
  } else {
    t_abs[i] = FALSE
  }
}
mean(t_abs)





  
  
  
  
  


