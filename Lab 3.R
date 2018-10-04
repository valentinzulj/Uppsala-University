library(tidyverse)
library(StatProg)
library(stringr)
library(forcats)
library(modelr)
library(devtools)
data("birpanel")
birpanel

birpanel <- birpanel %>%
  mutate(LBW = ifelse(dbirwt < 2500, TRUE, FALSE))

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


birpanel %>%
  mutate(pretri2 = 2*pretri2,
         pretri3 = 3*pretri3,
         prenatal = factor(novisit + pretri2 + pretri3),
         prenatal = fct_recode(prenatal,
                               "First_Tri" = "0",
                               "No_Visit" = "1",
                               "Second_Tri" = "2",
                               "Third_Tri" = "3")) %>%
  count(prenatal)
  
  
  
  
  


