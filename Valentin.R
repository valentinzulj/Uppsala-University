library(tidyverse) 
library(randomForest)
library(xtable)
library(modelr)
library(gbm)
library(xgboost)


#### Wrangling ####
data <-  as.tibble(read.csv("data_factors.csv"))
data <- data %>%
  select(-X) %>%
  mutate(Cover_Type = factor(Cover_Type),
         wild = factor(wild),
         soil = factor(soil))                  # Conversion to factors

s <- sample(1:nrow(data), nrow(data)/2, replace = FALSE)  # Sampling test set

test <- data[s, ]   # Test set
test <- test %>%
  select(-Id)
train <- data[-s, ] # Training set
train <- train %>%
  select(-Id)



#### Plots ####
data



#### Models ####

elev_solo <- randomForest(Cover_Type ~ Elevation, data = train)  # Using only elevation


r_mod_all <- randomForest(Cover_Type ~ ., data = train, 
                          mtry = round(sqrt(ncol(train)-1)),
                          importance = TRUE)  # Using all covariates

r_mod_imp <- randomForest(Cover_Type ~ Elevation + soil + h_road + h_fire, 
                          data = train, mtry = 2, importance = TRUE) # The most important

boost_mod <- gbm(Cover_Type ~ ., distribution = "multinomial",       # Boost model
                 data = train, n.trees = 500, shrinkage = 0.3, n.cores = 2)



#### Hit Rates ####

elv_class <- test %>%
  add_predictions(elev_solo) %>%
  mutate(hit = ifelse(Cover_Type == pred, TRUE, FALSE)) %>%
  summarize(hit_rate = mean(hit))
elv_class                          # Hit rate using only elevation

all_class <- test %>%
  add_predictions(r_mod_all) %>%
  mutate(hit = ifelse(Cover_Type == pred, TRUE, FALSE)) %>%
  summarize(hit_rate = mean(hit))
all_class                          # Hit rate using all variables

imp_class  <- test %>%
  add_predictions(r_mod_imp) %>%
  mutate(hit = ifelse(Cover_Type == pred, TRUE, FALSE)) %>%
  summarize(hit_rate = mean(hit))
imp_class                          # Hit rate using important variables

boost_class <- predict.gbm(boost_mod, test, # Boost predictions
                           type = 'response', n.trees = 500)

for(j in 1:ncol(boost_class)){
  for(i in 1:nrow(boost_class)){
    if(boost_class[i, j, ] == max(boost_class[i, , ])){
      boost_class[i, j, ] = j
    } else {
      boost_class[i, j, ] <- 0
    }
  }
}
boost_class <- rowSums(boost_class)
boost_class <- factor(boost_class)
boost_class <- boost_class == test$Cover_Type
mean(boost_class)
summary(boost_class)
