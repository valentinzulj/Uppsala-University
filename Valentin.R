library(tidyverse) 
library(randomForest)
library(xtable)
library(modelr)
library(gbm)
library(knitr)
library(nnet)

write_bib("nnet", file = "pack.bib")

#### Theme ####
theme_forest <- theme_classic(base_family = "Optima") +       # Changes font
  theme(panel.background = element_rect(fill = "gray96"),     # Panel colour
        plot.background = element_rect(fill = "gray96"),      # Plot colour
        plot.title = element_text(face = "bold",              
                                  hjust = 0.5, size = 15),    # Centers title
        plot.subtitle = element_text(colour = "gray35", 
                                     hjust = 0.5, size = 10), # Centers subtitle
        axis.text.x = element_text(face="bold"),              
        axis.text.y = element_text(face="bold"),
        panel.grid.minor = element_blank(),                   # Removes minor grid
        panel.grid.major = element_line(colour = "gray87"),   # Major grid colour
        axis.ticks.x = element_blank(),                       # Removes x ticks
        axis.ticks.y = element_blank(),                       # Removes y ticks
        axis.line.x.bottom = element_line(colour = "gray60"), # Colour of x axis
        axis.line.y.left = element_line(colour = "gray60"),   # Colour of y axis
        legend.background = element_rect(fill = "gray96",     # Background of legend
                                         colour = NA)
  )


#### Wrangling ####
data <-  as.tibble(read.csv("data_factors.csv"))
data <- data %>%
  select(-X) %>%
  mutate(Cover_Type = factor(Cover_Type),
         wild = factor(wild),
         soil = factor(soil))                  # Conversion to factors
set.seed(1)
s <- sample(1:nrow(data), nrow(data)/2, replace = FALSE)  # Sampling test set

test <- data[s, ]   # Test set
test <- test %>%
  select(-Id)
train <- data[-s, ] # Training set
train <- train %>%
  select(-Id)

t <- test %>%
  filter(soil == c(8,25))
train <- rbind(train,t)

test <- test %>%
  filter(soil != c(8,25))

#### Models ####

elev_solo <- randomForest(Cover_Type ~ Elevation, data = train)  # Using only elevation


r_mod_all <- randomForest(Cover_Type ~ ., data = train, 
                          mtry = round(sqrt(ncol(train)-1)),
                          importance = TRUE)  # Using all covariates

r_mod_imp <- randomForest(Cover_Type ~ Elevation + soil + h_road + h_fire, 
                          data = train, mtry = 2, importance = TRUE) # The most important

boost_mod <- gbm(Cover_Type ~ ., distribution = "multinomial",       # Boost model
                 data = train, n.trees = 1000, shrinkage = 0.35, n.cores = 2)

mult_mod <- multinom()


#### Tuning ####
nodes <- round(seq(1000, 10000,length.out = 5))
n <- length(nodes)
rates <- numeric(n)

for (i in 1:n){
  mod <- randomForest(Cover_Type ~ ., data = train, 
                      maxnodes = )
  elv <- test %>%
    add_predictions(mod) %>%
    mutate(hit = ifelse(Cover_Type == pred, TRUE, FALSE)) %>%
    summarize(hit_rate = mean(hit)) %>%
    pull(hit_rate)
  rates[i] <- elv                        
  print(i)
  
}

plot(rates)


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
                           type = 'response', n.trees = 1000)

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


#### Plots ####

imp <- as.tibble(importance(r_mod_all, type = 2))
vals <- imp %>%
  pull(MeanDecreaseGini)
names <- rownames(importance(r_mod_all, type = 2)) 
imp <- tibble(var = names,
              gini = vals)

imp %>%
  arrange(var) %>%
  ggplot(mapping = aes(x = gini, y = fct_reorder(var, gini))) +
  geom_point() +
  theme_forest +
  labs(x = "Mean decrease in Gini index",
       y = "Variable",
       title = "Variable Importance") +
  scale_y_discrete(labels = c("Slope", "Afternoon Shade", "Noon Shade", 
                              "Aspect", "Vertical Water",
                              "Morning Shade", "Horizontal Water", "Wilderness Area",
                              "Horizontal Fire", "Horizontal Road", "Soil Type", "Elevation"))+
  theme(panel.grid.major.x = element_blank())
