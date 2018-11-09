library(tidyverse) 
library(randomForest)
library(xtable)
library(modelr)
library(gbm)
library(knitr)
library(nnet)
library(party)

#### Wrangling ####
data_original <- read.csv("train.csv")

d <- data_original %>% # Making one area variable
  select(Wilderness_Area1:Wilderness_Area4)

for(i in 1:nrow(d)){
  for (j in 1:ncol(d)){
    if(d[i,j] == 1){
      d[i,j] <- j
    }
  }
} # Recoding areas
d <- d %>%
  
  mutate(wild = factor(rowSums(.))) %>% # Summing Rows
  select(wild)


e <- data_original %>% # Making one area variable
  select(Soil_Type1:Soil_Type40)

for(i in 1:nrow(e)){
  for (j in 1:ncol(e)){
    if(e[i,j] == 1){
      e[i,j] <- j
    }
  }
} # Recoding soil

e <- e %>%
  mutate(soil = factor(rowSums(.))) %>% # Summing rows
  select(soil)


data_original <- data.frame(data_original, d, e)
data_original <- as.tibble(data_original)
data_original <- data_original %>%
  mutate(h_fire = Horizontal_Distance_To_Fire_Points, # Changing names
         h_water = Horizontal_Distance_To_Hydrology,
         h_road = Horizontal_Distance_To_Roadways,
         v_water = Vertical_Distance_To_Hydrology,
         Cover_Type = factor(Cover_Type)) %>%         # Cover as factor
  select(-starts_with("Horizontal"), -starts_with("Vertical"), # Removing old variables 
         -starts_with("Wilderness"), - starts_with("Soil_Type"))

write.csv(data_original, "data_factors.csv")


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

tree <- ctree(Cover_Type ~ ., data = train)

mult <- multinom(Cover_Type ~ ., data = train)

elev_soil <- randomForest(Cover_Type ~ Elevation + soil, data = train,
                          maxnodes = 2500)  # Using only elevation
elev_soil$confusion                         # Confusion table

r_mod_all <- randomForest(Cover_Type ~ ., data = train,
                          importance = TRUE, maxnodes = 2500)  # Using all covariates
r_mod_all$confusion                         # Confusion table



#### Tuning ####
nodes <- round(seq(100, 10000,length.out = 15))
n <- length(nodes)
rates <- numeric(n)

for (i in 1:n){ # Testing the effect of maxnodes
  mod <- randomForest(Cover_Type ~ ., data = train, 
                      maxnodes = nodes[i])
  elv <- test %>%
    add_predictions(mod) %>%
    mutate(hit = ifelse(Cover_Type == pred, TRUE, FALSE)) %>%
    summarize(hit_rate = mean(hit)) %>%
    pull(hit_rate)
  rates[i] <- elv                        
  print(i)        # Keeping track of progress
  
}

plot(rates)

tune <- tibble(allowed = nodes,
               maxnodes = rates)


trees <- round(seq(100, 10000,length.out = 15))
k <- length(trees)
rate <- numeric(n)

for (i in 1:k){ # Testing the effect of maxnodes
  mod <- randomForest(Cover_Type ~ ., data = train, 
                      ntree = trees[i], maxnodes = 2500)
  elv <- test %>%
    add_predictions(mod) %>%
    mutate(hit = ifelse(Cover_Type == pred, TRUE, FALSE)) %>%
    summarize(hit_rate = mean(hit)) %>%
    pull(hit_rate)
  rate[i] <- elv                        
  print(i)
  
}

tune <- tibble(allowed = nodes,
               maxnodes = rates,
               trees = rate)
write.csv(tune, "tune.csv")


tune <- read.csv("tune.csv")

tune %>%
  ggplot() +
  geom_line(mapping = aes(x = allowed, y = maxnodes, col = "No. of Nodes")) +
  geom_line(mapping = aes(x = allowed, y = trees, col = "No. of Trees")) +
  labs(y = "Classification Rate (%)",
       x = "Number Allowed") +
  scale_y_continuous(breaks = c(0.75, 0.775, 0.8, 0.825, 0.850),
                     labels = c("75", "77.5", "80", "82.5", "85")) +
  scale_x_continuous(breaks = c(100, 2500, 5000, 7500, 10000),
                     labels = c("100", "2500", "5000", "7500", "10000")) +
  guides(color = guide_legend((title = NULL))) +
  theme_classic()
  
#### Hit Rates ####

tree_class <- test %>%
  add_predictions(tree) %>%
  mutate(hit = ifelse(Cover_Type == pred, TRUE, FALSE)) %>%
  select(Cover_Type, pred, hit) %>%
  summarize(hit_rate = mean(hit)) %>%
  pull(hit_rate)
tree_class

a <- table(test$Cover_Type, predict(tree, test))
b <- round((rowSums(a) - diag(a))/rowSums(a), digits = 3)
tree_conf <- cbind(a,b) # Confusion table

mult_class <- test %>%
  add_predictions(mult) %>%
  mutate(hit = ifelse(Cover_Type == pred, TRUE, FALSE)) %>%
  select(Cover_Type, pred, hit) %>%
  summarize(hit_rate = mean(hit)) %>%
  pull(hit_rate)
mult_class

a_mult <- table(test$Cover_Type, predict(mult, test))
b_mult <- round((rowSums(a_mult) - diag(a_mult))/rowSums(a_mult), digits = 3)
mult_conf <- cbind(a_mult,b_mult) # Confusion table


elv_class <- test %>%
  add_predictions(elev_soil) %>%
  mutate(hit = ifelse(Cover_Type == pred, TRUE, FALSE)) %>%
  summarize(hit_rate = mean(hit)) %>%
  pull(hit_rate)
elv_class                          # Hit rate using only elevation

all_class <- test %>%
  add_predictions(r_mod_all) %>%
  mutate(hit = ifelse(Cover_Type == pred, TRUE, FALSE)) %>%
  summarize(hit_rate = mean(hit)) %>%
  pull(hit_rate)
all_class                          # Hit rate using all variables




#### Plots ####

imp <- as.tibble(importance(r_mod_all, type = 2))
vals <- imp %>%
  pull(MeanDecreaseGini)
names <- rownames(importance(r_mod_all, type = 2)) 
imp <- tibble(var = names,
              gini = vals)

imp %>%  # Variable importance plot
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

ggplot(data, aes(x = Elevation, fill = Cover_Type)) + # Elevation density plot
  geom_density(alpha = 0.8) +
  labs(fill = "Cover Type",
       y = "Density") +
  theme_classic()

ggplot(data, aes(x = Hillshade_3pm, y = Elevation, color = Cover_Type, size = h_road)) +
  geom_point() +
  labs(color = "Cover Type",
       size = "Horiz. Dist. to Roads",
       x = "Afternoon Hillshade") +
  theme_classic()  # Elevation - hillshade scatterplot

ggplot(data, aes(x = Cover_Type, y = h_water, fill = Cover_Type)) +
  geom_boxplot() +
  labs(fill = "Cover Type",
       x = "Cover Type",
       y = "Horizontal Distance to Water") +
  theme_classic()   # Cover type - water boxplot

  
  ggplot(data, aes(y = Elevation, x = soil, color = Cover_Type, alpha = 0.3)) +
  geom_jitter() +
  labs(color = "Cover Type",
       x = "Soil Type",
       y = "Elevation") +
  guides(alpha = FALSE) +
  theme_classic()  # Soil type elevation csatterplot

#### Simulation ####
  nodes <- round(seq(100, 4000, length.out = 15))
  n <- length(nodes)
  models <- list()
  nodes[[2]]
  for (i in 1:length(nodes)) {
    models[[i]]  <- randomForest(Cover_Type ~ ., 
                                 data = data, 
                                 maxnodes = nodes[i])
    print(i)
  }
### New data ###
  new_data <- data
  probs <- matrix(ncol = 7, nrow = nrow(data))
  for (i in 1:ncol(data)){
    dep <- data %>%
      pull(i)
    indep <- data %>%
      select(-i)
    d <- cbind(dep, indep)
    if(is.factor(dep)){
      if(i == 7){
        model <- multinom(dep ~ ., data = d)
        probs <- predict(model, d, type = "prob")
      }
      model <- multinom(dep ~ ., data = d)
    }else{
      model <- lm(dep ~ ., data = d)
    }
    if(i == 1){
      new_data[, i] <- predict(model, d) + rnorm(mean = 0, sd = 99, n = 1)
    }else if(i == 7){
      for (j in 1:nrow(data)) {
        new_data[j,i] <- sample(c(1, 2, 3, 4, 5, 6, 7), size = 1, prob = probs[j, ], replace = TRUE)
      }
    }else{
      new_data[, i] <- predict(model, d)
    }
  }
  olika_prediktioner <- matrix(ncol = length(nodes), nrow = 30)   # Matrix for hit rate
  preddar <- as.data.frame(matrix(ncol = 1, nrow = nrow(data)))   # Vector for predicitons
  
  for (l in 1:30) {
    for (i in 1:ncol(data)){
      dep <- data %>%
        pull(i)
      indep <- data %>%
        select(-i)
      d <- cbind(dep, indep)
      if(is.factor(dep)){
        if(i == 7){
          model <- multinom(dep ~ ., data = d)
          probs <- predict(model, d, type = "prob")
        }
        model <- multinom(dep ~ ., data = d)
      }else{
        model <- lm(dep ~ ., data = d)
      }
      if(i == 1){
        new_data[, i] <- predict(model, d) + rnorm(mean = 0, sd = 99, n = 1)
      }else if(i == 7){
        for (j in 1:nrow(data)) {
          new_data[j,i] <- sample(c(1, 2, 3, 4, 5, 6, 7), size = 1, prob = probs[j, ], replace = TRUE)
        }
      }else{
        new_data[, i] <- predict(model, d)
        print(i)
      }
    }
    for (k in 1:length(nodes)){
      preddar <- predict(models[[k]], newdata = new_data)
      c <- as.data.frame(cbind(preddar, new_data$Cover_Type))
      c <- c %>%
        mutate(korrekt = ifelse(preddar == V2, TRUE, FALSE))
      olika_prediktioner[l, k] <- mean(c$korrekt)
    }
    print(l)
  }
  write.csv(olika_prediktioner, "olika_prediktioner.csv")
  