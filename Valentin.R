library(tidyverse) 
library(randomForest)
library(xtable)
library(modelr)
library(gbm)
library(knitr)
library(nnet)
library(party)

write_bib(c("party", "nnet"), file = "pack.bib")

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
  print(i)
  
}

plot(rates)

tune <- tibble(allowed = nodes,
               maxnodes = rates)

tune %>%
  ggplot(mapping = aes(x = allowed, y = maxnodes)) +
  geom_point()


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
