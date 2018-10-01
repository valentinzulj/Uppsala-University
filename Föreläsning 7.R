library(tidyverse)
library(stringr)
library(forcats)
library(modelr)
library(StatProg)

data(titanic)
titanic


ttn

ttn <- ttn %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex),
         Embarked = factor(Embarked))
ttn

select(ttn, Name)

ttn <- ttn %>%
  mutate(FamSize = SibSp + Parch + 1)

ggplot(ttn, aes(x = FamSize)) + 
  geom_bar()


ttn %>%
  summarize_all(funs(sum(. == "" | is.na(.))))

title <- c("Doe, Mr. John", "Doe, Mrs. Jane", "SMITH, Master. John")
str_extract(title, "Mr")

str_extract(title, "^Doe")   # ^ matches the beginning
str_extract(title, "Doe$")   # Something that ends with Doe
str_extract(title, "[ABCD]") # No capital a, b ,c or d in SMITH
str_extract(title, "[A-Z]")  # Matching all capitals
str_extract(title, "[A-Z]*") # Keeps going after first match

str_split(title, "Mr", simplify = TRUE)
str_split(title, "[,.]", simplify = TRUE)

str_trim("   Hello my friend   ")

str_split(title, "[,.]", simplify = TRUE)[, 2] %>%
  str_trim()

full_title <- str_split(ttn$Name, "[,.]", simplify = TRUE)[, 2] %>%
  str_trim()

ttn <- ttn %>%
  mutate(Title = full_title)
ttn

x <- 1:10
cut(x, c(0, 1, 5, 8, Inf)) # Grouping data to predetermined intervals

ttn <- ttn %>%
  mutate(FamSize = cut(FamSize, breaks = c(0, 1, 3, Inf),
                       labels = c("single", "small", "large")))

ttn <- ttn %>%
  mutate(Title = fct_collapse(Title, 
                              Miss = c("Ms", "Mlle", "Miss"),
                              Mrs = c("Mrs", "Mme"),
                      Noble = c("the Countess", "Sir", "Rev", "Major", "Lady",
                                "Jonkheer", "Dr", "Don", "Dona", "Col", "Capt")))
ttn

ttn <- ttn %>%
  mutate(Title = fct_relevel(Title,
                             c("Master", "Miss", "Mr", "Mrs", "Noble")))

filter(ttn, is.na(Age)) %>%
  ggplot(aes(x = Title, fill = Pclass)) +
  geom_bar(position = "dodge")


filter(ttn, !is.na(Age)) %>%
  ggplot(aes(x = Pclass, y = Age)) +
  geom_boxplot() +
  facet_grid(~ Title)

ttn <- ttn %>%
  group_by(Pclass, Title) %>%
  mutate(AgeImputed = is.na(Age),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age)) %>%
  ungroup()

ggplot(ttn, aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  facet_grid(~AgeImputed, margins = TRUE)

n <- 20
tibb <- tibble(y = rnorm(n), x1 = rnorm(n),
               x2 = rnorm(n), x3 = rnorm(n))

model_matrix(tibb, y ~ x1 + x2)
model_matrix(tibb, y ~ 0 + x1 + x2) # Supressing intercept
model_matrix(tibb, y ~ x1 + I(x1^2))
model_matrix(tibb, y ~ (x1 + x2 + x3)^2) # Includes interactions of degree k

model_matrix(tibb, y ~ .)


tibb <- tibb %>%
  mutate(fact = factor(rep(c("A", "B", "C", "D"), 5)))
tibb
model_matrix(tibb, y ~ fact )

mat <- ttn %>%
  model_matrix(Survived ~ Sex + Pclass)


m1 <- lm(as.numeric(Survived) ~ Sex + Pclass, data = ttn)
m1

m2 <- lm(as.numeric(Survived) ~ Sex + Pclass + Age + Title + FamSize, data = ttn)
summary(m2)


mod_1 <- glm(Survived ~ Sex + Pclass,
             data = filter(ttn, Train == TRUE),
             family = binomial)
summary(mod_1)

pred_1 <- predict(mod_1, newdata = filter(ttn, Train == FALSE))
pred_1 # returns log odds

pred_1 <- predict(mod_1, newdata = filter(ttn, Train == FALSE),
                  type = "response") # Predicts on response scale
pred_1

filter(ttn, Train == FALSE) %>%
  add_predictions(mod_1) %>% # From modelr package, add pred column (log odds)
  mutate(p_pred = exp(pred)/(1+exp(pred)),# Computing probs
         b_pred = round(p_pred), # Binary predictions
         correct = (Survived == b_pred)) %>% # Binary predictions
  select(p_pred, b_pred, Survived, correct) %>%
  summarize( pct = mean(correct))

pct_correct <- function(mod, ttn){
  filter(ttn, Train == FALSE) %>%
    add_predictions(mod) %>% # From modelr package, add pred column (log odds)
    mutate(p_pred = exp(pred)/(1+exp(pred)),# Computing probs
           b_pred = round(p_pred), # Binary predictions
           correct = (Survived == b_pred)) %>% # Binary predictions
    select(p_pred, b_pred, Survived, correct) %>%
    summarize(pct = mean(correct))
}

pct_correct(mod_1, ttn)

mod_2 <- glm(Survived ~Sex + Pclass + Title,
             data = filter(ttn, Train == TRUE),
             family = binomial)

mod_3 <- glm(Survived ~Sex + Pclass + Title + Age + FamSize,
             data = filter(ttn, Train == TRUE),
             family = binomial)

mod_4 <- glm(Survived ~Sex + Pclass + Title + Age + I(Age^2) + FamSize,
             data = filter(ttn, Train == TRUE),
             family = binomial)

pct_correct(mod_1, ttn)
pct_correct(mod_2, ttn)
pct_correct(mod_3, ttn)
pct_correct(mod_4, ttn)
