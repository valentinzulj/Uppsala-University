library(tidyverse)
data <- as.tibble(read.table("Exam Data.txt", header = TRUE))
data <- data %>%
  mutate(ID = factor(ID),
         weekday = factor(weekday),
         workingday = factor(workingday),
         weather = factor(weather))       # Creating factors
                
data <- data %>%
  mutate(test = casual/cnt)

data %>% summarize_all(funs(sum(is.na(.)))) # Looking for missingness

data %>%
  ggplot(mapping = aes(x = test)) +
  geom_density()
