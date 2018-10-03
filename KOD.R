library(tidyverse)


data <- read.csv("train.csv")
for(i in 1:ncol(data)){
  if(grepl("Type", colnames(data)[i])| grepl("Area", colnames(data)[i])){
    data[, i] = factor(data[, i])  
  }
}
data <- as.tibble(data)
data

data <- data %>%
  mutate(h_fire = Horizontal_Distance_To_Fire_Points,
         h_water = Horizontal_Distance_To_Hydrology,
         h_road = Horizontal_Distance_To_Roadways,
         v_water = Vertical_Distance_To_Hydrology) %>%
  select(-starts_with("Horizontal"), -starts_with("Vertical"))

data %>%
  ggplot(mapping = aes(x = Elevation, fill = Cover_Type, alpha = 0.5)) +
  geom_density()
