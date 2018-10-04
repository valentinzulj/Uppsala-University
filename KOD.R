library(tidyverse)
library(stringr)
require(gridExtra)

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

data <- read.csv("train.csv")

d <- data %>% # Making one area variable
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


e <- data %>% # Making one area variable
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


data <- data.frame(data, d, e)
data <- as.tibble(data)
data
data <- data %>%
  mutate(h_fire = Horizontal_Distance_To_Fire_Points,
         h_water = Horizontal_Distance_To_Hydrology,
         h_road = Horizontal_Distance_To_Roadways,
         v_water = Vertical_Distance_To_Hydrology,
         Cover_Type = factor(Cover_Type)) %>%
  select(-starts_with("Horizontal"), -starts_with("Vertical"),
         -starts_with("Wilderness"), - starts_with("Soil_Type"))

data

data %>%
  ggplot(mapping = aes(x = Elevation, fill = Cover_Type, alpha = 0.5)) +
  geom_density() +
  labs(title = "Density plots",
       subtitle = "Different cover types") +
  scale_alpha(guide = "none") +
  labs(x = "Elevation",
       y = "Density",
       fill = "Cover Type") +
  theme_forest

data %>%
  ggplot(mapping = aes(x = Cover_Type, y = h_road)) +
  geom_point()

bar_soil <- data %>%
  ggplot(mapping = aes(x = soil)) +
  geom_bar()

bar_wild <- data %>%
  ggplot(mapping = aes(x = wild)) +
  geom_bar()

grid.arrange(bar_soil, bar_wild, ncol = 2)




