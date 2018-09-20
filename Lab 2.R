install_github("ankargren/StatProg")
library(StatProg)
library(tidyverse)
library(devtools)
library(readr)
library(readxl)
data(senate)

#############################################
################ Exercise 1 #################
#############################################


### The so called canvas, with points added to it ###
p <- ggplot(data = senate, mapping = aes(
  x = presidential_approval, 
  y = election_result)) + 
  geom_point(colour = "gray53", alpha = 0.7, size = 3)
p

### Adding labels and caption ###
p <- p +
  labs(title = "Early Presidential Approval And Senate Outcomes",
       subtitle = "Senate results vs. January to June approval average, since 2006",
       x = "Early presidential approval",
       y = "Final margin",
       caption = "Source: Various Polls") 
p

### Colours ###
p <- p +
  theme(
    panel.background = element_rect(fill = "gray90"),
    plot.background = element_rect(fill= "gray90"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "gray80"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "gray35"),
    plot.caption = element_text(family = "Courier", face = "bold",
                                colour = "gray35"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(family = "Courier", face="bold", 
                               colour = "gray45"),
    axis.text.y = element_text(family = "Courier", face="bold",
                               colour = "gray45"),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank()
  )
p

### Axis ticks ###
p <- p +
  scale_y_continuous(breaks = c(-50, -25, 0, 25, 50),
                     labels=c("- 50", "- 25", "0", "25", "+50")) +
  scale_x_continuous(breaks = c(20, 30, 40, 50, 60),
                   labels = c("20%", "30", "40", "50", "60"))
p

### Lines ###
p <- p + 
  geom_smooth(method = "lm", se=FALSE, color = "red") + 
  geom_hline(yintercept = 0, size = 0.3) 
p

#############################################
################ Exercise 2 #################
#############################################

tab2 <- read_excel("tab2e.xls", sheet = "tab2e_16 years and older", 
                   range = cell_rows(11:1173), col_names=FALSE)
tab2

tab2 <- tab2 %>%
  fill(X__1, X__2) %>%
  na.omit(tab2)
tab2  

tab2 <- tab2 %>% 
  filter(X__3 == "Total") %>%
  arrange(X__11) %>%
  mutate(X__12 = X__4 * X__11)
tab2
  
tax <- read_excel("tax.xlsx", range = cell_rows(10:299),
                  col_names = FALSE)
tax
tax <- tax %>%
  mutate(X__2 = X__1, X_8 = X__8)%>%
  select(X__2, X_8)
tax

tab <- left_join(tab2, tax, "X__2")
tab

tab <- tab %>%
  mutate(X_8 = replace(X_8, X_8 == "..", NA)) %>%
  mutate(X__13 = as.numeric(X_8)/X__4)
tab

tab %>%
  ggplot() +
  geom_point(mapping = aes(x = X__11, y = X__13))

tab %>%
  group_by(X_8) %>%
  count(X_8) %>%
  View() ## 123 NAs

tab2 %>%
  arrange(X__12)
