library(tidyverse)
library(devtools)
install_github("ankargren/StatProg")
library(StatProg)
data(movies)

### Scatter ###

## Plot budget vs rating
ggplot(data=movies) +
  geom_point(mapping = aes(x = budget, y = rating))

ggplot(data=movies) +
  geom_point(mapping = aes(x = budget, y = rating, color=Action))

ggplot(data=movies) +
  geom_point(mapping = aes(x = budget, y = rating,
                           shape=Action, color=Action))

ggplot(data=movies) +
  geom_point(mapping = aes(x = budget, y = rating), color = "red",
             shape=21, alpha = 0.2, fill = "green", stroke = 1)

####################################################################
# Defining things inside the two brackets means adding layers to a #
# variable                                                         #
####################################################################

ggplot(data = movies) +
  geom_point(mapping=aes(x = log(budget), y = length,
              color = rating, alpha = rating))

### Plotting two layers ###
ggplot(data = movies) +
  geom_point(mapping = aes(x = budget, y = rating)) +
  geom_smooth(mapping = aes(x = budget, y = rating))

ggplot(data = movies, mapping = aes(x = budget, y = rating)) +
  geom_point(aes(color = Action)) +
  geom_smooth(method = "lm", se = FALSE) # Generates the same plot as above

####################################################################
# geom_smooth makes line, se renders CI                            #                                       #
####################################################################


ggplot(data = movies, mapping = aes(x = budget, y = rating)) +
  geom_point(aes(color = Action)) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~vote_group)

### Exercise ###

scatter <- ggplot(data = movies,
                  mapping = aes(x = budget, y = rating )) +
  labs(x= "Budget", y="Rating") +
  geom_point()
scatter

scatter_col <- scatter +
  geom_point(aes(color = votes)) +
  scale_color_gradient(low = "red", high = "black")
scatter_col

scatter_votegroup <- scatter +
  geom_point(aes(color = vote_group))
scatter_votegroup

scatter_votegroup1 <- scatter +
  geom_point(aes(color = as.numeric(vote_group)))
scatter_votegroup1

scatter_act_vote <- scatter +
  facet_wrap(~ Action ~ vote_group, nrow = 2, ncol = 4)
scatter_act_vote


### Bar charts ###
data(titanic)
titanic

bar_fill <- ggplot(data = titanic, mapping = aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill") # poistion = "fill" forces them to keep same scale

ggplot(data = titanic, mapping = aes(x = Age)) +
  geom_histogram(binwidth = 5)

ggplot(data = titanic, mapping = aes(x = Age)) +
  geom_density()

### Exercises ###
bar_col <- bar_fill +
  geom_bar(color="black")
bar_col

ggplot(data = titanic, mapping = aes(x = Age)) +
  geom_density(aes(fill=Pclass))

ggplot(data = titanic, mapping = aes(x = Pclass, y = Fare)) +
  geom_boxplot()

### Customization ###

bar_fill +
  labs(title = "Survival on the Titanic",
       subtitle = "First-class passengers had higher rate of survival",
       caption = "Source: Internet",
       x = "Passenger Class",
       y = "Proportion",
       fill = "Survival") +
  scale_fill_dicsrete(labels = c("Dead", "Alive"))
# discrete keeps colours from before

bar_fill +
  labs(title = "Survival on the Titanic",
       subtitle = "First-class passengers had higher rate of survival",
       caption = "Source: Internet",
       x = "Passenger Class",
       y = "Proportion",
       fill = "Survival") +
  scale_fill_manual(labels = c("Dead", "Alive"),
                    values = c("purple", "green"))+
  scale_x_discrete(labels = c("First", "Second", "Third")) +
  theme_bw()
# using manual, the colours have to be chosen as well
# scale_x changes the lables of the x-axis
# theme_bw() changes the theme, for example the background and borders

