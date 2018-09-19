library(tidyverse)
data(birpanel)


### Exercise ###
pl <- ggplot(data = birpanel)

pl +
  geom_bar(mapping = aes(x = idx))

pl +
  geom_density(mapping = aes(x = dbirwt, color = as.factor(male)))

pl +
  geom_bin2d(aes(x = dmage, y = dbirwt)) +
  scale_fill_gradientn(breaks=seq(0, 9000, by = 700), colours = rainbow(8))


### filter() ###

birpanel %>%
  filter(idx == 1, male == 0)

birpanel %>%
  filter(row_number() == 3) # gives the i:th obs

birpanel %>%
  filter(row_number() == n()) # gives the last obs

birpanel %>%
  slice(1:5) # takes out rows i:j

### arrange() ###
birpanel %>%
  arrange(desc(dbirwt)) # desc() gives you the heaviest baby

birpanel %>%
  arrange(momid3,desc(dbirwt)) # sorts heaviest babies for every given mom

### Exercises ###
birpanel %>%
  arrange(desc(dbirwt)) %>%
            slice(1:10)
            
slice(arrange(birpanel, desc(dbirwt)), 1:10)

birpanel %>%
  filter(collgrad == 1) %>%
  ggplot() + 
  geom_bin2d(aes(x = dmage, y = dbirwt)) +
  scale_fill_gradientn(colours = rainbow(6))
  
### select() ###
birpanel %>%
  select(dbirwt) # select the birth weight column

birpanel %>%
  select(momid3:dmage) # selects columns between momid3 and dmage

birpanel %>%
  select(starts_with("pretri")) # selects every variable starting with "pretri"

birpanel %>% 
  select(-momid3) # leaves momid3 out

birpanel %>%
  select(starts_with("pretri"), everything()) # reorders the columns

### Exercise ###
birpanel %>%
  arrange(desc(dbirwt)) %>%
  select(dbirwt, gestat)
  slice(1:10)
  
### mutate()/transmute() ###
birpanel %>%
  mutate(log_age = log(dmage)) %>% # creating log of moms age
  select(log_age, age)
  
birpanel %>%
  mutate(premature = ifelse(gestat < 37, 1, 0)) %>% # premature dummy
  select(premature, gestat)
  
birpanel <- birpanel %>%
  mutate(premature = ifelse(gestat < 37, 1, 0))

birpanel %>%
  mutate(cigar = replace(cigar, cigar == 99, NA))

### Exercises p. 41 ###

birpanel %>%
  mutate(new = ifelse(mplbir == stateres, 1, 0)) %>%
  select(new, mplbir, stateres)

birpanel %>%
  mutate(atl_hs = ifelse(dmeduc >=  12, 1, 0)) %>%
  select(atl_hs, momid3)

#or

birpanel %>%
  mutate(atl_hs = ifelse(hsgrad + somecoll + collgrad > 1, 1, 0)) %>%
  select(atl_hs, momid3)
  
### summarize() and group_by() ###
birpanel %>%
  summarize(median_birwt = median(dbirwt))

birpanel %>%
  group_by(premature, male) %>%
  summarize(median_birwt = median(dbirwt),
            median_age = median(dmage),
            n = n(), #number of obs
            n_mothers = n_distinct(momid3)) #number of mothers

birpanel %>% 
  count(premature)

birpanel_age <- birpanel %>%
  group_by(momid3) %>%
  mutate(age_change = dmage - lag(dmage)) %>%
  select(dmage, age_change, everything())

birpanel_age %>%
  ungroup() %>%
  count(age_change)
  

### Exercise p.57 ###
birpanel %>%
  arrange(desc(dbirwt)) %>%
  slice(1:10) %>%
  summarize(med = median(dmage),
            age_max = max(dmage),
            age_min = min(dmage))

birpanel %>%
  group_by(nlbnl) %>%
  summarize(med_birwt = median(dbirwt)) %>%
  ggplot() +
  geom_point(aes(x = nlbnl, y = med_birwt))
  
birpanel %>%
  group_by(momid3) %>%
  mutate(start_stop = smoke - lag(smoke)) %>%
  select(momid3, smoke, start_stop, everything()) %>%
  ungroup() %>%
  count(start_stop)
