library(tidyverse)
library(devtools)
library(StatProg)
library(scales)

# Data preparation
crime_data <- read_tsv("crime_data.tsv")
dictionary_county_facts <- read_csv("county_facts_dictionary.csv")
county_facts <- read_csv("county_facts.csv")
results <- read_csv("general_result.csv")

results <- results %>%
  arrange(combined_fips) # Sorterar datatt efter fips i storleksordning, minst först

colnames(results)[2] <- "fips" # Byter namn så att alla ska va samma

county_facts <- na.omit(county_facts, cols = "state_abbreviation") # Tar bort de rader som innehåller stat- eller landdata
colnames(county_facts)[1] <- "fips" # Byter namn så att alla ska va samma
county_facts$fips <- as.character(county_facts$fips) # För att kunna joina måste fips vara av samma class
results$fips <- as.character(results$fips)

results_county <- full_join(county_facts, results) # Lägger samman county_facts och results, matchar med fips

for (i in 1:nrow(crime_data)) {
  if(crime_data[i, 6] < 10){ # Om county-koden är mindre än 10, multipliceras stat-koden med 100
    crime_data[i, 5] <- crime_data[i, 5]*100
  }else{
    if(crime_data[i, 6] < 100 & crime_data[i, 6] > 9){ # Om county-koden är större än 10 men mindre än 100, multipliceras stat-koden med 10
      crime_data[i, 5] <- crime_data[i, 5]*10
    }
  }
}

crime_data <- unite(crime_data, FIPS_ST, FIPS_CTY, col = "fips", sep = "") # Lägger ihop stat-koden och county-koden till en gemensam fipskod
sammanslaget <- full_join(results_county, crime_data)  # Lägger samman results_county och crime_data, matchar med fips

sammanslaget <- sammanslaget %>%
  select(fips, per_gop_2016, total_votes_2016, everything()) # Flyttar fram kolumner av intresse


sammanslaget <- na.omit(sammanslaget, cols = "per_gop_2016") # Tar bort rader med NA på variabeln andel av rösterna för republikanerna

sammanslaget <- sammanslaget %>% # Lägger till en ny kolumn i datamaterialet, "andel av rösterna för republikanerna" - "andel av rösterna för demokraterna"
  mutate(gop_win = per_gop_2016 - per_dem_2016) %>%
  select(gop_win, everything())

for (i in 1:nrow(sammanslaget)) {
  if(sammanslaget[i, 1] > 0){ # Om gap_win är större än 0, kodas gap_win som 1, annars som 0. 1 = republikansk seger, 0 = demokratisk seger 
    sammanslaget[i, 1] <- 1
  }else{
    sammanslaget[i, 1] <- 0
  }
}


sammanslaget$gop_win <- as.factor(sammanslaget$gop_win) # Ändrar gap_wins class till factor


#  Exploratory data analysis
scatter1 <- ggplot(data = sammanslaget, mapping = aes(x = INC110213, y = per_gop_2016,
                                                      color = HSD310213)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, se = TRUE, color = "blue", size = 1.5) +
  scale_y_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80), 
                     labels = c("0%", "20%", "40%", "60%", "80%")) +
  scale_x_continuous(breaks = c(25000, 50000, 75000, 100000, 125000), 
                     labels = c("$25000", "$50000", "$75000", "$100000", "$125000")) +
  labs(title = "2016 US presidential election, county-level",
       subtitle = "Final result for the republican party explained by the median income",
       x = "Median household income, 2009-2013",
       y = "Final result for the Republican party")
scatter1

histogram1 <- ggplot(data = sammanslaget) + 
  geom_histogram(mapping = aes(x = per_gop_2016), fill = "red", alpha = 1) +
  geom_histogram(mapping = aes(x = per_gop_2012), fill = "blue", alpha = 0.5) +
  scale_x_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80), 
                     labels = c("0%", "20%", "40%", "60%", "80%")) +
  annotate(geom="text", x=0.93, y=287, label="2016", color="red", size = 5) +
  annotate(geom="text", x=0.93, y=267, label="2012", color="blue", alpha = 0.5, size = 5) +
  labs(title = "2016 and 2012 US presidential election, county-level",
       subtitle = "Histogram of the Republican party´s final result in 2016 and 2012",
       x = "Votes in percent",
       y = "Number of counties")

histogram1

boxplot1 <- ggplot(sammanslaget, aes(x = gop_win, y = EDU685213)) +
  geom_boxplot() +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80), 
                     labels = c("0%", "20%", "40%", "60%", "80%")) +
  scale_x_discrete(breaks = c(0, 1), labels = c("Democrats", "Republicans")) +
  labs(title = "2016 US presidential election",
       subtitle = "Boxplot of the educational level",
       x = "Winning party",
       y = "Bachelor's degree or higher, percent of persons age 25+, 2009-2013")

boxplot1

gamling <- ggplot(sammanslaget, aes(x = AGE775214, 
                                    fill = gop_win,
                                    color = gop_win)) + 
  geom_density() +
  facet_wrap(~gop_win, nrow = 2) +
  labs(title = "Elderly votes in the 2016 Presidiential Election",
       subtitle = "Histogram of the Republican party´s final result in 2016 and 2012",
       x = " Persons aged 65 or over, %",
       y = "Density",
       fill = "Carried by") +
  guides(color = FALSE) + # Removing legend for "color"
  scale_fill_manual(labels = c("GOP", "Dems"), # Labelling "fill" legends
                    values = c("red", "blue")) + # Using party colours for fill
  scale_color_manual(values = c("red", "blue")) + # Using party colour for borders
  theme(strip.background = element_blank(), # Removing facet_wrap labels
    strip.text.x = element_blank())
gamling
