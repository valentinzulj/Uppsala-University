library(tidyverse) # Needs to be installed first
library(devtools) # Needs to be installed first
library(StatProg)
library(scales)


# LÃ¤sa in dread csv() for comma-separated files
# read csv2() for semicolon-separated files,
# read tsv() for tab-separated files
# read delim() for files with a general separator
# read fwf() for files with fixed widths
# read table() for files where white space separates columns
# (read excel() for Excel files)ata

crime_data <- read_tsv("crime_data.tsv")
dictionary_county_facts <- read_csv("county_facts_dictionary.csv")
county_facts <- read_csv("county_facts.csv")
results <- read_csv("general_result.csv")


results <- results %>%
  arrange(combined_fips)

colnames(results)[2] <- "fips"

county_facts <- na.omit(county_facts, cols = "state_abbreviation")
colnames(county_facts)[1] <- "fips"
county_facts$fips <- as.character(county_facts$fips)
results$fips <- as.character(results$fips)

results_county <- full_join(county_facts, results)

crime_data %>%
  arrange()


for (i in 1:nrow(crime_data)) {
  if(crime_data[i, 6] < 10){
    crime_data[i, 5] <- crime_data[i, 5]*100
  }else{
    if(crime_data[i, 6] < 100 & crime_data[i, 6] > 9){
    crime_data[i, 5] <- crime_data[i, 5]*10
    }
  }
}

crime_data <- unite(crime_data, FIPS_ST, FIPS_CTY, col = "fips", sep = "")
sammanslaget <- full_join(results_county, crime_data)  

sammanslaget <- sammanslaget %>%
  select(fips, per_gop_2016, total_votes_2016, everything())


sammanslaget <- na.omit(sammanslaget, cols = "per_gop_2016")

sammanslaget <- sammanslaget %>%
  mutate(valdeltagande = total_votes_2016 / (PST045214*(1 - (AGE295214/100)))) %>%
  select(valdeltagande, everything())

ggplot(data = sammanslaget, mapping = aes(x = INC110213, y = per_gop_2016)) +
  geom_point(color = "red", size = 2) +
  geom_smooth(method = lm, se = FALSE, color = "blue", size = 1.5) +
  scale_y_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80), labels = c("0%", "20%", "40%", "60%", "80%")) +
  scale_x_continuous(breaks = c(25000, 50000, 75000, 100000, 125000), labels = c("$25000", "$50000", "$75000", "$100000", "$125000")) +
  labs(title = "2016 US presidential election, county-level",
       subtitle = "Final result for the republican party explained by the median income",
       x = "Median household income, 2009-2013",
       y = "Final result for the Republican party")


ggplot(data = sammanslaget) + 
  geom_histogram(mapping = aes(x = per_gop_2016), fill = "red", alpha = 1) +
  geom_histogram(mapping = aes(x = per_gop_2012), fill = "blue", alpha = 0.5) +
  scale_x_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80), labels = c("0%", "20%", "40%", "60%", "80%")) +
  annotate(geom="text", x=0.93, y=287, label="2016", color="red", size = 5) +
  annotate(geom="text", x=0.93, y=267, label="2012", color="blue", alpha = 0.5, size = 5) +
  labs(title = "2016 and 2012 US presidential election, county-level",
       subtitle = "Histogram of the Republican party´s final result in 2016 and 2012",
       x = "Votes in percent",
       y = "Number of counties")

democrats2016 <- sum(sammanslaget$votes_dem_2016)
democrats2016
republicans2016 <- sum(sammanslaget$votes_gop_2016)
republicans2016

ggplot(sammanslaget)+
  geom_bar(aes(x = sum(sammanslaget$votes_gop_2016)))
ggplot(data = sammanslaget) +
  geom_bar(mapping = aes(x = Pclass, fill = Survived)

test <- ggplot(data = senate, mapping = aes(x = presidential_approval, y = election_result)) +
  geom_point(alpha = 0.3, fill = "black", size = 2) +
  geom_smooth(method = lm, se = FALSE, color = "red", size = 1.5) +
  geom_abline(slope = 0, intercept = 0) +
  scale_y_continuous(breaks = c(-50, -25, 0, 25, 50), labels = c("-50", "-25", "0", "25", "+50")) +
  scale_x_continuous(breaks = c(20, 30, 40, 50, 60), labels = c("20%", "30", "40", "50", "60")) +
  labs(title = "Early Presidential Approval And Senate Outcomes?",
       subtitle = "Senate results vs. January to June approval average, since 2006",
       caption = c("FIVETHIRTYEIGHT                                                                                      SOURCE: VARIOUS POLLS"),
       x = "Early presidential approval",
       y = "Final margin")