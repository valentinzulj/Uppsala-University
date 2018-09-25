library(tidyverse) # Needs to be installed first
library(devtools) # Needs to be installed first
library(StatProg)
library(scales)


# Läsa in dread csv() for comma-separated files
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
  select(fips, per_gop_2016, everything())

sammanslaget <- na.omit(sammanslaget, cols = "per_gop_2016")

