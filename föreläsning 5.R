data(movies)
movies
library(StatProg)
library(tidyverse)

### Tibbles ###

class(movies)
movies_df <- as.data.frame(movies)
movies_df

tibb <- tibble(x1 = c("one", "two", "three"), y = 1:3)
tibb$x1
tibb$x ## Will not work, full name needed when subsetting tibbles

tibb_df <- as.data.frame(tibb)
tibb_df$x1
tibb_df$x ## Gives x1 even though there is no variable called "x"

tibb$x1 # Always gives a vector
tibb[, "x1"] #Generates another tibble

# Data frames return vector no matter which of the above methods is used

var <- "budget"
var
bud <- movies[, var]


annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(10)
)

ggplot(annoying) +
  geom_point(aes(`1`, `2`))

### Importing data ###

parse_integer(c("12", "45", "78", "a"))

parse_number(c("25", "$25", "25%"))

parse_number("1,25") # Using comma separation "1,25" will be augmented to 125

default_locale()
parse_number("10.200,25", locale = locale(decimal_mark = ","))

rb <- read_csv2("riksbank.csv")

rb <- read_csv2("riksbank.csv", col_types = cols(
  Date = col_date(format = "%d/%m/%Y"),
  Group = col_character(),
  Series = col_character(),
  Value = col_double()
))

ex <- read_tsv("riksbank_comma.txt", col_types = cols(
  Date = col_date("%d/%m/%Y"),
  Group = col_character(),
  Series = col_character(),
  Value = col_number()
),
locale = locale(decimal_mark = ","))
ex

### Tidy data ###
table1
table2
table3_top1
table3_top10

x <- 1:10
x
mean(log(x))
log(x) %>%
  mean()

table2
spread(table2, key = type, value = share) # Spreading obs across columns

table3_top1
gather(table3_top1, `2000`, `2010`, key = "year", value = "top_1")

air_passengers
gather(air_passengers,Jan:Dec, key = "month", value = "passengers")
vote_table

dat
ggplot(dat) +
  geom_line(aes(time, value, color = variable))

dat_wide <- spread(dat, key = variable, value = value)
dat_wide # Will not be able to plot it

### Relational data ###
table3_top1
table3_top10

tab3_1 <- gather(table3_top1, `2000`, `2010`, key = "year", value = "top_1")
tab3_2 <- gather(table3_top10, `2000`, `2010`, key = "year", value = "top_10")
tab_3 <- inner_join(tab3_1, tab3_2, by = c("country", "year"))
