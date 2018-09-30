library(tidyverse)            
library(xtable)
library(tidyverse)
library(ggrepel)
library(scales)

#####################################################
################# Linear Regression #################
#####################################################

linear_regression <- function(data, dep, indep, intercept = TRUE) {
  y <- as.matrix(data[, dep])
  x <- as.matrix(data[, indep])
  if (intercept == TRUE) { x <- cbind(1, x)
  }
  beta <- c(solve(crossprod(x)) %*% crossprod(x, y))
  fits <- x %*% beta
  resids <- y - fits
  sigma2 <- sum(resids^2)/(length(resids)-ncol(x))
  se <- sqrt(diag(sigma2 * solve(crossprod(x))))
  names(beta) <- colnames(x)
  return_obj <- list(beta = beta, se = se,
                     residuals = c(resids), fitted = c(fits),
                     sigma = sqrt(sigma2), dep = dep, indep = indep,
                     intercept = intercept, y = c(y))
  class(return_obj) <- "linear_regression"
  return(return_obj)
}

A <- data.frame(Y = rexp(100, rate =2),           # Depentent variable
                X1 = rnorm(100),                  # Independent variable
                X2 = rnorm(100, mean = 4, sd = 2) # Dependent variable
)

lin_mod <- linear_regression(data = A, dep = 1,   # y variable is first columnt in 'data'
                             indep = c(2,3)       # Regressors are in columns two and three
)

ci <- function(lin_mod, pos, alfa){
  i <- pos                             # Default means that pos = 1 gives interval for the intercept 
  lower <- lin_mod$beta[i] - qnorm(1-(alfa/2))*lin_mod$se[i] 
  upper <- lin_mod$beta[i] + qnorm(1-(alfa/2))*lin_mod$se[i]
  out <- list(lower = lower,
              upper = upper,
              c_level = 100*(1-alfa),
              var = pos)               # Things to use in return
  class(out) <- "linear_regression_ci" # Creating a new class for the output
  
  return(out)
} 

print.linear_regression_ci <- function(obj){ # Adjusting the printing method of the new output class
  print(paste0("A ", obj$c_level,            # Gives the confidence level of the interval 
               "% confidence interval for beta_", 
               obj$var,                      # The index denoting which beta is used
               " is given by: (", 
               round(obj$lower, digits = 3), # Rounding lower limit to three decimals
               ", ", 
               round(obj$upper, digits = 3), # Rounding upper limit to three decimals
               ")."))
} 

int <- ci(lin_mod, 1, 0.05)
int 

#####################################################
################# Stratified t-test #################
#####################################################

set.seed(2018)
strat <- tibble(x = c(rnorm(200, 25), rnorm(200, 45), rnorm(200, 75)),
                treatment = rep(1:2, 300),
                strata = c(rep(1, 200), rep(2, 200), rep(3, 200)))

set.seed(2018)
test <- tibble(x = c(rnorm(200, 25), rnorm(200, 45), rnorm(200, 75)),
               treatment = rep(1:2, 300))

vilgot <- 1:500 # For testing later


t_test <- function(data){
  if(is.tibble(data) & is.data.frame(data)){ 
    if(any(colnames(data) == "strata")){ # Stratified t-test
      d <- data %>%
        group_by(treatment, strata) %>%
        summarize(n = length(strata),     # Computing n
                  s2 = var(x),            # Computing s-squared
                  m = mean(x)) %>%        # Computing x-bar
        group_by(strata) %>%
        mutate(sprod = s2*(n-1)) %>%      # Multiplying n by variance
        summarize(nsum = sum(n),          # Summing number of obs
                  rnsum = sum(n) - 2,     # Subtracting 2
                  ssum = sum(sprod),      # Summing the n-variance products
                  nprod = prod(n),        # Multiplying the number of obs
                  mdiff = m[1]-m[2]) %>%  # Difference in means
        mutate(weights = (nprod/nsum)/sum(nprod/nsum),  # Computing weights    
               sigma2 = (nsum/nprod)*(ssum/rnsum)) %>%  # Computing sigma2
        select(mdiff, weights, sigma2) %>%
        summarize(numerator = sum(weights*mdiff),
                  denominator = sqrt(sum(weights^2*sigma2)),
                  t_stat = numerator/denominator,  # Test statistic
                  stratified = TRUE) %>%           # Logical statement
        select(t_stat, stratified)
      return(print.data.frame(d))
    } else {
      t <- data %>%
        group_by(treatment) %>%
        summarize(n = length(treatment),
                  s2 = var(x),
                  m = mean(x)) %>%
        mutate(sprod = s2*(n-1)) %>%
        summarize(nsum = sum(n),          # Summing number of obs
                  rnsum = sum(n) - 2,     # Subtracting 2
                  ssum = sum(sprod),      # Summing the n-variance products
                  nprod = prod(n),        # Multiplying the number of obs
                  mdiff = m[1]-m[2]) %>%  # Difference in means
        mutate(weights = (nprod/nsum)/sum(nprod/nsum),  # Computing weights    
               sigma2 = (nsum/nprod)*(ssum/rnsum)) %>%  # Computing sigma2
        select(mdiff, weights, sigma2) %>%
        summarize(numerator = sum(weights*mdiff),
                  denominator = sqrt(sum(weights^2*sigma2)),
                  t_stat = numerator/denominator, # Test statistic
                  stratified = FALSE) %>%         # Logical statement
        select(t_stat, stratified)
      return(print.data.frame(t))
    } # Closes simple t-test
    
  } #Closes first if statement 
  else {
    print("Data input needs to be a tibble or a data frame")
  } # Closes if statement
} #Closes function

t_test(strat)
t_test(test)
t_test(vilgot)


#####################################################
############### Presidential Election ###############
#####################################################


crime_data <- read_tsv("crime_data.tsv")   # Reading the data
dictionary_county_facts <- read_csv("county_facts_dictionary.csv")
county_facts <- read_csv("county_facts.csv")
results <- read_csv("general_result.csv")

results <- results %>%
  arrange(combined_fips)                   # Arranging the data by the smallest value of fips first.
colnames(results)[2] <- "fips"             # Changing column name
county_facts <- na.omit(county_facts, cols = "state_abbreviation") # Removing rows with state summaries
colnames(county_facts)[1] <- "fips"        # Changing column name so they are the same in both files
county_facts$fips <- as.character(county_facts$fips) # Column needs to be of same class to join together
results$fips <- as.character(results$fips)

results_county <- full_join(county_facts, results) # Joining county_facts och results


for (i in 1:nrow(crime_data)) {
  if(crime_data[i, 6] < 10){            # If the county-code is les than 10, the state-code is multiplied by 100
    crime_data[i, 5] <- crime_data[i, 5]*100
  }else{
    if(crime_data[i, 6] < 100 & crime_data[i, 6] > 9){ 
      crime_data[i, 5] <- crime_data[i, 5]*10 # If the county-code is greater than 9 but less than 100, the state-code is multiplied by 10
    }
  }
}

crime_data <- unite(crime_data, FIPS_ST, FIPS_CTY, col = "fips", sep = "") # Uniting the state- and county-code into one fips-code

sammanslaget <- full_join(results_county, crime_data) # Joining results_county och crime_Data

sammanslaget <- sammanslaget %>%
  select(fips, per_gop_2016, total_votes_2016, everything()) # Moving columns of interest to the beginning of the file

sammanslaget <- na.omit(sammanslaget, cols = "per_gop_2016") # Removing rows with missing values on republican result

sammanslaget <- sammanslaget %>% 
  mutate(gop_win = per_gop_2016 - per_dem_2016) %>% # Adding a new column to the file, gop_winW
  select(gop_win, everything())


for (i in 1:nrow(sammanslaget)) {
  if(sammanslaget[i, 1] > 0){ # If gap_win is bigger than 0, gap_win is coded as 1 
    sammanslaget[i, 1] <- 1 
  }else{
    sammanslaget[i, 1] <- 0 # If gap_win is smaller than 0, gap_win is coded as 0
  }
}

sammanslaget$gop_win <- as.factor(sammanslaget$gop_win) # Setting gop win to a factor variable

# Scatterplot of repblican election result, median income and persons per household
ggplot(data = sammanslaget, mapping = aes(x = INC110213, y = per_gop_2016, color = HSD310213)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, se = TRUE, color = "blue", size = 1.5) +
  scale_y_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80), 
                     labels = c("0%", "20%", "40%", "60%", "80%")) +
  scale_x_continuous(breaks = c(25000, 50000, 75000, 100000, 125000), 
                     labels = c("$25000", "$50000", "$75000", "$100000", "$125000")) +
  labs(title = "2016 US presidential election, county-level",
       subtitle = "Final result for the republican party explained by the median income",
       x = "Median household income, 2009-2013",
       y = "Final result for the Republican party",
       color = "Persons / households")

# Histogram of the Republican party´s final result in 2016 and 2012
ggplot(data = sammanslaget) + 
  geom_histogram(mapping = aes(x = per_gop_2016), 
                 fill = "red", alpha = 1) +
  geom_histogram(mapping = aes(x = per_gop_2012), 
                 fill = "blue", alpha = 0.5) +
  scale_x_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80), 
                     labels = c("0%", "20%", "40%", "60%", "80%")) +
  annotate(geom="text", x=0.93, y=287, label="2016", color="red", size = 5) +
  annotate(geom="text", x=0.93, y=267, label="2012", color="blue", alpha = 0.5, size = 5) +
  labs(title = "2016 and 2012 US presidential election, county-level",
       subtitle = "Histogram of the Republican party´s final result in 2016 and 2012",
       x = "Votes in percent",
       y = "Number of counties")

# Boxplot of educational level in counties won by each party
ggplot(sammanslaget, aes(x = gop_win, y = EDU685213, fill = gop_win)) +
  geom_boxplot() +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80), 
                     labels = c("0%", "20%", "40%", "60%", "80%")) +
  scale_x_discrete(breaks = c(0, 1), labels = c("Democrats", "Republicans")) +
  labs(title = "2016 US presidential election",
       subtitle = "Boxplot of the educational level",
       x = "Winning party",
       y = "B.Sc. or higher, % of persons age 25+, 2009-13",
       fill = "Carried by") +
  scale_fill_manual(labels = c("Dems", "GOP"),
                    values = c("blue", "red"))

# Distribution the share of elderly people, split by winning party
ggplot(sammanslaget, aes(x = AGE775214, 
                         fill = gop_win,
                         color = gop_win)) + 
  geom_density() +
  facet_wrap(~gop_win, nrow = 2) +
  labs(title = "Elderly votes in the 2016 Presidiential Election",
       x = " Persons aged 65 or over, %",
       y = "Density",
       fill = "Carried by") +
  guides(color = FALSE) +                         # Removing legend for "color"
  scale_fill_manual(labels = c("Dems", "GOP"),    # Labelling "fill" legends
                    values = c("blue", "red")) +  # Using party colours for fill
  scale_color_manual(values = c("blue", "red")) + # Using party colour for borders
  theme(strip.background = element_blank(),       # Removing facet_wrap labels
        strip.text.x = element_blank())

# Scatterplot of republican result, educational level, income and persons per household
ggplot(data = sammanslaget, aes(x = EDU635213, y = per_gop_2016, size = HSD310213, color = INC110213)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE, color = "blue", size = 1.5) +
  scale_y_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80), 
                     labels = c("0%", "20%", "40%", "60%", "80%")) +
  scale_x_continuous(breaks = c(60, 80, 100), 
                     labels = c("60%", "80%", "100%")) +
  scale_color_gradient(low = "purple", high = "green") +
  labs(title = "2016 US presidential election",
       x = "Share of population with high school graduation",
       y = "Republican result",
       color = "Median income",
       size = "Persons / household")

t(as.matrix(summary(sammanslaget$INC910213))) # Summary of the variable per capita income, dollars.
t(as.matrix(summary(sammanslaget$MURDER)))    # Summary of the variable murder.
t(as.matrix(summary(sammanslaget$PST045214))) # Summary of the variable population.

tab1 <- sammanslaget %>%
  select(gop_win, PST045214, ROBBERY, SBO315207, HSD310213, VET605213) %>%
  mutate(mrb = ROBBERY/PST045214,                 # Robberies per person
         mvet = VET605213/PST045214,
         GOP = gop_win) %>%                       # Veterans per person
  group_by(GOP) %>%
  summarize("Robberies" = sum(ROBBERY),           # Number of robberies
            "Avg. robberies" = mean(mrb),         # Mean number of robs/person
            "Black Firms" = mean(SBO315207),      # Mean share of black firms
            "Ppl per hshld" = mean(HSD310213),    # Average number of peeps per hhld
            "Share Veterans" = mean(mvet)) %>%
  select(GOP,"Robberies", "Avg. robberies","Black Firms",
         "Ppl per hshld", "Share Veterans")

tab1

sammanslaget %>%
  group_by(state_abbreviation) %>%
  select(SEX255214, SBO001207, RHI725214) %>%
  summarize(wmn = mean(SEX255214),     # Pct wmn
            mfrms = mean(SBO001207),   # Avg no. firms pr county
            hisp = mean(RHI725214))%>% # Pct hispanics
  ggplot(mapping = aes(y = mfrms, x = wmn)) +
  geom_point() +
  geom_label_repel(aes(label = state_abbreviation),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'blue') +
  labs(title = "Women, states and firms",
       x = "% Women",
       y = "Mean number of firms per state") +
  theme_classic()
