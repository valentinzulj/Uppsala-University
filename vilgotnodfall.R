
# Vilgots kod for nodfall
set.seed(2018)
strat <- tibble(x = c(rnorm(200, 25), rnorm(200, 45), rnorm(200, 75)),
                treatment = rep(1:2, 300),
                strata = c(rep(1, 200), rep(2, 200), rep(3, 200)))


weights <- numeric(length(unique(strat$strata)))
taljare <- numeric(length(unique(strat$strata)))
antal <- matrix(0, nrow = length(unique(strat$strata)), ncol = length(unique(strat$treatment)))
poolad_varians <- numeric(length(unique(strat$strata)))
var_mat <- matrix(0, nrow = length(unique(strat$strata)), ncol = length(unique(strat$treatment)))
mean_mat <- matrix(0, nrow = length(unique(strat$strata)), ncol = length(unique(strat$treatment)))

## Weights och poolad varians
for (i in 1:length(unique(strat$strata))) {
  for (j in 1:length(unique(strat$strata))) {
    for(k in 1:length(unique(strat$strata))){
      for (l in 1:length(unique(strat$treatment))) {
        antal[k, l] <- sum(strat$strata == k & strat$treatment == l)
        x <- strat %>%
          filter(strata == k & treatment == l)
        var_mat[k, l] <- var(x$x)
        mean_mat[k, l] <- mean(x$x)
      }
    }
    taljare[j] <- (antal[j, 1] * antal[j, 2])/(antal[j, 1] + antal[j, 2])
    poolad_varians[j] <- ((antal[j, 1] + antal[j, 2])/(antal[j, 1] * antal[j, 2])) * 
      (((antal[j, 1]-1) * var_mat[j, 1]) + ((antal[j, 2]-1) * var_mat[j, 2])) /
      (antal[j, 1] + antal[j, 2] - 2)
  }
  weights[i] <- taljare[i]/sum(taljare)
}

# Räkna ut t-värde
difference <- numeric(length(unique(strat$strata)))
varians <- numeric(length(unique(strat$strata)))

for(i in 1:length(unique(strat$strata))){
  difference[i] <- weights[i] * (mean_mat[i, 1] - mean_mat[i, 2])
  varians[i] <- (weights[i]^2) * (poolad_varians[i]^2)
}
t <- (sum(difference))/(sqrt(sum(varians)))
t
