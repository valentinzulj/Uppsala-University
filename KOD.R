library(tidyverse)            


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

A <- data.frame(Y = rexp(100, rate =2), # Depentent variable
                   X1 = rnorm(100),     # Independent variable
                   X2 = rnorm(100, mean = 4, sd = 2) # Dependent variable
                )

lin_mod <- linear_regression(data = A, dep = 1, # y variable is first columnt in 'data'
                  indep = c(2,3)                # regressors are in columns two and three
                            )

ci <- function(lin_mod, pos, alfa){
  i <- pos # Default means that pos = 1 gives interval for the intercept 
  lower <- lin_mod$beta[i] - qnorm(1-(alfa/2))*lin_mod$se[i] 
  upper <- lin_mod$beta[i] + qnorm(1-(alfa/2))*lin_mod$se[i]
  out <- list(lower = lower,
              upper = upper,
              c_level = 100*(1-alfa),
              var = pos) # Things to use in return
  class(out) <- "linear_regression_ci" # Creating a new class for the output
  
  return(out)
} 
  
print.linear_regression_ci <- function(obj){ # Adjusting the printing method of the new output class
  print(paste0("A ", obj$c_level, # Gives the confidence level of the interval 
               "% confidence interval for beta_", 
               obj$var, # The index denoting which beta is used
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


strat <- strat %>%
  group_by(treatment, strata) %>%
  summarize(n = length(strata), #computing n
            s2 = var(x), # Computing s-squared
            m = mean(x)) %>% # Computing x-bar
  group_by(strata) %>%
  mutate(sprod = s2*n) #multiplying n by variance
strat  


for(i in unique(strat$strata)){
strat %>%
    filter(strata == i) %>%
    summarize(nsum = sum(n),      # Summing number of obs
              rnsum = sum(n) - 2, # Subtracting 2
              ssum = sum(sprod),  # Summing the n-variance products
              nprod = prod(n)) %>% # Multiplying the number of obs
  print()
}  






  