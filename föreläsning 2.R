if (1==2){
  print("It's true!")
} else{
  print("It's not true..")
}

### Exercise 1 ###
x <- rnorm(3,1,1)
lx <- numeric(3)

if (all(x>0)) {
lx <- log(x)
} else {
  print("Not all x are positive")
} 

y <- rnorm(20,0,1)

if (any(y<0)) {
 y[y<0] <- -y[y<0]
}

### Loops ###
for(i in 1:3){
  print(i)
}

x <- numeric(4)
for (i in 1:4){
  x[i] <- i*4
}

x <- matrix(rnorm(20, 10, 2), nrow=5, ncol=4)
for(i in 1:4){
  x[, i] <- (x[, i] - mean(x[, i]))/sd(x[, i]) 
}

lognumbers <- c()
for(i in 1:10) {
  x <- rnorm(1, 1, 1)
  if(x>0){
    lognumbers <- c(lognumbers, log(x))
  }
}
lognumbers

### Exercise 2 ###
for(i in 1:4){
  print(i^2)
}

x <- numeric(1000)
for (i in length(x)){
  x <- i
}

### Exercise 3 ###
p<-0.5
y<- numeric(100)
for(i in 2:length(y)){
  y[i] <- y[i-1]*p + rnorm(1,0,1)
}

A <- matrix(0, nrow=4, ncol=5)
for(i in 1:4){
  for (j in (i:(i+1))){
    if (i==j) {
      A[i,j] <- 0.5
    } else {
      A[i,j] <- 1
    } #closes if
  } #closes second loop
} #closes first loop
A

### Functions ###

division <- function(num,denom) {
  return(num/denom)
}

division(4,2)


variance <- function(x) {
  n <- length(x)
  res <- sum((x-mean(x))^2)/(n-1)
  return(res)
}

x <- rnorm(1000)
variance(x)

variance <- function(x, unbiased=TRUE) {
  n <- length(x)
  if (unbiased) {
    res <- sum((x-mean(x))^2)/(n-1) # unbiased
  } else {
  res <- sum((x-mean(x))^2)/n# maximum likelihood
  }
  return(res)
}

x<-rnorm(2000)
variance(x)
variance(x, unbiased=FALSE)

xx<- matrix(x, nrow=1000, ncol=2)
variance(xx)
variance(x)

variance <- function(x, unbiased=TRUE) {
  if(is.vector(x) & is.numeric(x)){
    n <- length(x)
    if (unbiased) {
      res <- sum((x-mean(x))^2)/(n-1) # unbiased
    } else {
      res <- sum((x-mean(x))^2)/n# maximum likelihood
    }
    return(res)
  } else {
    stop(return("The input is not a numeric vector"))
  }
}

variance(x)
variance(xx)

variance2 <- function(x) {
  n <- length(x)
  unbiased <- sum((x-mean(x))^2)/(n-1)
  ML <- sum((x-mean(x))^2)/n
  res <- list(unbiased = unbiased, ML = ML)
  return(res)
}

variance2(x)
