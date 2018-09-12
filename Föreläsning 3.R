### Environments ###

temp <- function(x){
  aa <- 3
  return(aa^x)
}
temp(3)
# aa will not be in the global environment, can't call of it outside function

aa <- 2
temp2 <- function(x){
  return(aa^x)
}

temp2(3)
temp2(aa)

# what happens when calling temp2(aa)

### Implementing an algorithm ###

x <- c(3,4,2,1)

# Step 1: store current value in position 1
swap <- x[1]

# Step 2: Replace value in first position with the minimum value
minpos <- which.min(x) #finding position of minimum value
x[1] <- x[minpos]

# Step 3: Put the old position 1 value where the minimum used to sit
x[minpos] <- swap

# For second position
swap <- x[2]
minpos <- which.min(x[2:4]) + 1
x[2] <- x[minpos]
x[minpos] <- swap

swap <- x[3]
minpos <- which.min(x[3:4]) + 2
x[3] <- x[minpos]
x[minpos] <- swap

for (i in 1:length(x)){
  swap <- x[i]
  minpos <- which.min(x[i:length(x)]) +(i-1)
  x[i] <- x[minpos]
  x[minpos] <- swap
}

sr <- function(x){
  n <- length(x)
  for (i in 1:n){
    swap <- x[i]
    minpos <- which.min(x[i:n]) +(i-1)
    x[i] <- x[minpos]
    x[minpos] <- swap
  }
  return(x)
}
x <- runif(10)
x  
sr(x)

### Don't repeat yourself ###
df <- data.frame(replicate(6, sample(c(1:10,-99), 6, rep=TRUE)))
names(df) <- letters[1:6]
df
df$a[df$a == -99] <- NA
df$b[df$b == -99] <- NA
df$c[df$c == -99] <- NA
df$d[df$d == -99] <- NA
df$e[df$e == -99] <- NA
df$f[df$f == -99] <- NA
df
# Not the best way, create a function instead!!

fix_missing <- function(x){
  x[x == -99] <- NA
  return(x)
}
df$a <- fix_missing(df$a) # There is still repetition

# Use apply(...)

apply(X = df, MARGIN = 2, FUN = fix_missing)

### Object-oriented programming ###

plot(x)
plot(df)

y <- rnorm(10)
m <- lm(a~b, data = df)
summary(m)
summary(df)


obj <- list(x = 1:10)
class(obj) <- "aClass"
class(obj)

summary.aClass <- function(inputobj){
  x <- inputobj$x
  minmax <- c(min(x), max(x))
  return(cat("Minimum value:", minmax[1]))
}
summary(obj)

### Creating and R-package ###

