### Simulering ###
nodes <- round(seq(100, 4000, length.out = 15))
n <- length(nodes)
models <- list()
nodes[[2]]
for (i in 1:length(nodes)) {
  models[[i]]  <- randomForest(Cover_Type ~ ., 
                               data = data, 
                               maxnodes = nodes[i])
  print(i)
}
### New data ###
new_data <- data
probs <- matrix(ncol = 7, nrow = nrow(data))
for (i in 1:ncol(data)){
  dep <- data %>%
    pull(i)
  indep <- data %>%
    select(-i)
  d <- cbind(dep, indep)
  if(is.factor(dep)){
    if(i == 7){
      model <- multinom(dep ~ ., data = d)
      probs <- predict(model, d, type = "prob")
    }
    model <- multinom(dep ~ ., data = d)
  }else{
    model <- lm(dep ~ ., data = d)
  }
  if(i == 1){
    new_data[, i] <- predict(model, d) + rnorm(mean = 0, sd = 99, n = 1)
  }else if(i == 7){
    for (j in 1:nrow(data)) {
      new_data[j,i] <- sample(c(1, 2, 3, 4, 5, 6, 7), size = 1, prob = probs[j, ], replace = TRUE)
    }
  }else{
    new_data[, i] <- predict(model, d)
  }
}
olika_prediktioner <- matrix(ncol = length(nodes), nrow = 30)   # Matrix for hit rate
preddar <- as.data.frame(matrix(ncol = 1, nrow = nrow(data)))   # Vector for predicitons

for (l in 1:30) {
  for (i in 1:ncol(data)){
    dep <- data %>%
      pull(i)
    indep <- data %>%
      select(-i)
    d <- cbind(dep, indep)
    if(is.factor(dep)){
      if(i == 7){
        model <- multinom(dep ~ ., data = d)
        probs <- predict(model, d, type = "prob")
      }
      model <- multinom(dep ~ ., data = d)
    }else{
      model <- lm(dep ~ ., data = d)
    }
    if(i == 1){
      new_data[, i] <- predict(model, d) + rnorm(mean = 0, sd = 99, n = 1)
    }else if(i == 7){
      for (j in 1:nrow(data)) {
        new_data[j,i] <- sample(c(1, 2, 3, 4, 5, 6, 7), size = 1, prob = probs[j, ], replace = TRUE)
      }
    }else{
      new_data[, i] <- predict(model, d)
      print(i)
    }
  }
  for (k in 1:length(nodes)){
    preddar <- predict(models[[k]], newdata = new_data)
    c <- as.data.frame(cbind(preddar, new_data$Cover_Type))
    c <- c %>%
      mutate(korrekt = ifelse(preddar == V2, TRUE, FALSE))
    olika_prediktioner[l, k] <- mean(c$korrekt)
  }
  print(l)
}
write.csv(olika_prediktioner, "olika_prediktioner.csv")
