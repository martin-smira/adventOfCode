

# Part 1 input
discPossStates <- c(7, 13, 3, 5, 17, 19)
discCurrState <- c(0, 0, 2, 2, 0, 7)

# Part 2 input
discPossStates <- c(7, 13, 3, 5, 17, 19, 11)
discCurrState <- c(0, 0, 2, 2, 0, 7, 0)


falling <- 1:length(discPossStates)

maxComb <- prod(discPossStates)
times <- 0:maxComb

#### Implementation 1 - for loops (slower)
for (t in times) {

  discHitState <- (discCurrState + falling + t) %% discPossStates
  
  if (all(discHitState == 0)) {
    
    print(t)
    break
  }
}


#### Implementation 2 - faster
x1 <- matrix(discCurrState, nrow = maxComb + 1, length(discCurrState), byrow = TRUE)
x2 <- x1 + times
x3 <- t(apply(x2, 1, function(x) {x + falling}))
x4 <- t(apply(x3, 1, function(x) {x %% discPossStates}))
x5 <- t(apply(x4, 1, function(x) {all(x == 0)}))

which(x5) - 1



