# nOT COMPLETE - THE COMPUTATION HERE WOULD TAKE AGES


rm(list = ls())

library(stringr)

searchPossMoves <- function(state) {
  
  gridUsed <- state$gridUsed
  gridAvail <- state$gridAvail
  
  possibleMoves <- list()
  
  for (i in 1:maxX) {
    for (j in 1:maxY) {
      
      cUsed <- gridUsed[i, j]
      
      if (cUsed != 0) {
        
        iNot1 <- i != 1
        jNot1 <- j != 1
        iNotMax <- i != maxX
        jNotMax <- j != maxY
        
        if (iNot1 && cUsed <= gridAvail[i - 1, j]) 
          possibleMoves[[length(possibleMoves) + 1]] <- list(origPos = c(i, j), newPos = c(i - 1, j))
        
        if (iNotMax && cUsed <= gridAvail[i + 1, j])
          possibleMoves[[length(possibleMoves) + 1]] <- list(origPos = c(i, j), newPos = c(i + 1, j))
        
        if (jNot1 && cUsed <= gridAvail[i, j - 1])
          possibleMoves[[length(possibleMoves) + 1]] <- list(origPos = c(i, j), newPos = c(i, j - 1))
        
        if (jNotMax && cUsed <= gridAvail[i, j + 1])
          possibleMoves[[length(possibleMoves) + 1]] <- list(origPos = c(i, j), newPos = c(i, j + 1))
      }
    }
  }
  return(possibleMoves)
}

updateState <- function(state, move) {
  
  moveSize <- state$gridUsed[move$origPos[1], move$origPos[2]]
  
  state$gridUsed[move$origPos[1], move$origPos[2]] <- 0
  state$gridUsed[move$newPos[1], move$newPos[2]] <- state$gridUsed[move$newPos[1], move$newPos[2]] + moveSize
  
  state$gridAvail[move$origPos[1], move$origPos[2]] <- state$gridAvail[move$origPos[1], move$origPos[2]] + moveSize
  state$gridAvail[move$newPos[1], move$newPos[2]] <- state$gridAvail[move$newPos[1], move$newPos[2]] - moveSize
  
  state$gridGoal[move$newPos[1], move$newPos[2]] <- state$gridGoal[move$origPos[1], move$origPos[2]]
  state$gridGoal[move$origPos[1], state$origPos[2]] <- 0
  
  state$goalDist <- goalDistComputation(state$gridGoal)
  
  # state$step <- state$step + 1
  
  return(state)
}

goalDistComputation <- function(gridGoal) {
  goalDist <- ((which(gridGoal == 1) - 1) / maxX) - 1 + which(gridGoal == 1) %% maxX
  
  return(goalDist)
}

#########################################################################################################
startTime <- Sys.time()

input <- read.table("input22.txt", sep = "", header = TRUE, skip = 1)
nInput <- nrow(input)

InputLocation <- str_match(input$Filesystem, "-x(\\d+)-y(\\d+)")[, 2:3]
input$locX <- as.numeric(InputLocation[, 1]) + 1
input$locY <- as.numeric(InputLocation[, 2]) + 1

input$SizeNum <- as.numeric(str_match(input$Size, "(\\d+)T")[, 2])
input$AvailNum <- as.numeric(str_match(input$Avail, "(\\d+)T")[, 2])
input$UsedNum <- as.numeric(str_match(input$Used, "(\\d+)T")[, 2])

maxX <- max(input$locX)
maxY <- max(input$locY)

# head(input)

gridUsed <- matrix(input$UsedNum, maxX, maxY)
gridUsed2 <- matrix(input$UsedNum, maxY, maxX)

gridAvail <- matrix(input$AvailNum, maxX, maxY)

gridGoal <- matrix(0, maxX, maxY)
gridGoal[1, maxY] <- 1

goalDist <- goalDistComputation(gridGoal)


possibleStates <- list()
allPastStates <- list()
possibleStates[[1]] <- list(gridUsed = gridUsed, gridAvail = gridAvail, gridGoal = gridGoal, 
                            goalDist = goalDist)
allPastStates[[1]] <- possibleStates[[1]]
nAllPastStates <- length(allPastStates)
# countPairs <- 0
# possibleMove <- matrix(NA, nrow = 100, ncol = 2)
iter <- 0

maxIter <- 10

while(TRUE) {
  
  iter <- iter + 1
  
  possibleMoves <- list()
  
  if (iter == 1) {
    
    possibleMoves[[1]] <- searchPossMoves(possibleStates[[1]])
    
  } else {
    
    # possibleMoves <- list()
    for (s in seq_along(possibleStates)) {
      possibleMoves[[s]] <- searchPossMoves(possibleStates[[s]])
    }
    # possibleMoves <- unlist(newPossibleMoves, recursive = FALSE)
    
  }
  
  nPossStateMoves <- length(unlist(possibleMoves, recursive = FALSE))
  
  cat(iter, " - possibleMoves = ", nPossStateMoves, "\n")
  
  newPossibleStates <- list()
  for (s in seq_along(possibleStates)) {
    
    posMove <- possibleMoves[[s]]
    
    for (pm in seq_along(posMove)) {
      
      newState <- updateState(possibleStates[[s]], posMove[[pm]])
      
      newPossibleStates[[length(newPossibleStates) + 1]] <- newState
      allPastStates[[length(allPastStates) + 1]] <- newState
    }
  }
  
  # possibleStates <- newPossibleStates[!duplicated(newPossibleStates)]
  
  # allPastStates <- list()
  
  tmpRange <- (nAllPastStates + 1):(nAllPastStates + nPossStateMoves)
  trueNewPossibleStatesIdx <- which(!duplicated(allPastStates)[tmpRange])
  
  allPastStates <- allPastStates[!duplicated(allPastStates)]
  possibleStates <- newPossibleStates[trueNewPossibleStatesIdx]
  # possibleStates
  
  # cat("possibleStates = ", length(possibleStates), "\n\n")
  
  
  if (iter >= maxIter) {
    
    break
  }
}

endTime <- Sys.time()

endTime - startTime

# x = unlist(possibleStates, recursive = FALSE)

# possibleStates[[400]]$goalDist
# x = sapply(possibleStates, function(x) {x$goalDist})
# summary(x)
