library(stringr)
library(digest)

openDoors <- function(input, path, currPos) {

  openDoorLetters <- letters[2:6]
  doorVecOrder <- c("U", "D", "L", "R")
  
  wholeInput <- str_c(input, path)
  hash <- digest(wholeInput, "md5", serialize = FALSE)
  doors <- str_split(hash, "")[[1]][1:4]
  
  openDoors <- doors %in% openDoorLetters
  
  openDoors2 <- c(ifelse(currPos[1] == 1, FALSE, openDoors[1]),
                  ifelse(currPos[1] == 4, FALSE, openDoors[2]),
                  ifelse(currPos[2] == 1, FALSE, openDoors[3]),
                  ifelse(currPos[2] == 4, FALSE, openDoors[4]))
  
  validDoors <- doorVecOrder[openDoors2]
  
  return(validDoors)
}

updateState <- function(moveSet) {
  
  doorTaken <- moveSet$possMove
  
  path <- moveSet$path
  currPos <- moveSet$pos
  step <- moveSet$step
  
  if (doorTaken == "U") {
    currPos[1] <- currPos[1] - 1
  } else if (doorTaken == "D") {
    currPos[1] <- currPos[1] + 1
  } else if (doorTaken == "L") {
    currPos[2] <- currPos[2] - 1
  } else if (doorTaken == "R") {
    currPos[2] <- currPos[2] + 1
  }
  
  path <- str_c(path, doorTaken)
  step <- step + 1
  
  updatedState <- list(path = path, step = step, currPos = currPos)
  
  return(updatedState)
}

currPos <- c(1, 1)
moveQueue <- list()
succPaths <- c()
succPathsStep <- c()
input <- "awrkjxxr"
step <- 0 
path <- ""



while(TRUE) {

  possMoves <- openDoors(input, path, currPos)
  
  for (m in seq_along(possMoves)) {
    moveQueue[[length(moveQueue) + 1]] <- list(possMove = possMoves[m], step = step, 
                                               path = path, pos = currPos)
  }
  
  if (length(moveQueue) == 0) {
    break
  }
  
  while(TRUE) {
  
    updatedState <- updateState(moveQueue[[1]])
    moveQueue <- moveQueue[-1]
    
    currPos <- updatedState$currPos
    path <- updatedState$path
    step <- updatedState$step
    
    
    if (all(currPos == c(4, 4))) {
      # print(step)
      
      succPaths <- c(succPaths, path)
      succPathsStep <- c(succPathsStep, step)
      
    } else {
      
      break
    }
  }
}


tail(succPaths, 1)
tail(succPathsStep, 1)

