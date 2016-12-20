
nElves <- 3018458  # 

input <- rep(1, nElves)

moves <- 0
i <- 0

while(TRUE) {
  i <- i + 1
  
  possibleCurrElfIdx <- ((i - 1) %% nElves) + 1
  possibleCurrElf <- input[possibleCurrElfIdx]
  
  if (possibleCurrElf != 0) {

    j <- 0
    while(TRUE) {
      j <- j +1
      
      possibleNextElfIdx <- ((i + j - 1) %% nElves) + 1
      possibleNextElf <- input[possibleNextElfIdx]
      
      if (possibleNextElf != 0) {
        
        input[possibleCurrElfIdx] <- possibleCurrElf + possibleNextElf
        input[possibleNextElfIdx] <- 0
        
        moves <- moves + 1
        
        break
      }
    }
  }

  if (moves == (nElves - 1)) {
    break
  }
}

which(input != 0)


