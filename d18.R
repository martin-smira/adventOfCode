library(stringr)

input <- readLines("input18.txt")
nRows <- 400000

nTiles <- str_length(input)

finalMap <- matrix(NA, nRows, nTiles)

finalMap[1, ] <- str_split(input, "")[[1]]


for (r in 1:(nRows - 1)) {
  
  
  x <- c(".", finalMap[r, ], ".")
  tileIdxs <- cbind(1:nTiles, 2:(nTiles + 1), 3:(nTiles + 2))
  tiles <- matrix(x[tileIdxs], nTiles, 3)
  
  newRow <- character(nTiles)
  for (t in 1:nTiles) {
  
    
    left <- tiles[t, 1]
    cent <- tiles[t, 2]
    righ <- tiles[t, 3]
    
    cond1 <- (left == "^" && righ == ".")
    cond2 <- (left == "." && righ == "^")
    
    check <- cond1 || cond2
    
    if (check) {
      newRow[t] <- "^"
    } else {
      newRow[t] <- "."
    }
  }
  
  finalMap[r + 1, ] <- newRow
}


sum(finalMap == ".")



