library(stringr)


input <- readLines("input08.txt")
nInput <- length(input)

screenSize <- c(6, 50)
screen <- matrix("", screenSize[1], screenSize[2])

for (i in 1:nInput) {
  
  # i <- 5
  # print(input[i])
  
  operation <- str_match_all(input[i], "(\\w+) (\\w+)")[[1]][1, 2:3]
  
  
  if (operation[1] == "rect") {
    
    scSize <- as.numeric(str_match_all(input[i], "rect (\\d+)x(\\d+)")[[1]][1, 2:3])
    
    screen[1:scSize[2], 1:scSize[1]] <- "X"
    
  } else if (operation[1] == "rotate") {
    
    
    if (operation[2] == "row") {
      
      rowBy <- as.numeric(str_match_all(input[i], "rotate row y=(\\d+) by (\\d+)")[[1]][1, 2:3])
      row <- rowBy[1] + 1
      
      idxs <- 1:screenSize[2]
      idxs <- (idxs - rowBy[2]) %% screenSize[2]
      idxs[idxs == 0] <- screenSize[2]
      
      screen[row, ] <- screen[row, idxs]
      
    } else if (operation[2] == "column") { 
      
      colBy <- as.numeric(str_match_all(input[i], "rotate column x=(\\d+) by (\\d+)")[[1]][1, 2:3])
  
      col <- colBy[1] + 1
      
      idxs <- 1:screenSize[1]
      idxs <- (idxs - colBy[2]) %% screenSize[1]
      idxs[idxs == 0] <- screenSize[1]
      
      screen[, col] <- screen[idxs, col]
      
    }
  }
}

screen
sum(screen == "X")

