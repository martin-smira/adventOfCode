
library(stringr)

input <- readLines("input21.txt")
# input <- c("swap position 4 with position 0",
#            "swap letter d with letter b",
#            "reverse positions 0 through 4",
#            "rotate left 1 step",
#            "move position 1 to position 4",
#            "move position 3 to position 0",
#            "rotate based on position of letter b",
#            "rotate based on position of letter d")
nInput <- length(input)

# # Part 1 input
# letters <- c("a", "b", "c", "d", "e", "f", "g", "h")
# rowOrder <- 1:nInput

# Part 2 input
letters <- c("f", "b", "g", "d", "c", "e", "a", "h")
rowOrder <- nInput:1

nLetters <- length(letters)




for (i in 1:nInput) {
  
  operation <- str_extract(input[i], "^\\w+ \\w+")

  if (operation == "swap position") {
    pos <- str_match(input[i], "^swap position (\\d) with position (\\d)")[1,2:3]
    
    pos <- as.numeric(pos) + 1 
    
    l1 <- letters[pos[1]]
    l2 <- letters[pos[2]]
    
    letters[pos[1]] <- l2
    letters[pos[2]] <- l1
    
    
  } else if (operation == "swap letter") {
    
    lett <- str_match(input[i], "^swap letter (\\w) with letter (\\w)")[1,2:3]
    
    p1 <- which(letters == lett[1])
    p2 <- which(letters == lett[2])
    
    letters[p1] <- lett[2]
    letters[p2] <- lett[1]
    
  } else if (operation == "rotate right") {
    
    move <- as.numeric(str_match(input[i], "^rotate right (\\d) step")[1,2])
    
    idx <- ((1:nLetters) - move) %% nLetters
    idx[idx==0] <- nLetters
    
    letters <- letters[idx]
    
  } else if (operation == "rotate left") {
    
    move <- as.numeric(str_match(input[i], "^rotate left (\\d) step")[1,2])
    
    idx <- ((1:nLetters) + move) %% nLetters
    idx[idx==0] <- nLetters
    
    letters <- letters[idx]  
    
  } else if (operation == "rotate based") {
    
    lett <- str_match(input[i], "^rotate based on position of letter (\\w)")[1,2]
    
    move <- which(letters == lett)
    
    if (move > 4) {
      move <- move + 1
    }
    
    idx <- ((1:nLetters) - move) %% nLetters
    idx[idx==0] <- nLetters
    
    letters <- letters[idx]
    
  } else if (operation == "reverse positions") {
    
    pos <- str_match(input[i], "^reverse positions (\\d) through (\\d)")[1,2:3]
    pos <- as.numeric(pos) + 1 
    
    
    if (pos[1] != 1) {
      idxBefore <- 1:(pos[1] - 1)
    } else {
      idxBefore <- c()
    }
    if (pos[2] != nLetters) {
      idxAfter <- (pos[2] + 1):nLetters
    } else {
      idxAfter <- c()
    }
    
    idx <- c(idxBefore, pos[2]:pos[1], idxAfter)
    letters <- letters[idx]
    
  } else if (operation == "move position") {
  
    pos <- str_match(input[i], "^move position (\\d) to position (\\d)")[1,2:3]
    pos <- as.numeric(pos) + 1 
  
    x <- 1:nLetters
    x = x[-pos[1]]
  
    if (pos[2] != 1) {
      idxBefore <- 1:(pos[2] - 1)
    } else {
      idxBefore <- c()
    }
    if (pos[2] != nLetters) {
      idxAfter <- (pos[2]):(nLetters - 1)
    } else {
      idxAfter <- c()
    }
    
    idx <- c(x[idxBefore], pos[1], x[idxAfter])
    letters <- letters[idx]
  }
  
  cat(i, " - ", input[i], "\n")
  cat(letters, "\n\n")
}

str_c(letters, collapse = "")
