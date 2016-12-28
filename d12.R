library(stringr)

funD12 <- function(a, b, c, d) {

  instrNo <- 1
  
  while (TRUE) {
    
    # print(instrNo)
    
    instr <- str_sub(input[instrNo], 1, 3)
  
    if (instr == "cpy") {
      
      x <- str_match(input[instrNo], "cpy (\\d+|\\w{1}) (\\w{1})")
      
      from <- x[1, 2]
      to <- x[1, 3]
      
      assign(to, ifelse(from %in% c("a", "b", "c", "d"), as.numeric(get(from)), as.numeric(from)))
      
      instrNo <- instrNo + 1
      
    } else if (instr == "jnz") {
      
      x <- str_match(input[instrNo], "jnz (\\w+) (-?\\d+)")
      
      check <- x[1, 2]
      move <- x[1, 3]
      
      checkVal <- ifelse(check %in% c("a", "b", "c", "d"), as.numeric(get(check)), as.numeric(check))
      
      if (checkVal != 0) {
        instrNo <- instrNo + as.numeric(move)
      } else {
        instrNo <- instrNo + 1  
      }
      
    } else if (instr == "inc") {
  
      x <- str_match(input[instrNo], "inc (\\w{1})")
      var <- x[1, 2]
      
      assign(var, as.numeric(get(var)) + 1)
      
      instrNo <- instrNo + 1
      
    } else if (instr == "dec") {
  
      x <- str_match(input[instrNo], "dec (\\w{1})")
      var <- x[1, 2]
      
      assign(var, as.numeric(get(var)) - 1)
      
      instrNo <- instrNo + 1
    }  else {
      
      print("Something wrong")
    }
      
    if (instrNo > 23) {
      break
    } 
  }
  return(a)
}

input <- readLines("input12.txt")


# part 1 (took me 5 mins)
sol1 <- funD12(0, 0, 0, 0)
# part 2 (took me 10 mins)
sol2 <- funD12(0, 0, 1, 0)

print(sol1)
print(sol2)  # 9227737

