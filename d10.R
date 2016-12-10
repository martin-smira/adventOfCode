# Advent of code Day 10

library(stringr)

input <- readLines("input10.txt")


isValueInst <- !is.na(str_extract(input, "value(.*)"))

valueInstructionsIdx <- which(isValueInst)
moveInstructionsIdx <- which(!isValueInst)


initState <- as.data.frame(str_match(input[valueInstructionsIdx],
                                     "value (\\d+) goes to bot (\\d+)")[, 3:2],
                           stringsAsFactors = FALSE)
names(initState) <- c("bot", "value")

moveInstructions <- as.data.frame(str_match(input[moveInstructionsIdx], 
                                            "bot (\\d+) gives low to (\\w+) (\\d+) and high to (\\w+) (\\d+)")[, 2:6],
                                  stringsAsFactors = FALSE)
names(moveInstructions) <- c("bot", "lowTo", "lowToNum", "highTo", "highToNum")

# Kinda starts here
state <- initState

output <- data.frame(number = character(), value = integer(), stringsAsFactors = FALSE)

while(TRUE) {

  movingBots <- as.numeric(names(which(table(state$bot) == 2)))
  
  for (movingBot in movingBots) {
    movingBotVals <- as.numeric(state[state$bot == movingBot, "value"])
    
    currentInstruction <- moveInstructions[moveInstructions$bot == movingBot, ]
    
    state <- state[state$bot != movingBot, ]
    
    if (currentInstruction$lowTo == "bot") {
      state <- rbind(state, c(currentInstruction$lowToNum, min(movingBotVals)))
    } else if (currentInstruction$lowTo == "output") {
      output <- rbind(output, c(currentInstruction$lowToNum, min(movingBotVals)),
                      stringsAsFactors = FALSE)
    }
    if (currentInstruction$highTo == "bot") {
      state <- rbind(state, c(currentInstruction$highToNum, max(movingBotVals)))
    } else if (currentInstruction$highTo == "output") {
      output <- rbind(output, c(currentInstruction$highToNum, max(movingBotVals)),
                      stringsAsFactors = FALSE)
    }
  
    # Part 1 result
    if (all(movingBotVals %in% c(17, 61))) {
      print(movingBot)
    }
  }
  if (nrow(state) == 0) {
    break
  }
}

# Part 2 result
prod(as.numeric(output[output[,1] %in% 0:2, 2]))

