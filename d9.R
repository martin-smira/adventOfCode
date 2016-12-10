#### Advent Calendar - day 9


library(stringr)

input <- scan("input9.txt", character())


## Part 1
remaining <- input
processed <- ""


while (str_length(remaining) != 0) {
  
  lenRemaining <- str_length(remaining)
  
  beforeMarker <- str_sub(str_extract(remaining, "\\w*\\("), end = -2L)
  marker <- str_extract(remaining, "\\(\\d+x\\d+\\)")
  
  if (!is.na(marker)) {
    lenMarker <- str_length(marker)
    lenBeforeMarker <- str_length(beforeMarker)
    
    num <- str_match(marker, "(\\d+)x(\\d+)")
    num1 <- as.numeric(num[1, 2])
    num2 <- as.numeric(num[1, 3])
    
    remaining <- str_sub(remaining, lenBeforeMarker + lenMarker + 1)
    
    processed <- str_c(processed, beforeMarker,str_dup(str_sub(remaining, 1, num1), num2))
    
    remaining <- str_sub(remaining, num1 + 1)
  } else {
    
    processed <- str_c(processed, remaining)
    
    break
  }
}

# Result = 152851
str_length(processed)


## Part 2


input

x <- str_extract_all(input, "\\(\\d+x\\d+\\)")




# remaining <- input

input <- "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"

# remaining <- processed
# processed <- ""

multiplier <- 1
sum <- 0
# 
# while (TRUE) {
#  
#   
#   x <- funXXX(input, sum, multiplier)
#   
#   
#   
#   x
#   
# }



funXXX <- function(input, sum, multiplier) {
  
  beforeMarker <- str_sub(str_extract(input, "\\w*\\("), end = -2L)
  marker <- str_extract(input, "\\(\\d+x\\d+\\)")
  
  if (!is.na(marker)) {
    lenMarker <- str_length(marker)
    lenBeforeMarker <- str_length(beforeMarker)
    
    sum <- sum + lenBeforeMarker * multiplier
    
    num <- str_match(marker, "(\\d+)x(\\d+)")
    num1 <- as.numeric(num[1, 2])
    num2 <- as.numeric(num[1, 3])
    
    # multiplier <- multiplier * num2
    
    remainingInputIdx <- lenBeforeMarker + lenMarker
    
    subInput <- str_sub(input, remainingInputIdx + 1, remainingInputIdx + num1)
    remainingInput <- str_sub(input, remainingInputIdx + num1 + 1)
    

    
    addSum1 <- funXXX(subInput, 0, multiplier * num2)
    addSum2 <- funXXX(remainingInput, 0, multiplier)
    
    # cat(paste(subInput, sum, multiplier * num2, addSum1, "\n", ", "))
    # cat(paste(remainingInput, sum, multiplier, addSum2, "\n\n\n", ", "))
    
    sum <- sum + addSum1 + addSum2
    
  } else {
    
    sum <- sum + str_length(input) * multiplier
  }
  
  return(sum)
}

x <- funXXX("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN", 0, 1)
x <- funXXX("(27x12)(20x12)(13x14)(7x10)(1x12)A", 0, 1)
x <- funXXX("X(8x2)(3x3)ABCY", 0, 1)
x <- funXXX(input, 0, 1)



x

