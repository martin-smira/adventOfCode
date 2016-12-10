# Day 2



input = scan("input02.txt", character())
nDig = length(input)

now = 5

# Part 1

output = c()

for (i in 1:nDig) {
  
  sequence = input[i]
  seqLength = nchar(sequence)
  
  for (j in 1:seqLength) {
    
    instruct <- substr(sequence, j, j)
  
    if (instruct == "L" & !(now %in% c(1, 4, 7))) {
      now = now - 1 
    } else if (instruct == "R" & !(now %in% c(3, 6, 9))) {
      now = now + 1 
    } else if (instruct == "U" & !(now %in% c(1, 2, 3))) {
      now = now - 3 
    } else if (instruct == "D" & !(now %in% c(7, 8, 9))) {
      now = now + 3
    }
  }
  output[i] <- now
}

# Part 1 answer: 65556
output




# Part 2

output = c()

for (i in 1:nDig) {
  
  sequence = input[i]
  seqLength = nchar(sequence)
  
  for (j in 1:seqLength) {
    
    instruct <- substr(sequence, j, j)
    
    if (instruct == "L" & !(now %in% c(1, 2, 5, 10, 13))) {
      now = now - 1 
    } else if (instruct == "R" & !(now %in% c(1, 4, 9, 12, 13))) {
      now = now + 1 
    } else if (instruct == "U" & !(now %in% c(1, 2, 4, 5, 9))) {
      if (now %in% c(3, 13)) {
        now = now - 2
      } else {
        now = now - 4
      }
      
    } else if (instruct == "D" & !(now %in% c(5, 9, 10, 12, 13))) {
      if (now %in% c(1, 11)) {
        now = now + 2
      } else {
        now = now + 4
      }
    }
  }
  output[i] <- now
}

# Part 2 answer: CB779
output