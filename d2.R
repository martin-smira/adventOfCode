# Day 2

now = 5

input = scan("input2.txt", character())
nDig = length(input)

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

# Correct answer: 65556
output