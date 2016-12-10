library(stringr)

input <- scan("input04.txt", character())

# Part 1

nLines = length(input)

extNum <- c()
match <- c()
for (i in 1:nLines) {
  extNum[i] <- str_extract(input[i], "\\d+")  
  
  x1 = str_extract(input[i], "^[a-z-]+")
  x2 = str_replace_all(x1, "-", "")
  x3 = str_split(x2, "")
  
  res = paste(names(sort(table(x3), decreasing = TRUE)[1:5]), collapse = "")
  
  y1 = str_extract(input[i], "([a-z]+)\\]$")
  y2 <- str_replace_all(y1, "]", "")
  
  match[i] = (y2 == res)
}

# Answer = 158835
sum(as.numeric(extNum) * as.numeric(match))



# Part 2
nLines = length(input)

extNum <- c()
match <- c()
deciphered <- c()
for (i in 1:nLines) {
  extNum[i] <- str_extract(input[i], "\\d+")  
  
  x1 = str_extract(input[i], "^[a-z-]+")
  x2 = str_replace_all(x1, "-", "")
  x3 = str_split(x2, "")[[1]]
  
  
  # decipher machine 
  decip <- c()
  for (j in 1:length(x3)) {
    num <- (which(x3[j] == letters) + as.numeric(extNum[i])) %% 26
    decip[j] = ifelse(num == 0, "z", letters[num])  
  }
  
  deciphered[i] <- paste(decip, collapse = "")
}

# Identify the right row
idx = which(str_detect(deciphered, "north"))

deciphered[idx]

# Answer
extNum[idx]
