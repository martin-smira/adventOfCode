library(stringr)

input <- readLines("input07.txt")

input[1]

inBrack <- str_match_all(input, "\\[(\\w+)\\]")
inBrack <- lapply(inBrack, function(x) {x[, 2]})

outBrack <- str_match_all(input, "(?:(\\w+)\\[|\\](\\w+))")
outBrack <- lapply(outBrack, function(x) {c(x[x[, 2] != "", 2], x[x[, 3] != "", 3])})

# Part 1
match <- c()
outCheck <- list()
inCheck <- list()

for (i in 1:length(input)) {
  
  outCheck <- any(!is.na(str_extract(outBrack[[i]], "(\\w)(?!\\1)\\w\\1")))
  
  inCheck <- all(is.na(str_extract(inBrack[[i]], "(\\w)(?!\\1)(\\w)\\2\\1")))
  

  match[i] <- inCheck && outCheck
}

# Part 1 answer
sum(match)


### Part 2
match <- c()
ext <- list()

for (i in 1:length(input)) {
  
  currentInput <- outBrack[[i]]
  
  xxx <- max(nchar(currentInput)) - 2
  
  abas <- unlist(str_extract_all(currentInput, "(\\w)(?!\\1)\\w\\1"))
  
  for (x in 1:xxx) {
    
    abasAdd <- unlist(str_extract_all(str_sub(currentInput, x), "(\\w)(?!\\1)\\w\\1"))
    
    abas <- c(abas, abasAdd[!(abasAdd %in% abas)])
  }

  nAbas <- length(abas)
  
  if (nAbas != 0) {
  
    abasReorder <- paste0(str_sub(abas, 2), str_sub(abas, 2, 2))
    abasReorderPaste <- paste0("(", paste(abasReorder, collapse = "|"), ")")
    
    inMatch <- str_extract(inBrack[[i]], abasReorderPaste) 
    
  } else {
    inMatch <- NA
  }
  match[i] <- any(!is.na(inMatch))
}

# Part 2 answer = 260
sum(match)



