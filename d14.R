rm(list = ls())

require(digest)
require(stringr)

input <- "ihaygndm"

part <- 1  # 1 or 2

if (part == 1) {
  nStretch <- 1
} else if (part == 2) {
  nStretch <- 2017
}

toHash <- paste0(input, 0:25000)

for (i in 1:nStretch) {
  toHash <- sapply(toHash, digest, algo = "md5", serialize = FALSE)
}
hash <- toHash

match3 <- str_match(hash, "(\\w)\\1\\1")
match5 <- str_match(hash, "(\\w)\\1\\1\\1\\1")

isNotNa3 <- which(!is.na(match3[, 2]))
isNotNa5 <- which(!is.na(match5[, 2]))

goodHash3 <- match3[isNotNa3, 2]
goodHash5 <- match5[isNotNa5, 2]


res <- 0
for (i in 1:length(goodHash3)) {
  gh3 <- goodHash3[i]
  ghi3 <- isNotNa3[i]
  
  
  startSearch <- ghi3 + 1
  endSearch <- ghi3 + 1000
 
  gh5s <- goodHash5[which(isNotNa5 %in% startSearch:endSearch)]
  
  hit <- any(gh3 == gh5s)
  
  res <- res + hit
  
  if (res >= 64) {
    print(res)
    print(ghi3 - 1)
    
    break
  }
}

