library(stringr)

input <- 11101000110010100
fillLen <- 35651584

a <- input
res <- as.numeric(str_split(a, "")[[1]])

while (TRUE) {
  
  a <- res
  b <- a
  
  b1 <- b[length(b):1]
  b2 <- 1 - as.numeric(b1)
  
  res <- c(a, 0, b2)
  res
  
  if (length(res) >= fillLen) {
    
    res <- res[1:fillLen]
    
    break  
  }
}

while (TRUE) {
  
  lRes <- length(res)

  newRes <- vector(mode = "integer", length = lRes / 2)
  
  seqI <- seq(1, lRes - 1, 2)
  
  for (i in 1:length(seqI)) {
    newRes[i] <- as.numeric(res[seqI[i]] == res[seqI[i] + 1])
  }

  # print(newRes)
  
  lNewRes <- length(newRes)
  
  if ((lNewRes %% 2) == 1) {
    
    print(str_c(newRes, collapse = ""))
    
    break
    
  } else {
    res <- newRes
  }
}






