
## Kinda cheat solution - I found the pattern by hand and finished here
iddqd = 3018458

x = 1594324
y = 1

xx = 1594324:(2 * (x - 1)) 

# Correct result = 1424135
which(xx == iddqd)



## This should be correct solution, but it might take many many hours to compute
res <- c()

nElves <- 3018458L#
for (xxx in 1L:2000L) {
  
  nElves <- xxx

  elvesLeft <- 1L:nElves

  start = Sys.time()
  
  while(TRUE) {
  
    nElvesLeft <- length(elvesLeft)
    currElf <- elvesLeft[1L]
    stolenElf <- (nElvesLeft + 2L) %/% 2L
    

    elvesLeft <- elvesLeft[c(-1L, -stolenElf)]
    elvesLeft <- c(elvesLeft, currElf)
    
  
    if ((nElvesLeft %% 10000) == 0) {
      
      lap <- Sys.time()
      
      cat(nElvesLeft / 1000, "  -  ", lap - start, "\n")
      
      start <- lap
      
    }
    
    if (nElvesLeft == 1L) {
      print("Finished")
      print(elvesLeft)
      break
    }
  }
}