library(stringr)
library(gtools)

generatePerms <- function(elevPos, x) {    
  itemsOnOrigFloor <- x[[paste0("floor", elevPos)]]
  nItemsOnOrigFloor <- length(itemsOnOrigFloor)
  
  if (elevPos == 1) {
    elevToOpts <- 2
  } else if (elevPos == 2) {
    elevToOpts <- c(1, 3)
  } else if (elevPos == 3) {
    elevToOpts <- c(2, 4)
  } else if (elevPos == 4) {
    elevToOpts <- 3
  }
  
  nToElevOpts <- 1:min(nItemsOnOrigFloor, 2)
  
  perms <- list()
  for (i in elevToOpts) {
    for (j in nToElevOpts) {
      nToElev <- nToElevOpts[j]
      
      if (j == 1) {
        itemToMoveOpts <- matrix(itemsOnOrigFloor, nrow = nItemsOnOrigFloor)
      } else {
        itemToMoveOpts <- permutations(nItemsOnOrigFloor, 2, itemsOnOrigFloor)
      }
      
      for (k in 1:nrow(itemToMoveOpts)) {
        perms[[length(perms) + 1]] <- list(elevPos = elevPos, elevTo = i, nToElev = j, 
                                           itemToMove = itemToMoveOpts[k, ], x = x)
      }
    }
  }
  return(perms)
}

runPerms <- function(perms) {
  
  passedPermCount <- 0
  passedPerms <- list()
  
  nPerm <- length(perms)
  
  for (perm in 1:nPerm) {
    
    itemToMove <- perms[[perm]]$itemToMove
    elevTo <- perms[[perm]]$elevTo
    nToElev <- perms[[perm]]$nToElev
    elevPos <- perms[[perm]]$elevPos
    x <- perms[[perm]]$x
    
    itemsOnOrigFloor <- x[[paste0("floor", elevPos)]]
    
    changedOldFloor <- itemsOnOrigFloor[!(itemsOnOrigFloor %in% itemToMove)]
    changedNewFloor <- c(x[[paste0("floor", elevTo)]], itemToMove)
    
    # oldMaterial <- str_sub(changedOldFloor, 1, 1)
    # oldDevice <- str_sub(changedOldFloor, 2, 2)
    # newMaterial <- str_sub(changedNewFloor, 1, 1)
    # newDevice <- str_sub(changedNewFloor, 2, 2)
    
    oldMaterial <- substr(changedOldFloor, 1, 1)
    oldDevice <- substr(changedOldFloor, 2, 2)
    newMaterial <- substr(changedNewFloor, 1, 1)
    newDevice <- substr(changedNewFloor, 2, 2)
    
    check1 <- all((oldMaterial[oldDevice == "M"] %in% oldMaterial[oldDevice == "G"]) | 
                    length(oldMaterial[oldDevice == "G"]) == 0)
    check2 <- all((newMaterial[newDevice == "M"] %in% newMaterial[newDevice == "G"]) | 
                    length(newMaterial[newDevice == "G"]) == 0)
    
    check3 <- nToElev == 1 || ((elevPos - elevTo) != 1)
    check <- check1 && check2 && check3
    
    # IF ok, do the change
    if (check) {
      
      passedPermCount <- passedPermCount + 1 
      passedPerms[[passedPermCount]] <- perms[[perm]]
      
      if (length(changedOldFloor > 0)) {
        passedPerms[[passedPermCount]]$x[[paste0("floor", elevPos)]] <- sort(changedOldFloor)
      } else {
        passedPerms[[passedPermCount]]$x[[paste0("floor", elevPos)]] <- c()
      }
      passedPerms[[passedPermCount]]$x[[paste0("floor", elevTo)]] <- sort(changedNewFloor)
      passedPerms[[passedPermCount]]$elevPos <- elevTo
    } 
  }
  return(passedPerms)
}


####################################################################################################


# Part 1
initX <- list(floor1 = sort(c("LG", "TG", "TM", "PG", "RG", "RM", "CG", "CM")),
              floor2 = sort(c("LM", "PM")))
materials <- c("C", "L", "P", "R", "T")

# Part 2
# initX <- list(floor1 = sort(c("LG", "TG", "TM", "PG", "RG", "RM", "CG", "CM", "EG", "EM", "DG", "DM")),
#               floor2 = sort(c("LM", "PM")))
# materials <- c("C", "D", "E", "L", "P", "R", "T")

initElevPos <- 1

maxSteps <- 100

nTrueNewPermsLimit <- 2000
scoreQuantileCut <- 0.2

terminationScore = length(unlist(initX)) * 4

allPastStates <- list(list(elevPos = initElevPos, x = initX))

### STarting  
# Step 1  
perms <- generatePerms(initElevPos, initX)  
perms <- runPerms(perms)

# Step 2 and further 
step <- 0
score <- 0

while(TRUE) {
  step <- step + 1
  
  nPerms <- length(perms)
  
  nAllPastStates <- length(allPastStates)
  
  newPerms <- list()
  allPastPerms <- list()
  count <- 0 
  countPast <- 0 
  for (p in 1:nPerms) {
    
    
    
    simpleX <- perms[[p]]$x
    
    xG <- logical(4)
    xM <- logical(4)
    states <- character(7)
    names(states) <- materials
    
    countNonPairs <- 0
    
    for (m in materials) {
      tmp <- str_c(m, c("G", "M"))
      tmpG <- str_c(m, c("G"))
      tmpM <- str_c(m, c("M"))
      
      for (f in 1:4) {
        # xG[f] <- tmpG %in% simpleX[[str_c("floor", f)]]
        # xM[f] <- tmpM %in% simpleX[[str_c("floor", f)]]
        
        xG[f] <- any(tmpG == simpleX[[str_c("floor", f)]])
        xM[f] <- any(tmpM == simpleX[[str_c("floor", f)]])
      }
      
      xGw <- which(xG)
      xMw <- which(xM)
      
      if (xGw == xMw) {
        states[m] <- str_c("pair_f", xGw)
      } else {
        countNonPairs <- countNonPairs + 1
        
        states[m] <- str_c(countNonPairs, "__Gf", xGw, "_Mf", xMw)
      }
    }
    
    newX <- unname(sort(states))
        
 
    
    allPastStates[[length(allPastStates) + 1]] <- list(elevPos = perms[[p]]$elevPos, x = newX)
  }
  
  tmpRange <- (nAllPastStates + 1):(nAllPastStates + nPerms)
  trueNewPermsIdxs <- which(!duplicated(allPastStates)[tmpRange])
  nTrueNewPerms <- length(trueNewPermsIdxs)
  
  cat("\n", step, "  -  ", length(trueNewPermsIdxs), ", ", max(score))
  
  allPastStates <- allPastStates[!duplicated(allPastStates)]
  
  trueNewPerms <- perms[trueNewPermsIdxs]
  
  # Score an its limit
  score <- sapply(trueNewPerms, function(x) {length(x$x$floor1) * 1 + 
      length(x$x$floor2) * 2 + length(x$x$floor3) * 3 + length(x$x$floor4) * 4})
  
  scoreQuantile <- quantile(score, scoreQuantileCut)
  
  # scoreLimit <- (score > step)
  if (nTrueNewPerms > nTrueNewPermsLimit) {
    scoreLimit <- (score >= scoreQuantile)
    
    trueNewPerms <- trueNewPerms[scoreLimit]
    nTrueNewPerms <- length(trueNewPerms)
  } 
  
  
  
  for (i in 1:nTrueNewPerms) {
    newPerms[[length(newPerms) + 1]] <- generatePerms(trueNewPerms[[i]]$elevPos, trueNewPerms[[i]]$x)
  }
  
  perms <- unlist(newPerms, recursive = FALSE)
  perms <- perms[!duplicated(perms)]
  
  
  
  
  if (max(score) == terminationScore) {
    cat("\nFINISHED AT STEP", step)
    break
  } else if (step >= maxSteps) {
    cat("\nTERMINATED at ", step)
    break
  } else {
    perms <- runPerms(perms)
  }
}


