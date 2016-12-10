
input <- scan("input01.txt", character())

# input <- c("R8", "R4", "R4", "R8")

dir <- substr(input, 1, 1)

len <- as.numeric(gsub(",", "", substr(input, 2, 10)))


coordX = c(0)
coordY = c(0)
direction = 0

step <- 0

for (i in 1:length(dir)) {
  
  if (dir[i] == "L") {
    direction = (direction - 90) %% 360
  } else if (dir[i] == "R") {
    direction = (direction + 90) %% 360
  }
  
  for (l in 1:len[i]) {
    step <- step + 1
    
    if (direction == 0) {
      coordX[step + 1] <- coordX[step]
      coordY[step + 1] <- coordY[step] + 1
    } else if (direction == 90) {
      coordX[step + 1] <- coordX[step] + 1
      coordY[step + 1] <- coordY[step]
    } else if (direction == 180) {
      coordX[step + 1] <- coordX[step]
      coordY[step + 1] <- coordY[step] - 1
    } else if (direction == 270) {
      coordX[step + 1] <- coordX[step] - 1
      coordY[step + 1] <- coordY[step]
    }
    # 
    # xMatch = (coordX[step + 1] == coordX[-(step + 1)])
    # match = coordY[step + 1] == coordY[c(xMatch, FALSE)]
  }
  
  # if (any(match)) {
  #   cat(coordX[i + 1], coordY[i + 1])
  #   
  #   break
  # }
}

coor <- cbind(coordX, coordY)
coor

dup <- which(duplicated(rbind(coor)))

# Part 2 result (=143)
sum(abs(coor[dup[1], ]))

