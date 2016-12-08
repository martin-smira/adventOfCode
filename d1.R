
input <- scan("input1.txt", character())

dir <- substr(input, 1, 1)

len <- as.numeric(gsub(",", "", substr(input, 2, 10)))





coordX = c(0)
coordY = c(0)
direction = 0


for (i in 1:length(dir)) {
  
  if (dir[i] == "L") {
    direction = (direction - 90) %% 360
  } else if (dir[i] == "R") {
    direction = (direction + 90) %% 360
  }
  
  if (direction == 0) {
    coordX[i+1] <- coordX[i]
    coordY[i+1] <- coordY[i] + len[i]
  } else if (direction == 90) {
    coordX[i+1] <- coordX[i] + len[i]
    coordY[i+1] <- coordY[i]
  } else if (direction == 180) {
    coordX[i+1] <- coordX[i]
    coordY[i+1] <- coordY[i] - len[i]
  } else if (direction == 270) {
    coordX[i+1] <- coordX[i] - len[i]
    coordY[i+1] <- coordY[i]
  }
  
  xMatch = (coordX[i+1] == coordX[-(i+1)])
  match = coordY[i+1] == coordY[c(xMatch, FALSE)]
  
  if (any(match)) {
    cat(coordX[i+1], coordY[i+1])
    
    break
  }
}

coor <- cbind(coordX, coordY)

duplicated(rbind(coor, c(0,0)))
