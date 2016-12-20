
input = read.table("input20.txt", FALSE, "-")



start = 0

loop <- 0
while (TRUE) {
  
  loop <- loop + 1
  
  xxx = which(input[, 1] <= start)
  
  if (length(xxx) == 0) {
    print(start)
    break
  } else {
  
    start <- max(input[xxx, 2]) + 1
    
    input <- input[-xxx, ]
  }
}

loop
