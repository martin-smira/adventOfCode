
input = read.table("input20.txt", FALSE, "-")



start = 0
end <- 4294967295

countIPs <- 0
step <- 0

while (TRUE) {

  step <- step + 1
    
  xxx = which(input[, 1] <= start)
  
  if (length(xxx) == 0) {
    
    xxx <- which(input[, 1] == min(input[, 1]))
    countIPs <- countIPs + input[xxx[1], 1] - start
  } 
  
  start <- max(start, max(input[xxx, 2]) + 1)
   
  input <- input[-xxx, ]

  if (nrow(input) == 0 || all(input[, 2] <= start)) {

    countIPs <- countIPs + end - start + 1
    break
  }
}

# Correct result = 101
countIPs

