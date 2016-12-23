library(stringr)

input <- read.table("input22.txt", sep = "", header = TRUE, skip = 1)
nInput <- nrow(input)

input$AvailNum <- as.numeric(str_match(input$Avail, "(\\d+)T")[, 2])
input$UsedNum <- as.numeric(str_match(input$Used, "(\\d+)T")[, 2])

countPairs <- 0
for (i in 1:nInput) {
  
  cInput <- input[i, ]
  
  if (cInput$UsedNum != 0)
    countPairs <- countPairs + sum(cInput$UsedNum <= input$AvailNum[-i])
}


