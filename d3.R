

input <- read.table("input3.txt", sep = "")

ukol = 2 # 1 or 2

if (ukol == 2) {
  input = as.data.frame(matrix(unlist(input), ncol = 3, byrow = TRUE))
}

input$V12 <- input$V1 + input$V2
input$V13 <- input$V1 + input$V3
input$V23 <- input$V2 + input$V3


check1 <- input$V23 > input$V1
check2 <- input$V13 > input$V2
check3 <- input$V12 > input$V3


checkOverall <- check1 & check2 & check3


table(checkOverall)



