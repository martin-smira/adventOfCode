library(stringr)

input <- readLines("input06.txt")

x <- str_split(input, "", simplify = TRUE)

table()

# Part 1 answer
paste(apply(x, 2, function(x) {names(which.max(table(x)))}), collapse = "")
# Part 2 answer
paste(apply(x, 2, function(x) {names(which.max(table(x)))}), collapse = "")
