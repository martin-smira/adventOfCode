rm(list = ls())

require(digest)
require(stringr)

input <- "reyedfim"


start = Sys.time()

toHash <- paste0(input, 0:3e+7)

hash = sapply(toHash, digest, algo = "md5", serialize = FALSE)

match <- str_match(hash, "^0{5}(.)(.)")

isNotNa <- which(!is.na(match[, 2]))

goodHash <- match[isNotNa, ]

# Part 1 answer
paste(goodHash[1:8, 2], collapse = "")

# Part 2 answer
x = goodHash[goodHash[, 2] %in% 0:7, ]
x[order(x[, 2]), ]
paste(x[order(x[, 2]), 3], collapse = "")


end = Sys.time()
end - start

