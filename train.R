if (!require("pacman")) install.packages("pacman")
library(pacman)

a <- 3.14        # double
b <- 5L          # integer
c <- "R-lang"    # character
d <- TRUE        # logical

notes <- c(12, 15.5, 8, 19, 14)

items <- seq(1L, 4L, length = 50)
print(items)

 
items_2 <- rep(x = 3, 10)
 
print(items_2)


notes <- seq(3, 16, length=20)
ajouts = rep(x=2, 20)

x <- "15"
y <- as.character(2.4)

premier = as.logical(3)
class(premier)
deux = as.numeric(premier)
print(deux)

value = (bool)3



notes[notes >= 14]

notes <- c(12, 15.5, 8, 19, 14, 6, 19)
print(length(notes))

bonus <- rep(x=2, length(notes))

notes + bonus

print(seq(1,9, length=9))

mat <- matrix(1:9, nrow = 3, byrow = TRUE)

rowSums(mat)
colSums(mat)
mat[3, 1]
