library(tidyverse)
library(randomcoloR)

makeMatrix <- function(n) {
  z <- data.frame(matrix(ncol = n + 1, nrow = n ^ 2))
  
  z[1] <- rep(1:n, rep(n, n), length.out = n ^ 2)
  z[2] <- rep(1:n, rep(n, n))

  for(i in 1:(n-1)) {
    z[i+2] <- randomColor(count = n ^ 2)
    nam <- paste("X", i+2, sep = "")
    q <- paste("+ geom_tile(aes(X1, X2, width = 0.8, height = 0.8, fill = ", nam, "))", sep = "")
  }
  
  print(q)

  ggplot(z) +
    geom_tile(aes(X1, X2, fill = X3)) +
    geom_tile(aes(X1, X2, width = 0.8, height = 0.8, fill = X4)) +
    geom_tile(aes(X1, X2, width = 0.5, height = 0.5, fill = X5)) +
    
    theme_void() +
    theme(legend.position = "none")
  
}

makeMatrix(4)


