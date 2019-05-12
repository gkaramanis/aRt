library(tidyverse)
library(randomcoloR)

makeMatrix <- function(n) {
  z <- matrix(ncol = n + 1, nrow = n ^ 2)
  
  z[, 1] <- rep(1:n, rep(n, n), length.out = n ^ 2)
  z[, 2] <- rep(1:n, rep(n, n))

  for(i in 1:(n-1)) {
    nam <- paste("V", i+2, sep = "")
    z[, i+2] <- randomColor(count = n ^ 2)
    # assign(nam, randomColor(count = n ^ 2))
  }

  return(as.data.frame(z))
  # data.frame(x, y, sapply(ls(pattern="^c[0-9]+$"), get))
}

m <- makeMatrix(4)

ggplot(m) +
  geom_tile(aes(V1, V2,
                fill = V3)) +
  geom_tile(aes(V1, V2,
                width = 0.8, height = 0.8,
                fill = V4)) +
  geom_tile(aes(V1, V2,
                width = 0.5, height = 0.5,
                fill = V5)) +
  theme_void() +
  theme(legend.position = "none")

n=5
z <- matrix(ncol = n + 1, nrow = n ^ 2)
colnames(z[,1]) <- "x"

