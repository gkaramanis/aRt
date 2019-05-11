library(tidyverse)
library(randomcoloR)

n <- 4
x <- rep(1:n, rep(n, n), length.out = n*n)
y <- rep(1:n, rep(n, n))
c <- randomColor(count = n*n)
f <- randomColor(count = n*n)

m <- data.frame(x, y, f, c)

ggplot(m) +
  geom_tile(aes(x, y, fill = f, color = c), size = 2)


