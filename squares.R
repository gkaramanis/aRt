library(tidyverse)
library(randomcoloR)

n <- 5
x <- rep(1:n, rep(n, n), length.out = n ^ 2)
y <- rep(1:n, rep(n, n))
c1 <- randomColor(count = n ^ 2)
c2 <- randomColor(count = n ^ 2)
c3 <- randomColor(count = n ^ 2)

m <- data.frame(x, y, c1, c2, c3)

ggplot(m) +
  geom_tile(aes(x, y,
                fill = c1)) +
  geom_tile(aes(x, y,
                width = 0.8, height = 0.8,
                fill = c2)) +
  geom_tile(aes(x, y,
                width = 0.5, height = 0.5,
                fill = c3)) +
  theme_void() +
  theme(legend.position = "none")
