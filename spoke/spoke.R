library(tidyverse)
library(ambient)

n = 100

pnt <- tibble(x = 1:n) %>% 
  mutate(y = list(1:n/2)) %>% 
  unnest(y) %>% 
  mutate(angle = (x / n) * pi)

ggplot(pnt) +
  geom_spoke(aes(x, y, angle = angle, radius = 1), size = 1, alpha = 0.5) +
  coord_fixed() +
  theme_void() +
  ggsave("spoke/spoke.png")
