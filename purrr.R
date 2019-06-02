library(tidyverse)

polygon <- function(n) {
  tibble(
    x    = accumulate(1:(n-1), ~.x+cos(.y*2*pi/n), .init = 0),
    y    = accumulate(1:(n-1), ~.x+sin(.y*2*pi/n), .init = 0),
    xend = accumulate(2:n,     ~.x+cos(.y*2*pi/n), .init = cos(2*pi/n)),
    yend = accumulate(2:n,     ~.x+sin(.y*2*pi/n), .init = sin(2*pi/n)))
}

polygon(5) -> df1
df1 %>% mutate(angle = atan2(yend-y, xend-x)+pi/2,
               x = 0.5*x+0.5*xend,
               y = 0.5*y+0.5*yend,
               xend = x+0.2*cos(angle),
               yend = y+0.2*sin(angle)) %>% 
  select(x, y, xend, yend) -> df2
df1 %>% bind_rows(df2) -> df
ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()


