library(ggplot2)

geom_heart <- function(..., colour = "#67001f", size = 0.5, fill = "#b2182b",
                       mul = 1.0, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  
  data <- data.frame(t=seq(0, 10 * pi, by = 1))
  
  x <- function(t) 16*sin(t)^3
  y <- function(t) 13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t)
  
  data$x <- x(data$t) * mul
  data$y <- y(data$t) * mul
  
  data <- rbind(data, data[1,])
  
  layer(
    data = data,
    mapping = aes(x=x, y=y),
    stat = "identity",
    geom = ggplot2::GeomPolygon,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      size = size,
      colour = colour,
      fill = fill,
      ...
    )
  )
  
}

theme_heart <- function() {
    theme(plot.title = element_text(hjust = 0.5, size = 28)) +
    theme(plot.margin = margin(30, 30, 30, 30))
}

ggplot() +
  geom_heart(colour = NA) +
  coord_equal() +
  theme_heart()
