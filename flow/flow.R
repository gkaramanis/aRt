library(tidyverse)
library(ambient)
library(here)

# Settings
nx = 300 # points on x axis
ny = 300 # points on y axis
n_curves = 10000 # number of lines to draw
n_steps = 20 # number of steps in every drawn line
step_length = 3 # length of step
limit = n_steps * step_length # limits when calculating lines
curve_stroke = 0.3 # stroke width of lines
curve_alpha = 0.5 # alpha of lines

# Create matrix with nx * ny points, values are angle with Perlin noise
pnt <- array(noise_perlin(c(nx, ny)), dim = c(nx, ny), dimnames = list(1:nx, 1:ny)) * pi * 2

# Create matrix for lines (segments with start and end points)
dat <- matrix(nrow = n_curves * n_steps, ncol = 5) 
colnames(dat) <- c("x_start", "y_start", "a", "x_end", "y_end")

# Loop for calculations
for (l in 1:n_curves-1) { # loop through lines
  # random start points within limits
  x_start = runif(1, min = limit, max = nx - (limit))
  y_start = runif(1, min = limit, max = ny - (limit))
  # get angle from the original matrix
  a = pnt[x_start, y_start]
  
  for (i in 1:n_steps) { # loop through steps in every line
    # calculate end point for first step
    x_end = x_start + step_length * cos(a)
    y_end = y_start + step_length * sin(a)
    # write first row of points and angle
    dat[i + l * n_steps, 1] = x_start
    dat[i + l * n_steps, 2] = y_start
    dat[i + l * n_steps, 3] = a
    dat[i + l * n_steps, 4] = x_end
    dat[i + l * n_steps, 5] = y_end
    # create starting point for next step from previous end point, get angle for the "new" point from the original matrix 
    x_start = x_end
    y_start = y_end
    a = pnt[x_start, y_start]
  }
}

# Convert matrix to data frame
dat_df <- as.data.frame(dat)

# Plot and save image
ggplot(dat_df) +
  geom_segment(aes(x = x_start, y = y_start,
                   xend = x_end, yend = y_end),
               size = curve_stroke, alpha = curve_alpha) +
  coord_fixed(xlim = c(limit * 1.5, nx - limit * 1.5), ylim = c(limit * 1.5, ny - limit * 1.5)) + # "crop" to fill the frame
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  ggsave(here::here("flow", "plots", paste0("flow-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 7, height = 7)

# Create data frame from settings
settings <- data.frame(nx, ny,
                       n_curves, n_steps,
                       step_length, curve_stroke, curve_alpha)

# Path to settings file
settings_file <- paste0("flow-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")

# Write settings to file
write_csv(settings, here::here("flow", "plots", settings_file))
