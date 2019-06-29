library(imager)
library(dplyr)
library(ggplot2)
library(scales)

# Download the image
# urlfile="http://ereaderbackgrounds.com/movies/bw/Frankenstein.jpg"
# 
file = "./portraits/p1.jpg"
# if (!file.exists(file)) download.file(urlfile, destfile = file, mode = 'wb')

# Load, convert to grayscale, filter image (to convert it to bw) and sample
load.image(file) %>% 
  grayscale() %>%
  threshold("45%") %>% 
  as.cimg() %>% 
  as.data.frame()  %>% 
  sample_n(8000, weight=(1-value)) %>% 
  select(x, y) -> data

# Rearrange the original points according the TSP output
data_to_plot <- data

# A little bit of ggplot to plot results
ggplot(data_to_plot, aes(x, y)) +
  geom_point(size = 6, alpha = 0.09, shape = 8) +
  scale_y_continuous(trans=reverse_trans())+
  coord_fixed()+
  theme_void()

# Do you like the result? Save it! (Change the filename if you want)
ggsave("./portraits/p1.png", dpi=600, width = 4, height = 5)
