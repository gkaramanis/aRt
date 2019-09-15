library(imager)
library(dplyr)
library(ggplot2)
library(scales)

# Download the image
# urlfile="http://ereaderbackgrounds.com/movies/bw/Frankenstein.jpg"
 
img = "kr"
file = paste("./portraits/", img, ".jpg", sep = "")

# if (!file.exists(file)) download.file(urlfile, destfile = file, mode = 'wb')

# Load, convert to grayscale, filter image (to convert it to bw) and sample
load.image(file) %>% 
  grayscale() %>%
  threshold("45%") %>% 
  as.cimg() %>% 
  as.data.frame()  %>% 
  sample_n(12000, weight=(1-value)) %>% 
  select(x, y) -> data

data %>% 
ggplot(aes(x, y)) +
  geom_tile(height = 15, width = 15, alpha = 0.03, fill = "grey70") +
  # geom_step(size = 0.6, alpha = 0.1, color = "grey40") +
  scale_y_continuous(trans = reverse_trans()) +
  coord_fixed()+
  theme_void()

ggsave(paste("./portraits/", img, "-art.png", sep = ""), dpi=600, width = 4, height = 5)
