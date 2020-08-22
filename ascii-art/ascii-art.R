library(ggplot2)
library(dplyr)
library(magick)


ramp <- " .:-=+*#%@"

# Read in image and convert to grayscale
img_name <- "keanu"

img <- image_read(paste0("ascii-art/images/", img_name, ".jpg")) %>%
  image_convert(colorspace = "gray")

# Get dimensions
img_w <- image_info(img)$width
img_h <- image_info(img)$height
img_ratio <- img_w / img_h

# Resize the longest dimension to 80 pixels
if (img_w >= img_h) {
  img <- image_resize(img, "80")
} else {
  img <- image_resize(img, ("x80"))
}

# Create array and number rows and columns
img_array <- drop(as.integer(img[[1]]))
rownames(img_array) <- 1:nrow(img_array)
colnames(img_array) <- 1:ncol(img_array)

# Create data frame from array and rename columns
img_df <- as.data.frame.table(img_array) %>% 
  `colnames<-`(c("y", "x", "b")) %>% 
  mutate(
		across(everything(), as.numeric),
		# map b (0-255) to bf (1-0), so that "brighter" values become smaller numbers
		bf = 1 - b / 255,
		# and then map to i (1-10) to use with the character ramp
		i = round(bf * 10)
		) %>%
  rowwise() %>%
  mutate(c = substr(ramp, i, i))
		
ggplot(img_df) +
	geom_text(aes(x, y, label = c), family = "Courier Bold", size = max(img_df$y)/20) +
	scale_y_reverse() +
	coord_fixed() +
	theme_void() +
	ggsave(paste0("ascii-art/plots/", img_name, "-ascii.png"), width = 8, height = 8 / img_ratio)
