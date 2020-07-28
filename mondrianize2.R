# Erin Becker
# Created July 27, 2020

# load packages
library(ggplot2)

# random mondrian with random data

random_mondrian <- function(boxes, color_palette = NULL, opacity = TRUE) {
  
  # set default color palette if none provided
  if(is.null(color_palette)) {
    color_palette <- c("red", "blue", "white", "black", "yellow")
  }

  # generate colors for plot by sampling from palette
  colors_rand <- sample(color_palette, boxes, replace = TRUE)
  
  # initialize dataframe to store random values of dimensions
  data_rand <- data.frame(color = colors_rand, alpha = 1)
  
  # list the dimensions that are going to be randomly generated
  dimensions <- c("xmin_rand", "xmax_rand", "ymin_rand", "ymax_rand")
  
  # add alpha as variable if opacity is TRUE
  if(opacity == TRUE) dimensions <- c(dimensions, "alpha")
  
  # generate random values of dimensions
  for(i in dimensions) data_rand[[i]] = runif(boxes, 0, 1)
  
  # make the plot
  ggplot() + 
    geom_rect(data_rand, mapping = 
                aes(xmin = xmin_rand, xmax = xmax_rand, 
                    ymin = ymin_rand, ymax = ymax_rand),
              fill = data_rand$color,
              alpha = data_rand$alpha) +
    theme_void() + 
    theme(plot.background = element_rect(color = "black"))
}




# my_colors <- c("red", "blue", "white", "black", "yellow")
# colors_rand <- sample(my_colors, 10, replace = TRUE)
# 
# data_rand <- data.frame(color = colors_rand)
# 
# dimensions <- c("xmin_rand", "xmax_rand", "ymin_rand", "ymax_rand",
#                 "alpha")
# 
# for(i in dimensions) data_rand[[i]] = runif(10, 0, 1)
# 
# ggplot() + 
#   geom_rect(data_rand, mapping = 
#               aes(xmin = xmin_rand, xmax = xmax_rand, 
#                   ymin = ymin_rand, ymax = ymax_rand),
#               fill = data_rand$color,
#             alpha = data_rand$alpha) +
#   theme_void() + 
#   theme(plot.background = element_rect(color = "black"))
