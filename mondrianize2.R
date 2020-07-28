# Erin Becker
# Created July 27, 2020

# load packages
library(ggplot2)

# random mondrian with random data

random_mondrian <- function(boxes, color_palette = NULL, 
                            opacity = TRUE, borders = FALSE, 
                            border_scarcity = 1) {
  
  # set default color palette if none provided
  if(is.null(color_palette)) {
    color_palette <- c("red", "blue", "white", "black", "yellow", 
                       "darkgreen", "darkblue")
  }

  # generate fill colors for plot by sampling from palette
  fill_rand <- sample(color_palette, boxes, replace = TRUE)
    
  # initialize dataframe to store random values of dimensions
  data_rand <- data.frame(fill = fill_rand, alpha = 1)
  
  # list the dimensions that are going to be randomly generated
  dimensions <- c("xmin_rand", "xmax_rand", "ymin_rand", "ymax_rand")
  
  # add alpha as variable if opacity is TRUE
  if(opacity == TRUE) dimensions <- c(dimensions, "alpha")
  
  # generate random values of dimensions
  for(i in dimensions) data_rand[[i]] = runif(boxes, 0, 1)
  
  # make the plot
  plot <- ggplot() + 
    geom_rect(data_rand, mapping = 
                aes(xmin = xmin_rand, xmax = xmax_rand, 
                    ymin = ymin_rand, ymax = ymax_rand),
              fill = data_rand$fill,
              alpha = data_rand$alpha) +
    theme_void() + 
    theme(plot.background = element_rect(color = "black"))
  
  # add border lines if borders = TRUE
  if(borders == TRUE) {
    
    # add border variables to data
    data_rand$vbord <- sample(c(TRUE, rep(FALSE, border_scarcity)), nrow(data_rand), 
                              replace = TRUE)
    data_rand$hbord <- sample(c(TRUE, rep(FALSE, border_scarcity)), nrow(data_rand),
                              replace = TRUE)
    
    # add lines to plot
    plot <- plot +
      geom_hline(yintercept = 
                    data_rand[which(data_rand$hbord == TRUE),]$ymin_rand, 
                    color = "black") +
      geom_vline(xintercept = 
                   data_rand[which(data_rand$vbord == TRUE),]$xmin_rand,
                 color = "black")
  }
  plot
}

# random_mondrian <- function(boxes, color_palette = NULL, 
#                             opacity = TRUE, borders = FALSE) {
#   
#   # set default color palette if none provided
#   if(is.null(color_palette)) {
#     color_palette <- c("red", "blue", "white", "black", "yellow", 
#                        "darkgreen", "darkblue")
#   }
#   
#   # generate fill colors for plot by sampling from palette
#   fill_rand <- sample(color_palette, boxes, replace = TRUE)
#   
#   # initialize dataframe to store random values of dimensions
#   data_rand <- data.frame(fill = fill_rand, alpha = 1)
#   
#   # list the dimensions that are going to be randomly generated
#   dimensions <- c("xmin_rand", "xmax_rand", "ymin_rand", "ymax_rand")
#   
#   # add alpha as variable if opacity is TRUE
#   if(opacity == TRUE) dimensions <- c(dimensions, "alpha")
#   
#   # generate random values of dimensions
#   for(i in dimensions) data_rand[[i]] = runif(boxes, 0, 1)
#   
#   # make the plot
#   plot <- ggplot() + 
#     geom_rect(data_rand, mapping = 
#                 aes(xmin = xmin_rand, xmax = xmax_rand, 
#                     ymin = ymin_rand, ymax = ymax_rand),
#               fill = data_rand$fill,
#               alpha = data_rand$alpha) +
#     theme_void() + 
#     theme(plot.background = element_rect(color = "black"))
#   
#   # add border lines if borders = TRUE
#   if(borders == TRUE) {
#     
#     # add border variables to data
#     data_rand$vbord <- sample(c(TRUE, FALSE, FALSE, FALSE, FALSE), nrow(data_rand), 
#                               replace = TRUE)
#     data_rand$hbord <- sample(c(TRUE, FALSE, FALSE, FALSE, FALSE), nrow(data_rand),
#                               replace = TRUE)
#     
#     # add lines to plot
#     plot <- plot +
#       geom_hline(yintercept = 
#                    data_rand[which(data_rand$hbord == TRUE),]$ymin_rand, 
#                  color = "black") +
#       geom_vline(xintercept = 
#                    data_rand[which(data_rand$vbord == TRUE),]$xmin_rand,
#                  color = "black")
#   }
#   plot
# }
