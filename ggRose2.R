ggRose2 <- function (data, mapping, palette = "Reds", color = "black", 
          size = 0.1, ...) 
{
        source("ggBar2.R")
        p <- ggBar2(data, mapping, stat = "identity", width = 1, 
                   color = color, size = size, palette = palette, polar = TRUE, 
                   ...)
        p
}