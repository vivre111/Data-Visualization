library(png)
library(pixmap)

#' @title  Keim's recursive pattern algorithm data visualization
#' @description recGlyph generates a data matrix of png format or a class of "pixmap" to display color strings in an order of recursive pattern provided by Keim in the space given a vector of colors
#' @param col vector of colors
#' @param width vector of width in each level of algorithm
#' @param height vector of height in each level of algorithm
#' @param fill logical value indicating whether the color strings repeat from the beginning if number of colors in col is smaller than the total number of colors to display
#' @param type string specifying the format of output. "png" means the output is a data matrix in the png format, and "pixmap" means the output is a class of pixmap.
#' @return a data matrix of png format or a class of "pixmap" that is used to plot. See \code{\link[pixmap]{pixmap}}, \code{\link[png]{writePNG}}, \code{\link[graphics]{rasterImage}}
#' @author Jiahua Liu, Wayne Oldford
#' @references Keim, Daniel A. 1996. "Pixel-Oriented Visualization Techniques for Exploring Very Large Data Bases." \emph{Journal of Computational and Graphical Statistics} 5 (1). Taylor & Francis: 58-77.
#' @examples
#' width=c(1,6,1,12,1)
#' height=c(1,1,4,1,3)
#' myPngmat <- recGlyph(width=width, height=height)
#' myPngmat <- recGlyph(width=width, height=height, col = colors(), fill = TRUE)
#' writePNG(myPngmat, target = "myplot.png")
#' plot(0,type='n', xlim=c(0,1), ylim=c(0,1), axes = FALSE,xlab = "", ylab = "")
#' rasterImage(myPngmat,0,0,1,1)
#' myPixmap <- recGlyph(width=width, height=height, col = colors(), type = "pixmap")
#' plot(myPixmap)
#' 
#' @export


recGlyph <- function(col, width, height, fill = FALSE, type = c("png","pixmap")){
  oldw <- getOption("warn")
  options(warn = -1)
  type <- match.arg(type)
  if(missing(width) | missing(height)) stop("width or height are missing")
  maxLength <- prod(width)*prod(height)
  Height <- prod(height)
  Width <- prod(width)
  if(missing(col)) col <- rainbow(64) # an example
  if(maxLength < length(col)) warning("Total length is smaller than the number of colors")
  if (fill){
    if(length(col) <= maxLength) {
      col <- rep_len(col, length.out = maxLength)
    }
  }
  switch(type,
         png = {
           size_height <- ceiling(480/Height)
           size_width <- ceiling(480/Width)
           png_mat <- array(dim = c(size_height*Height,size_width*Width,4))
           col_mat <- matrix(0,nrow = 4,ncol = maxLength)
           col_mat[,1:min(maxLength,length(col))] <- col2rgb(col,alpha = TRUE)[,1:min(maxLength,length(col))]/255
           pngFn <- function(x,y, col) {
             for(i in 1:size_height){
               for(j in 1:size_width){
                 png_mat[size_height*y+i,size_width*x+j,] <<- col
               }
             }
           }
           locNum <- 1
           SetPixel <- function(x,y,col_mat){
             pngFn(x,y, col = col_mat[,locNum])
             locNum <<- locNum + 1
           }
           col <- col_mat
         },
         pixmap = {
           pix_col <- character(maxLength)
           pixFn <- function(x,y, num, col) {
             index <- x*prod(height) + y + 1
             pix_col[index] <<- col
           }
           locNum <- 1
           SetPixel <- function(x,y,col){
             pixFn(x,y, locNum, col = col[locNum])
             locNum <<- locNum + 1
           }
         })
  level <- length(height)
  next_x <- numeric(level+1)
  next_y <- numeric(level+1)
  next_x[1] <- 1
  next_y[1] <- 1
  for(i in 2: (level+1)){
    next_x[i] <- prod(width[1:(i-1)])
    next_y[i] <- prod(height[1:(i-1)])
  }
  RecPStep <- function(x, y, level, col){
    if (level == 0){
      SetPixel(x,y,col)
    }
    else {
      if (level == 1){
        for(h in 1:height[level]){
          if (h%%2 == 1){
            for(w in 1:width[level]){
              RecPStep(x,y,level-1,col)
              x <- x + next_x[level]
            }
          }
          else {
            for(w in 1:width[level]){
              x <- x - next_x[level]
              RecPStep(x,y,level-1,col)
            }
          }
          y <- y + next_y[level]
        }
      }
      else {
        for(h in 1:height[level]){
          for(w in 1:width[level]){
            RecPStep(x,y,level-1,col)
            x <- x + next_x[level]
          }
          x <- x - next_x[level+1]
          y <- y + next_y[level]
        }
      }
    }
  }
  RecPStep(0, 0, level, col)
  switch(type,
         png = {
           result <- png_mat
         },
         pixmap = {
           result <- pixmap::pixmapIndexed(1:(maxLength), nrow=prod(height), col=pix_col)
         })
  options(warn = oldw)
  return(result)
}


