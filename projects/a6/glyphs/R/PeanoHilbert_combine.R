library(png)
library(pixmap)

#' @title  Hilbert curve data visualization
#' @description HilbertGlyph generates a data matrix of png format or a class of "pixmap" which can be used to display color strings in an order of Hilbert curve in the space given a vector of colors
#' @param col vector of colors
#' @param fill logical value indicating whether the color strings repeat from the beginning if number of colors in col is smaller than the total length of Hilbert curve
#' @param type string specifying the format of output. "png" means the output is a data matrix in the png format, and "pixmap" means the output is a class of pixmap.
#' @return a data matrix of png format or a class of "pixmap" that is used to plot. See \code{\link[pixmap]{pixmap}}, \code{\link[png]{writePNG}}, \code{\link[graphics]{rasterImage}}
#' @author Jiahua Liu, Wayne Oldford
#' @examples
#' myPngmat <- HilbertGlyph()
#' myPngmat <- HilbertGlyph(col = colors())
#' myPngmat <- HilbertGlyph(col = colors(), fill = TRUE)
#' writePNG(myPngmat, target = "myplot.png")
#' plot(0,type='n', xlim=c(0,1), ylim=c(0,1), axes = FALSE,xlab = "", ylab = "")
#' rasterImage(myPngmat, 0, 0, 1, 1)
#' mypixmap <- HilbertGlyph(type = "pixmap")
#' mypixmap <- HilbertGlyph(col = colors(), fill = TRUE, type = "pixmap")
#' plot(mypixmap)
#' 
#' @export

HilbertGlyph <- function(col, fill = FALSE, type = c("png","pixmap")){
  oldw <- getOption("warn")
  options(warn = -1)
  type <- match.arg(type)
  if(missing(col)) col <- rainbow(64)  # an example
  levels <- 1:13
  level <- levels[findInterval(length(col),2^(2*levels),left.open = TRUE)+1]
  if(level >= 13) stop("Too many colors to display")
  maxLength <- 2^(2*level)
  sideLength <- 2^level
  if (fill){
    if(length(col) <= maxLength) {
      col <- rep_len(col, length.out = maxLength)
    }
  }
  switch(type,
         png = {
           size <- ceiling(480/sideLength)
           png_mat <- array(dim = c(size*sideLength,size*sideLength,4))
           col_mat <- matrix(0,nrow = 4,ncol = maxLength)
           col_mat[,1:min(maxLength,length(col))] <- col2rgb(col,alpha = TRUE)[,1:min(maxLength,length(col))]/255
           pngFn <- function(x,y, col) {
             for(i in 1:size){
               for(j in 1:size){
                 png_mat[size*y+i,size*x+j,] <<- col
               }
             }
           }
           locNum <- 1
           SetPixel <- function(x,y,col_mat){
             pngFn(x,y, col = col_mat[,locNum])
             locNum <<- locNum+1
           }
           col <- col_mat
         },
         pixmap = {
           pix_col <- character(maxLength)
           pixFn <- function(x,y, num, col) {
             index <- x*2^level + y + 1
             pix_col[index] <<- col
           }
           locNum <- 1
           SetPixel <- function(x,y,col){
             pixFn(x,y, locNum, col = col[locNum])
             locNum <<- locNum + 1
           }
         }
  )
  move <- function(d){
    switch(d,
           down = {y <<- y+1
           x <<- x},
           up = {y <<- y-1
           x <<- x},
           right = {x <<- x+1
           y <<- y},
           left = {x <<- x-1
           y <<- y}
    )
  }
  if (level %% 2 == 1){
    HilbertStep <- function(R,D,L,U,level, col){
      if (level > 0){
        HilbertStep(D,R,U,L,level-1, col)
        SetPixel(x,y,col)
        move(R)
        
        HilbertStep(R,D,L,U,level-1, col)
        SetPixel(x,y,col)
        move(D)
        
        HilbertStep(R,D,L,U,level-1, col)
        SetPixel(x,y,col)
        move(L)
        
        HilbertStep(U,L,D,R,level-1, col)
      }
    }
  } else {
    HilbertStep <- function(D,R,U,L,level, col){
      if (level > 0){
        HilbertStep(R,D,L,U,level-1, col)
        SetPixel(x,y,col)
        move(R)
        
        HilbertStep(D,R,U,L,level-1, col)
        SetPixel(x,y,col)
        move(D)
        
        HilbertStep(D,R,U,L,level-1, col)
        SetPixel(x,y,col)
        move(L)
        
        HilbertStep(L,U,R,D,level-1, col)
      }
    }
  }
  ## Initialize x and y to zero
  x = 0;y= 0
  HilbertStep("right", "down", "left", "up", level, col)
  SetPixel(x, y, col)
  switch(type,
         png = {
           result <- png_mat
         },
         pixmap = {
           result <- pixmap::pixmapIndexed(1:maxLength, nrow=2^level, col=pix_col)
         })
  options(warn = oldw)
  return(result)
}



