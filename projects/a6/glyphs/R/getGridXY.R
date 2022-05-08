#' @title  Get x and y coordinates in a grid
#' @description getGridXY generates a matrix of x and y coordinates in a grid. The justification of each coordinate is in center.
#' @param n number of coordinates to generate
#' @param nrows number of rows in the grid
#' @param ncols number of columns in the grid
#' @param byrow logical value indicating whether the order of coordinates in the grid is byrow or not
#' @return a matrix which gives the x and y coordinates of a grid
#' @author Jiahua Liu
#' @examples
#' x <- getGridXY(16)
#' x
#' @export

getGridXY <- function(n, nrows, ncols = NULL, byrow =TRUE ) {
  Grid_matrix <- matrix(nrow = n, ncol = 2)
  if (missing(nrows) & is.null(ncols)){
    sideLength <- ceiling(sqrt(n))
    for(i in 1:n){
      Grid_matrix[i,] <- c((i-1)%%sideLength + 0.5, sideLength-0.5-(i-1)%/%sideLength)
    }
  }
  else{
    if (!missing(nrows)) ncols <- ceiling(n/nrows)
    if (missing(nrows) & !is.null(ncols)) nrows <- ceiling(n/ncols)
    if (byrow){
      for(i in 1:n){
        Grid_matrix[i,] <- c((i-1)%%ncols + 0.5, nrows-0.5-(i-1)%/%ncols)
      }
    } else{
      for(i in 1:n){
        Grid_matrix[i,] <- c((i-1)%/%nrows + 0.5, nrows-0.5-(i-1)%%nrows)
      }
    }
  }
  return(Grid_matrix)
}

