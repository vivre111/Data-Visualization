#' @title  HSI to RGB Conversion
#' @description hsi2rgb transforms colors from HSI space (hue/saturation/intensity) into RGB space (red/green/blue)
#' @param h vector of 'hue' values in [0,360] or 3-row hsi matrix
#' @param s vector of 'saturation' values in [0,1], or NULL when r is a matrix
#' @param i vector of 'intensity' values in [0,1], or NULL when r is a matrix
#' @details HSI is a variation of the HSV model, which provides color scales with monotonically increasing or decreasing brightness after linear interpolation
#' @return A matrix with a column for each color. The three rows of the matrix indicate "red","green","blue" values and are named "r", "g", and "b" in [0,255]
#'         accordingly.
#' @references Keim, Daniel A, and Hans-Peter Kriegel. 1995. "Issues in Visualizing Large Databases." In \emph{Visual Database Systems} 3, 203-14. Springer.
#' @author Jiahua Liu
#' @examples
#' x=c(0,360)
#' y=c(1,0.4)
#' f=approxfun(x,y) # linear introploation function
#' l <- 100000 # number of colors
#' H=seq(x[1],x[2],length.out = l)
#' I=f(H)
#' S=rep(1,length(H))
#' R=hsi2rgb(H,S,I)[1,]
#' G=hsi2rgb(H,S,I)[2,]
#' B=hsi2rgb(H,S,I)[3,]
#' col1=rgb(R,G,B,maxColorValue = 255)
#' gr1=(0.34*R+0.5*G+0.16*B)/255
#' col2=grey(gr1)
#' par(mfrow=c(2,1))
#' barplot(rep(1,length(H)),col = col1,border = NA,beside = FALSE,space = c(0,0),main="HSI color mapping")
#' barplot(rep(1,length(H)),col = col2,border = NA,beside = FALSE,space = c(0,0))
#' 
#' @export
#' 

hsi2rgb <- function(h, s = NULL, i = NULL, maxColorValue = 255){
  hsi <- if (is.null(s) && is.null(i))
    as.matrix(h)
  else rbind(h, s, i)
  if (!is.numeric(hsi))
    stop("hsi matrix must be numeric")
  d <- dim(hsi)
  if (d[1L] != 3L)
    stop("hsi matrix must have 3 rows")
  n <- d[2L]
  if (n == 0L)
    return(cbind(c(r = 1, g = 1, b = 1))[, 0L])
  hsi[1L,] <- hsi[1L,]/360
  if (any(0 > hsi) || any(hsi > 1))
    stop("h values must be in [0,360], s and i must be in [0,1]")
  h=hsi[1L,]*360
  s=hsi[2L,]
  i=hsi[3L,]
  value <- function(hue){
    pure <- 0.5*(1+cos(hue/180*pi))
    val <- i*(1-s*(1-pure))
    return(val)
  }
  r <- value(h)*maxColorValue
  g <-value(h+240)*maxColorValue
  b <- value(h+120)*maxColorValue
  RGB_matrix <- rbind(r,g,b)
  return(RGB_matrix)
}


