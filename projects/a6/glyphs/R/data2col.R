#' @title  Data to color mapping
#' @description data2col provides a mapping from data values to color values by using hsi color space. One can use \code{hueRange}, \code{intensityRange}, \code{alpha}, \code{alpha}, \code{numCols}, and \code{maxColorValue} to adjust the mapping.
#' Also, missingCol specifies the color to the missing data.
#' @param data vector of data values
#' @param cols a vector of rgb colours or NULL (default).  If NULL, cols is constructed from \code{hueRange}, \code{intensityRange}, \code{alpha}, \code{alpha}, \code{numCols}, and \code{maxColorValue}.
#' @param hueRange numeric vectors with values in [0, 360], it specify the hue range. It does not need to be in ascending order since it is only used in the linear interpolation
#' @param intensityRange numeric vectors with values in [0, 1], it specifies the intersity range. It does not need to be in ascending order since it is only used in the linear interpolation
#' @param alpha number in [0,1]. It specifies the \code{alpha} transparency value.
#' @param numCols number. It specifies the number of colors used to do the mapping, and the higher the value is, the more distinguished between different data values.
#' @param maxColorValue number in (0, 255]. giving the maximum of the color values range.
#' @param xLow lower value of one's interested range
#' @param origin middle value in one's interested range in (\code{xLow}, \code{xHigh}), which specifies the data value mapping to the center of color scale
#' @param xHigh higher value of one's interested range
#' @param missingCol color character specifying the color for the missing data. It is in the form of "#rrggbbaa", and the default is yellow.
#' @param outRangeCol color characters specifying the color for the data outside our interested range. If the \code{length} is two, the first will specify the color for the data lower than the range,
#' and the second is for higher. the color should have the same form with \code{missingCol}, and the default is "blue" and "red".
#' @return a vector of color chacacters which can be used to plot
#' @author Jiahua Liu, Wayne Oldford
#' @examples
#' data <- 1:10
#' col <- data2col(data)
#' barplot(rep(1:length(data)),col = col)
#' @export
data2col <- function(data,
                     cols = NULL,
                     hueRange=c(0,360), intensityRange = c(0.4, 1), alpha=1,
                     numCols=100000,
                     maxColorValue = 255,
                     xLow = min(data, na.rm = TRUE),
                     origin = NULL,
                     xHigh = max(data, na.rm = TRUE),
                     missingCol = NULL,
                     outRangeCol = NULL){
  if (!is.numeric(alpha) | alpha > 1 | alpha < 0)  stop("alpha must be between 0 and 1")
  if (is.null(missingCol)) {
    missingCol <- grDevices::adjustcolor(grDevices::rgb(t(grDevices::col2rgb("yellow")),
                                             maxColorValue = 255, alpha=255),
                                         alpha.f = alpha)
  }
  if (is.null(outRangeCol)) {
    outRangeCol <- c(grDevices::adjustcolor(grDevices::rgb(t(grDevices::col2rgb("blue")),
                                                maxColorValue = 255, alpha=255),
                                            alpha.f = alpha),
                     grDevices::adjustcolor(grDevices::rgb(t(grDevices::col2rgb("red")),
                                                maxColorValue = 255, alpha=255),
                                            alpha.f = alpha)
    )
  } else {
    if (length(outRangeCol) <= 1){
      outRangeCol <- rep(outRangeCol,2)
    } else {
      outRangeCol <- outRangeCol[1:2]
    }
  }
  low_col <- outRangeCol[1]
  high_col <- outRangeCol[2]
  
  missing_loc <- which(is.na(data))
  if (length(missing_loc) == 0) {
    dataNoMissing <- data
  } else {
    dataNoMissing <- data[-missing_loc]
  }
  if (length(dataNoMissing) > 0) {
    if (xHigh < xLow) stop("xHigh must be higher than xLow")
    if(is.null(origin)) {
      if(xHigh > xLow) {
        dataNoMissing <- (dataNoMissing-xLow) / (xHigh -xLow)
        xLow <- 0
        xHigh <- 1
      } else {
        dataNoMissing <- rep(if (dataNoMissing < xLow) -1 else {
          if (dataNoMissing > xHigh) 2  else 0.5},
          length(dataNoMissing)
        )
        xLow <- 0
        xHigh <- 1
      }
    } else {
      if (origin >= xHigh | origin <= xLow) stop("origin must be in the range (xLow, xHigh)")
      bottomRange <- abs(origin - xLow)
      topRange <- abs(xHigh - origin)
      if (topRange > bottomRange){
        shift <- (0.5 * xHigh - origin) / (xHigh -origin)
        scale <- 0.5 / (xHigh - origin)
      } else {
        shift <- -0.5 * xLow / (origin - xLow)
        scale <- 0.5 / (origin -xLow)
      }
      dataNoMissing <- scale * dataNoMissing + shift
      xLow <-  scale * xLow + shift
      xHigh <-  scale * xHigh + shift
    }
    dataNoMissing[dataNoMissing > 1] = 1
    high_loc <- which(dataNoMissing > xHigh)
    low_loc <- which(dataNoMissing < xLow)
    inRange_loc <- which(dataNoMissing <= xHigh & dataNoMissing >= xLow)
    if (length(inRange_loc) != 0) {
      dataNoMissing_inRange <- dataNoMissing[inRange_loc]
      if (is.null(cols))
      {
        # linear interpolation function
        interpolator=approxfun(hueRange, intensityRange)
        H <- seq(hueRange[1],hueRange[2],length.out = numCols)
        I <- interpolator(H)
        S <- rep(1, length(H))
        rgbVals <- hsi2rgb(H, S, I, maxColorValue = maxColorValue)
        cols <- grDevices::rgb(t(rgbVals), maxColorValue = maxColorValue, alpha = alpha*maxColorValue)
      } else {numCols <- length(cols)}
      
      pixelCols <- character(length(data))
      
      dataNoMissing_inRange[which(dataNoMissing_inRange == 0)] <- 1/numCols
      
      if (length(missing_loc)==0) {
        if (length(inRange_loc) != 0){
          pixelCols[inRange_loc] <- cols[ceiling(dataNoMissing_inRange*numCols)]
        }
      } else {
        pixelCols[-missing_loc] <- cols[ceiling(dataNoMissing_inRange*numCols)]
        pixelCols[missing_loc] <- missingCol
      }
      if (length(high_loc) != 0){
        pixelCols[high_loc] <- high_col
      }
      if (length(low_loc) != 0){
        pixelCols[low_loc] <- low_col
      }
    } else {
      pixelCols <- character(length(data))
      if (length(missing_loc) != 0){
        pixelCols[missing_loc] <- missingCol
      }
      if (length(high_loc) != 0){
        pixelCols[high_loc] <- high_col
      }
      if (length(low_loc) != 0){
        pixelCols[low_loc] <- low_col
      }
    }
  } else {
    pixelCols <- character(length(data))
    pixelCols[missing_loc] <- missingCol
  }
  if (any(is.na(pixelCols) == TRUE)) stop("all values should be numeric")
  return(pixelCols)
}
