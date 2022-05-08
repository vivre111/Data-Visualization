#' @title  Make glyphs
#' @description Given various forms of multidimensional data and a function of data vectors to colors, make a corresponding list of glyphs in png format or a class of pixmap. The colors in each glyph is placed in the order of Hilbert curve,
#'  Morton curve or a method provided by Keim which is better used when the data has some inherent structure such as time series. By default, the data2col function is given, and one can
#'  adjust it by the parameters given (see \code{\link{data2col}}), or one can provide his own function.
#' @param data a data frame, matrix, where each row or column is for an individual, it can also be a list of data vectors for each individual
#' @param glyph_type if "Hilbert"(Default) colors in each glyph is placed in the order of Hilbert curve, if "Morton", it is in Morton curve, and if "rectangle" it is the recursive rectangular method provided by Keim.
#' @param width vector of width in each level of algorithm provided by Keim, must be given if glyph_type is "rectangle"
#' @param height vector of height in each level of algorithm provided by Keim if glyph_typbe is "rectangle"
#' @param cols a vector of rgb colours or NULL (default).  If NULL, cols is constructed from hueRange, intensityRange, alpha, numCols, and maxColorValue.
#' @param hueRange numeric vectors with values in [0, 360], it specifies the hue range. It does not need to be in ascending order since it is only used in the linear interpolation
#' @param intensityRange numeric vectors with values in [0, 1], it specifies the intersity range. It does not need to be in ascending order since it is used in the linear interpolation
#' @param alpha number in [0,1]. It specifies the \code{alpha} transparency value.
#' @param numCols number. It specifies the number of colors used to do the mapping, and the higher the value is, the more distinguished between different data values.
#' @param maxColorValue number in (0, 255] giving the maximum of the color values range.
#' @param xLow lower value of one's interested range
#' @param origin middle value in one's interested range in (\code{xLow}, \code{xHigh}), which specifies the data value mapping to the center of color scale, or "mean", "median" can be specified to set the origin as the mean or median value
#' for each glyph
#' @param xHigh higher value of one's interested range
#' @param missingCol color character specifying the color for the missing data. It is in the form of "#rrggbbaa", and the default is yellow.
#' @param outRangeCol color characters specifying the color for the data outside our interested range. If the \code{length} is two, the first will specify the color for the data lower than the range,
#' and the second is for higher. the color should have the same form with \code{missingCol}, and the default is "blue" and "red".
#' @param data2colfn function mapping from data values to color values, if missing, the function will be \code{\link{data2col}}
#' @param type string specifying the format of output. "png" means the output is a list of data matrices in the png format, and "pixmap" means the output is a list of class of pixmaps.
#' @param byrow logical value. If \code{TRUE} each row in the data frame or matrix will be considered as an individual. Otherwise, each column will be an individual.
#' @param ... Arguments passed to \code{data2colfn}
#' @return a list of data matrices of png format or a list of pixmaps
#' @author Jiahua Liu, Wayne Oldford
#' @examples
#' library(qrmdata)
#' data("SP500_const") # load the constituents data from qrmdata
#' time <- c("2007-01-03", "2009-12-31") # specify time period
#' x <- SP500_const[paste0(time, collapse = "/"),] # grab out data
#' data_c <- list() # complete data
#' for (i in 1:ncol(x)){
#'   data_c[[i]] <- x[,i]
#' }
#' x <- t(na.omit(t(x)))
#' data <- split(x,col(x)) # omit the missing data
#'
#' Glyphs <- make_glyphs(data[1:24]) # list of glyphs in the png format
#' Glyphs <- make_glyphs(data_c[1:24], glyph_type = "Morton")
#' width=c(1,6,1,12,1)
#' height=c(1,1,4,1,3)
#' Glyphs <- make_glyphs(data_c[1:24], width = width, height = height, glyph_type = "rectangle", alpha = 0.7)
#' Glyphs_pixmap <- make_glyphs(data_c[1:24], glyph_type = "Morton", type = "pixmap", intensityRange = c(1,0.4))
#'
#' # list of png format plot
#' sideLength <- ceiling(sqrt(length(Glyphs)))
#' plot(0,type='n', xlim=c(0, sideLength), ylim=c(0,sideLength),axes = FALSE,xlab = "", ylab = "")
#' glyph_x <- 0.7
#' glyph_y <- 0.5
#' plot <- Map(function(Glyphs_i,i){
#'   rasterImage(Glyphs_i,(i-1)%%sideLength, sideLength-1-(i-1)%/%sideLength, (i-1)%%sideLength+glyph_x, sideLength-(1-glyph_y)-(i-1)%/%sideLength)
#' }, Glyphs,1:length(Glyphs))
#'
#' # list of class of pixmap plot
#' sideLength <- ceiling(sqrt(length(Glyphs)))
#' glyph_x <- 0.7
#' glyph_y <- 0.5
#' plot(0,type='n', xlim=c(0, sideLength), ylim=c(0,sideLength),axes = FALSE,xlab = "", ylab = "")
#' plot <- Map(function(Glyphs_i,i){
#'   addlogo(Glyphs_i, px=c((i-1)%%sideLength, (i-1)%%sideLength+glyph_x), py=c(sideLength-1-(i-1)%/%sideLength, sideLength-(1-glyph_y)-(i-1)%/%sideLength))
#' },Glyphs_pixmap, 1:length(Glyphs_pixmap))
#' 
#' @export

make_glyphs <- function(data,
                        glyph_type = c("Hilbert", "Morton", "rectangle"),
                        width, height,
                        cols = NULL,
                        hueRange = c(0,360), intensityRange = c(0.4, 1), alpha=1,
                        numCols = 100000,
                        maxColorValue = 255,
                        xLow = NULL,
                        origin = NULL,
                        xHigh = NULL,
                        missingCol = NULL,
                        outRangeCol = NULL,
                        data2colfn,
                        type = c("png","pixmap"),
                        byrow = TRUE, ...){
  oldw <- getOption("warn")
  options(warn = -1)
  if (is.data.frame(data) | is.matrix(data)){
    if (byrow) {data <- lapply(1:nrow(data), FUN= function(i) {as.numeric(data[i, ])})
    } else {data <- lapply(1:ncol(data), FUN= function(i) {as.numeric(data[ ,i])})}
  } else {
    if (!is.list(data)) stop("the data is not in correct form")
  }
  type <- match.arg(type)
  glyph_type <- match.arg(glyph_type)
  if (!is.null(origin)) {
    if (origin != "mean" & origin != "median" & !is.numeric(origin)) stop("origin is not in correct form")
  }
  switch(glyph_type,
         Hilbert = {
           if (missing(data2colfn)){
             result <- Map(function(data_i){
               col_i <- data2col(data_i, cols=cols,
                                 hueRange=hueRange, intensityRange = intensityRange, alpha=alpha,
                                 numCols=numCols,
                                 maxColorValue = maxColorValue,
                                 xLow = if (is.null(xLow)) min(data_i, na.rm = TRUE) else xLow,
                                 origin = if (is.null(origin)) NULL else {
                                   if (origin == "mean") mean(data_i, na.rm = TRUE) else {
                                     if (origin == "median") median(data_i, na.rm = TRUE) else origin
                                   }
                                 },
                                 xHigh = if (is.null(xHigh)) max(data_i, na.rm = TRUE) else xHigh,
                                 missingCol = missingCol,
                                 outRangeCol = outRangeCol)
               HilbertGlyph(col = col_i, type=type)
             },data)
           } else {
             result <- Map(function(data_i){
               col_i <- data2colfn(data_i, ...)
               HilbertGlyph(col = col_i, type=type)
             },data)
           }
         },
         Morton = {
           if (missing(data2colfn)){
             result <- Map(function(data_i){
               col_i <- data2col(data_i, cols=cols,
                                 hueRange=hueRange, intensityRange = intensityRange, alpha=alpha,
                                 numCols=numCols,
                                 maxColorValue = maxColorValue,
                                 xLow = if (is.null(xLow)) min(data_i, na.rm = TRUE) else xLow,
                                 origin = if (is.null(origin)) NULL else {
                                   if (origin == "mean") mean(data_i, na.rm = TRUE) else {
                                     if (origin == "median") median(data_i, na.rm = TRUE) else origin
                                   }
                                 },
                                 xHigh = if (is.null(xHigh)) max(data_i, na.rm = TRUE) else xHigh,
                                 missingCol = missingCol,
                                 outRangeCol = outRangeCol)
               MortonGlyph(col = col_i, type=type)
             },data)
           } else {
             result <- Map(function(data_i){
               col_i <- data2colfn(data_i, ...)
               MortonGlyph(col = col_i, type=type)
             },data)
           }
         },
         rectangle = {
           if(missing(width) | missing(height)) stop("width and height must be given")
           if (missing(data2colfn)){
             result <- Map(function(data_i){
               col_i <- data2col(data_i, cols=cols,
                                 hueRange=hueRange, intensityRange = intensityRange, alpha=alpha,
                                 numCols=numCols,
                                 maxColorValue = maxColorValue,
                                 xLow = if (is.null(xLow)) min(data_i, na.rm = TRUE) else xLow,
                                 origin = if (is.null(origin)) NULL else {
                                   if (origin == "mean") mean(data_i, na.rm = TRUE) else {
                                     if (origin == "median") median(data_i, na.rm = TRUE) else origin
                                   }
                                 },
                                 xHigh = if (is.null(xHigh)) max(data_i, na.rm = TRUE) else xHigh,
                                 missingCol = missingCol,
                                 outRangeCol = outRangeCol)
               recGlyph(col_i, width, height, type=type)
             },data)
           } else {
             result <- Map(function(data_i){
               col_i <- data2colfn(data_i, ...)
               recGlyph(col_i, width, height, type=type)
             },data)
           }
         })
  options(warn = oldw)
  return(result)
}
