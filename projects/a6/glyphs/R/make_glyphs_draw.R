library(png)
library(jpeg)
library(tiff)
#library(rtiff)

#' @title  Make glyphs in different classes
#' @description Given a list of data vectors, make a corresponding list of glyphs in one of four different classes which are "png", "jpeg", "pixmap" and "tiff".
#' @param data a list of data vectors
#' @param draw_fun function of drawing in a device given a data vector
#' @param type string specifying the format of output. "png" means the output is a list of data matrices in the png format, "jpeg" means the output is in the jpeg format and so on.
#' @param width the width of the device
#' @param height the height of the device
#' @param mar A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot
#' @param ... Arguments passed to `png`, `jpeg`, `pixmap`, or `tiff` according to the `type`
#' @return a list of glyphs in different classes corresponding to the list of data
#' @author Wayne Oldford, Jiahua Liu
#' @examples
#' n <- 16
#' data <- list()
#' for (i in 1:n){
#'   data[[i]] <- rnorm(500)
#' }
#' glyphs <- make_glyphs_draw(data, hist, type = "jpeg", mar = rep(4, 4), width = 200, height = 200)
#' x <- getGridXY(n)
#' plot_glyphs(x, glyphs = glyphs, glyphWidth = 0.8, glyphHeight = 0.6, axes = FALSE, xlab = "", ylab = "")
#' 
#' 
#' @export



make_glyphs_draw <- function (data, draw_fun,
                              type = c("png", "jpeg", "pixmap" , "tiff" #, "bmp"
                                       ),
                              width = 50, height = 50, mar=rep(1, 4), ...) {
  
  # Note that for small images, it may be necessary to have the draw function
  # use small margins as in par(mar=rep(0.5, 4))
  #
  tmp <- tempdir()
  type = match.arg(type)
  files <- Map(function(data_i, i) {
    file <- file.path(tmp, paste0("img", i, ".", type))
    switch(
      type,
      png =  grDevices::png(file, width = width, height = height,
                            ...),
      #bmp =  grDevices::bmp(file, width = width, height = height,
      #                      ...),
      tiff =  grDevices::tiff(file, width = width, height = height,
                              ...),
      pixmap =  grDevices::tiff(file, width = width, height = height,
                              ...),
      jpeg =  grDevices::jpeg(file, width = width, height = height,
                             ...)
    )
    par(mar=mar)
    draw_fun(data_i)
    grDevices::dev.off()
    file
  }, data, 1:length(data))

  switch(
    type,
    png =   lapply(files, function(f) png::readPNG(f)),
    #bmp =   lapply(files, function(f) read.bmp(f)), # read.bmp fails
    tiff =  lapply(files, function(f) tiff::readTIFF(f)),
    pixmap = lapply(files, function(f) tiff::readTIFF(f)),  # Returns a pixmapRGB
    jpeg =  lapply(files, function(f) jpeg::readJPEG(f))
  )
}
