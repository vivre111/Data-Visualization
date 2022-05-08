library(grid)

#' @title  Plot glyphs
#' @description plot_glyphs plots a list of glyphs by basic plot or the plot in "grid" package
#' @param x a numeric vector specifying x-location or a matrix whose first column specify x-location and second column specify y-location
#' @param y a numeric vector specifying y-location, if x is a matrix, y is NULL.
#' @param glyphs a list of glyphs which are data matrices in picture format or pixmaps
#' @param glyphWidth width of each glyph
#' @param glyphHeight height of each glyph
#' @param just the justification of the rectangle relative to its (x, y) location
#' @param type string specifying the format used to plot. "raster" means each glyph in the list is a data matrix in the png, jpeg or tiff format, and "pixmap" means it is a class of pixmap.
#' "grid" means it will be ploted by the way in "grid" package. See \code{\link[graphics]{rasterImage}}, \code{\link[pixmap]{pixmap}}, \code{\link[grid]{grid.raster}}
#' @param add logical value, indicate whether the plot is add on to the original plot or not
#' @param ... Arguments passed to \code{plot} if \code{add=FALSE}
#' @author Jiahua Liu
#' @examples
#' library(dplyr)
#' # ozone
#' data_ozone <- list()
#' for(i in 1:(24*24)){
#'   col_number <- (i-1) %% 24 + 1
#'   row_number <- (i-1) %/% 24 + 1
#'   data_ozone[[i]] <- as.vector(nasa$mets$ozone[col_number,row_number,,])
#' }
#' glyphs_ozone <- make_glyphs(data = data_ozone, width = c(1,12,1), height = c(1,1,6),
#'                             glyph_type = "Keim", type = "pixmap")
#' x <- getGridXY(length(glyphs_ozone))
#' plot_glyphs(x, glyphs = glyphs_ozone, type = "pixmap")
#' # rainbow color
#' glyphs_rainbow <- make_glyphs(data = data_temperature[1:25], width = c(1,12,1), height = c(1,1,6), cols = rainbow(3),
#'                               glyph_type = "Keim")
#' x <- getGridXY(length(glyphs_rainbow))
#' x[,1] <- x[,1]/ceiling(max(x[,1]))
#' x[,2] <- x[,2]/ceiling(max(x[,2]))
#' plot_glyphs(x, glyphs = glyphs_rainbow, type = "grid")
#' 
#' @export

plot_glyphs  <- function(x, y=NULL,  glyphs, glyphWidth, glyphHeight,
                         just = c("centre", "center",
                                  "top", "bottom", 
                                  "left", "right", 
                                  "bottomleft", "topright", 
                                  "bottomright", "topleft"),
                         type = c("raster","pixmap", "grid"), add =FALSE,
                         xlab = "x", ylab = "y",
                         xlim = NULL, ylim = NULL, ...){
  type <- match.arg(type)
  just <- match.arg(just)
  ## Check to get x and y
  if (is.null(y)) {
    y <- x[,2]
    x <- x[,1]
  }
  if (length(unique(x)) == 1){
    interval_x <- 1
  } else {
    interval_x <- (max(x)-min(x))/(length(unique(x))-1)
  }
  if (length(unique(y)) == 1){
    interval_y <- 1
  } else {
    interval_y <- (max(y)-min(y))/(length(unique(y))-1)
  }
  if (missing(glyphWidth)) {
    if (length(unique(x)) == 1){
      x <- rep(0.5, length(x))
      glyphWidth <- 1
    } else {
      glyphWidth <- interval_x
    }
  }
  if (missing(glyphHeight)) {
    if (length(unique(y)) == 1){
      y <- rep(0.5, length(y))
      glyphHeight <- 1
    } else {
      glyphHeight <- interval_y
    }
  }
  switch(type,
         raster = {
           if (!add) {
             if (length(unique(x)) == 1 & length(unique(y)) == 1){
               if (is.null(xlim)) xlim <- c(0,1)
               if (is.null(ylim)) ylim <- c(0,1)
               plot(0, type='n', xlim=xlim, ylim=ylim, xlab = xlab, ylab = ylab, ...)
             }
             if (length(unique(x)) == 1 & !length(unique(y)) == 1){
               if (is.null(xlim)) xlim <- c(0,1)
               if (is.null(ylim)) ylim <- c(min(y)-interval_y/2,max(y)+interval_y/2)
               plot(0, type='n', xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...)
             }
             if (!length(unique(x)) == 1 & length(unique(y)) == 1){
               if (is.null(xlim)) xlim <- c(min(x)-interval_x/2, max(x)+interval_x/2)
               if (is.null(ylim)) ylim <- c(0,1)
               plot(0, type='n', xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...)
             }
             if (!length(unique(x)) == 1 & !length(unique(y)) == 1){
               if (is.null(xlim)) xlim <- c(min(x)-interval_x/2, max(x)+interval_x/2)
               if (is.null(ylim)) ylim <- c(min(y)-interval_y/2,max(y)+interval_y/2)
               plot(0, type='n', xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...)
             }
           }
           switch(just,
                  centre = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      rasterImage(Glyphs_i, x_i - glyphWidth/2, y_i - glyphHeight/2,
                                  x_i + glyphWidth/2, y_i + glyphHeight/2)
                    }, glyphs,x, y)
                  },
                  center = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      rasterImage(Glyphs_i, x_i - glyphWidth/2, y_i - glyphHeight/2,
                                  x_i + glyphWidth/2, y_i + glyphHeight/2)
                    }, glyphs,x, y)
                  },
                  top = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      rasterImage(Glyphs_i, x_i - glyphWidth/2, y_i - glyphHeight,
                                  x_i + glyphWidth/2, y_i)
                    }, glyphs,x, y)
                  },
                  bottom = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      rasterImage(Glyphs_i, x_i - glyphWidth/2, y_i,
                                  x_i + glyphWidth/2, y_i + glyphHeight)
                    }, glyphs,x, y)
                  },
                  left = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      rasterImage(Glyphs_i, x_i, y_i - glyphHeight/2,
                                  x_i + glyphWidth, y_i + glyphHeight/2)
                    }, glyphs,x, y)
                  },
                  right = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      rasterImage(Glyphs_i, x_i - glyphWidth, y_i - glyphHeight/2,
                                  x_i, y_i + glyphHeight/2)
                    }, glyphs,x, y)
                  },
                  bottomleft = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      rasterImage(Glyphs_i, x_i, y_i,
                                  x_i + glyphWidth, y_i + glyphHeight)
                    }, glyphs,x, y)
                  },
                  topright = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      rasterImage(Glyphs_i, x_i - glyphWidth, y_i - glyphHeight,
                                  x_i, y_i)
                    }, glyphs,x, y)
                  },
                  bottomright = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      rasterImage(Glyphs_i, x_i - glyphWidth, y_i,
                                  x_i, y_i + glyphHeight)
                    }, glyphs,x, y)
                  },
                  topleft = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      rasterImage(Glyphs_i, x_i, y_i - glyphHeight,
                                  x_i + glyphWidth, y_i)
                    }, glyphs,x, y)
                  })
         },
         pixmap = {
           if (!add) {
             if (length(unique(x)) == 1 & length(unique(y)) == 1){
               plot(0,type='n', xlim=range(c(0,1)), ylim=range(c(0,1)),
                    xlab = xlab, ylab = ylab, ...)
             }
             if (length(unique(x)) == 1 & !length(unique(y)) == 1){
               plot(0,type='n', xlim=range(c(0,1)), ylim=range(c(min(y)-interval_y/2,max(y)+interval_y/2)),
                    xlab = xlab, ylab = ylab, ...)
             }
             if (!length(unique(x)) == 1 & length(unique(y)) == 1){
               plot(0,type='n', xlim=range(c(min(x)-interval_x/2, max(x)+interval_x/2)), ylim=range(c(0,1)),
                    xlab = xlab, ylab = ylab, ...)
             }
             if (!length(unique(x)) == 1 & !length(unique(y)) == 1){
               plot(0,type='n', xlim=range(c(min(x)-interval_x/2, max(x)+interval_x/2)), ylim=range(c(min(y)-interval_y/2,max(y)+interval_y/2)),
                    xlab = xlab, ylab = ylab, ...)
             }
           }
           switch(just,
                  centre = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      pixmap::addlogo(Glyphs_i, px=c(x_i - glyphWidth/2, x_i + glyphWidth/2),
                              py=c(y_i - glyphHeight/2, y_i + glyphHeight/2))
                    },glyphs, x, y)
                  },
                  center = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      pixmap::addlogo(Glyphs_i, px=c(x_i - glyphWidth/2, x_i + glyphWidth/2),
                              py=c(y_i - glyphHeight/2, y_i + glyphHeight/2))
                    },glyphs, x, y)
                  },
                  top = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      pixmap::addlogo(Glyphs_i, px=c(x_i - glyphWidth/2, x_i + glyphWidth/2),
                              py=c(y_i - glyphHeight, y_i))
                    },glyphs, x, y)
                  },
                  bottom = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      pixmap::addlogo(Glyphs_i, px=c(x_i - glyphWidth/2, x_i + glyphWidth/2),
                              py=c(y_i, y_i + glyphHeight))
                    },glyphs, x, y)
                  },
                  left = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      pixmap::addlogo(Glyphs_i, px=c(x_i, x_i + glyphWidth),
                              py=c(y_i - glyphHeight/2, y_i + glyphHeight/2))
                    },glyphs, x, y)
                  },
                  right = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      pixmap::addlogo(Glyphs_i, px=c(x_i - glyphWidth, x_i),
                              py=c(y_i - glyphHeight/2, y_i + glyphHeight/2))
                    },glyphs, x, y)
                  },
                  bottomleft = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      pixmap::addlogo(Glyphs_i, px=c(x_i, x_i + glyphWidth),
                              py=c(y_i, y_i + glyphHeight))
                    },glyphs, x, y)
                  },
                  topright = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      pixmap::addlogo(Glyphs_i, px=c(x_i - glyphWidth, x_i),
                              py=c(y_i - glyphHeight, y_i))
                    },glyphs, x, y)
                  },
                  bottomright = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      pixmap::addlogo(Glyphs_i, px=c(x_i - glyphWidth, x_i),
                              py=c(y_i, y_i + glyphHeight))
                    },glyphs, x, y)
                  },
                  topleft = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      pixmap::addlogo(Glyphs_i, px=c(x_i, x_i + glyphWidth),
                              py=c(y_i - glyphHeight, y_i))
                    },glyphs, x, y)
                  })

         },
         grid = {
           if (!add) {grid.newpage()}
           switch(just,
                  centre = ,
                  center = ,
                  top = ,
                  bottom = ,
                  left = ,
                  right = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      grid.raster(Glyphs_i, x_i, y_i,
                                  width = glyphWidth, height = glyphHeight,
                                  just = just, ...)
                    },glyphs, x, y)
                  },
                  bottomleft = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      grid.raster(Glyphs_i, x_i, y_i,
                                  width = glyphWidth, height = glyphHeight,
                                  just = c("bottom", "left"), ...)
                    },glyphs, x, y)
                  },
                  topright = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      grid.raster(Glyphs_i, x_i, y_i,
                                  width = glyphWidth, height = glyphHeight,
                                  just = c("top", "right"), ...)
                    },glyphs, x, y)
                  },
                  bottomright = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      grid.raster(Glyphs_i, x_i, y_i,
                                  width = glyphWidth, height = glyphHeight,
                                  just = c("bottom", "right"), ...)
                    },glyphs, x, y)
                  },
                  topleft = {
                    plot <- Map(function(Glyphs_i,x_i, y_i){
                      grid.raster(Glyphs_i, x_i, y_i,
                                  width = glyphWidth, height = glyphHeight,
                                  just = c("top", "left"), ...)
                    },glyphs, x, y)
                  })
         })
}

