## ----knitr_setup, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----setup, warning=FALSE, message=FALSE, error=FALSE-------------------------
library(glyphs)
library(jpeg)

## -----------------------------------------------------------------------------
library(dplyr)
names(nasa$mets)
names(nasa$dims)

## -----------------------------------------------------------------------------
data_temperature <- list()
for(i in 1:(24*24)){
  col_number <- (i-1) %% 24 + 1
  row_number <- (i-1) %/% 24 + 1
  data_temperature[[i]] <- as.vector(nasa$mets$temperature[col_number,row_number,,])
}
str(data_temperature[1:3]) # show the first three elements in the data

## ---- fig.show='hold'---------------------------------------------------------
par(mar = rep(1.5,4))
# get glyphs
glyphs_temperature <- make_glyphs(data = data_temperature, width = c(1,12,1), height = c(1,1,6),
                                  glyph_type = "rectangle", type = "pixmap")
x <- expand.grid(nasa$dims$long, nasa$dims$lat) # get the latitude and longitude as coordinates
# plot glyphs
plot_glyphs(x, glyphs = glyphs_temperature, type = "pixmap", xlab = "", ylab = "",
            xaxs = "i", yaxs = "i", cex.axis = 0.8, mgp = c(3, 0.5, 0))
title("Nasa data temperature trend plot\n(Default)", line = 0.1, cex.main = 0.8)
# color mapping plot
x <- c(0, 360)
y <- c(0.4, 1)
f <- approxfun(x, y)
l <- 100000
H <- seq(x[1],x[2], length.out = l)
I <- f(H)
S <- rep(1,length(H))
R <- hsi2rgb(H, S, I)[1,]
G <- hsi2rgb(H, S ,I)[2,]
B <- hsi2rgb(H, S, I)[3,]
col_hsi <- rgb(t(hsi2rgb(H, S, I)), maxColorValue = 255)
barplot(rep(1,length(H)), col = col_hsi, border = NA, beside = FALSE, space = c(0,0),
        axes = FALSE, main="Color mapping", cex.main = 0.9)

## ---- fig.show='hold'---------------------------------------------------------
par(mar = rep(1.5,4))
# get glyphs
glyphs_temperature <- make_glyphs(data = data_temperature, width = c(1,12,1), height = c(1,1,6),
                                  hueRange = c(240,360), glyph_type = "rectangle", type = "pixmap")
x <- expand.grid(nasa$dims$long, nasa$dims$lat) # get the latitude and longitude as coordinates
# plot glyphs
plot_glyphs(x, glyphs = glyphs_temperature, type = "pixmap", xlab = "", ylab = "",
            xaxs = "i", yaxs = "i", cex.axis = 0.8, mgp = c(3, 0.5, 0))
title("Nasa data temperature trend plot\n(Red vs Blue)", line = 0.1, cex.main = 0.8)
# color mapping plot
x <- c(240, 360)
y <- c(0.4, 1)
f <- approxfun(x, y)
l <- 100000
H <- seq(x[1],x[2], length.out = l)
I <- f(H)
S <- rep(1,length(H))
R <- hsi2rgb(H, S, I)[1,]
G <- hsi2rgb(H, S ,I)[2,]
B <- hsi2rgb(H, S, I)[3,]
col_hsi <- rgb(t(hsi2rgb(H, S, I)), maxColorValue = 255)
barplot(rep(1,length(H)), col = col_hsi, border = NA, beside = FALSE, space = c(0,0),
        axes = FALSE, main="Color mapping", cex.main = 0.9)

## ---- fig.show='hold'---------------------------------------------------------
library(colorspace)
par(mar = rep(1.5,4))
cols <- diverge_hcl(21) # get colors
glyphs_temperature <- make_glyphs(data = data_temperature, width = c(1,12,1), height = c(1,1,6),
                                  cols = cols, glyph_type = "rectangle", type = "pixmap")
x <- expand.grid(nasa$dims$long, nasa$dims$lat) # get the latitude and longitude as coordinates
plot_glyphs(x, glyphs = glyphs_temperature, type = "pixmap", xlab = "", ylab = "",
            xaxs = "i", yaxs = "i", cex.axis = 0.8, mgp = c(3, 0.5, 0))
title("Nasa data temperature trend plot\n(Diverge scale)", line = 0.1, cex.main = 0.8)
# color mapping
pal <- function(col, border = "light gray")
{
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}
pal(cols)

## ---- fig.align='center', fig.height=7, fig.width=7---------------------------
library(maps)
par(mar = rep(1.5,4))
long <- nasa$dims$long
lat <- nasa$dims$lat
interval_x <- (max(long)-min(long))/(length(unique(long))-1)
interval_y <- (max(lat)-min(lat))/(length(unique(lat))-1)
# draw map and make the map able to change the size
map_glyph <- make_glyphs_draw(data = list(glyphs_temperature), draw_fun = function(glyph_i){
  # draw map background
  map("world", xlim = c(min(long)-interval_x/2, max(long)+interval_x/2),
      ylim = c(min(lat)-interval_y/2, max(lat)+interval_y/2), border = FALSE,
      col=adjustcolor("grey70", alpha.f = 0.7), fill=TRUE, bg = "grey90", myborder = 0.001)
  map.axes(cex.axis = 0.8, mgp = c(3, 0.5, 0)) # add axes on the map
  grid(nx = length(nasa$dims$long), col = "white", lty = 1) # add grid
  # plot the glyphs
  plot_glyphs(x, glyphs = glyph_i, type = "pixmap",
              glyphWidth = 1.8, glyphHeight = 1.5, add = TRUE)
}, mar = rep(0,4), width = 960, height = 960)
plot(0,type='n', xlim=c(0, 1), ylim=c(0, 1), axes = FALSE, xlab = "", ylab = "")
title("Nasa data temperature trend plot\n(Diverge scale)", line = 0.1, cex.main = 0.8)
rasterImage(map_glyph[[1]], 0, 0, 1, 1)

## ---- fig.align='center', fig.height=7, fig.width=7---------------------------
par(mar = rep(1.5,4))
xnew <- seq(1, 72, length.out = 100)
x <- expand.grid(nasa$dims$long, nasa$dims$lat) # get the latitude and longitude as coordinates
# get time series glyphs
timeseries_glyph <- make_glyphs_draw(data = data_temperature, draw_fun = function(data_i){
  sm_i <- smooth.spline(data_i, df = 12) # smooth.spline fitting with df = 12
  ypred_i <- predict(sm_i, x = xnew)$y 
  plot(xnew, ypred_i, type = "l", lwd = 4, axes = FALSE, xlab = "", ylab = "",
       xaxs = "i", yaxs = "i")
}, type = "png", width = 100, height = 100, bg = NA)
# draw map and make the map able to change the size
map_glyph_timeseries <- make_glyphs_draw(data = list(timeseries_glyph), draw_fun = function(glyph_i){
  # draw map background
  map("world", xlim = c(min(long)-interval_x/2, max(long)+interval_x/2),
      ylim = c(min(lat)-interval_y/2, max(lat)+interval_y/2), border = FALSE,
      col=adjustcolor("grey70", alpha.f = 0.7), fill=TRUE, bg = "grey90", myborder = 0.001)
  map.axes(cex.axis = 1.5, mgp = c(3, 0.5, 0)) # add map axis
  grid(nx = length(nasa$dims$long), col = "white", lty = 1, lwd = 2) # add grids
  plot_glyphs(x, glyphs = timeseries_glyph, add = TRUE) # plot glyphs
}, mar = rep(0,4), width = 1500, height = 1500)
plot(0,type='n', xlim=c(0, 1), ylim=c(0, 1), axes = FALSE, xlab = "", ylab = "")
title("Time series glyphs plot\n(local scaling)", line = 0.1, cex.main = 0.8)
rasterImage(map_glyph_timeseries[[1]], 0, 0, 1, 1)

## ---- fig.align='center', fig.height=7, fig.width=7---------------------------
par(mar = rep(1.5,4))
ylim <- c(min(unlist(data_temperature)), max(unlist(data_temperature)))
timeseries_glyph_commonscale <- make_glyphs_draw(data = data_temperature, 
draw_fun = function(data_i){
  sm_i <- smooth.spline(data_i, df = 12) # smooth.spline fitting with df = 12
  ypred_i <- predict(sm_i, x = xnew)$y 
  plot(xnew, ypred_i, type = "l", lwd = 4, axes = FALSE, xlab = "", ylab = "",
       xaxs = "i", yaxs = "i", ylim = ylim)
}, type = "png", width = 100, height = 100, bg = NA)
# draw map and make the map able to change the size
map_glyph_timeseries <- make_glyphs_draw(data = list(timeseries_glyph_commonscale), 
draw_fun = function(glyph_i){
  # draw map background
  map("world", xlim = c(min(long)-interval_x/2, max(long)+interval_x/2),
      ylim = c(min(lat)-interval_y/2, max(lat)+interval_y/2), border = FALSE,
      col=adjustcolor("grey70", alpha.f = 0.7), fill=TRUE, 
      bg = "grey90", myborder = 0.001)
  map.axes(cex.axis = 1.5, mgp = c(3, 0.5, 0)) # add map axis
  grid(nx = length(nasa$dims$long), col = "white", lty = 1, lwd = 2) # add grids
  plot_glyphs(x, glyphs = glyph_i, add = TRUE) # plot glyphs
}, mar = rep(0,4), width = 1500, height = 1500)
plot(0,type='n', xlim=c(0, 1), ylim=c(0, 1), axes = FALSE, xlab = "", ylab = "")
title("Time series glyphs plot\n(common scaling)", line = 0.1, cex.main = 0.8)
rasterImage(map_glyph_timeseries[[1]], 0, 0, 1, 1)

