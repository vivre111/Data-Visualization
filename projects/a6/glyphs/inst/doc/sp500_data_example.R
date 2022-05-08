## ----setup, warning=FALSE, message=FALSE, error=FALSE-------------------------
library(glyphs)
library(jpeg)

## -----------------------------------------------------------------------------
library(qrmdata)
data("SP500_const") # load the constituents data from qrmdata
time <- c("2007-01-03", "2009-12-31") # specify time period
data_sp500 <- SP500_const[paste0(time, collapse = "/"),] # grab out data

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(head(data_sp500[,1:7], 7))

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(head(SP500_const_info, 7))

## -----------------------------------------------------------------------------
data_complete <- list() # complete data
for (i in 1:ncol(data_sp500)){
 data_complete[[i]] <- as.vector(data_sp500[,i])
}
x <- t(na.omit(t(data_sp500))) # omit the missing data
data_omitNA <- split(x,col(x)) # split the data into list
str(data_omitNA[1:3]) # present the first three stocks indices in the data list

## ---- fig.show='hold'---------------------------------------------------------
par(mar = rep(1.5,4))
width=c(1,5,1,12,1) # set the width
height=c(1,1,4,1,3) # set the height
# complete data
glyph_complete <- make_glyphs(data = data_complete[1:9], glyph_type = "rectangle",
                              width = width, height = height, origin = "mean")
x <- getGridXY(length(glyph_complete)) # get the coordinates
plot_glyphs(x, glyphs = glyph_complete, axes = FALSE, xlab = "", ylab = "",
            glyphWidth = 0.8, glyphHeight = 0.6,
            main = "First 9 stocks of complete data", cex.main = 0.8)
# omit missing data
glyph_Nomissing <- make_glyphs(data = data_omitNA[1:9], glyph_type = "rectangle",
                               width = width, height = height, origin = "mean")
x <- getGridXY(length(glyph_Nomissing)) # get the coordinates
plot_glyphs(x, glyphs = glyph_Nomissing, axes = FALSE, xlab = "", ylab = "",
            glyphWidth = 0.8, glyphHeight = 0.6,
            main = "First 9 stocks of no missing data", cex.main = 0.8)

## ---- fig.show='hold'---------------------------------------------------------
par(mar = rep(1.5,4))
library(colorspace)
cols <- rev(diverge_hcl(21)) # diverge color from blue to red
# complete data
glyph_complete <- make_glyphs(data = data_complete[1:9], glyph_type = "rectangle", cols = cols,
                              width = width, height = height, origin = "mean")
x <- getGridXY(length(glyph_complete)) # get the coordinates
plot_glyphs(x, glyphs = glyph_complete, axes = FALSE, xlab = "", ylab = "",
            glyphWidth = 0.8, glyphHeight = 0.6,
            main = "First 9 stocks of complete data", cex.main = 0.8)
# omit missing data
glyph_Nomissing <- make_glyphs(data = data_omitNA[1:9], glyph_type = "rectangle", cols = cols,
                               width = width, height = height, origin = "mean")
x <- getGridXY(length(glyph_Nomissing)) # get the coordinates
plot_glyphs(x, glyphs = glyph_Nomissing, axes = FALSE, xlab = "", ylab = "",
            glyphWidth = 0.8, glyphHeight = 0.6,
            main = "First 9 stocks of no missing data", cex.main = 0.8)

## ---- fig.show='hold'---------------------------------------------------------
par(mar = rep(1.5,4))
# get the average stock indices for each day
data_average <- list(Reduce("+", data_omitNA) / length(data_omitNA))
# make glyph
average_glyph <- make_glyphs(data = data_average, glyph_type = "rectangle", cols = cols,
                             width = width, height = height)
x <- getGridXY(length(average_glyph)) # get x and y coordinates to plot
# plot it
plot_glyphs(x, glyphs = average_glyph, axes = FALSE, xlab = "", ylab = "",
            main = "Average stock indices", cex.main = 0.8)
# color mapping plot
pal <- function(col, border = "light gray", ...)
{
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "", ...)
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}
pal(cols, main = "color mapping", cex.main = 0.8)

## -----------------------------------------------------------------------------
bank_loc <- which(SP500_const_info$Subsector == "Banks") # get the stock location
as.vector(SP500_const_info[bank_loc,]$Ticker) # stock ticker

## ---- fig.width=5, fig.height=5, fig.align='center'---------------------------
par(mar = rep(1.5,4))
data_bank <- data_complete[bank_loc] # get the bank stock data
bank_average <- Reduce("+", data_bank)/length(bank_loc) # calculate the average
data_bank[[length(bank_loc)+1]] <- bank_average # add the average to the data list
glyphs_bank <- make_glyphs(data_bank, width = width, height = height, glyph_type = "rectangle",
                           origin = "mean", cols = cols) # make glyphs
x <- getGridXY(length(glyphs_bank)) # get grid
plot_glyphs(x, glyphs = glyphs_bank, glyphWidth = 0.8, glyphHeight = 0.6,
            axes = FALSE, xlab = "", ylab = "",
            main = "Bank stock indices", cex.main = 0.8) # plot the glyphs
text(x, labels = c(as.vector(SP500_const_info[bank_loc,]$Ticker), "Average"), col = "grey30")

## -----------------------------------------------------------------------------
investment_loc <- which(SP500_const_info$Subsector == "Investment Banking & Brokerage")
as.vector(SP500_const_info[investment_loc,]$Ticker)

## ---- fig.align='center', fig.height=5, fig.width=5---------------------------
par(mar = rep(1.5,4))
data_investment <- data_complete[investment_loc] # get the bank stock data
bank_average <- Reduce("+", data_investment)/length(investment_loc) # calculate the average
data_investment[[length(investment_loc)+1]] <- bank_average # add the average to the data list
glyphs_investment <- make_glyphs(data_investment, width = width, height = height, glyph_type = "rectangle",
                           origin = "mean", cols = cols) # make glyphs
x <- getGridXY(length(glyphs_investment)) # get grid
plot_glyphs(x, glyphs = glyphs_investment, glyphWidth = 0.8, glyphHeight = 0.6,
            axes = FALSE, xlab = "", ylab = "") # plot the glyphs
title("Investment bank stock indices", line = 0, cex.main = 0.8)
text(x, labels = c(as.vector(SP500_const_info[investment_loc,]$Ticker), "Average"), col = "grey30")

