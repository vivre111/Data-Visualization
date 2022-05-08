#
#  Functions for demonstrating the effects of
#  various Areal aggregations.
#
#  The examples here have areas as functions of
#  squares in a rectangular grid.
#
#  Author:  R.W. Oldford

require(RColorBrewer)

#
#  The functions
# 

get_col <- function(mat,i,j, breaks, cols=NULL, palette="Blues") {
	if (is.null(cols)) {
		cols <- brewer.pal(length(breaks)+1, palette)}
	val <- 1
	for (b in breaks) {if (mat[i,j] > b) {
			val <- val + 1}
			}
	cols[val]
	}

col_areas <- function(matrix,
												 breaks=NULL,
												 cols=NULL, 
												 palette="Blues", 
												 ...){
	if (is.null(breaks)) {
			breaks <- unique(fivenum(matrix))}
		 	
  plot(c(0, 100*ncol(matrix)),
  			c(0, 100*nrow(matrix)), frame.plot=TRUE,
  			type="n",
  			xlab="West    <----------->    East", 
  			ylab="South   <----------->   North", axes=FALSE, ...)
  			
  nr <- nrow(matrix)
  nc <- ncol(matrix)			
	for (i in 1:nr) {
		for (j in 1:nc) {
		    rect((j-1)*100,
		         (nr-i+1)*100,
		         j*100,
		         (nr-i)*100,
		         border=NA,
		         col=get_col(matrix,i,j,breaks,cols,palette))
		         }
		       }
}
                   
AggregateByID <- function(data, ID) {
	IDs <- unique(as.vector(ID))
	total <- data
	for (c in IDs) {
		total[ID ==c] <- sum(total[ID ==c])}
	total
	}
