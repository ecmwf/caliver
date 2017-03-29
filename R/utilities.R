### UTILITY FUNCTIONS

# Rotate raster from -180:180 to 0:360 extent
# Author: Skipton Woolley
# Date : Feb 2015
# Licence GPL v3
# source: https://gist.github.com/skiptoniam/b5acc7b84e37fc20d514

rotate_2_360 <- function(x, filename='', ...){
  
  # require(raster)
  
  e <- raster::extent(x)
  xrange <- e@xmax - e@xmin
  if (xrange < 350 | xrange > 370 | e@xmin < -190 | e@xmax > 190) {
    
    warning('this does not look like an appropriate object for this function, use the rotate function in raster package')
    
  }
  
  ext1 <- raster::extent(0, 180, -90, 90)
  if (is.null(raster::intersect(e, ext1))) {
    r1 <- NULL
  } else {
    r1 <- raster::crop(x, ext1)
  }		
  #ext2 <- extent(-180+res(x)[1], 0, -90, 90)
  ext2 <- raster::extent(-180, 0, -90, 90)
  if (is.null(raster::intersect(e, ext2 ))) {
    r2 <- NULL
  } else {
    r2 <- raster::crop(x, ext2)
    r2 <- raster::shift(r2, 360)
  }
  ln <- names(x)
  if (is.null(r1)) {
    out <- r2
  } else if (is.null(r2)) {
    out <- r1		
  } else {
    out <- raster::merge(r1, r2, overlap=FALSE)
  }
  names(out) <- names(x)
  out@z <- x@z
  
  p <- raster::projection(out)	
  if (length(grep("\\+over", p)) > 0) {
    raster::projection(out) <- gsub("[[:space:]]\\+over", "", p)
  }
  
  if (filename != '') {
    out <- raster::writeRaster(out, filename, ...)
  }
  return(out)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
# 
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
}
