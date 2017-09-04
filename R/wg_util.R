###################################
## Utility functions that I frequently use
##
## author: Willson Gaul (and others as indicated)
## created: 10 July 2017
## last modified: 24 August 2017
###################################

pa <- function(x){ 
  # function to convert to presence / absence
  # ARGS: x: a dataframe or vector with numeric values to be converted to 0 or 1.
  x[x > 0] <- 1
  x
}

make_name <- function(number, prefix) {
  # function to create names by assigning a prefix and a number
  # ARGS: prefix: character string type identifier (e.g. "hec" or "site")
  #       number: individual identifier
  name <- paste(prefix, number, sep = "")
  name
}



## Multiple plot function
# from Cookbook for R
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
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
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## Grid Ref to hectad
gridref_to_hec <- function(GR = "NULL") {
  # convert a grid ref (letter form) of any precision to hectad (3 characters)
  # ARGS: GR = character string giving the grid reference (e.g. "B1641", "B161415)
  # VALUE: a character string giving the 3-character hectad grid ref (e.g. "B14")
  if(is.null(GR) || !is.character(GR)) {
    stop("Provide a character string to 'GR'.")
  }
  
  
}