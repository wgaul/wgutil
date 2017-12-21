#' Convert to presence / absence
#'
#' @export
#'
#' @param x a dataframe or vector with numeric values to be converted to 0 or 1.
pa <- function(x){
  x[x > 0] <- 1
  x
}

#' Calculate the logit of a number
#'
#'#' Calculates logit(p) according to the formula:
#' log(p / (1-p))
#'
#' @export
#'
#' @param p numeric value
#' @return the logit of p
logit <- function(p) {
  log(p / (1 - p))
}

#' Calculate logistic of a number
#'
#' Calculates logistic(x) according to the formula:
#' 1 / (1 + exp(-x))
#'
#' @export
#'
#' @param x a numeric value
#' @return the logistic of x
logistic <- function(x) {
  # ARGS: x: a linear combination of parameters (e.g. prob + coef*varValue)
  1 / (1 + exp(-x))
}

#' Create names by assigning a prefix and a number
#'
#' @export
#'
#' @examples
#' lapply(1:5, FUN = make_name, prefix = "species")
#'
#' @param suffix character string or integer to be used in suffix of name
#' @param prefix character string type identifier (e.g. "hec" or "site")
#' (integer won't work to start character string name)
make_name <- function(suffix, prefix) {
  name <- paste(prefix, suffix, sep = "")
  name
}



#' Multiple plot function from Cookbook for R
#'
#' @export
#'
#' @param ... ggplot objects can be passed in ..., or to plotlist (as a list of
#'   ggplot objects)
#' @param plotlist optional a list of ggplot objects
#' @param cols Number of columns in layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
#'
#' @seealso \url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}
#'
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
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

