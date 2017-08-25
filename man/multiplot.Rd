\name{multiplot}
\alias{multiplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple graphs on one page (ggplot2)
}
\description{
Prints multiple ggplot 2 plots together.  This is code from Winston Chang's online "Cookbook for R". Pass plots in by the ... arguments or to 'plotlist'.
}
\usage{
multiplot(..., plotlist = NULL, file, cols = 1, layout = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ Any number of ggplot objects. }
  \item{plotlist}{ A list of ggplot objects. }
  \item{file}{ I don't know what this is and I don't think it appears anywhere in the function... }
  \item{cols}{ An integer giving the number of columns for the final layout. }
  \item{layout}{ A matrix specifying locations for each plot (see Details).  If present, 'cols' is ignored.}
}
\details{
 If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1 will go in the upper left, 2 will go in the upper right, and 3 will go all the way across the bottom.
}
\value{ Returns NULL and prints the plots as a side effect. }
\references{
Cookbook for R. Winston Chang.  http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
}
\author{
Winston Chang with modifications by Willson Gaul
}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(ggplot2)
df <- data.frame(l = 1:5, m = rep(4, 5), n = c(1,1,2,7,9))
plot_a <- ggplot(data = df, aes(x = l, y = m)) + 
  geom_point()
plot_b <- ggplot(data = df, aes(x = l, y = n)) + 
  geom_point()
plot_c <- ggplot(data = df, aes(x = m, y = n)) + 
  geom_point()
plot_d <- ggplot(data = df, aes(x = n)) + 
  geom_histogram()
multiplot(plot_a, plot_b, plot_c, plot_d, cols = 2)
multiplot(plot_a, plot_b, plot_c, plot_d, cols = 4)

## The function is currently defined as
function (..., plotlist = NULL, file, cols = 1, layout = NULL) 
{
    library(grid)
    plots <- c(list(...), plotlist)
    numPlots = length(plots)
    if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), 
            ncol = cols, nrow = ceiling(numPlots/cols))
    }
    if (numPlots == 1) {
        print(plots[[1]])
    }
    else {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), 
            ncol(layout))))
        for (i in 1:numPlots) {
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, 
                layout.pos.col = matchidx$col))
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
