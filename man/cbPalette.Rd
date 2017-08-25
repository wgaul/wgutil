\name{cbPalette}
\alias{cbPalette}
\alias{cbbPalette}
\docType{data}
\title{
Colorblind color palettes
}
\description{
 Colorblind-friendly color palettes for graphing.  From Winston Chang's Cookbook for R. 
}
\usage{
data("cbPalette")
data("cbbPalette")
}
\format{
  The format is:
 chr [1:8] "#999999" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" ...
}
\details{
cbPalette gives a colorblind-friendly color palette with grey.  
cbbPalette gives a colorblind-friendly color palette with black.
}
\source{
http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(cbPalette)

df <- data.frame(species = c(rep("dog", 3), rep("cat", 3)), 
                 weight = c(5, 10, 12, 7, 4, 5), 
                 height = c(3.2, 4.5, 4.7, 4, 3.5, 3.3))
ggplot(data = df, aes(x = height, y = weight, color = species)) + 
  geom_point() + 
  geom_line() + 
  scale_colour_manual(values=cbPalette)
}
\keyword{datasets}
