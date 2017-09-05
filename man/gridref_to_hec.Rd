\name{gridref_to_hec}
\alias{gridref_to_hec}
\title{
  Function to convert a grid ref (letter form) of any precision to hectad (3 characters)
}
\description{
  This function takes an Irish or British Grid reference (letter form) of any precision and returns a grid reference at hectad precision.  So, the input grid ref can have any number of digits (e.g. "B161415"), and this function will produce the corresponding grid reference with only 2 digits, one each for eastings and northings (e.g. "B14").
}
\usage{
gridref_to_hec(gr)
}
\arguments{
  \item{gr}{
    A character vector giving the original Irish or British grid reference (letter form) (e.g. "B161415").
}
}
\details{
  Extra digits are removed by string matching using \code{\link{grep}} and \link{regular_expression}.
}
\value{
  gridref_to_hec(gr) returns a character string with the letter(s) of the original grid reference, followed by a single digit for each of eastings and northings.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Willson Gaul
}
\note{
%%  ~~further notes~~
}

\seealso{
\code{\link{grep}}, \code{\link{regular_expression}}
}
\examples{
ref <- "B161415"
hectad <- gridref_to_hec(ref)

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
