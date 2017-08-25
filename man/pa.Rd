\name{pa}
\alias{pa}
\title{
Convert abundance observations to presence / absence
}
\description{
Converts abundance values greater than 0 to 1 for use in presence / absence applications.
}
\usage{
pa(x)
}
\arguments{
  \item{x}{ a data frame or vector with numeric values indicating abundance or some other measure of occurrence.
}
}
\details{}
\value{
 An object of the same kind as 'x' with all values greater than '0' transformed to '1'.
}
\references{
}
\author{
Willson Gaul
}
\note{}
\seealso{
}
\examples{
 df <- data.frame(species = c("rabbit", "cat", "gnu"), abundance = c(30, 0, 1))
 df[, 2] <- pa(df[, 2])
}

