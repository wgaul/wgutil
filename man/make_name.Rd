\name{make_name}
\alias{make_name}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create Names
}
\description{
Create a name for a single individual entitie by assigning a prefix and a number. This is basically a function to call 'paste()' using loop or apply. This function can be used with loops or apply to create some number of entities with a prefix indicating their type.  For example, create names for 5 simulated species with names "sp1", "sp2", ..."sp5".  Use these names for e.g. column names, ID variable values, or object names.
}
\usage{
make_name(number, prefix)
}
\arguments{
  \item{number}{ n integer indexing individuals of the type given in 'prefix'. }
  \item{prefix}{ a character string indicating a type for the name. }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 A character string of the prefix and number pasted together.  
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Willson Gaul}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
make_name(1, "species")

# make a data frame of simulated sampling locations
site_df <- data.frame(site_name = rep(NA, 10), 
                      elevation = c(round(rnorm(5, mean = 10, sd = 2)), 
                                    round(rnorm(5, mean = 200, sd = 10))))
site_df$site_name[1:5] <- mapply(make_name, 1:5, MoreArgs = list(prefix = "swamp"))
site_df$site_name[6:10] <- mapply(make_name, 1:5, 
                                  MoreArgs = list(prefix = "mountain"))

## The function is currently defined as
function (number, prefix) 
{
    name <- paste(prefix, number, sep = "")
    name
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
