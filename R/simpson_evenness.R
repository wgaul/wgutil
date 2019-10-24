#' Simpson's evenness
#'
#' Measure evenness using Simpson's evenness
#'
#' This function implements the Simpson's evenness metric.  For a description of
#' Simpson diversity, see Magurran & McGill (2011) Box 5.1 p. 57.  "D = sum over
#' p_i^2 gave the probability that two individuals drawn at random from an
#' infinite community would belong to the same species.  ... As such, D is the
#' inverse of diversity.... The most common way of converting homogeneity into
#' diversity is D = 1/D."  Simpson evenness is scaled 0 to 1 by dividing Simpson
#' diversity by species richness (or number of hectads in my case).
#'
#' Simpson_e = (1 / sumOver(p_i)^2) / S
#'
#' where S is number of observed species (or number of hectads) and p_i is the
#' proportion of abundance for species i (p_i = N_i/N)
#'
#' @export
#'
#' @examples
#' dat <- c(1, 5, 3, 2, 2, 2, 1) # data with uneven values
#' dat2 <- c(5, 5, 4, 2, 2, 2, 1) # data with more even values
#' simpson_even(dat)
#' simpson_even(dat2) # Simpson's evenness value is higher for more even data
#'
#' # Simpson's evenness is insensitive to number of observations
#' identical(simpson_even(dat), simpson_even(c(dat, dat)))
#'
#' @references Magurran, Anne E. and Brian J. McGill.  2011.  \emph{Biological
#' Diversity: Frontiers in Measurement and Assessment}.  Oxford University Press.
#'
#'
#' @param x a vector of abundances or some other numbers for which evenness is to be measured
#' @param na.rm if TRUE, NA values will be removed before computing Simpson's evenness
#' @return the Simpson's evenness value for x

simpson_even <- function(x, na.rm = FALSE) {
  if(na.rm) x <- x[!is.na(x)]
  S = length(x)
  x_p = x/sum(x)
  simpsons_d = 1 / sum(sapply(x_p, function(x) {x^2}))
  E = simpsons_d / S
  E
}
