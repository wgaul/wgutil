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
#'
#' @param x a vector of abundances or some other numbers for which evenness is to be measured
#' @return the Simpson's evenness value for x

simpson_even <- function(x) {
  S = length(x)
  x_p = x/sum(x)
  simpsons_d = 1 / sum(sapply(x_p, function(x) {x^2}))
  E = simpsons_d / S
  E
}
