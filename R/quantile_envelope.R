#' Calculate quantile envelopes
#'
#' Calculate the pointwise quantile envelopes for comparison to observed
#' data.
#'
#' TODO: figure out difference b/t pointwise and complete envelopes
#'
#' TODO: define a plot.env.envelope method
#'
#' @seealso \code{\link[ggplot2]{geom_density}}
#'
#' @export
#'
#' @examples
#' # make some example data
#' obs <- c(1:5, 7:8, 11:12, 19:25)
#' dat <- data.frame(observed = obs, s1 = sample(obs, replace = T),
#'                    s2 = sample(obs, replace = T),
#'                    s3 = sample(obs, replace = T),
#'                    s4 = sample(obs, replace = T),
#'                    s5 = sample(obs, replace = T))
#' # calculate the pointwise 95% quantile envelopes
#' result <- quantile_envelope(x = dat, n.eval.points = 20,
#'                             upper = 0.975, lower = 0.025)
#' plot(result$observed ~ result$x, type = "l", col = "black",
#' ylim = c(min(result$low_bnd), max(result$hi_bnd)),
#' main = "Quantile Envelope", ylab = "Density")
#' lines(result$low_bnd ~ result$x, col = "blue")
#' lines(result$hi_bnd ~ result$x, col = "blue")
#'
#' @importFrom magrittr "%>%"
#'
#' @param x a dataframe with the observed values (column must be named
#'   "observed") and a column for each replicate to be used for quantile
#'   calculation
#' @param upper a numeric value giving the desired upper quantile boundary (0
#'   to 1)
#' @param lower a numeric value giving the desired lower quantile boundary (0
#'   to 1)
#' @param n.eval.points integer giving the number of points at which to compute
#'   pointwise quantiles.  This is passed to \code{\link[ggplot2]{geom_density}}.
#' @param type character vector indicating whether to produce envelope values
#'   as a density ("density") or expected value ("count")
#' @return a data frame with three columns holding the
#'   observed values, the upper envelope values, and the lower envelope values
quantile_envelope <- function(x, upper = 0.975, lower = 0.025,
                                  n.eval.points, type = "density") {
  requireNamespace("tidyverse", quietly = TRUE)

  browser()

  if(!("observed" %in% colnames(x))) stop("x must include a column named 'observed' and containing the observed values.")
  if(colnames(x)[1] != "observed") {
    x <- x[, c(which(colnames(x) == "observed"),
               which(colnames(x) != "observed"))]
  }

  # gather all datasets (observed and simulated/resampled) for use in ggplot2
  long_x <- tidyr::gather(x, key = "dataset", value = "value")

  ## calculate probability density functions for all datasets using ggplot2
  get_dens <- function(x, n.eval.points = n.eval.points) {
    gg <- ggplot2::ggplot(data = x, ggplot2::aes(value, group = dataset)) +
      ggplot2::geom_density(n = n.eval.points)
    ggplot2::ggplot_build(gg) # return ggplot object including points used for plot
  }

  gg_dens <- get_dens(long_x, n.eval.points = n.eval.points)

  if(type == "density") {
    result_df <- dplyr::select(gg_dens$data[[1]],
                               c(x, density, group)) %>%
      tidyr::spread(key = group, value = density)
  }
  if(type == "count") {
    result_df <- dplyr::select(gg_dens$data[[1]],
                               c(x, count, group)) %>%
      tidyr::spread(key = group, value = count)
  }

  # calculate quantiles
  colnames(result_df)[which(colnames(result_df) == "1")] <- "observed"
  result_df$low_bnd <- apply(
    result_df[, -(which(colnames(result_df) %in% c("x", "observed")))],
    MARGIN = 1, FUN = quantile,
    probs = lower)
  result_df$hi_bnd <- apply(
    result_df[, -(which(colnames(result_df) %in% c("x", "observed")))],
    MARGIN = 1, FUN = quantile,
    probs = upper)

  result_df <- dplyr::select(result_df, c(x, observed, low_bnd, hi_bnd))
  result_df
}
