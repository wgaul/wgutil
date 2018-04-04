#################################
## Environmental Space simulation envelopes
## 
## TODO:  - figure out difference b/t pointwise and complete envelopes
##        - define a plot.env.envelope
## 
## author: Willson Gaul
## created: 4 April 2018
## last modified: 4 April 2018
################################

## overall structure:
# - bin continuous values into n breaks/bins
# - make a column with the count of number of points in each bin
# - have a column for the observed pattern and for each simulated pattern
# - find the row-wise percentiles to find the upper and lower percentile 
#     boundaries for each bin
# - make a column/vector of those bin-wise percentile values and plot it
#

## make some test data
# obs <- c(1:5, 7:8, 11:12, 19:25)
# 
# test <- data.frame(obs = obs, s1 = sample(obs, replace = T), 
#                    s2 = sample(obs, replace = T), s3 = sample(obs, replace = T), 
#                    s4 = sample(obs, replace = T), s5 = sample(obs, replace = T))

quantile_envelope <- function(x, upper = 0.975, lower = 0.025, 
                                  n.eval.points, type = "density") {
  # ARGS: x = vector of observed values
  #       upper = numeric value of desired upper quantile (0 to 1)
  #       lower = numeric value of desired lower quantile (0 to 1)
  #       n.eval.points = number of points at which to computing pointwise 
  #                       quantiles
  #       type = character indicating whether to produce envelope values and 
  #               plots as a density ("density") or expected value ("count")
  #       
  # Value: an env.envelope data frame with 3 columns - the observed environmental 
  #         values, the upper envelope values, and the lower envelope values
  
  # browser()
  

  long_x <- gather(x, key = "dataset", value = "value")
  
  ## calculate probability density functions for all datasets
  get_dens <- function(x, n.eval.points = n.eval.points) {
    gg <- ggplot(data = x, aes(value, group = dataset)) + 
      geom_density(n = n.eval.points)
    ggplot_build(gg) # return points used for plot
  }
  
  gg_dens <- get_dens(long_x, n.eval.points = n.eval.points)
  
  if(type == "density") {
    result_df <- select(gg_dens$data[[1]], c(x, density, group)) %>% 
      spread(key = group, value = density)
  }
  if(type == "count") {
    result_df <- select(gg_dens$data[[1]], c(x, count, group)) %>% 
      spread(key = group, value = count)
  }
  
  result_df$low_bnd <- apply(result_df[, -1], MARGIN = 1, FUN = quantile, 
                             probs = lower)
  result_df$hi_bnd <- apply(result_df[, -1], MARGIN = 1, FUN = quantile, 
                            probs = upper)
  colnames(result_df)[which(colnames(result_df) == "1")] <- "observed"
  
  result_df <- select(result_df, c(x, observed, low_bnd, hi_bnd))
  
  ## gather for use in ggplot
  # result_df <- select(result_df, c(x, observed, low_bnd, hi_bnd)) %>%
  #   gather(key = "dataset", value = density, observed, low_bnd, hi_bnd)
  
  class(result_df) <- "env.envelope"
  result_df
}

## for testing
# result <- environmental_envelope(x = test, n.eval.points = 20)



## in base plot
## TODO: define a plot.env.envelope method
plot(result$observed ~ result$x, type = "l", col = "black", 
     ylim = c(min(result$low_bnd), max(result$hi_bnd)), 
     main = "Quantile Envelope", ylab = "Density")
lines(result$low_bnd ~ result$x, col = "blue")
lines(result$hi_bnd ~ result$x, col = "blue")

## in ggplot2
# ggplot(result, aes(x = x, y = density, color = dataset)) + 
#   geom_line()