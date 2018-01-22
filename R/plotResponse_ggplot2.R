#' Plot species probability of occurrence along with the response curve for
#' each of 2 predictor variables in marginal plots.  This
#'
#' For the \code{coefs} argument, use an argument like
#' \code{coefs = list(elevation = c(0.5, 0.1, 0.3))} for a response of the form
#' logit(prob_of_occurrence) = 0.5 + 0.1*elevation + 0.3*(elevation^2).
#' Use a \code{0} for the
#' coefficient of the polynomial term to produce a straght-line response.
#'
#' @seealso my code is based on this:
#' \url{https://stackoverflow.com/questions/17370853/align-ggplot2-plots-vertically/17371177#17371177}
#'
#' @export
#'
#' @examples
#'
#' @param sp_name character string giving the species name
#' @param df data frame holding values for all predictor variables, principal
#'   components, and responses (logit(p) and probability)
#'   for each site in the raster
#' @param pred_1 character string giving column name of predictor variable 1
#' @param pred_2 character string giving column name of predictor variable 2
#' @param resp - character string giving column name of the response variable
#'   (e.g. probability of occurrence)
#' @param coefs - a named list giving a numeric vector with the coefficients
#'   for the linear predictors, including the intercept and first and second
#'   degree polynomials for each of the
#'   predictor variables. The name is the same as the name of the predictor
#'   variable, and the values are numeric coefficients.
#' @param axis1_offset - the value that is subtracted from the values in the
#'  raster of the first predictor before squaring that predictor.
#' @param axis2_offset - the value subtracted from the values in the raster of
#'  the second predictor before squaring that predictor.
#' @return This function prints to the graphics device rather than returning an
#'   object.
plotResponse_ggplot2 <- function(sp_name, df, pred_1, pred_2, resp, coefs,
                                 axis1_offset = 0, axis2_offset = 0) {
  requireNamespace("gtable", quietly = TRUE)

  warning("This function doesn't print plots with exactly aligned axes.  This
          is meant only for rough vetting of predictor-response relationships.")

  p1_col <- which(colnames(df) == pred_1)
  p2_col <- which(colnames(df) == pred_2)
  resp_col <- which(colnames(df) == resp)

  mid <- ggplot2::ggplot(data = df, aes(x = df[, p1_col],
                                        y = df[, p2_col],
                                        color = df[, resp_col])) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = colnames(df)[p1_col],
                  y = colnames(df)[p2_col],
                  color = colnames(df)[resp_col]) +
    ggplot2::ggtitle(sp_name) +
    ggplot2::theme(legend.position = "left")


  ax1_resp_plot <- ggplot2::ggplot(
    data = df,
    aes(x = df[, p1_col],
        y = logistic(
          coefs[which(names(coefs) == pred_1)][[1]][[1]] +
            coefs[which(names(coefs) == pred_1)][[1]][[2]]*df[, p1_col]+
            coefs[which(names(coefs) == pred_1)][[1]][[3]]*(
              -((df[, p1_col] + axis1_offset)^2))))) +
    ggplot2::geom_line() +
    ggplot2::labs(x = colnames(df)[p1_col],
                  y = "Probability") +
    ggplot2::theme_bw()

  ax2_resp_plot <- ggplot2::ggplot(
    data = df,
    aes(x = df[, p2_col],
        y = logistic(
      coefs[which(names(coefs) == pred_2)][[1]][[1]] +
        coefs[which(names(coefs) == pred_2)][[1]][[2]]*df[, p2_col]+
        coefs[which(names(coefs) == pred_2)][[1]][[3]]*(
          -((df[, p2_col] + axis2_offset)^2))))) +
    ggplot2::geom_line() +
    ggplot2::labs(x = colnames(df)[p2_col],
                  y = "Probability") +
    ggplot2::theme_bw() +
    coord_flip()

  mid_g <- ggplot2::ggplotGrob(mid)
  panel_id <- mid_g$layout[mid_g$layout$name == "panel", c("t", "l", "b", "r")]
  mid_g <- gtable::gtable_add_cols(mid_g, unit(1.25, "in"))

  mid_g <- gtable::gtable_add_grob(mid_g,
                                   ggplot2::ggplotGrob(ax2_resp_plot),
                                   t = panel_id$t, l = ncol(mid_g),
                                   b = panel_id$b + 4)

  mid_g <- gtable::gtable_add_rows(mid_g, unit(1.25, "in"), 0)
  mid_g <- gtable::gtable_add_grob(mid_g,
                           ggplot2::ggplotGrob(ax1_resp_plot),
                           t = 1, l = panel_id$l-3, r = panel_id$r)
  grid::grid.newpage()
  grid::grid.draw(mid_g)
}
