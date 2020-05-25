#' Simpson's, Shannon, and Camargo evenness
#'
#' Measure evenness using Simpson's, Shannon, and Camargo evenness
#'
#' This function implements the Simpson's, Shannon, and Camargo evenness metrics.
#'
#' @export
#'
#' @examples
#'
#' @param x A data frame containing latitude and longitude coordinates.
#' @param lat,lon Character string giving the names of the latitude and
#'   longitude columns.
#' @param orig_crs A character string giving the proj4string specifying the
#'   coordinate reference system that the original lat/lon coordinates are in.
#' @param precision Integer giving the desired precision (in meters) of results
#' @return a data frame with all rows and all columns of x, plus two additional
#'   columns for eastings and northings.
lat_lon_to_osi <- function(x, lat = "Latitude", lon = "Longitude",
                           orig_crs = NULL, precision = 1) {
  if (!requireNamespace("sp", quietly = T)) {
    stop("Package sp needed for this function to work.  Please install it.",
         call. = F)
  }
  if(!is.null(orig_crs)){
    orig_crs <- sp::CRS(orig_crs)
  } else stop("Please specify a valid crs.")

  sp::coordinates(x) <- x[, c(lon, lat)]
  sp::proj4string(x) <- orig_crs
  conv_points <- sp::spTransform(x, "+init=epsg:29903")
  dimnames(conv_points@coords)[[2]][which(
    dimnames(conv_points@coords)[[2]] == "Latitude")] <- "northings"
  dimnames(conv_points@coords)[[2]][which(
    dimnames(conv_points@coords)[[2]] == "Longitude")] <- "eastings"

  # truncate results to the desired precision
  conv_points@coords[, 1] <- conv_points@coords[, 1] -
    conv_points@coords[, 1]%%precision
  conv_points@coords[, 2] <- conv_points@coords[, 2] -
    conv_points@coords[, 2]%%precision

  conv_points <- as.data.frame(conv_points)
  conv_points
}
