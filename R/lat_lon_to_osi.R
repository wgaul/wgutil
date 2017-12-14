#' Convert WGS84 lat/long to OSI TM75 eastings/northings.
#'
#' Convert latitude/longitude to eastings and northings on the Irish OSI TM75
#' grid.
#'
#' This function calls \code{\link[sp]{spTransform}} to convert
#' latitude/longitude coordinates from WGS84 or some other specified CRS to the
#' Irish OSI TM75 grid.  The proj4string for the CRS of the original coordinates
#' must be specified in \code{orig_crs}. (e.g. "+init=epsg:4326" for WGS 84).
#'
#' Use \code{precision_meters} to find the hectad or 1km block in which the
#' original coordinates lie.  For example, \code{precision_meters = 1} gives
#' results precise to 1 meter, and \code{precision_meters = 10000} gives
#' the eastings/northings of the hectad in which the original coordinates lie.
#'
#' The main purpose of this function is to make this slightly more automatic for
#' Willson's PhD work by providing the destination epsg:29903 code in the
#' function.  The function also re-names the output coordinate fields to
#' 'eastings' and 'northings'.
#'
#' @seealso \code{\link[sp]{spTransform}}, \code{\link[sp]{CRS}},
#' \code{\link{rgdal}}, \code{\link[sp]{proj4string}},
#' \url{http://spatialreference.org/ref/epsg/tm75-irish-grid/}
#'
#' @export
#'
#' @examples
#' set.seed(500)
#' df <- data.frame(site = c("A", "B", "C", "D", "E"),
#'                  Latitude = rnorm(mean = 54.3, sd = 1, 5),
#'                  Longitude = rnorm(mean = -6, sd = 0.5, 5))
#' df <- lat_lon_to_osi(df, orig_crs = "+init=epsg:4326")
#' plot(df$Latitude ~ df$Longitude)
#' plot(df$northings ~ df$eastings)
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
