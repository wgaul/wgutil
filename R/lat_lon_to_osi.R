#########################################
## Function to convert WGS84 lat/long to OSI TM75 eastings/northings
##
## author: Willson Gaul
## created: 3 November 2017
## last modified: 6 Nov 2017
#########################################

#' Convert WGS84 lat/long to OSI TM75 eastings/northings.
#'
#' Convert latitude/longitude to eastings and northings on the Irish OSI TM75
#' grid.
#'
#' This function calls \code{\link[sp]{spTransform}} to convert
#' latitude/longitude coordinates from WGS84 to the Irish OSI TM75 grid.  The
#' main purpose of this function is to make this slightly more automatic for
#' Willson's PhD work by providing the destination epsg:29903 code in the
#' function.  The function also re-names the output coordinate fields to
#' 'eastings' and 'northings'.
#'
#' @seealso \code{\link[sp]{spTransform}}, \code{\link[sp]{CRS}},
#' \code{\link{rgdal}}, \code{\link[sp]{proj4string}},
#' \url{http://spatialreference.org/ref/epsg/tm75-irish-grid/}
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
#' @param orig_crs A character string giving the proj4string to pass to
#'   \code{\link[sp]{CRS}}.
lat_lon_to_osi <- function(x, lat = "Latitude", lon = "Longitude",
                           orig_crs = NULL) {


  if (!requireNamespace("sp", quietly = T)) {
    stop("Package sp needed for this function to work.  Please install it.",
         call. = F)
  }
  if(!is.null(orig_crs)){
    orig_crs <- sp::CRS(orig_crs)
  } else stop("Please specify a valid crs.")

  sp::coordinates(x) <- x[, c(lat, lon)]
  sp::proj4string(x) <- orig_crs
  conv_points <- sp::spTransform(x, "+init=epsg:29903")
  dimnames(conv_points@coords)[[2]][which(
    dimnames(conv_points@coords)[[2]] == "Latitude")] <- "northings"
  dimnames(conv_points@coords)[[2]][which(
    dimnames(conv_points@coords)[[2]] == "Longitude")] <- "eastings"
  conv_points <- as.data.frame(conv_points)
  conv_points
}
