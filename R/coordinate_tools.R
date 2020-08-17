#' Convert a Matrix of Cartesian Coordinates into Spherical Coordinates
#' @param x A 3 column matrix with x, y and z coordinates.
#' @return A 3 column matrix with radius, inclination and azimuth.
#' @export
#' @importFrom assertthat assert_that
#' @examples
#' x <- rbind(
#'   c(0, 0, 1),
#'   c(sqrt(8 / 9), 0, -1 / 3),
#'   c(-sqrt(2 / 9), sqrt(2 / 3), -1 / 3),
#'   c(-sqrt(2 / 9), -sqrt(2 / 3), -1 / 3)
#' )
#' cart2sphere(x)
cart2sphere <- function(x) {
  assert_that(inherits(x, "matrix"))
  assert_that(ncol(x) == 3)

  r <- sqrt(rowSums(x ^ 2))
  phi <- atan2(x[, 2], x[, 1])
  theta <- acos(x[, 3] / r)

  cbind(r, phi, theta)
}

#' Convert a Matrix of Spherical Coordinates into an Spatial Object
#' @param x A 3 column matrix with radius, inclination and azimuth.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom sf st_as_sf
#' @examples
#' x <- rbind(
#'   c(0, 0, 1),
#'   c(sqrt(8 / 9), 0, -1 / 3),
#'   c(-sqrt(2 / 9), sqrt(2 / 3), -1 / 3),
#'   c(-sqrt(2 / 9), -sqrt(2 / 3), -1 / 3)
#' )
#' x <- cart2sphere(x)
#' sphere2latlong(x)
sphere2latlong <- function(x) {
  assert_that(inherits(x, "matrix"))
  assert_that(ncol(x) == 3)

  ll <- data.frame(
    long = x[, 2] * 180 / pi,
    lat = 90 - x[, 3] * 180 / pi
  )
  st_as_sf(ll, coords = c("long", "lat"), crs = 4326)
}
