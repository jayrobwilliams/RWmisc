#' Point-Polygon Distances
#'
#' Calculate the maximum or minimum possible distance from a point to the edge
#' of a given polygon.
#'
#' @name point.poly.dist
#'
#' @param poly A simplefeatures object of class polygon or multipolygon.
#' @param point A simplefeatures object of class point.
#' @param max Logical; return maximum or minimum distance?
#'
#' @import sf
#'
#' @return Maximum or minimum distance between a point and a polygon.
#' @export
#'
#' @examples
#' library(sf)
#' polys <- st_sfc(st_polygon(list(rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0)))),
#' crs = 4326)
#' points <- st_sfc(st_multipoint(rbind(c(.25, .5), c(.75, .5))), crs = 4326)
#' point.poly.dist(points, polys)

point.poly.dist <- function(poly, point, max = T) {

  ## project point using polygon CRS
  point <- st_transform(point, st_crs(poly))

  ## check to ensure polygon and point have same CRS
  if (st_crs(poly) != st_crs(point)) stop('polygon and point do not share same CRS')

  ## extract border vertices and point coordinates
  border <- st_coordinates(poly)[, 1:2]
  point <- st_coordinates(point)

  ## extract eastings and northings of all border vertices
  longs <- border[, 1]
  lats <- border[, 2]

  ## euclidean distance
  dist_fx <- function(long, lat, cap = point) {

    return(sqrt((long - cap[, 1])^2 + (lat - cap[, 2])^2))

  }

  ## calculate distance from point to every border vertex
  dists <- mapply(dist_fx, longs, lats, MoreArgs = list(point))

  ## return maximum or minimum distance from point to polygon edge
  if (max) {

    return(max(dists))

  } else {

    return(min(dists))

  }

}
