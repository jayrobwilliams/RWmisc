#' Simplefeature Downgrade
#'
#' Downgrade a Simplefeature Geometry Object to a DataFrame.
#'
#' @param x A simplefeature object.
#'
#' @import sf
#'
#' @return A DataFrame.
#' @export
#'
#' @examples
st_drop_geometry <- function(x) {

  if (inherits(x, 'sf')) {

    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'

  }

  return(x)
}

#' UTM Convenience Functions
#'
#' Functions for converting latitude-longitude data to UTM.
#'
#' @name UTM.functions
#' @aliases UTM.functions
NULL

long2UTM <- function(long) {

  floor((long + 180) / 6) %% 60 + 1

}

#' @rdname UTM.functions
#'
#' @param long A vector of longitude values.
#'
#' @return UTM vector of zone numbers.
#' @export
#'
#' @examples
UTMzones <- function(long) {

  unique(long2UTM(long))

}

chooseUTM <- function(long) {

  zone <- round(mean(long2UTM(long)))

  zone

}

#' @rdname UTM.functions
#'
#' @param sfo A simplefeature object in latitude-longitude CRS.
#'
#' @import sf
#'
#' @return A simplefeature object projected to UTM CRS.
#' @export
#'
#' @examples
projectUTM <- function(sfo) {

  ## determine if spatial object is a SpatialPoints or SpatialPolygons object and use either
  ## the coords or polygons slot to access the longitude(s) of the spatial object
  if (attr(st_geometry(sfo), 'class')[1] == 'sfc_POINT') {

    ## find average UTM zone using longitude(s) of SpatialPoints object
    zone <- chooseUTM(st_coordinates(sfo)[1])

    ## save latitude mean to determine if features falls in southern hemisphere
    lat.mean <- mean(st_coordinates(sfo)[2])

  }

  if (attr(st_geometry(sfo), 'class')[1] == 'sfc_MULTIPOLYGON') {

    ## find average UTM zone using longitude(s) of SpatialPolygons object
    zone <- chooseUTM(mean(st_coordinates(sfo)[, 1]))

    ## save latitude mean to determine if features falls in southern hemisphere
    lat.mean <- mean(st_coordinates(sfo)[, 2])

  }

  ## if average of latitude values is negative, add +south the coordinate reference system
  if (lat.mean >= 0) {

    ## create coordinate reference system object to project spatial object
    zone <- st_crs(paste('+proj=utm +zone=', zone, sep = ''))

  } else {

    ## create coordinate reference system object to project spatial object
    zone <- st_crs(paste('+proj=utm +south +zone=', zone, sep = ''))

  }

  ## project spatial object
  sfo <- st_transform(sfo, zone)

  ## return projected spatial object
  sfo

}


#' Point-Polygon Distances
#'
#' Calculate the maximum or minimum possible distance from a point to the edge
#' of a given polygon.
#'
#' @param poly A simplefeatures object of class polygon or multipolygon.
#' @param point A simplefeatures object of class point.
#' @param max Logical; return maximum or minimum distance?
#'
#' @return Maximum or minimum distance between a point and a polygon.
#' @export
#'
#' @examples
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
