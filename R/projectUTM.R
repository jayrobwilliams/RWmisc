#' Project to UTM
#'
#' Project an object in latitude/longitude to UTM.
#'
#' @name projectUTM
#' @param sfo A simplefeature object in latitude-longitude CRS.
#'
#' @import sf
#'
#' @return A simplefeature object projected to UTM CRS.
#' @export
#'
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' st_crs(projectUTM(nc))

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
