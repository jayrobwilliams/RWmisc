#' Weight Raster Cells by Overlapping Polygons
#'
#' Weight raster cells by overlapping polygons to avoid overcounting when aggregating by polygons
#'
#' @param raster a RasterLayer object.
#' @param polygons a SpatialPolygons, SpatialPolygonsDataFrame, or simple feature
#' collection with at least two features. The function will still work with only
#' one polygon, but values will be unchanged, and the result will be equivalent
#' to `mask(raster, polygons)`.
#' @param count a logical indicating whether to return a raster with the count of
#' polygons intersecting each cell, or a raster with original values weighted by
#' 1/number of intersecting polygons.
#'
#' @details  This function takes a raster and a set of polygons as arguments.
#' It counts the number of polygons that intersect each raster cell. It can
#' return either a raster with the count of the number of intersecting polygons
#' as cell values or the original raster with cell values weighted by 1 / the
#' number of intersecting polygons (the default behavior). Cells that do not
#' intersect any polygons will receive a value of `NA`. If the extent of the
#' polygons is less than the extent of the raster, then the function will warn
#' that it is cropping the raster to the polygons' extent.
#'
#' @return a RasterLayer object.
#' @export
#'
#' @import raster
#' @import sp
#' @import sf
#'
#' @examples
overlap.weight <- function(raster, polygons, count = F) {

  ## create list for raster from each polygon
  rasters <- list()

  ## loop through polygons
  for (i in 1:nrow(polygons)) {

    ## get polygon j
    polygon <- polygons[i, ]

    ## crop raster to polygon
    raster_poly <- crop(raster, polygon)

    ## set all cell values to 1 to represent polygon presence
    raster_poly[] <- 1

    ## mask out cells outside polygon
    raster_poly <- mask(raster_poly, polygon)

    ## append to list
    rasters[[i]] <- raster_poly

  }

  ## set function to mosaic rasters
  rasters$fun <- sum

  ## mosaic rasters, summing overlapping cells
  raster_poly_count <- do.call(mosaic, rasters)

  ## return raster of overlapping counts
  if (count) return(raster_poly_count)

  ## divide by 1/polygon count
  raster_poly_count <- 1 / raster_poly_count

  ## return the weighted original raster
  return(raster * raster_poly_count)

}
