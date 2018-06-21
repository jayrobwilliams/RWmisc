#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: misc R functions         ##
## created: June 21, 2018            ##
## updated: June 21, 2018            ##
#######################################



## this function takes a raster and a set of polygons as arguments. it counts
## the number of polygons that intersect each raster cell. it can return either
## a raster with the count of the number of intersecting polygons as cell values
## or the original raster with cell values weighted by 1 / the number of
## intersecting polygons (the default behavior). cells that do not intersect
## any polygons will receive a value of NA. if the extent of the polygons is less
## than the extent of the raster, then the function will warn that it is
## cropping the raster to the polygons' extent.

## arguments:
## raster: a RasterLayer object
## polygons: a SpatialPolygons, SpatialPolygonsDataFrame, or simple feature
## collection with at least two features. the function will still work with only
## one polygon, but values will be unchanged, and the result will be equivalent
## to mask(raster, polygons).
## count: logical, indicating whether to return a raster with the count of
## polygons intersecting each cell, or a raster with original values weighted by
## 1/number of intersecting polygons.



overlap.weight <- function(raster, polygons, count = F) {
  
  ## load raster and sp
  require(sp)
  require(raster)
  
  ## load sf if polygons object is a simple feature
  if (any(class(polygons) == 'sf')) require(sf)
  
  ## create list for individual rasters
  rasters <- list()
  
  ## loop through polygons in existence in year i
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
  
  ## divide by 1/polygon count
  raster_poly_count <- 1 / raster_poly_count
  
  ## return either the weighting raster or the weighted original raster
  if (count) return(raster_poly_count) else return(raster * raster_poly_count)
  
}



###################
## end of script ##
###################