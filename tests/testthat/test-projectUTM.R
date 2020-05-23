test_that("projectUTM works", {
  library(sf)
  nc <- st_read(system.file("shape/nc.shp", package="sf"))
  expect_equal(st_crs(projectUTM(nc))[["input"]], "+proj=utm +zone=17")
})
