poly_t <- st_sfc(st_polygon(list(rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0)))),
                 crs = 4326)
points_t <- st_sfc(st_multipoint(rbind(c(.25, .5), c(.75, .5))), crs = 4326)

test_that("point.poly.dist works", {
  expect_equal(point.poly.dist(points_t, poly_t), 100135, tolerance = 1e-3)
})

test_that("point.poly.dist works with arguments", {
  expect_equal(point.poly.dist(points_t, poly_t, max = FALSE), 61895.43,
               tolerance = 1e-3)
  expect_equal(point.poly.dist(points_t, poly_t, max = FALSE,
                               by_element = TRUE)[2], 61895.43,
               tolerance = 1e-3)
})

test_that("point.poly.dist works with Spatial", {
  expect_error(point.poly.dist(as_Spatial(points_t), poly_t), NA)
  expect_error(point.poly.dist(points_t, as_Spatial(poly_t)), NA)
})

test_that("point.poly.dist works with projected CRS", {
  expect_equal(point.poly.dist(points_t, projectUTM(poly_t)), 100201.5,
               tolerance = 1e-3)
})
