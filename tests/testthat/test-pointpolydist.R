test_that("point.poly.dist works", {
  polys <- st_sfc(st_polygon(list(rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0)))),
                  crs = 4326)
  points <- st_sfc(st_multipoint(rbind(c(.25, .5), c(.75, .5))), crs = 4326)
  expect_equal(point.poly.dist(points, polys), 0.9013878, tolerance = 1e-5)
})

test_that("point.poly.dist works with arguments", {
  polys <- st_sfc(st_polygon(list(rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0)))),
                  crs = 4326)
  points <- st_sfc(st_multipoint(rbind(c(.25, .5), c(.75, .5))), crs = 4326)
  expect_equal(point.poly.dist(points, polys, max = FALSE), 0.559017, tolerance = 1e-5)
})
