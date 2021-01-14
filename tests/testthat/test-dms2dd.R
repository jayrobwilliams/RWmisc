test_that("dms2dd works with heterogeneous input", {
  ll <- data.frame(lon = c("-122° 19' 55\"",
                           "71° 3' 32\" W"),
                   lat = c("47° 36' 22\"",
                           "42° 21' 36\" N"))
  expect_equal(dms2dd(ll[, 'lon'], ll[, 'lat'])[1,2], 47.60611, tolerance = 1e-3)
})
