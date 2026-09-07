test_that("utm_epsg returns correct northern hemisphere zone", {
  # London: zone 30N
  expect_equal(utm_epsg(-0.1, 51.5), 32630)
})

test_that("utm_epsg returns correct southern hemisphere zone", {
  # Near the AOI encountered during development, Bolivia: zone 20S
  expect_equal(utm_epsg(-65.06, -10.08), 32720)
})

test_that("utm_epsg rejects out-of-range coordinates", {
  expect_error(utm_epsg(200, 0))
  expect_error(utm_epsg(0, 100))
})

test_that("calc_gfc_tiles resolves an AOI straddling a 10-degree boundary to the correct single tile", {
  aoi <- sf::st_as_sf(
    sf::st_as_sfc(sf::st_bbox(c(xmin = -65.1577, ymin = -10.1504,
                                 xmax = -64.9708, ymax = -10.0065),
                               crs = 4326))
  )
  tiles <- calc_gfc_tiles(aoi)
  bb <- sf::st_bbox(tiles)
  expect_equal(as.numeric(bb["ymin"]), -20)
  expect_equal(as.numeric(bb["ymax"]), -10)
  expect_equal(as.numeric(bb["xmin"]), -70)
  expect_equal(as.numeric(bb["xmax"]), -60)
})
