context("color_stack")

test_that("valid inputs", {
  # load example data
  data(netherlands, package = "mapmisc")
  nldElev2 <- color_stack(nldElev, "Spectral")
  # tests
  expect_true(inherits(nldElev2, "RasterStack"))
  expect_equal(raster::nlayers(nldElev2), 4)
  expect_true(raster::compareRaster(nldElev, nldElev2[[1]], tolerance = 1e-5,
                                    stopiffalse = FALSE))
  expect_equal(names(nldElev2), c("red", "green", "blue", "alpha"))
  expect_true(raster::cellStats(nldElev2[[1]], "max") <= 255)
  expect_true(raster::cellStats(nldElev2[[2]], "max") <= 255)
  expect_true(raster::cellStats(nldElev2[[3]], "max") <= 255)
  expect_true(raster::cellStats(nldElev2[[4]], "max") <= 255)
})

test_that("valid inputs (fake large data)", {
  # load example data
  raster::rasterOptions(chunksize = 1000)
  data(netherlands, package = "mapmisc")
  nldElev2 <- color_stack(nldElev, "Spectral")
  # tests
  expect_true(inherits(nldElev2, "RasterStack"))
  expect_equal(raster::nlayers(nldElev2), 4)
  expect_true(raster::compareRaster(nldElev, nldElev2[[1]], tolerance = 1e-5,
                                    stopiffalse = FALSE))
  expect_equal(names(nldElev2), c("red", "green", "blue", "alpha"))
  expect_true(raster::cellStats(nldElev2[[1]], "max") <= 255)
  expect_true(raster::cellStats(nldElev2[[2]], "max") <= 255)
  expect_true(raster::cellStats(nldElev2[[3]], "max") <= 255)
  expect_true(raster::cellStats(nldElev2[[4]], "max") <= 255)
  raster::rasterOptions(chunksize = 1e+7)
})

test_that("invalid inputs", {
  # load example data
  data(netherlands, package = "mapmisc")
  # tests
  expect_error(color_stack(raster::stack(nldElev, nldElev), "Spectral"))
  expect_error(color_stack(raster::brick(nldElev, nldElev), "Spectral"))
  expect_error(color_stack(nldElev, "Spectral1"))
  expect_error(color_stack(nldElev, "Spectral", -5))
  expect_error(color_stack(nldElev, "Spectral", 3))
})
