context("tiles")

test_that("tiles", {
  skip_on_cran()
  # load data
  data(netherlands, package = "mapmisc")
  # create folder to save tiles
  d <- file.path(tempdir(), "tiles")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  # make tiles
  expect_warning(p <- tiles(nldElev, d, zoom = "10-12", cache = FALSE))
})

test_that("tiles (parallel)", {
  skip_on_cran()
  # load data
  data(netherlands, package = "mapmisc")
  # create folder to save tiles
  d <- file.path(tempdir(), "tiles2")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  # make tiles
  expect_warning(p <- tiles(nldElev, d, zoom = "10-12", threads = 2,
                            cache = FALSE))
})
