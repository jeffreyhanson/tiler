context("create_tiles")

test_that("make tiles", {
  # load data
  data(netherlands, package = "mapmisc")
  # create folder to save tiles
  d <- file.path(tempdir(), "tiles")
  # make tiles
  p <- create_tiles(nldEvel, d)
})
