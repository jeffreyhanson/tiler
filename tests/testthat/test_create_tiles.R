context("create_tiles")

test_that("make tiles", {
  # load data
  data(netherlands, package = "mapmisc")
  # create folder to save tiles
  d <- file.path(tempdir(), "tiles")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  # make tiles
  nldElev <- raster::projectRaster(nldElev, crs = sp::CRS("+init=epsg:3857"))
  p <- create_tiles(nldElev, d)
})
