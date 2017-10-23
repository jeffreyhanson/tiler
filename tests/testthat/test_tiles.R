context("tiles")

test_that("tiles", {
  skip_on_cran()
  # load data
  data(netherlands, package = "mapmisc")
  # create folder to save tiles
  d <- file.path(tempdir())
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  # make tiles
  expect_warning(p <- tiles(nldElev, d, zoom = "10-12", cache = FALSE))
  # tests
  expect_true(file.exists(d))
  expect_true(file.exists(file.path(d, "tiles")))
  expect_true(all(file.exists(file.path(d, "tiles", c(10:12)))))
  expect_true(file.exists(file.path(d, "tiles", "checksum.txt")))
  # cleanup
  unlink(d)
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
  # tests
  expect_true(file.exists(d))
  expect_true(file.exists(file.path(d, "tiles")))
  expect_true(all(file.exists(file.path(d, "tiles", c(10:12)))))
  expect_true(file.exists(file.path(d, "tiles", "checksum.txt")))
  # cleanup
  unlink(d)
})
