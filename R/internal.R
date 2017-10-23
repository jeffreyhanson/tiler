#' Validate zoom data
#'
#' This function checks that zoom data are specified in a correct format.
#'
#' @param x \code{\link{character}} object.
#'
#' @return \code{\link{logical}} is it valid?
#'
#' @examples
#' # valid inputs
#' is_valid_zoom("10")
#' is_valid_zoom("1-10")
#'
#' # invalid inputs
#' is_valid_zoom("A")
#' is_valid_zoom("10.1")
#' is_valid_zoom("1-10.1")
#'
#' @noRd
is_valid_zoom <- function(x) {
  assertthat::assert_that(assertthat::is.string(x))
  if (grepl("-", x))
   x <- strsplit(x, "-")[[1]]
  for (i in seq_along(x)) {
    if (suppressWarnings(is.na(as.integer(x[i]))))
      return(FALSE)
    if (as.character(suppressWarnings(as.integer(x[i]))) != x[i])
      return(FALSE)
  }
  return(TRUE)
}

#' Color stack
#'
#' Create a \code{\link[raster]{RasterStack-class}} object containing color
#' values that correspond to data in a  \code{\link[raster]{RasterLayer-class}}
#' object.
#'
#' @param x \code{\link[raster]{RasterLayer-class}} object.
#'
#' @param colors The color palette (see \code{\link{colorNumeric}}) or function
#'   to use to color the raster values (hint: if providing a function, set
#'   \code{na.color} to \code{"#00000000"} to make \code{NA} areas transparent).
#'   Defaults to \code{"Spectral"}.
#'
#' @param opacity The base opacity of the raster, expressed from 0 to 1.
#'   Defaults to 1.
#'
#' @return \code{\link[raster]{RasterStack-class}} object with three bands. The
#'   bands correspond to red, green, blue, and alpha intensities.
#'
#' @examples
#' # load example data
#' data(netherlands, package = "mapmisc")
#'
#' # plot data
#' plot(nldElev, main = "elevation")
#'
#' # create color stack
#' nldElev2 <- color_stack(nldElev, "Spectral", 1)
#'
#' # plot color stack
#' plotRGB(nldElev2, main = "elevation")
#'
#' @noRd
color_stack <- function(x, colors = "Spectral", opacity = 1) {
  assertthat::assert_that(inherits(x, "RasterLayer"), raster::nlayers(x) == 1,
                          assertthat::is.string(colors) ||
                          inherits(colors, "function"),
                          assertthat::is.scalar(opacity),
                          opacity >= 0, opacity <= 1)
  ## convert colors to function if a character
  if (!is.function(colors))
    colors <- leaflet::colorNumeric(colors,
                                    domain = c(raster::cellStats(x, "min"),
                                               raster::cellStats(x, "max")),
                                    na.color = "#00000000", alpha = TRUE)
  ## apply color scheme
  # create empty raster data
  red <- suppressWarnings(raster::setValues(x, NA_real_))
  green <- suppressWarnings(raster::setValues(x, NA_real_))
  blue <- suppressWarnings(raster::setValues(x, NA_real_))
  alpha <- suppressWarnings(raster::setValues(x, NA_real_))
  # start writing data
  if (canProcessInMemory(x, 5)) {
    v <- grDevices::col2rgb(colors(raster::values(x)), alpha = TRUE)
    red <- raster::setValues(red, v[1, ])
    green <- raster::setValues(green, v[2, ])
    blue <- raster::setValues(blue, v[3, ])
    alpha <- raster::setValues(alpha, v[4, ])
  } else {
    f1 <- raster::rasterTmpFile()
    f2 <- raster::rasterTmpFile()
    f3 <- raster::rasterTmpFile()
    f4 <- raster::rasterTmpFile()
    red <- writeStart(red, f1)
    green <- writeStart(green, f2)
    blue <- writeStart(blue, f3)
    alpha <- writeStart(alpha, f4)

    # process data in chunks
    bs <- blockSize(x)
    for (i in seq_len(bs$n)) {
      v <- getValues(x, row = bs$row[i], nrows = bs$nrows[i] )
      v <- grDevices::col2rgb(colors(v), alpha = TRUE)
      red <- raster::writeValues(red, v[1, ], bs$row[i])
      green <- raster::writeValues(green, v[2, ], bs$row[i])
      blue <- raster::writeValues(blue, v[3, ], bs$row[i])
      alpha <- raster::writeValues(alpha, v[4, ], bs$row[i])
    }
    # finish writing files
    red <- raster::writeStop(red)
    green <- raster::writeStop(green)
    blue <- raster::writeStop(blue)
    alpha <- raster::writeStop(alpha)
    red <- raster::raster(f1)
    green <- raster::raster(f2)
    blue <- raster::raster(f3)
    alpha <- raster::raster(f4)
  }
  ## create raster stack
  out <- raster::stack(red, green, blue, alpha)
  names(out) <- c("red", "green", "blue", "alpha")
  return(out)
}
