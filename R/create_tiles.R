#' Create tiles
#'
#' Create tiles for a spatial dataset.
#'
#' @param x \code{\link[raster]{RasterLayer-class}} object.
#'
#' @param output_dir \code{\link{character}} directory in which to save tiles.
#'
#' @param zoom \code{character} Zoom levels to render. The levels can be
#'   specified as a single number (e.g \code{"10"}) or as a range
#'   (e.g. \code{"1-10"}).
#'
#' @param verbose \code{logical} Should information be printed during
#'   processing?
#'
#' @param threads \code{numeric} Number of threads to use for processing.
#'   Defaults to 1.
#'
#' @param cache \code{logical} if tiles already exist in argument to
#'    \code{output_dir} should the processing be skipped?
#'
#' @param ... not used.
#'
#' @details This function creates a tile layer, following the
#'   \href{https://wiki.osgeo.org/wiki/Tile_Map_Service_Specification}{Tile Map Service specification}, using the object in argument to \code{x} and saves it
#'   in the argument to \code{output_dir}.
#'
#' @seealso \code{\link[leaflet]{addTiles}}.
#'
#' @return \code{character} path containing the tile map template.
#'
#' @examples
#' # load leafleat
#' library(leaflet)
#'
#' # load data
#' data(netherlands, package = "mapmisc")
#'
#' # create directory to save tiles
#' d <- file.path(tempdir(), "tiles")
#' dir.create(d, showWarnings = FALSE, recursive = TRUE)
#'
#' # create tiles
#' p <- create_tiles(nldElev, d)
#'
#' \donttest{
#' # create leaflet map
#' m <- leaflet() %>%
#'      setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>%
#'      addTiles(p)
#'
#' # render tiles in leaflet
#' print(m)
#' }
#' @export
create_tiles <- function(x, ...) UseMethod("create_tiles")

#' @rdname create_tiles
#' @method create_tiles default
#' @export
create_tiles.default <- function(x, ...)
  stop(paste0("Spatial data sets of type ", class(x)[1], "are not supported."))

#' @rdname create_tiles
#' @method create_tiles RasterLayer
#' @export
create_tiles.RasterLayer <- function(x, output_dir, zoom = "1-10",
                                     palette = "Spectral",
                                     verbose = FALSE, threads = 1,
                                     cache = TRUE) {
  ## validate input arguments
  assertthat::assert_that(inherits(x, "RasterLayer"),
                          assertthat::is.dir(output_dir),
                          assertthat::is.string(zoom),
                          assertthat::is.flag(verbose),
                          !is.na(verbose),
                          assertthat::is.count(threads),
                          threads <= parallel::detectCores(),
                          assertthat::is.flag(cache),
                          !is.na(cache))
  if (!is_valid_zoom(zoom))
    stop("argument to zoom is not correct format.")
  if (is.na(as.character(x@crs)))
    stop("argument to x must have a defined coordinate reference system.")
  if (!raster::compareCRS(x@crs, sp::CRS("+init=epsg:3857")))
    stop("argument to x must be projected to mercator (epsg:3857).")
  ## create tile data
  # create TLS template
  output_dir <- normalizePath(output_dir)
  tls <- paste0("file://", output_dir, "/tiles/{z}/{x}/{y}.png")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  # save data to disk
  path <- file.path(tempdir(), "tiles.tif")
  x <- create_color_stack(x, palette)
  raster::writeRaster(x, path, NAflag =-9999, overwrite = FALSE)
  # check if tiles are already created
  key <- unname(tools::md5sum(path))
  if (cache)
    if (file.exists(file.path(output_dir, "tiles/checksum.txt")))
      if (readLines(file.path(output_dir, "tiles/checksum.txt")) == key)
        return(tls)
  # run tile generation script
  withr::with_dir(
    output_dir,
    system(paste0("python ",
      system.file("inst/python/gdal2tiles-multiprocess.py", package = "tiler"),
      " -z ", zoom, " -a -9999 --processes=", threads, ifelse(verbose,
      "--verbose", ""), " --leaflet \"", path, "\"")))
  # delete raster data from disk
  unlink(path)
  # store md5sum in tile folder
  writeLines(key, file.path(output_dir, "checksum.txt"))
  ## return result
  return(tls)
}
