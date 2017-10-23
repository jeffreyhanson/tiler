#' Tiles
#'
#' Create tiles for a spatial dataset.
#'
#' @param x \code{\link[raster]{RasterLayer-class}} object.
#'
#' @param output_dir \code{\link{character}} directory in which to save tiles.
#'
#' @param colors The color palette (see \code{\link{colorNumeric}}) or function
#'   to use to color the raster values (hint: if providing a function, set
#'   \code{na.color} to \code{"#00000000"} to make \code{NA} areas transparent).
#'   Defaults to \code{"Spectral"}.
#'
#' @param zoom \code{character} Zoom levels to render. The levels can be
#'   specified as a single number (e.g \code{"10"}) or as a range
#'   (e.g. \code{"1-10"}). Defaults to \code{"5-16"}.
#'
#' @param threads \code{numeric} number of threads to use for processing.
#'   Defaults to 1.
#'
#' @param project \code{logical} If argument to \code{x} is not already in the
#'   coordinate reference system expected by Leaflet (Mercator;
#'   \code{EPSG:3857}), should the data be automatically reprojected? If
#'   argument to \code{project} is \code{FALSE}, then the user will need
#'   to ensure that the data is in the correct coordinate reference system.
#'
#' @param verbose \code{logical} Should information be printed during
#'   processing?
#'
#' @param cache \code{logical} if tiles already exist in argument to
#'    \code{output_dir} should the processing be skipped?
#'
#' @param shiny \code{logical} are the tiles being served in a shiny
#'    applications?
#' @param ... not used.
#'
#' @details This function creates a tile layer, following the
#'   \href{https://wiki.osgeo.org/wiki/Tile_Map_Service_Specification}{Tile Map Service specification}, using the object in argument to \code{x} and saves it
#'   in the argument to \code{output_dir}.
#'
#' @seealso \code{\link[leaflet]{addTiles}}.
#'
#' @return \code{character} tile scheme.
#'
#' @examples
#' # load leafleat
#' library(leaflet)
#'
#' # load example data set
#' data(netherlands, package = "mapmisc")
#'
#' \donttest{
#' # create leaflet map and visualize data using tiles
#' map <- leaflet() %>%
#'        setView(lng = 5.75560, lat = 50.94723, zoom = 12) %>%
#'        addProviderTiles("Esri.WorldImagery", group = "Basemap") %>%
#'        addTiles(tiles(nldElev, output_dir = tempdir(), zoom = "10-16"),
#'                 group = "Elevation",
#'                 options = tileOptions(minZoom = 5, maxZoom = 16,
#'                                       tms = TRUE)) %>%
#'        addLayersControl(baseGroups = "Basemap", overlayGroups = "Elevation",
#'                         options = layersControlOptions(collapsed = FALSE))
#'
#' # render map
#' print(map)
#' }
#' @export
tiles <- function(x, ...) UseMethod("tiles")

#' @rdname tiles
#' @method tiles default
#' @export
tiles.default <- function(x, ...)
  stop(paste0("Spatial data sets of type ", class(x)[1], "are not supported."))

#' @rdname tiles
#' @method tiles RasterLayer
#' @export
tiles.RasterLayer <- function(x, output_dir, colors = "Spectral",
                              zoom = "5-16", threads = 1, project = TRUE,
                              verbose = FALSE, cache = TRUE, shiny = FALSE,
                              ...) {
  ## validate input arguments
  assertthat::assert_that(inherits(x, "RasterLayer"),
                          assertthat::is.dir(output_dir),
                          assertthat::is.string(zoom),
                          assertthat::is.count(threads),
                          threads <= parallel::detectCores(),
                          assertthat::is.flag(project),
                          assertthat::is.flag(verbose),
                          !is.na(verbose),
                          assertthat::is.flag(cache),
                          !is.na(cache),
                          assertthat::is.flag(shiny),
                          !is.na(shiny))
  if (!is_valid_zoom(zoom))
    stop("argument to zoom is not correct format.")
  ## create tile data
  # reproject data if needed
  if (project)
    x <- leaflet::projectRasterForLeaflet(x)
  # create TLS template
  output_dir <- normalizePath(output_dir)
  if (!shiny) {
    tls <- paste0("file://", output_dir, "/tiles/{z}/{x}/{y}.png")
  } else {
    shiny::addResourcePath("output", output_dir)
    tls <- "/output/tiles/{z}/{x}/{y}.png"
  }
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  # create color stack
  path <- file.path(output_dir, "tiles.tif")
  s <- color_stack(x, colors)
  # extract NODATA value
  nodata <- color_stack(raster::raster(matrix(c(NA_real_ , 1), ncol = 2,
                                                               nrow = 1)))[1]
  # save data to disk
  raster::writeRaster(s, path, overwrite = TRUE, datatype = "INT1U",
                      nodata = nodata[1])
  # check if tiles are already created
  key <- unname(tools::md5sum(path))
  if (cache)
    if (file.exists(file.path(output_dir, "tiles/checksum.txt")))
      if (readLines(file.path(output_dir, "tiles/checksum.txt")) == key)
        return(tls)
  # make tiles
  cmd <- paste0("python ",
                system.file("python", "gdal2tiles.py", package = "tiler"),
                " -z ", zoom, " -a ", paste(nodata, collapse = ","),
                " -w none --processes=", threads,
                ifelse(verbose, " --verbose", ""), " \"", path, "\" \"",
                output_dir, "/tiles\"")
  res <- system(cmd)
  if (!identical(res, 0L))
    stop("tile processing failed.")
  # delete raster data from disk
  unlink(path)
  # store md5sum in tile folder
  writeLines(key, file.path(output_dir, "tiles/checksum.txt"))
  ## return result
  return(tls)
}
