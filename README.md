
<!--- README.md is generated from README.Rmd. Please edit that file -->
Tiles for visualizing large spatial data
========================================

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![Travis Build Status](https://img.shields.io/travis/jeffreyhanson/tiler/master.svg?label=build)](https://travis-ci.org/jeffreyhanson/tiler) [![Coverage Status](https://codecov.io/github/jeffreyhanson/tiler/coverage.svg?branch=master)](https://codecov.io/github/jeffreyhanson/tler?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tiler)](https://CRAN.R-project.org/package=tiler)

The *tiler R* package can be used to render spatial data sets as [tiles](https://en.wikipedia.org/wiki/Tile_Map_Service). The advantage of rendering spatial data as tiles for use in web applications is that the tile format reduces the amount of data that users need to download to view the data.

Installation
------------

This package requires *Python* 2.7 to be installed on the system. Additionally, the *gdal* python library also needs to be installed. This can be achieved by running the following code in the terminal on Ubuntu 16.04.

``` bash
sudo apt-add-repository ppa:ubuntugis/ubuntugis-unstable
sudo apt-get update
sudo apt-get install gdal-bin python-gdal
```

Now, the *tiler R* package can be installed using the following *R* code:

``` r
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("jeffreyhanson/tiler")
```

Example usage
-------------

Let's use this package to create tiles for visualizing a raster data set. Although the data set used in this example is rather small, the `tiles` function can be used to create tile data for large raster data sets. This means that *shiny* applications can include large raster data sets without users needing to wait a long time to view the data.

``` r
# load packages
library(leaflet)
library(tiler)

# load example data set
data(netherlands, package = "mapmisc")

# create leaflet map and visualize data using tiles
map <- leaflet() %>%
       setView(lng = 5.75560, lat = 50.94723, zoom = 12) %>%
       addProviderTiles("Esri.WorldImagery", group = "Basemap") %>%
       addTiles(tiles(nldElev, output_dir = tempdir(), zoom = "10-16"),
                group = "Elevation",
                options = tileOptions(minZoom = 5, maxZoom = 16,
                                      tms = TRUE)) %>%
       addLayersControl(baseGroups = "Basemap", overlayGroups = "Elevation",
                        options = layersControlOptions(collapsed = FALSE))

# render map
print(map)
```

Note that we set the argument `output_dir` to a temporary directory. For real-world applications, you would set this to a persistent directory so that the tiling process is only performed once.

Citation
--------

    Hanson JO (2017) tiler: Tiles for mapping large spatial data. R package version 0.0.0.4. https://github.com/jeffreyhanson/tiler.
