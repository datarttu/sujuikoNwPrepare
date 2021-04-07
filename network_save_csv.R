#' Save network links and nodes as csv files
#'
#' Reads `LINK_LAYER` and `NODE_LAYER` layers from `GPKG_PATH` Geopackage
#' and saves them to `LINK_CSV` and `NODE_CSV` csv files for database import.
#' Geometries are serialized as PostGIS-compatible WKB.
#'
#' Usage:
#'
#' Rscript network_make_nodes.R <GPKG_PATH> <LINK_LAYER> <LINK_CSV> <NODE_LAYER> <NODE_CSV>

library(dplyr)
library(readr)
library(sf)
library(lwgeom)

args <- commandArgs(trailingOnly = TRUE)

stopifnot(length(args) >= 5)

GPKG_PATH <- args[1]
LINK_LAYER <- args[2]
LINK_CSV <- args[3]
NODE_LAYER <- args[4]
NODE_CSV <- args[5]

links <- st_read(dsn = GPKG_PATH, layer = LINK_LAYER)

links %>%
  mutate(geom_wkt = st_as_text(geom)) %>%
  st_drop_geometry() %>%
  write_csv(file = LINK_CSV, na = '')
message(sprintf('Links written to %s', LINK_CSV))

nodes <- st_read(dsn = GPKG_PATH, layer = NODE_LAYER)

nodes %>%
  mutate(geom_wkt = st_as_text(geom)) %>%
  st_drop_geometry() %>%
  write_csv(file = NODE_CSV, na = '')
message(sprintf('Nodes written to %s', NODE_CSV))
