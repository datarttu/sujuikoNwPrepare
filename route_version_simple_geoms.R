#' Create simple line geometries of route versions
#'
#' NOTE: This script has side effects (writing to disk),
#' review before running!
#'
#' This script reads `data/route_version_stops.csv` and
#' `data/stop_versions_filtered.csv`, makes route version stop sequences
#' into line geometries and saves the result into Geopackage
#' `data/sujuiko_nw_prepared.gpkg` as layer `rtver_simple_lines`.
#'
#' NOTE: As the line geometries are only used for extracting necessary
#' street network links by hand, we only use versions nr 1 from stops.
#' This way we do not need to deal with intersections of stop and route versions (yet).
#'
#' Example usage:
#' > Rscript --vanilla route_version_simple_geoms.R
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(readr))
suppressMessages(library(sf))
options('dplyr.summarise.inform' = FALSE)

ROUTE_VER_STOP_FILE <- file.path('data', 'route_version_stops.csv')
STOP_VER_FILE <- file.path('data', 'stop_versions.csv')

rtv_stops <- read_csv(
  ROUTE_VER_STOP_FILE,
  col_types = cols(
    route = col_character(),
    direction = col_integer(),
    version_id = col_integer(),
    stop_seq = col_integer(),
    stop_id = col_integer(),
    hastus_place = col_character(),
    version_start = col_date(format = "%Y-%m-%d"),
    version_end = col_date(format = "%Y-%m-%d")
  )
)

stop_ver <- read_csv(
  STOP_VER_FILE,
  col_types = cols_only(
    stop_id = col_integer(),
    version_id = col_integer(),
    stop_lat = col_double(),
    stop_lon = col_double()
  )
) %>%
  filter(version_id == 1) %>%
  select(-version_id)

rtv_merged <- rtv_stops %>%
  left_join(stop_ver, by = 'stop_id')

rtv_merged

rtv_lines <- rtv_merged %>%
  st_as_sf(coords = c('stop_lon', 'stop_lat'), crs = 4326) %>%
  arrange(route, direction, version_id, stop_seq) %>%
  group_by(route, direction, version_id, version_start, version_end) %>%
  summarise(do_union = FALSE) %>%
  st_cast('LINESTRING') %>%
  #' Transform into TM35 coordinates
  st_transform(crs = 3067)

out_gpkg_name <- file.path('data', 'sujuiko_nw_prepared.gpkg')
out_layer <- 'rtver_simple_lines'

st_write(rtv_lines,
         dsn = out_gpkg_name,
         layer = out_layer,
         delete_layer = TRUE)
message(nrow(rtv_lines), ' route version lines written as ',
        out_layer, ' to ', out_gpkg_name)
