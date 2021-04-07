#' Filter and save stop versions used by route versions
#'
#' NOTE: This script has side effects (saving to disk),
#' review before running!
#'
#' This script reads stop versions and route version stops from `data/`,
#' and saves a filtered file `stop_versions_filtered.csv`
#' with only the stop versions present in the route version stops
#' (by stop id and version validity ranges).
#' This way we do not have to deal with unnecessary stop data later.
#'
#' Example usage:
#' > Rscript --vanilla stops_used_by_route_versions.R data/stop_versions.csv data/route_version_stops.csv data/out/stop_versions_filtered.csv

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(readr))
options('dplyr.summarise.inform' = FALSE)

args <- commandArgs(trailingOnly = TRUE)

stopifnot(length(args) >= 3)

STOP_VER_FILE <- args[1]
ROUTE_VER_STOP_FILE <- args[2]
OUT_FILE <- args[3]

stopifnot(file.exists(STOP_VER_FILE))
stopifnot(file.exists(ROUTE_VER_STOP_FILE))
stopifnot(file.exists(OUT_FILE))

all_stops <- read_csv(
  STOP_VER_FILE,
  col_types = cols(
    stop_id = col_integer(),
    version_id = col_integer(),
    stop_code = col_character(),
    stop_desc = col_character(),
    stop_lat = col_double(),
    stop_lon = col_double(),
    zone_id = col_character(),
    parent_station = col_integer(),
    vehicle_type = col_integer(),
    version_start = col_date(format = "%Y-%m-%d"),
    version_end = col_date(format = "%Y-%m-%d")
  )
)

message(nrow(all_stops), ' stop versions read in total')

route_version_stops <- read_csv(
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

message(nrow(route_version_stops), ' route version stops read in total')

uniq_stops <- route_version_stops %>%
  group_by(stop_id) %>%
  summarise(first_start = min(version_start),
            last_end = max(version_end)) %>%
  ungroup()

message(nrow(uniq_stops), ' unique stop ids in route version stops')

#' FIXME: This leaves some necessary stops out for some reason... :
# filt_stops <- all_stops %>%
  # inner_join(uniq_stops, by = 'stop_id') %>%

  # filter(!(version_end < first_start | last_end < version_start)) %>%
  # select(colnames(all_stops))

filt_stops <- all_stops %>%
  filter(stop_id %in% uniq_stops$stop_id)

message(nrow(filt_stops), ' stop versions matching unique stops in route version stops')

out_name <- OUT_FILE

message('Writing filtered stops to ', out_name)
write_csv(filt_stops, path = out_name, na = '')
