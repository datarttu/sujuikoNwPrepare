#' Make stops from stop versions
#'
#' NOTE: This script has side effects (saving to disk),
#' review before running!
#'
#' Read date range versioned stops from `data/`
#' from each stop pick the version that is valid for the longest time,
#' create EPSG:3067 WKT geometry from lat and long,
#' and save to `data/all_stops.csv`.
#'
#' After this, you probably want to filter out the stops such that
#' only the stops required by route versions are imported to the db.
#'
#' Arttu K / HSL 4/2021

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(readr))
suppressMessages(library(lubridate))
suppressMessages(library(sf))
options('dplyr.summarise.inform' = FALSE)

STOP_VER_FILE <- file.path('data', 'stop_versions.csv')
STOPS_OUT_FILE <- file.path('data', 'all_stops.csv')

stver <- read_csv(
  file = STOP_VER_FILE,
  col_types = cols(
    stop_id = col_double(),
    version_id = col_double(),
    stop_code = col_character(),
    stop_desc = col_character(),
    stop_lat = col_double(),
    stop_lon = col_double(),
    zone_id = col_character(),
    parent_station = col_double(),
    vehicle_type = col_double(),
    version_start = col_date(format = "%Y-%m-%d"),
    version_end = col_date(format = "%Y-%m-%d")
  )
)

stops_max_dur <- stver %>%
  mutate(version_duration = int_length(interval(version_start, version_end))) %>%
  group_by(stop_id) %>%
  filter(version_duration == max(version_duration)) %>%
  filter(version_start == min(version_start)) %>%
  ungroup() %>%
  print()

#' NOTE: Defining default value for stop radius in meters here.
#' Field for Hastus place is created but not yet filled.
#' Vehicle type mapping from GTFS types to text values is only done
#' for street network modes bus and tram, for the time being.
stops_out <- stops_max_dur %>%
  st_as_sf(coords = c('stop_lon', 'stop_lat'), crs = 4326) %>%
  st_transform(crs = 3067) %>%
  mutate(stop_radius_m = 20.0,
         stop_mode = case_when(
           vehicle_type == 0 ~ 'tram',
           TRUE ~ 'bus'
         ),
         stop_place = NA_character_,
         source_date = version_start,
         geom_text = st_as_text(geometry)
  ) %>%
  st_drop_geometry() %>%
  select(stop_id, stop_radius_m, stop_mode, stop_code,
         stop_name = stop_desc, stop_place, parent_stop_id = parent_station,
         source_date, geom_text) %>%
  print()

write_csv(stops_out, file = STOPS_OUT_FILE, na = '')
