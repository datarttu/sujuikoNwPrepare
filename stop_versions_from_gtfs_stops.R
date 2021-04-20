#' Make stop versions dataset from GTFS stops
#'
#' NOTE: This script has side effects (saving to disk),
#' review before running!
#'
#' This script combines stops.txt files from consecutive days from `data/stops/`
#' such that changes in stop attributes result in a new version of a stop id,
#' and saves the resulting stop versions as csv file in `data/`.
#' If days are missing in between, stop data is assumed unchanged during that time.
#'
#' Terminals, stations and any other entries with `location_type != 0` are dropped.
#'
#' Example usage:
#' > Rscript --vanilla stop_versions_from_gtfs_stops.R
#'
#' Arttu K / HSL 3/2021

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(readr))
options('dplyr.summarise.inform' = FALSE)

#' Note that ALL .rds files are read from here.
SOURCE_DIR <- file.path('data', 'in', 'stops')
TARGET_DIR <- file.path('data', 'out')

in_files <- list.files(path = SOURCE_DIR, pattern = '.rds', full.names = TRUE)

stopifnot(length(in_files) > 0)

message(sprintf('%d input files from %s to %s',
                length(in_files),
                in_files[1],
                tail(in_files, n = 1)))

dt <- lapply(in_files, readRDS) %>%
  bind_rows()

message(sprintf('%d stop rows in total', nrow(dt)))

#' The data should look like this:
# stop_id stop_code stop_name       stop_desc     stop_lat stop_lon zone_id location_type parent_station vehicle_type version_date
#   <int> <chr>     <chr>           <chr>            <dbl>    <dbl> <chr>           <dbl>          <dbl>        <dbl> <date>
# 1 1010102 H2007     Ritarihuone     Mariankatu        60.2     25.0 A                   0             NA            3 2020-09-02
# 2 1010103 H2008     Kirkkokatu      Mariankatu 13     60.2     25.0 A                   0             NA            3 2020-09-02
# 3 1010104 H2009     Kirkkokatu      Mariankatu 12     60.2     25.0 A                   0             NA            3 2020-09-02
#   ...

dt_tidy <- dt %>%
  filter(location_type == 0) %>%
  select(stop_id, stop_code, stop_desc, stop_lat, stop_lon,
         zone_id, parent_station, vehicle_type, version_date) %>%
  #' We strip any excess whitespaces from character columns so they do not cause
  #' unnecessary data changes.
  #' We also round lat and lon coordinates to 5 decimals, which corresponds to
  #' ~ 1 m accuracy, roughly. This way tiny changes in coordinates do not cause
  #' a new data version either.
  mutate(across(c(stop_code, stop_desc, zone_id), trimws)) %>%
  mutate(across(c(stop_lat, stop_lon), round, digits = 5)) %>%
  #' We then mark the rows within each stop_id where attributes have changed
  #' with 1/0 so we get to calculate the version numbers using cumulative sum.
  group_by(stop_id) %>%
  arrange(stop_id, version_date) %>%
  mutate(
    has_changed = as.integer(
      coalesce(stop_code != lag(stop_code), FALSE) |
        coalesce(stop_desc != lag(stop_desc), FALSE) |
        coalesce(stop_lat != lag(stop_lat), FALSE) |
        coalesce(stop_lon != lag(stop_lon), FALSE) |
        coalesce(zone_id != lag(zone_id), FALSE) |
        coalesce(parent_station != lag(parent_station), FALSE) |
        coalesce(vehicle_type != lag(vehicle_type), FALSE)
    )
  ) %>%
  mutate(version_id = 1 + cumsum(has_changed))

message(sprintf('%d stop rows after filtering', nrow(dt_tidy)))

#' Finally we squeeze unique stop versions into one row per stop_id + version_id.
#' (Could use any suitable function for the attrs that are the same within a version,
#' or group by all the attributes, but first() will do.)
dt_versions <- dt_tidy %>%
  group_by(stop_id, version_id) %>%
  summarise(stop_code = first(stop_code),
            stop_desc = first(stop_desc),
            stop_lat = first(stop_lat),
            stop_lon = first(stop_lon),
            zone_id = first(zone_id),
            parent_station = first(parent_station),
            vehicle_type = first(vehicle_type),
            version_start = min(version_date),
            version_end = max(version_date)) %>%
  group_by(stop_id) %>%
  arrange(stop_id, version_id) %>%
  #' Even if there are gaps in the GTFS files, we ensure no gaps are left
  #' by extending the end dates to the start of the next version.
  #' This does not work for the last version of a stop so leave it unchanged.
  mutate(version_end = coalesce(lead(version_start) - 1, version_end)) %>%
  ungroup()

versions_summary <- dt_versions %>%
  select(version_id) %>%
  table()

message('Number of stop entries by number of versions:\n',
        paste0(capture.output(versions_summary), collapse = '\n'))

# first_date <- min(dt_versions$version_start) %>%
#   as.character(format = '%Y%m%d')
# last_date <- max(dt_versions$version_end) %>%
#   as.character(format = '%Y%m%d')
out_name <- file.path(TARGET_DIR, 'stop_versions.csv')

message(sprintf('Writing stop versions to %s', out_name))
write_csv(dt_versions, file = out_name, na = '')

