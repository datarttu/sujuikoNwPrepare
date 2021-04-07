#' Make route version and route version stops files
#' from custom Hastus route csv files
#'
#' NOTE: This script has side effects (saving to disk),
#' review before running!
#'
#' This script reads all the .csv files from `data/in/routes/`,
#' constructs route versions with unique ids and validity ranges
#' as well as their respective stop sequences,
#' and saves them into `data/route_versions.csv` and `data/route_version_stops.csv`.
#'
#' The contents of a source file should look like this:
#' route;direction;stop_id;stop_name;stop_seq;hastus;hastus_seq;last;segment;valStart;valEnd
#' 1014;1;1204101;Hernesaaren laituri;1;1HERNE;1;FALSE;1HERNE;2020-09-21;2100-12-31
#' 1014;1;1204115;Pihlajasaarenkatu;2;;;FALSE;1HERNE - 1VSK;2020-09-21;2020-10-18
#' 1014;1;1204115;Pihlajasaarenkatu;2;;;FALSE;1HERNE - 1PMK;2020-10-19;2100-12-31
#' 1014;1;1204113;Henry Fordin katu;3;;;FALSE;1HERNE - 1VSK;2020-09-21;2020-10-18
#'
#' Example usage:
#' > Rscript --vanilla route_versions_from_hastus_files.R
#'
#' Arttu K / HSL 3/2021
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(readr))
options('dplyr.summarise.inform' = FALSE)

#' Note that ALL .csv files are read from here.
SOURCE_DIR <- file.path('data', 'in', 'routes')
TARGET_DIR <- file.path('data')

in_files <- list.files(path = SOURCE_DIR, pattern = '.csv', full.names = TRUE)

message(sprintf('%d input files from %s to %s',
                length(in_files),
                in_files[1],
                tail(in_files, n = 1)))

dt_raw <- lapply(in_files, read_delim,
                 delim = ';',
                 col_types = cols_only(
                   route = col_character(),
                   direction = col_integer(),
                   stop_id = col_integer(),
                   stop_seq = col_integer(),
                   hastus = col_character(),
                   valStart = col_date(format = '%Y-%m-%d'),
                   valEnd = col_date(format = '%Y-%m-%d')
                 ),
                 # The source files are made on Windows, and the Hastus places
                 # contain Scandinavian letters.
                 locale = locale(encoding = 'Windows-1252')) %>%
  bind_rows() %>%
  rename(hastus_place = hastus,
         val_start = valStart,
         val_end = valEnd)

message(sprintf('%d rows read in total', nrow(dt_raw)))

#' NOTE: There seem to be conflict cases in per-stop versions,
#' i.e., route+direction+stop_seq combinations with overlapping
#' (val_start, val_end) ranges.
#' This should be fixed in the source data, but for now we manipulate
#' those entries by capping val_end to the next val_start ordered by val_start.
dt_fixed <- dt_raw %>%
  arrange(route, direction, stop_seq, val_start) %>%
  group_by(route, direction, stop_seq) %>%
  mutate(val_end_changed = (val_end != coalesce(lead(val_start) - 1, val_end)),
         val_end = coalesce(lead(val_start) - 1, val_end)) %>%
  ungroup()

message(sprintf('%d rows where overlapping val_end has been fixed',
                nrow(dt_fixed %>% filter(val_end_changed))))

#' Validity ranges should apply to the entire route version,
#' but in the source data they only apply to each different stop row.
#' Let us fix this by decomposing entire routes to different versions
#' even if there is just one change in stops between route versions.

route_versions <- dt_fixed %>%
  select(-val_end_changed) %>%
  distinct(route, direction, val_start, val_end) %>%
  mutate(val_end = val_end + 1) %>%
  pivot_longer(cols = c(val_start, val_end),
               values_to = 'version_start') %>%
  select(-name) %>%
  distinct() %>%
  arrange(route, direction, version_start) %>%
  group_by(route, direction) %>%
  mutate(version_end = lead(version_start) - 1) %>%
  filter(!is.na(version_end)) %>%
  mutate(version_id = row_number()) %>%
  ungroup() %>%
  select(route, direction, version_id, version_start, version_end)

versions_summary <- route_versions %>%
  select(version_id) %>%
  table()

message(sprintf('%d distinct route versions in total',
                nrow(route_versions)))

message('Number of route versions by number of versions:\n',
        paste0(capture.output(versions_summary), collapse = '\n'))

dt <- dt_fixed %>%
  inner_join(route_versions, by = c('route', 'direction')) %>%
  filter(version_start >= val_start &
           version_end <= val_end) %>%
  arrange(route, direction, version_id, stop_seq)

conflicting_stop_seq <- dt %>%
  arrange(route, direction, version_id, stop_seq) %>%
  group_by(route, direction, version_id) %>%
  filter(stop_seq != row_number())

if (nrow(conflicting_stop_seq) > 0) {
  stop(sprintf('%d rows where stop_seq differs from real order number within route, direction and version_id'))
}

route_version_stops <- dt %>%
  select(route, direction, version_id, stop_seq, stop_id,
         hastus_place, version_start, version_end) %>%
  arrange(route, direction, version_id, stop_seq)

message(sprintf('%d route version stops in total',
                nrow(route_version_stops)))

rv_out_name <- file.path('data', 'route_versions.csv')
message('Writing route versions to ', rv_out_name)
write_csv(route_versions, path = rv_out_name, na = '')

rvs_out_name <- file.path('data', 'route_version_stops.csv')
message('Writing route version stops to ', rvs_out_name)
write_csv(route_version_stops, path = rvs_out_name, na = '')

