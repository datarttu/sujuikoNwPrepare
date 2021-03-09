#' Download daily stops.txt files from GTFS feeds
#'
#' NOTE: This script has side effects (downloading from web and saving to disk).
#' Review before running!
#'
#' This script tries to download daily HSL GTFS stops.txt files
#' from https://transitfeeds.com/p/helsinki-regional-transport/735
#' using a given date range, keeps just the columns we need,
#' and saves data.tables into .rds files in `data/stops/`.
#'
#' Example usage:
#' > Rscript --vanilla download_stops_from_gtfs_feeds.R 2020-09-14 2020-09-30
#'
#' Arttu K / HSL 3/2021

suppressMessages(library(readr))
suppressMessages(library(curl))

args <- commandArgs(trailingOnly = TRUE)

stopifnot(length(args) >= 2)

START_DATE <- as.Date(args[1])
END_DATE <- as.Date(args[2])
stopifnot(START_DATE <= END_DATE)

TARGET_DIR <- file.path('data', 'stops')

date_rng <- seq(START_DATE, END_DATE, 1)

#' Create the output directory silently if needed.
dir.create(TARGET_DIR)

#' Read stops.txt using a date
#'
#' Tries to read the GTFS dataset of the given date from OpenMobilityData,
#' returns NULL if the dataset does not exist (not all dates have a dataset).
#' When downloaded succesfully, only necessary columns of the dataset are kept,
#' and the given date is added as `version_date` column.
#'
#' @param version_date A Date object.
#'
#' @return A data.table of stops.
read_stops_from_date <- function(version_date) {

  url <- paste0(
    'https://openmobilitydata-data.s3-us-west-1.amazonaws.com/public/feeds/helsinki-regional-transport/735/',
    as.character(version_date, format = '%Y%m%d'),
    '/original/stops.txt'
  )

  con <- curl::curl(url)

  tryCatch({
    open(con, 'rb')
    dt <- read_csv(
      file = con,
      col_types = cols_only(
        stop_id = col_integer(),
        stop_code = col_character(),
        stop_name = col_character(),
        stop_desc = col_character(),
        stop_lat = col_double(),
        stop_lon = col_double(),
        zone_id = col_character(),
        location_type = col_double(),
        parent_station = col_double(),
        vehicle_type = col_double()
      )
    )
    dt$version_date <- version_date
    message('OK ', url)
  },
  error = function(e) {
    message('FAILED ', url)
    close(con)
    return(NULL)
  })

  close(con)
  return(dt)
}

read_and_save <- function(version_date) {
  dt <- read_stops_from_date(version_date)
  if (is.null(dt)) {
    return()
  }
  out_name <- file.path(
    TARGET_DIR,
    paste0('stops_', as.character(version_date, format = '%Y-%m-%d'), '.rds')
  )
  saveRDS(dt, file = out_name)
  message('SAVED ', out_name)
}

foo <- lapply(date_rng, read_and_save)
rm(foo)
