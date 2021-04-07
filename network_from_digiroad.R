#' Reformat road links from Digiroad subset for manual changes and database import.
#'
#' `DR_LINKS_SUBSET` linestring layer in `GPKG_PATH` Geopackage shall contain
#' the original Digiroad `DR_LINKKI` fields and features that are to be included
#' in the database import: delete unnecessary features first by hand.
#'
#' This script transforms the links for database import:
#'
#' - Unnecessary fields are dropped, field types are fixed and fields renamed.
#' - `i_node` and `j_node` fields are created but left empty, nodes are created later.
#' - `AJOSUUNTA` is recoded into boolean `oneway`, and if it says that traversing
#'   is allowed opposite to the digitized direction (`AJOSUUNTA == 3`),
#'   then the geometry is reversed.
#' - `link_modes` is set to `"{bus}"` (for Postgres array input).
#' - `data_source` is set to `Digiroad`.
#' - `source_date` is set to the current date.
#'
#' The result is saved to the same geopackage as layer `RESULT_LINKS`.
#'
#' After this, modify the `RESULT_LINKS` features if needed, and mark new / modified
#' links with `data_source = 'Manual'`.
#'
#' Usage:
#'
#' Rscript --vanilla network_from_digiroad.R <GPKG_PATH> <DR_LINKS_SUBSET> <RESULT_LINKS>

library(sf)
library(lwgeom)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)

stopifnot(length(args) >= 3)

GPKG_PATH <- args[1]
DR_LINKS_SUBSET <- args[2]
RESULT_LINKS <- args[3]

stopifnot(file.exists(GPKG_PATH))
avail_layers <- st_layers(GPKG_PATH)$name
stopifnot(DR_LINKS_SUBSET %in% avail_layers)
if (RESULT_LINKS %in% avail_layers) {
  stop(sprintf('%s already in the gpkg layers: delete or rename it first', RESULT_LINKS))
}

dr_orig <- st_read(dsn = GPKG_PATH, layer = DR_LINKS_SUBSET)

dr_out <- dr_orig %>%
  mutate(link_id = as.integer(LINK_ID),
         i_node = NA_integer_,
         j_node = NA_integer_,
         oneway = case_when(
           AJOSUUNTA == 2 ~ FALSE,
           AJOSUUNTA == 3 ~ TRUE,
           AJOSUUNTA == 4 ~ TRUE,
           TRUE ~ NA
         ),
         link_modes = '{"bus"}',
         link_label = TIENIMI_SU,
         data_source = 'Digiroad',
         source_date = format(Sys.Date(), '%Y-%m-%d'),
         geom = if_else(AJOSUUNTA == 3, st_cast(st_reverse(geom), 'GEOMETRY'), st_cast(geom, 'GEOMETRY'))) %>%
  select(link_id, i_node, j_node, oneway, link_modes, link_label, data_source, source_date, geom) %>%
  mutate(geom = st_cast(geom, 'LINESTRING'))

#' Warn about invalid data
dupl_link_ids <- dr_out %>%
  st_drop_geometry() %>%
  group_by(link_id) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  pull(link_id)
if (length(dupl_link_ids) > 0) {
  warning(sprintf('%d duplicate link ids: %s ...',
                  paste(head(dupl_link_ids, n = 5), collapse = ', ')))
}

na_oneways <- dr_out %>%
  st_drop_geometry() %>%
  filter(is.na(oneway)) %>%
  pull(link_id)
if (length(na_oneways) > 0) {
  warning(sprintf('%d links with NA oneway: %s ...',
                  paste(head(na_oneways, n = 5), collapse = ', ')))
}

st_write(dr_out, dsn = GPKG_PATH, layer = RESULT_LINKS)
