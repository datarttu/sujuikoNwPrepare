#' Make nodes for network links and update link node values
#'
#' Reads from `GPKG_PATH` Geopackage layer `LINK_LAYER` containing linestring links,
#' makes unique node point geometries based on the link end points,
#' updates `LINK_LAYER` `i_node` and `j_node` values,
#' and saves the node points to as `NODE_LAYER` to the Geopackage.
#'
#' Usage:
#'
#' Rscript network_make_nodes.R <GPKG_PATH> <LINK_LAYER> <NODE_LAYER>

library(dplyr)
library(sf)
library(lwgeom)

args <- commandArgs(trailingOnly = TRUE)

stopifnot(length(args) >= 3)

GPKG_PATH <- args[1]
LINK_LAYER <- args[2]
NODE_LAYER <- args[3]

stopifnot(file.exists(GPKG_PATH))
avail_layers <- st_layers(GPKG_PATH)$name
stopifnot(LINK_LAYER %in% avail_layers)
if (NODE_LAYER %in% avail_layers) {
  stop(sprintf('%s already in the gpkg layers: delete or rename it first', NODE_LAYER))
}

links <- st_read(dsn = GPKG_PATH, layer = LINK_LAYER) %>%
  select(link_id, oneway, link_modes, link_label, data_source,
         source_date, geom)

links_inode <- links %>%
  select(link_id, geom) %>%
  mutate(geom = st_startpoint(geom))

links_jnode <- links %>%
  select(link_id, geom) %>%
  mutate(geom = st_endpoint(geom))

nodes <- rbind(links_inode, links_jnode) %>%
  select(geom) %>%
  distinct() %>%
  # Number the nodes by X and Y order
  mutate(nd_x = st_coordinates(geom)[, 'X'], nd_y = st_coordinates(geom)[, 'Y']) %>%
  arrange(nd_x, nd_y) %>%
  mutate(node_id = row_number()) %>%
  select(node_id, geom)

links_inode_noded <- st_join(x = links_inode, y = nodes, join = st_intersects) %>%
  st_drop_geometry() %>%
  select(link_id, i_node = node_id)

links_jnode_noded <- st_join(x = links_jnode, y = nodes, join = st_intersects) %>%
  st_drop_geometry() %>%
  select(link_id, j_node = node_id)

links_out <- links %>%
  left_join(y = links_inode_noded, by = 'link_id') %>%
  left_join(y = links_jnode_noded, by = 'link_id') %>%
  select(link_id, i_node, j_node, oneway, link_modes, link_label, data_source,
         source_date, geom)

#' STOP on invalid data
links_nogeom <- st_drop_geometry(links_out)
missing_inode <- links_nogeom %>%
  filter(is.na(i_node)) %>%
  pull(link_id)
if (length(missing_inode) > 0) {
  stop(sprintf('%d links with NA i_node: %s ...',
                  length(missing_inode),
                  paste(head(missing_inode, n = 5), collapse = ', ')))
}
missing_jnode <- links_nogeom %>%
  filter(is.na(j_node)) %>%
  pull(link_id)
if (length(missing_jnode) > 0) {
  stop(sprintf('%d links with NA j_node: %s ...',
                  length(missing_jnode),
                  paste(head(missing_jnode, n = 5), collapse = ', ')))
}
i_eq_j <- links_nogeom %>%
  filter(i_node == j_node & !is.na(i_node)) %>%
  pull(link_id)
if (length(i_eq_j) > 0) {
  stop(sprintf('%d links with i_node == j_node: %s ...',
                  length(i_eq_j),
                  paste(head(i_eq_j, n = 5), collapse = ', ')))
}

st_write(nodes, dsn = GPKG_PATH, layer = NODE_LAYER)

# NOTE: The link layer gets OVERWRITTEN here
st_write(links_out, dsn = GPKG_PATH, layer = LINK_LAYER, delete_layer = TRUE)
