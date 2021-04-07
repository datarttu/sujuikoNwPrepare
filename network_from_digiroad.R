#' WORK IN PROGRESS
#'
#' Goals:
#' - Fetch data from Digiroad WFS, constrained by KUNTAKOODI
#'
#' Now:
#' - Uses local Digiroad dump from gpkg

library(sf)
library(lwgeom)
library(dplyr)

# Koko alueen dump
dr_all <- st_read(dsn = 'data/sujuiko_nw_prepared.gpkg',
                  layer = 'dr_hkiespvankau_2021-04-01')

# Käsin tehty poiminta, josta puuttuu kenttiä
dr_manual <- st_read(dsn = 'data/sujuiko_nw_prepared.gpkg',
                     layer = 'sujuiko_digiroad_irrotus')

dr_filt <- dr_all %>%
  filter(LINK_ID %in% dr_manual$LINK_ID)

dr_filt$geom <- if_else(dr_filt$AJOSUUNTA == 3, st_reverse(dr_filt$geom), dr_filt$geom)

# Muotoillaan sujuiko-kannalle sopivaksi
dr_out <- dr_filt %>%
  # select(link_id = LINK_ID,
  #        i_node = ALKUSOLMU,
  #        j_node = LOPPUSOLMU,
  #        oneway = AJOSUUNTA,
  #        link_label = TIENIMI_SU,
  #        geom = geom) %>%
  mutate(link_id = as.integer(LINK_ID),
         i_node = as.integer(case_when(
           AJOSUUNTA == 3 ~ LOPPUSOLMU,
           TRUE ~ ALKUSOLMU
         )),
         j_node = as.integer(case_when(
           AJOSUUNTA == 3 ~ ALKUSOLMU,
           TRUE ~ LOPPUSOLMU
         )),
         oneway = case_when(
           AJOSUUNTA == 2 ~ FALSE,
           AJOSUUNTA == 3 ~ TRUE,
           AJOSUUNTA == 4 ~ TRUE,
           TRUE ~ NA
         ),
         link_label = TIENIMI_SU,
         data_source = 'Digiroad',
         source_date = '2021-04-01',
         geom = if_else(AJOSUUNTA == 3, st_cast(st_reverse(geom), 'GEOMETRY'), st_cast(geom, 'GEOMETRY'))) %>%
  select(link_id, i_node, j_node, oneway, link_label, data_source, source_date, geom) %>%
  mutate(geom = st_cast(geom, 'LINESTRING'))

dr_out
dr_out %>%
  st_drop_geometry() %>%
  group_by(oneway) %>%
  summarise(n = n())

# st_write(dr_out, dsn = 'data/sujuiko_nw_prepared.gpkg',
#          layer = 'nw_links_prepared',
#          delete_layer = TRUE)

#' NODET
#'
#' - Muodostetaan linkkien alku- ja loppusolmuista
#' - Noden pitää olla yksikäsitteinen, eli yhdessä sijainnissa yksi node_id
#' - Jos näin ei ole, poistetaan kyseiset nodet, korvataan ne uudella nodella node_id 1 ...
#' - Korvaukset pitää tehdä myös linkkeihin
#' - Luodaan lisäksi nodet linkeille, joilta puuttuu alku- tai loppusolmu

nodes <- dr_out %>%
  mutate(geom = st_startpoint(geom)) %>%
  select(node_id = i_node, geom) %>%
  rbind(
    dr_out %>%
      mutate(geom = st_endpoint(geom)) %>%
      select(node_id = j_node, geom)
  ) %>%
  distinct()

duplicated_nodes <- nodes %>%
  # st_drop_geometry() %>%
  group_by(node_id) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(-n, node_id)
