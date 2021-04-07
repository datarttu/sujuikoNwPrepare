# Network preparation for sujuiko model

A collection of scripts and instructions for preparing network data for the [sujuiko database](https://github.com/datarttu/sujuikoDB).

## Requirements

- R > 4.0
  - Install packages included the `library()` calls manually, or use [renv](https://rstudio.github.io/renv/articles/renv.html) and `renv.lock` to restore a dev environment similar to the one used by the author
- QGIS > 3.16 for manual data wrangling

## Instructions

**NOTE:** Review the source code and comments in the scripts before running them!
They might contain bugs and need changes to suit your local environment.

You might want to run the scripts with `Rscript --vanilla ...` flag so the program will not ask you to save the R workspace.
The flag is left out here for simplicity.

Start by initializing the directory tree for data files:

```
mkdir data
mkdir data/in
mkdir data/out
```

GTFS source data is in WGS84 (EPSG:4326) coordinates, Digiroad uses ETRS-TM35 (EPSG:3067).
Output datasets for the database are saved in ETRS-TM35 coordinate system.

### Stop versions

This step aims to create temporal versions of transit stops, taking into account changes to stop locations, for example, that have taken place on a particular date.

There is no good "temporally versioned" stop dataset at hand, therefore the stops are downloaded from GTFS feeds separately for each date.
Try not to clutter the API with unnecessary requests, and only fetch the data for the date range you really need.

```
Rscript download_stops_from_gtfs_feeds.R 2020-09-14 2020-09-30
```

This will download and save stop datasets as `.rds` files for further scripts.
Note that the data is not available for every possible date.

Then merge the per-date stop datasets into one versioned dataset:

```
Rscript stop_versions_from_gtfs_stops.R
```

After you are ready with route versions, you can create a subset of stops only used by the route versions:

```
Rscript stops_used_by_route_versions.R data/stop_versions.csv \
  data/route_version_stops.csv \
  data/out/stop_versions_filtered.csv
```

### Route versions

Raw data files for route versions and their stop sequences are created from HSL public transport registry.
They are not openly available.

Make separate route version datasets into `data/route_versions.csv` and `data/route_version_stops.csv`:

```
Rscript route_versions_from_hastus_files.R
```

The following script creates simple stop-to-stop linestring geometries from the route versions (modify the file paths inside the script if needed):

```
Rscript route_version_simple_geoms.R
```

This will write the geometries into a Geopackage `data/sujuiko_nw_prepared.gpkg`, layer `rtver_simple_lines`.
You can then pick road network links used by buses using this layer as a visual help.

### Road network

Note that we focus on road network used by buses here.
Tram network would be available at least in OSM but is not covered here (yet).

See [Digiroad documentation](https://vayla.fi/vaylista/aineistot/digiroad/aineisto/rajapinnat) and download road link data from your target area.
You can use the WFS API or an older data dump available from the site.
Unfortunately there is no automated way to get links used by transit vehicles directly from the API, so you have to do that work e.g. in QGIS.
Save your link subset as linestring layer to `data/sujuiko_nw_prepared.gpkg`.

Let us assume you saved the layer as `dr_linkki_extract`.
Now run the script that transforms the data for manual changes before database import:

```
Rscript network_from_digiroad.R data/sujuiko_nw_prepared.gpkg dr_linkki_extract dr_link_transformed
```

Now open the `dr_link_transformed` layer in QGIS and make manual changes and additions if needed.
For new and modified links, set `data_source` to `'Manual'` so you can later see in the database which links originate from Digiroad directly and which not.
This field could indicate other data sources too, such as OSM.

When you have finished the link edits, create the node point layer and update link node references:

```
Rscript network_make_nodes.R data/sujuiko_nw_prepared.gpkg dr_link_transformed dr_node_transformed
```

*Note:* Digiroad includes nodes too (`ALKUSOLMU` and `LOPPUSOLMU`), but there are many links with missing node references, and some duplicate node ids too.
And we would have to create nodes for manually created links anyway.
This is way we create the node data set from scratch, purely based on the modified link geometries.

Review links and nodes in QGIS.
If they are valid and ready for database import, output them as csv:

```
Rscript network_save_csv.R data/sujuiko_nw_prepared.gpkg \
  dr_link_transformed data/out/nw_link.csv \
  dr_node_transformed data/out/nw_node.csv
```

`gzip` the files if you want, and copy them to a directory from which they can be read to the database.
