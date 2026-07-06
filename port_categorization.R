# load packages
library(sf)
library(osmdata)
library(dplyr)
library(ggplot2)
library(rcrisp)
library(purrr)
library(dbscan)
library(terra)
library(lwgeom)
library(exactextractr)
library(tidyr)
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

# get port
version <- "260317_2"
result_path <- paste0("05_data_output/segment_gathered/", version, "_means.gpkg")
dsn <- paste0("05_data_output/segment_gathered/", version, "_port.gpkg")
cluster_result <- st_read(result_path)
port <- cluster_result %>%
  filter(cluster == 6) %>%
  st_union() %>%
  st_cast("POLYGON") %>%
  st_as_sf() %>%
  mutate(id = row_number(),
         area = as.numeric(st_area(x))
         )

st_write(port, dsn, delete_layer = TRUE)


# industrial landuse mismatch
version <- "260317_2"
result_path <- paste0("05_data_output/segment_gathered/", version, "_means.gpkg")
dsn <- paste0("05_data_output/segment_gathered/", version, "_port_mismatched.gpkg")

city_list <- c("Alblasserdam", "Arnhem", "Breda", "Dordrecht", "Eindhoven",
               "Geertruidenberg", "Gennep", "Gorinchem", "Helmond", "Hendrik-Ido-Ambacht", "Hertogenbosch",
               "Meierijstad", "Nieuwegein", "Nijmegen", "Oosterhout", "Oss", "Papendrecht", "Ridderkerk",
               "Roermond", "Roosendaal", "Sittard-Geleen", "Sliedrecht", "Tiel", "Tilburg", "Utrecht",
               "Venlo", "Vijfheerenlanden", "Waalwijk", "Wageningen", "Zwijndrecht")
all_industrial <- list()
for (city_name in city_list) {
  file_path <- paste0("05_data_output/industrial_landuse/", city_name, ".gpkg")
  x <- st_read(file_path, layer = "cleaned")
  all_industrial[[city_name]] <- x
}
all_industrial <- bind_rows(all_industrial)
all_industrial <- all_industrial %>%
  st_cast("POLYGON")
segment <- st_read(result_path)
segment_filtered <- segment %>%
  filter(!cluster %in% c(2, 6)) %>%
  filter(!is.na(cluster))
segment_filtered <- segment_filtered %>%
  mutate(seg_area = as.numeric(st_area(.)))
seg_ind_intersection <- st_intersection(segment_filtered, all_industrial)
ind_area_tbl <- seg_ind_intersection %>%
  mutate(ind_area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(ind_area = sum(ind_area))
segment_ratio <- segment_filtered %>%
  left_join(ind_area_tbl, by = "id") %>%
  mutate(
    ind_area = tidyr::replace_na(ind_area, 0),
    ind_ratio = ind_area / seg_area
  )
segments_industrial50 <- segment_ratio %>%
  filter(ind_ratio > 0.5)
st_write(segments_industrial50, dsn, delete_layer = TRUE)


# residential and leisure landuse mismatch
path_1 <- "01_data/02_nl_topography/OSM/gelderland/gis_osm_landuse_a_free_1.shp"
path_2 <- "01_data/02_nl_topography/OSM/limburg/gis_osm_landuse_a_free_1.shp"
path_3 <- "01_data/02_nl_topography/OSM/noord-brabant/gis_osm_landuse_a_free_1.shp"
path_4 <- "01_data/02_nl_topography/OSM/utrecht/gis_osm_landuse_a_free_1.shp"
path_5 <- "01_data/02_nl_topography/OSM/zuid-holland/gis_osm_landuse_a_free_1.shp"
gelderland <- st_read(path_1) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry)
limburg <- st_read(path_2) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry)
noord_brabant <- st_read(path_3) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry)
utrecht <- st_read(path_4) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry)
zuid_holland <- st_read(path_5) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry)
all <- bind_rows(gelderland, limburg, noord_brabant, utrecht, zuid_holland)
unique(all$fclass)

selected <- c("park", "allotments", "recreation_ground", "retail", "commercial", 
              "residential", "nature_reserve", "forest")
public <- all %>%
  filter(fclass %in% selected)
public_union <- public %>%
  st_union() %>%
  st_cast("POLYGON") %>%
  st_as_sf()

version <- "260317_2"
result_path <- paste0("05_data_output/segment_gathered/", version, "_means.gpkg")
dsn_2 <- paste0("05_data_output/segment_gathered/", version, "_urban_mismatched.gpkg")

segment <- st_read(result_path)
segment_filtered <- segment %>%
  filter(cluster %in% c(2, 6))
segment_filtered <- segment_filtered %>%
  mutate(seg_area = as.numeric(st_area(.)))
seg_intersection <- st_intersection(segment_filtered, public_union)
urban_area_tbl <- seg_intersection %>%
  mutate(urban_area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(urban_area = sum(urban_area))
segment_urban_ratio <- segment_filtered %>%
  left_join(urban_area_tbl, by = "id") %>%
  mutate(
    urban_area = tidyr::replace_na(urban_area, 0),
    urban_ratio = urban_area / seg_area
  )
segments_urban50 <- segment_urban_ratio %>%
  filter(urban_ratio > 0.5)
st_write(segments_urban50, dsn_2, delete_layer = TRUE)


# get NOVEX intersecting segments
version <- "260317_2"
result_path <- paste0("05_data_output/segment_gathered/", version, "_means.gpkg")
dsn_3 <- paste0("05_data_output/segment_gathered/", version, "_novex.gpkg")
novex_path <- "01_data/02_nl_topography/pdok/novex.gpkg"
novex <- st_read(novex_path)
segment <- st_read(result_path)
segment_filtered <- segment %>%
  filter(cluster %in% c(2, 5, 6)) %>%
  filter(!is.na(cluster))
segments_novex <- segment_filtered %>%
  filter(lengths(st_intersects(., novex)) > 0)
st_write(segments_novex, dsn_3, delete_layer = TRUE)
