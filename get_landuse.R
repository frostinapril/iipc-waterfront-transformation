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
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

city_list <- c("Alblasserdam", "Arnhem", "Breda", "Dordrecht", "Eindhoven", 
               "Geertruidenberg", "Gennep", "Gorinchem", "Helmond", "Hendrik-Ido-Ambacht", "Hertogenbosch",
               "Meierijstad", "Nieuwegein", "Nijmegen", "Oosterhout", "Oss", "Papendrecht", "Ridderkerk", 
               "Roermond", "Roosendaal", "Sittard-Geleen", "Sliedrecht", "Tiel", "Tilburg", "Utrecht", 
               "Venlo", "Vijfheerenlanden", "Waalwijk", "Wageningen", "Zwijndrecht")

# city_name <- "Zwijndrecht"
# crs <- 28992
# file_path <- paste0("05_data_output/segment_manual/", city_name, ".gpkg")
# dsn <- paste0("05_data_output/public_landuse/", city_name, ".gpkg")
# segment <- st_read(file_path)
# bb <- segment %>%
#   st_transform(4326) %>%
#   st_bbox()
# leisure <- opq(bbox = bb, timeout = 120) %>%
#   add_osm_feature(key = "leisure") %>%
#   osmdata_sf()
# leisure_sf <- leisure$osm_polygons %>%
#   st_transform(crs) %>%
#   select(osm_id, geometry)
# 
# green <- opq(bbox = bb, timeout = 120) %>%
#   add_osm_feature(key = "landuse", value = c("forest", "grass", 
#                                              "meadow", "grassland", "village_green")
#   ) %>%
#   osmdata_sf()
# green_1 <- green$osm_polygons %>%
#   st_transform(crs) %>%
#   select(osm_id, geometry)
# green_2 <- green$osm_multipolygons %>%
#   st_transform(crs) %>%
#   select(osm_id, geometry)
# green_sf <- rbind(green_1, green_2)
# 
# public_space_sf <- rbind(leisure_sf, green_sf)
# 
# public_clean <- public_space_sf %>%
#   st_make_valid() %>%
#   st_geometry() %>%
#   st_union() %>%
#   st_collection_extract("POLYGON")
# public_clean <- st_as_sf(public_clean)
# names(public_clean)[names(public_clean) == "x"] <- "geometry"
# st_geometry(public_clean) <- "geometry"
# st_write(public_clean, dsn, layer = "added", delete_layer = TRUE)

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

selected <- c("forest", "grass", "meadow", "heath", "park", "nature_reserve", "allotments", 
              "recreation_ground", "orchard", "scrub", "vineyard")

public <- all %>%
  filter(fclass %in% selected)

for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  segment <- st_read(file_path)
  dsn <- paste0("05_data_output/public_landuse/", city_name, ".gpkg")
  public_relevant <- st_intersection(public, segment)
  st_write(public_relevant, dsn, layer = "classified", delete_layer = TRUE)
  
  
  public_clean <- public_relevant %>%
      st_make_valid() %>%
      st_geometry() %>%
      st_union() %>%
      st_collection_extract("POLYGON")
  public_clean <- st_as_sf(public_clean)
  names(public_clean)[names(public_clean) == "x"] <- "geometry"
  st_geometry(public_clean) <- "geometry"
  st_write(public_clean, dsn, layer = "cleaned", delete_layer = TRUE)
}
