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
               "Geertruidenberg", "Gennep", "Gorinchem", "Helmond", "Hendrik-Ido-Ambacht",
               "Meierijstad", "Nieuwegein", "Nijmegen", "Oosterhout", "Oss", "Papendrecht", "Ridderkerk", 
               "Roermond", "Roosendaal", "Sittard-Geleen", "Sliedrecht", "Tiel", "Tilburg", "Utrecht", 
               "Venlo", "Vijfheerenlanden", "Waalwijk", "Wageningen", "Zwijndrecht")

path_1 <- "01_data/02_nl_topography/OSM/gelderland/gis_osm_roads_free_1.shp"
path_2 <- "01_data/02_nl_topography/OSM/limburg/gis_osm_roads_free_1.shp"
path_3 <- "01_data/02_nl_topography/OSM/noord-brabant/gis_osm_roads_free_1.shp"
path_4 <- "01_data/02_nl_topography/OSM/utrecht/gis_osm_roads_free_1.shp"
path_5 <- "01_data/02_nl_topography/OSM/zuid-holland/gis_osm_roads_free_1.shp"
gelderland <- st_read(path_1) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry, maxspeed)
limburg <- st_read(path_2) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry, maxspeed)
noord_brabant <- st_read(path_3) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry, maxspeed)
utrecht <- st_read(path_4) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry, maxspeed)
zuid_holland <- st_read(path_5) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry, maxspeed)
all <- bind_rows(gelderland, limburg, noord_brabant, utrecht, zuid_holland)
unique(all$fclass)


selected <- c("track_grade1", "service", "track_grade2", "track_grade4", "track_grade3", 
              "track", "residential", "unclassified", "living_street", "track_grade5", 
              "motorway", "tertiary", "secondary", "motorway_link", "trunk", "primary", "busway", 
              "primary_link", "secondary_link", "tertiary_link", "trunk_link", "unknown")


vehicle <- all %>%
  filter(fclass %in% selected)


for (city_name in city_list) {
  dsn <- paste0("05_data_output/vehicle_network/", city_name, ".gpkg")
  bb <- getbb(city_name)
  bbox <- st_bbox(c(
    xmin = bb["x", "min"],
    ymin = bb["y", "min"],
    xmax = bb["x", "max"],
    ymax = bb["y", "max"]
  ), crs = 4326) %>%
    st_as_sfc() %>%
    st_as_sf() %>%
    st_transform(crs = 28992)
  buffer <- st_buffer(bbox, 2000)
  vehicle_relevant <- st_intersection(vehicle, buffer)
  vehicle_relevant <- vehicle_relevant %>%
    st_cast("LINESTRING")
  st_write(vehicle_relevant, dsn, delete_layer = TRUE)
}

city_name <- "'s-Hertogenbosch"
dsn <- paste0("05_data_output/vehicle_network/Hertogenbosch.gpkg")
bb <- getbb(city_name)
bbox <- st_bbox(c(
  xmin = bb["x", "min"],
  ymin = bb["y", "min"],
  xmax = bb["x", "max"],
  ymax = bb["y", "max"]
), crs = 4326) %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_transform(crs = 28992)
buffer <- st_buffer(bbox, 2000)
vehicle_relevant <- st_intersection(vehicle, buffer)
vehicle_relevant <- vehicle_relevant %>%
  st_cast("LINESTRING")
st_write(vehicle_relevant, dsn, delete_layer = TRUE)
