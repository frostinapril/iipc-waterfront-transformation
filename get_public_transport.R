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

city_list <- c("Waalwijk", "Wageningen", "Zwijndrecht")

for (city_name in city_list) {
  crs <- 28992
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  dsn <- paste0("05_data_output/public_transport/", city_name, ".gpkg")
  segment <- st_read(file_path)
  bb <- segment %>%
    st_transform(4326) %>%
    st_bbox()
  
  bus <- opq(bbox = bb, timeout = 120) %>%
    add_osm_feature(key = "highway", value = "bus_stop") %>%
    osmdata_sf()
  bus_sf <- bus$osm_points %>%
    st_transform(crs) %>%
    select(osm_id, geometry)
  tram <- opq(bbox = bb, timeout = 120) %>%
    add_osm_feature(key = "railway", value = "tram_stop") %>%
    osmdata_sf()
  tram_sf <- tram$osm_points %>%
    st_transform(crs) %>%
    select(osm_id, geometry)
  
  stops <- rbind(bus_sf, tram_sf)
  
  st_write(stops, dsn, delete_layer = TRUE)
}

