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


# parameters for each city
city_name <- "Zwijndrecht"
crs <- 28992
file_path <- paste0("05_data_output/segment_manual/", city_name, ".gpkg")
segment <- st_read(file_path)
bb <- segment %>%
  st_transform(4326) %>%
  st_bbox()
building <- opq(bbox = bb, timeout = 120) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()
building_sf <- building$osm_polygons %>%
  st_transform(crs) %>% 
  select(osm_id, building, geometry)
if (!"start_date" %in% names(building$osm_polygons)) {
  building_sf$start_date <- NA_real_
} 
building_sf$start_date <- as.numeric(building$osm_polygons$start_date)
dsn <- paste0("05_data_output/port_city_building/", city_name, ".gpkg")
st_write(building_sf, dsn, append = FALSE)








