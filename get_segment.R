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

city_list <- c("Breda", "Dordrecht")
for(city_name in city_list) {
  corridor_path <- paste0("05_data_output/segment_modified/corridor/", city_name, ".gpkg")
  network_path <- paste0("05_data_output/segment_modified/network_division/", city_name, ".gpkg")
  network <- st_read(network_path)
  corridor <- st_read(corridor_path)
  
  network_u <- st_union(network)
  
  segment <- corridor %>%
    st_split(network_u) %>%
    st_collection_extract("POLYGON") %>%
    st_as_sf()
  
  
  filename <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  plot(segment)
  st_write(segment, filename, append = FALSE)
}
