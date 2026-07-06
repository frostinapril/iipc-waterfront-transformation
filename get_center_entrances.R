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


for (city_name in city_list) {
  center_path <- paste0("05_data_output/urban_center/", city_name, ".gpkg")
  pedestrian_path <- paste0("05_data_output/pedestrian_network/", city_name, ".gpkg")
  dsn <- paste0("05_data_output/center_entrance/", city_name, ".gpkg")
  center <- st_read(center_path)
  pedestrian <- st_read(pedestrian_path)
  
  center_edge <- st_boundary(center)
  center_edge <- st_make_valid(center_edge)
  pedestrian <- st_make_valid(pedestrian)
  entrances <- st_intersection(pedestrian, center_edge)
  entrances <- st_collection_extract(entrances, "POINT") %>%
    st_cast("POINT")
  
  st_write(entrances, dsn, delete_layer = TRUE)
}
