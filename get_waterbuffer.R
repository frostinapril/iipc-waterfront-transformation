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
  water_path <- paste0("01_data/01_port_city/water_shape/", city_name, ".gpkg")
  dsn <- paste0("01_data/01_port_city/water_shape_buffered/", city_name, ".gpkg")
  water <- st_read(water_path)
  buffer <- st_buffer(water, 500)
  plot(buffer)
  st_write(buffer, dsn, append = TRUE)
}

