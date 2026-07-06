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
city_name <- "Sittard-Geleen"
crs <- 28992
bb <- getbb(city_name)

# get the network for delineation
railway <- opq(bbox = bb, timeout = 120) %>%
  add_osm_feature(key = "railway") %>%
  osmdata_sf()
railway_sf <- railway$osm_lines %>%
  st_transform(crs) %>%
  select(osm_id, name, maxspeed, geometry)

highway <- opq(bbox = bb, timeout = 120) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "trunk", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()
highway_sf <- highway$osm_lines %>%
  st_transform(crs) %>%
  select(osm_id, name, maxspeed, geometry)

# formulate the network
network <- rbind(highway_sf, railway_sf)

filename <- paste0("05_data_output/network/", city_name, ".gpkg")
st_write(network, filename, append=FALSE)
