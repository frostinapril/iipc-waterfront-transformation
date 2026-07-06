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

path <- "01_data/02_nl_topography/pdok/heavy_industry.gpkg"
dsn <- "05_data_output/heavy_industry.gpkg"
city_name <- "Alblasserdam"
file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
segment <- st_read(file_path)
industry <- st_read(path)
industry_u <- industry %>%
  st_union() %>%
  st_cast("POLYGON") %>%
  st_as_sf()

st_write(industry_u, dsn, append = FALSE)
