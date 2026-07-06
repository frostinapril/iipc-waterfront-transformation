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

file_path <- "05_data_output/segment_a2/Meierijstad.gpkg"
segment <- st_read(file_path)
