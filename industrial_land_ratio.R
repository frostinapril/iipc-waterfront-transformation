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
library(exactextractr)
library(tidyr)
library(dbscan)
library(units)
library(spdep)
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))


city_list <- c("Alblasserdam", "Arnhem", "Breda", "Dordrecht", "Eindhoven",
               "Geertruidenberg", "Gennep", "Gorinchem", "Helmond", "Hendrik-Ido-Ambacht", "Hertogenbosch",
               "Meierijstad", "Nieuwegein", "Nijmegen", "Oosterhout", "Oss", "Papendrecht", "Ridderkerk",
               "Roermond", "Roosendaal", "Sittard-Geleen", "Sliedrecht", "Tiel", "Tilburg", "Utrecht",
               "Venlo", "Vijfheerenlanden", "Waalwijk", "Wageningen", "Zwijndrecht")

file_path <- paste0("05_data_output/segment_gathered/260428.gpkg")
segment <- st_read(file_path)
segment <- segment %>%
  select(-urbanity_land_ratio)
landuse <- st_read("01_data/02_nl_topography/pdok/landuse.gml")
landuse <- landuse %>%
  st_transform(28992)
unique(landuse$description)
industrial <- landuse %>%
  filter(description %in% c("Industrial area and offices", "Dumping site", "Mining area", 
                            "Car wreck site", "Area for storing industrial water")) %>%
  select(description)

industrial_int <- st_intersection(industrial, segment)
industrial_summary <- industrial_int %>%
  mutate(industrial_area = as.numeric(st_area(geometry))) %>%
  group_by(id) %>% 
  summarise(
    industrial_area_seg = sum(industrial_area)
  ) %>%
  st_drop_geometry()
segment <- segment %>%
  left_join(industrial_summary, by = "id") %>%
  mutate(industrial_land_ratio = ifelse(is.na(industrial_area_seg), 0, industrial_area_seg / area_m2)) %>%
  select(-industrial_area_seg)

residential <- landuse %>%
  filter(description == "Residential") %>%
  select(description)
residential_int <- st_intersection(residential, segment)
residential_summary <- residential_int %>%
  mutate(residential_area = as.numeric(st_area(geometry))) %>%
  group_by(id) %>% 
  summarise(
    residential_area_seg = sum(residential_area)
  ) %>%
  st_drop_geometry()
segment <- segment %>%
  left_join(residential_summary, by = "id") %>%
  mutate(residential_land_ratio = ifelse(is.na(residential_area_seg), 0, residential_area_seg / area_m2)) %>%
  select(-residential_area_seg)


urbanity <- landuse %>%
  filter(description %in% c("Retail trade, hotel and catering", "Public institutions", 
                            "Socio-cultural facility", "Park and public garden", "Holiday recreation")) %>%
  select(description)
urbanity_int <- st_intersection(urbanity, segment)

urbanity_summary <- urbanity_int %>%
  mutate(urbanity_area = as.numeric(st_area(geometry))) %>%
  group_by(id) %>% 
  summarise(
    urbanity_area_seg = sum(urbanity_area)
  ) %>%
  st_drop_geometry()
segment <- segment %>%
  left_join(urbanity_summary, by = "id") %>%
  mutate(urbanity_land_ratio = ifelse(is.na(urbanity_area_seg), 0, urbanity_area_seg / area_m2)) %>%
  select(-urbanity_area_seg)


boxplot(segment$urbanity_land_ratio)
boxplot(segment$residential_land_ratio)




landuse_u <- st_read("01_data/02_nl_topography/OSM/utrecht/gis_osm_landuse_a_free_1.shp") %>%
  select(fclass) %>%
  st_transform(28992)
landuse_l <- st_read("01_data/02_nl_topography/OSM/limburg/gis_osm_landuse_a_free_1.shp") %>%
  select(fclass) %>%
  st_transform(28992)
landuse_g <- st_read("01_data/02_nl_topography/OSM/gelderland/gis_osm_landuse_a_free_1.shp") %>%
  select(fclass) %>%
  st_transform(28992)
landuse_nb <- st_read("01_data/02_nl_topography/OSM/noord-brabant/gis_osm_landuse_a_free_1.shp") %>%
  select(fclass) %>%
  st_transform(28992)
landuse_zh <- st_read("01_data/02_nl_topography/OSM/zuid-holland/gis_osm_landuse_a_free_1.shp") %>%
  select(fclass) %>%
  st_transform(28992)
landuse <- bind_rows(landuse_g, landuse_l, landuse_nb, landuse_u, landuse_zh)
unique(landuse$fclass)

industrial <- landuse %>%
  filter(fclass %in% c("industrial", "quarry"))
industrial_int <- st_intersection(industrial, segment)

industrial_summary <- industrial_int %>%
  mutate(industrial_area = as.numeric(st_area(geometry))) %>%
  group_by(id) %>% 
  summarise(
    industrial_area_seg = sum(industrial_area)
  ) %>%
  st_drop_geometry()
segment <- segment %>%
  left_join(industrial_summary, by = "id") %>%
  mutate(industrial_land_ratio = ifelse(is.na(industrial_area_seg), 0, industrial_area_seg / area_m2))


urbanity <- landuse %>%
  filter(fclass %in% c("retail", "commercial", "residential", "recreation_ground"))
urbanity_int <- st_intersection(urbanity, segment)

urbanity_summary <- urbanity_int %>%
  mutate(urbanity_area = as.numeric(st_area(geometry))) %>%
  group_by(id) %>% 
  summarise(
    urbanity_area_seg = sum(urbanity_area)
  ) %>%
  st_drop_geometry()
segment <- segment %>%
  left_join(urbanity_summary, by = "id") %>%
  mutate(urbanity_land_ratio = ifelse(is.na(urbanity_area_seg), 0, urbanity_area_seg / area_m2))


segment <- segment %>%
  mutate(industrial_land_ratio = ifelse(industrial_land_ratio > 1, 1, industrial_land_ratio))
segment <- segment %>%
  mutate(urbanity_land_ratio = ifelse(urbanity_land_ratio > 1, 1, urbanity_land_ratio))


st_write(segment, file_path, delete_layer = TRUE)
