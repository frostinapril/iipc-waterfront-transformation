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
# for (city_name in city_list) {
#   crs <- 28992
#   dsn <- paste0("05_data_output/amenity/", city_name, ".gpkg")
#   bb <- getbb(city_name)
#   osm <- opq(bbox = bb, timeout = 120) %>%
#     add_osm_feature(key = "amenity") %>%
#     osmdata_sf()
#   amenity_pts <- osm$osm_points
#   amenity_pts <- st_transform(amenity_pts, crs = crs)
#   amenity_pts <- amenity_pts %>%
#     select(osm_id, amenity, geometry)
#   amenity_pts <- amenity_pts %>%
#     filter(!is.na(amenity))
#   unique(amenity_pts$amenity)
#   keep_types <- c(
#     "cinema",
#     "restaurant",
#     "bar",
#     "cafe",
#     "library", 
#     "fast_food",
#     "ice_cream",
#     "theatre",
#     "community_center",
#     "pub",
#     "biergarten",
#     "nightclub",
#     "arts_centre"
#   )
#   
#   amenity_pts_filtered <- amenity_pts %>%
#     filter(amenity %in% keep_types)
#   st_write(amenity_pts_filtered, dsn, delete_layer = TRUE)
# }

city_list <- c("Alblasserdam", "Arnhem", "Breda", "Dordrecht", "Eindhoven", 
               "Geertruidenberg", "Gennep", "Gorinchem", "Helmond", "Hendrik-Ido-Ambacht", "Hertogenbosch",
               "Meierijstad", "Nieuwegein", "Nijmegen", "Oosterhout", "Oss", "Papendrecht", "Ridderkerk", 
               "Roermond", "Roosendaal", "Sittard-Geleen", "Sliedrecht", "Tiel", "Tilburg", "Utrecht", 
               "Venlo", "Vijfheerenlanden", "Waalwijk", "Wageningen", "Zwijndrecht")

path_1 <- "01_data/02_nl_topography/OSM/gelderland/gis_osm_pois_free_1.shp"
path_2 <- "01_data/02_nl_topography/OSM/limburg/gis_osm_pois_free_1.shp"
path_3 <- "01_data/02_nl_topography/OSM/noord-brabant/gis_osm_pois_free_1.shp"
path_4 <- "01_data/02_nl_topography/OSM/utrecht/gis_osm_pois_free_1.shp"
path_5 <- "01_data/02_nl_topography/OSM/zuid-holland/gis_osm_pois_free_1.shp"
gelderland <- st_read(path_1) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry)
limburg <- st_read(path_2) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry)
noord_brabant <- st_read(path_3) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry)
utrecht <- st_read(path_4) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry)
zuid_holland <- st_read(path_5) %>%
  st_transform(crs = 28992) %>%
  select(fclass, geometry)
all <- bind_rows(gelderland, limburg, noord_brabant, utrecht, zuid_holland)
unique(all$fclass)

selected <- c("pub", "camp_site", "fast_food", "supermarket", "sports_centre", 
              "viewpoint", "library", "restaurant", "picnic_site", "cafe", "bakery", 
              "department_store", "florist", "butcher", "greengrocer", "beverages", 
              "gift_shop", "cinema", "chalet", "bar", "sports_shop", "community_centre", 
              "jeweller", "arts_centre", "bookshop", "biergarten", "toy_shop", "kiosk", 
              "market_place", "playground", "dog_park", "park", "general", "nightclub", 
              "food_court")

amenity <- all %>%
  filter(fclass %in% selected)

for (city_name in city_list) {
  dsn <- paste0("05_data_output/amenity/", city_name, ".gpkg")
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
  amenity_relevant <- st_intersection(amenity, bbox)
  st_write(amenity_relevant, dsn, delete_layer = TRUE)
}

city_name <- "'s-Hertogenbosch"
dsn <- paste0("05_data_output/amenity/Hertogenbosch.gpkg")
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
amenity_relevant <- st_intersection(amenity, bbox)
st_write(amenity_relevant, dsn, delete_layer = TRUE)
