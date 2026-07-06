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
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))


city_list <- c("Alblasserdam", "Arnhem", "Breda", "Dordrecht", "Eindhoven",
               "Geertruidenberg", "Gennep", "Gorinchem", "Helmond", "Hendrik-Ido-Ambacht", "Hertogenbosch",
               "Meierijstad", "Nieuwegein", "Nijmegen", "Oosterhout", "Oss", "Papendrecht", "Ridderkerk",
               "Roermond", "Roosendaal", "Sittard-Geleen", "Sliedrecht", "Tiel", "Tilburg", "Utrecht",
               "Venlo", "Vijfheerenlanden", "Waalwijk", "Wageningen", "Zwijndrecht")

# add index
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  segment <- st_read(file_path)
  segment <- segment %>%
    mutate(id = paste0(city_name, "_", row_number()))
  st_write(segment, file_path, delete_layer = TRUE)
}


# calculate the density index of the buildings
# old version
# for (city_name in city_list) {
#   dsm_path <- paste0("01_data/02_nl_topography/dsm_filled/", city_name, ".tif")
#   dtm_path <- paste0("01_data/02_nl_topography/dtm_filled/", city_name, ".tif")
#   file_path <- paste0("05_data_output/port_city_building/", city_name, ".gpkg")
#   segment_path <- paste0("05_data_output/segment_manual/", city_name, ".gpkg")
#   
#   building <- st_read(file_path)
#   segment <- st_read(segment_path)
#   dsm <- rast(dsm_path)
#   dtm <- rast(dtm_path)
#   dtm <- resample(dtm, dsm, method = "bilinear")
#   ndsm <- dsm - dtm
#   crs(ndsm) <- "EPSG:28992"
#   
#   building$h_mean <- exact_extract(ndsm, building, 'mean')
#   building <- building %>%
#     mutate(
#       floor_number = case_when(
#         is.na(h_mean)        ~ NA_integer_,
#         h_mean < 4           ~ 1L,
#         h_mean >= 4          ~ floor(h_mean / 3)
#       )
#     )
#   b_seg <- st_intersection(
#     building,
#     segment
#   )
#   b_seg <- b_seg %>%
#     mutate(
#       footprint = as.numeric(st_area(.))
#     )
#   b_seg <- b_seg %>%
#     mutate(
#       gfa = footprint * floor_number
#     )
#   segment_summary <-
#     b_seg %>%
#     st_drop_geometry() %>%
#     group_by(id) %>%
#     summarise(
#       footprint_sum = sum(footprint, na.rm = TRUE),
#       gfa_sum       = sum(gfa, na.rm = TRUE)
#     )
#   segment <- segment %>%
#     left_join(segment_summary, by = "id")
#   segment <- 
#     segment %>%
#     mutate(
#       fsi = gfa_sum / area_m2,
#       gsi = footprint_sum / area_m2,
#       l   = fsi / gsi,
#       osr = (1 - gsi) / fsi
#     )
#   st_write(segment, segment_path, delete_layer = TRUE)
# }

# # calculate industry proportion
# # old version
# for (city_name in city_list) {
#   file_path <- paste0("05_data_output/port_city_building/", city_name, ".gpkg")
#   segment_path <- paste0("05_data_output/segment_manual/", city_name, ".gpkg")
#   dsm_path <- paste0("01_data/02_nl_topography/dsm_filled/", city_name, ".tif")
#   dtm_path <- paste0("01_data/02_nl_topography/dtm_filled/", city_name, ".tif")
#   buildings <- st_read(file_path)
#   segment <- st_read(segment_path)
#   segment <- segment %>%
#     select(-industrial_ratio)
#   dsm <- rast(dsm_path)
#   dtm <- rast(dtm_path)
#   dtm <- resample(dtm, dsm, method = "bilinear")
#   ndsm <- dsm - dtm
#   crs(ndsm) <- "EPSG:28992"
#   
#   buildings$h_mean <- exact_extract(ndsm, buildings, 'mean')
#   buildings <- buildings %>%
#     mutate(
#       floor_number = case_when(
#         is.na(h_mean)        ~ NA_integer_,
#         h_mean < 4           ~ 1L,
#         h_mean >= 4          ~ floor(h_mean / 3)
#       )
#     )
#   b_int <- st_intersection(
#     buildings,
#     segment
#   )
#   b_int <- b_int %>%
#     mutate(
#       footprint = as.numeric(st_area(.)),
#       gfa = footprint * floor_number
#     )
#   total_gfa <- b_int %>%
#     st_drop_geometry() %>%
#     group_by(id) %>%
#     summarise(
#       gfa_total = sum(gfa, na.rm = TRUE)
#     )
#   industrial_gfa <- b_int %>%
#     filter(building %in% c("industrial", "yes")) %>%
#     st_drop_geometry() %>%
#     group_by(id) %>%
#     summarise(
#       gfa_industrial = sum(gfa, na.rm = TRUE)
#     )
#   segment <- segment %>%
#     left_join(total_gfa, by = "id") %>%
#     left_join(industrial_gfa, by = "id") %>%
#     mutate(
#       gfa_total = replace_na(gfa_total, 0),
#       gfa_industrial = replace_na(gfa_industrial, 0),
#       industrial_ratio =
#         ifelse(gfa_total > 0,
#                gfa_industrial / gfa_total,
#                0)
#     )
#   segment <- segment %>%
#     select(-gfa_total, -gfa_industrial)
#   st_write(segment, segment_path, delete_layer = TRUE)
# }




# caculate area
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  segment <- st_read(file_path)
  segment <- segment %>%
    mutate(area_m2 = as.numeric(st_area(geom)))
  st_write(segment, file_path, delete_layer = TRUE)
}




# calculate average ndvi
for (city_name in city_list) {
  raster_path <- paste0("01_data/01_port_city/satellite_images/", city_name,".tif")
  raster <- rast(raster_path)
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  segment <- st_read(file_path)

  satellite_clipped <- crop(raster, segment) %>%
    mask(segment)

  red <- satellite_clipped[["B4"]]
  nir <- satellite_clipped[["B8"]]
  ndvi <- (nir - red) / (nir + red)
  crs(ndvi) <- "EPSG:28992"

  segment$ndvi_mean <- exact_extract(ndvi, segment, 'mean')
  st_write(segment, file_path, delete_layer = TRUE)
}




# calculate the demographic index
demography <- st_read("01_data/01_port_city/demography/cbs_vk500_2024_v1.gpkg")
demography <- demography %>%
  select(aantal_inwoners, geom) %>%
  mutate(aantal_inwoners = pmax(aantal_inwoners, 0))

for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_manual/", city_name, ".gpkg")
  segment <- st_read(file_path)
  inter <- st_intersection(
    demography %>% mutate(grid_area = as.numeric(st_area(geom))), segment)
  inter <- inter %>%
    mutate(
      inter_area = as.numeric(st_area(geom)),
      residents_part = aantal_inwoners * inter_area / grid_area
    )
  segment_pop <- inter %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(residents_sum = sum(residents_part, na.rm = TRUE))
  segment <- segment %>%
    left_join(segment_pop, by = c("id" = "id"))
  st_write(segment, file_path, delete_layer = TRUE)
}





# calculate density index using rudifun dataset
# rudifun <- st_read("01_data/02_nl_topography/rudifun/zuid_holland.gpkg")
# rudifun <- rudifun %>%
#   select(geom, FSI_24, GSI_24, OSR_24, L_24, MXI_24)
# city_list <- c("Alblasserdam", "Dordrecht", "Gorinchem", "Hendrik-Ido-Ambacht", 
#                "Papendrecht", "Ridderkerk", "Sliedrecht", "Zwijndrecht")


# rudifun <- st_read("01_data/02_nl_topography/rudifun/gelderland.gpkg")
# rudifun <- rudifun %>%
#   select(geom, FSI_24, GSI_24, OSR_24, L_24, MXI_24)
# city_list <- c("Arnhem", "Nijmegen", "Tiel", "Wageningen")


# rudifun <- st_read("01_data/02_nl_topography/rudifun/noord_brabant.gpkg")
# rudifun <- rudifun %>%
#   select(geom, FSI_24, GSI_24, OSR_24, L_24, MXI_24)
# city_list <- c("Breda", "Eindhoven", "Geertruidenberg", "Helmond", "Hertogenbosch", "Meierijstad", "Oosterhout", "Oss", "Roosendaal", "Tilburg", "Waalwijk")

# rudifun <- st_read("01_data/02_nl_topography/rudifun/limburg.gpkg")
# rudifun <- rudifun %>%
#   select(geom, FSI_24, GSI_24, OSR_24, L_24, MXI_24)
# city_list <- c("Gennep", "Roermond", "Sittard-Geleen", "Venlo")

rudifun <- st_read("01_data/02_nl_topography/rudifun/utrecht.gpkg")
rudifun <- rudifun %>%
  select(geom, FSI_24, GSI_24, OSR_24, L_24, MXI_24)
city_list <- c("Nieuwegein", "Utrecht", "Vijfheerenlanden")


for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  segment <- st_read(file_path)
  segment <- segment %>%
    select(-fsi, -gsi, -osr, -l, -mxi)
  int <- st_intersection(rudifun, segment)
  int <- int %>%
    mutate(a = as.numeric(st_area(.)))
  seg_index <-
    int %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(
      fsi = sum(FSI_24 * a, na.rm = TRUE) / sum(a, na.rm = TRUE),
      gsi = sum(GSI_24 * a, na.rm = TRUE) / sum(a, na.rm = TRUE),
      osr = sum(OSR_24 * a, na.rm = TRUE) / sum(a, na.rm = TRUE),
      l = sum(L_24 * a, na.rm = TRUE) / sum(a, na.rm = TRUE),
      mxi = sum(MXI_24 * a, na.rm = TRUE) / sum(a, na.rm = TRUE)
    )
  segment <- segment %>%
    left_join(seg_index, by = "id")
  st_write(segment, file_path, delete_layer = TRUE)
}




# calculate industrial ratio using industrial landuse data
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_manual/", city_name, ".gpkg")
  industrial_path <- paste0("05_data_output/industrial_landuse/", city_name, ".gpkg")
  segment <- st_read(file_path)
  industrial_poly <- st_read(industrial_path, layer = "cleaned")
  int <- st_intersection(industrial_poly, segment)
  int <- int %>%
    mutate(a = as.numeric(st_area(.)))
  industrial_area_tbl <-
    int %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(
      industrial_area = sum(a, na.rm = TRUE)
    )
  segment <- segment %>%
    left_join(industrial_area_tbl, by = "id") %>%
    mutate(
      industrial_area = ifelse(is.na(industrial_area), 0, industrial_area),
      industrial_land_ratio =
        industrial_area / area_m2
    )
  segment <- segment %>%
    select(-industrial_ratio, -industrial_area)
  st_write(segment, file_path, delete_layer = TRUE)
}


# calculate amenity density using osm poi points
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  amenity_path <- paste0("05_data_output/amenity/", city_name, ".gpkg")
  segment <- st_read(file_path)
  amenity_pts <- st_read(amenity_path)
  segment <- segment %>%
    select(-amenity_count, -amenity_density)
  amenity_joined <- st_join(amenity_pts, segment, join = st_within)
  amenity_count_tbl <-
    amenity_joined %>%
    st_drop_geometry() %>%
    count(id, name = "amenity_count")
  segment <- segment %>%
    left_join(amenity_count_tbl, by = "id") %>%
    mutate(
      amenity_count = ifelse(is.na(amenity_count), 0, amenity_count),
      amenity_density = amenity_count / (area_m2 / 1e6)
    )
  segment <- segment %>%
    mutate(amenity_density_t = log1p(amenity_density))
  st_write(segment, file_path, delete_layer = TRUE)
}



# calculate public space density
for (city_name in city_list) {
  water_path <- paste0("01_data/01_port_city/water_shape/", city_name, ".gpkg")
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  public_space_path <- paste0("05_data_output/public_landuse/", city_name, ".gpkg")
  
  segment <- st_read(file_path)
  
  # segment <- segment %>%
  #   filter(id != "Wageningen_6")
  # st_write(segment, file_path, delete_layer = TRUE)
  # segment <- segment %>%
  #   select(-area_buffer, -area_public, -public_ratio)
  
  water <- st_read(water_path)
  public_space <- st_read(public_space_path, layer = "cleaned")
  buffer <- st_buffer(water, 500)
  int <- st_intersection(segment, buffer) %>%
    select(id, geom) %>%
    mutate(area_buffer = as.numeric(st_area(.)))
  int_public <- st_intersection(int, public_space) %>%
    select(id, geom) %>%
    mutate(area_public = as.numeric(st_area(.))) %>%
    st_drop_geometry()
  
  int <- int %>%
    st_drop_geometry()
  segment <- segment %>%
    left_join(int, by = "id") %>%
    mutate(
      area_buffer = ifelse(is.na(area_buffer), 0, area_buffer)
    )
  segment <- segment %>%
    left_join(int_public, by = "id") %>%
    mutate(
      area_public = ifelse(is.na(area_public), 0, area_public),
      public_ratio = area_public / area_buffer
    )
  st_write(segment, file_path, delete_layer = TRUE)
}


for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  segment <- st_read(file_path)
  segment <- segment %>%
    mutate(public_ratio_log = replace_na(public_ratio_log, 0),
           public_ratio = replace_na(public_ratio, 0))
  st_write(segment, file_path, delete_layer = TRUE)
}



# get isochrome polygon
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  isochrome_path <- paste0("05_data_output/isochrome_center/", city_name, ".gpkg")
  dsn <- paste0("05_data_output/reachable_area_center/", city_name, ".gpkg")
  
  segment <- st_read(file_path)
  isochrome <- st_read(isochrome_path)
  
  pts_grouped <- isochrome %>%
    group_by(origin_point_id) %>%
    summarise(geometry = st_combine(geom), .groups = "drop")
  
  hulls <- pts_grouped %>%
    mutate(geometry = st_convex_hull(geometry))
  hulls_poly <- hulls %>%
    filter(st_dimension(geometry) == 2)
  all_union <- st_union(hulls_poly)
  reachable_area <- st_as_sf(all_union)
  st_write(reachable_area, dsn, delete_layer = TRUE)
}


# calculate accessibility
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  reachable_path <- paste0("05_data_output/reachable_area_center/", city_name, ".gpkg")
  segment <- st_read(file_path)
  reachable <- st_read(reachable_path)
  int <- st_intersection(segment, reachable) %>%
    select(id, geom) %>%
    mutate(area_reachable_center = as.numeric(st_area(.))) %>%
    st_drop_geometry()
  segment <- segment %>%
    left_join(int, by = "id") %>%
    mutate(
      area_reachable_center = ifelse(is.na(area_reachable_center), 0, area_reachable_center),
      reachable_ratio = area_reachable_center / area_m2
    )
  st_write(segment, file_path, delete_layer = TRUE)
}


for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  segment <- st_read(file_path)
  segment <- segment %>%
    select(-reachable_ratio)
  segment <- segment %>%
    mutate(
      reachable_ratio = area_reachable / area_m2,
      reachable_ratio_center = area_reachable_center / area_m2
    )
  st_write(segment, file_path, delete_layer = TRUE)
}


# variable process to reduce skewness
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  segment <- st_read(file_path)
  segment <- segment %>%
    mutate(osr_log = log1p(osr),
           public_ratio_log = log1p(public_ratio),
           center_log = log1p(reachable_ratio_center))
  st_write(segment, file_path, delete_layer = TRUE)
}

for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  segment <- st_read(file_path)
  segment <- segment %>%
    select(-center_percentile)
  segment <- segment %>%
    mutate(center_percentile = percent_rank(reachable_ratio_center))
  st_write(segment, file_path, delete_layer = TRUE)
}

for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  segment <- st_read(file_path)
  segment <- segment %>%
    mutate(env_impact_log = log1p(env_impact))
  st_write(segment, file_path, delete_layer = TRUE)
}


# get the urban center using POI
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  poi_path <- paste0("05_data_output/amenity/", city_name, ".gpkg")
  dsn <- paste0("05_data_output/urban_center/", city_name, ".gpkg")
  
  segment <- st_read(file_path)
  poi <- st_read(poi_path)
  coords <- st_coordinates(poi)
  db <- dbscan(coords, eps = 300, minPts = 10)
  poi$cluster <- db$cluster
  cluster_sizes <- poi %>%
    filter(cluster > 0) %>%
    count(cluster, name = "n")
  k <- max(1, ceiling(0.30 * nrow(cluster_sizes)))
  top_clusters <- cluster_sizes %>%
    arrange(desc(n)) %>%
    slice_head(n = k) %>%
    pull(cluster)
  
  urban_centers <- poi %>%
    filter(cluster %in% top_clusters) %>%
    group_by(cluster) %>%
    summarise(
      geom = st_convex_hull(st_combine(geom)),
      n_poi = n(),
      .groups = "drop"
    )
  st_write(urban_centers, dsn, delete_layer = TRUE)
}


# # calculate the relative distance index
# industry_path <- "05_data_output/heavy_industry.gpkg"
# industry <- st_read(industry_path)
# 
# for (city_name in city_list) {
#   file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
#   center_path <- paste0("05_data_output/urban_center/", city_name, ".gpkg")
#   segment <- st_read(file_path)
#   # segment <- segment %>%
#   #   select(-ui_index)
#   center <- st_read(center_path)
#   # bb <- segment %>%
#   #   st_bbox() %>%
#   #   st_as_sfc() %>%
#   #   st_as_sf()
#   # bb_buffer <- st_buffer(bb, 1500)
#   # industry_relevant <- st_intersection(industry, bb_buffer)
#   industry_u <- st_union(industry)
#   center_u <- st_union(center)
#   d_i <- st_distance(segment, industry_u)
#   d_u <- st_distance(segment, center_u)
#   denom <- d_i + d_u
#   segment$ui_index <- as.numeric((d_i - d_u) / denom)
#   st_write(segment, file_path, delete_layer = TRUE)
# }


# calculate the environmental impact index
industry_path <- "05_data_output/heavy_industry.gpkg"
heavy_industry <- st_read(industry_path)

max_distance <- 1500          # only consider industry within 2 km (set NA for no cutoff)
decay_type   <- "exp"        # "linear" or "exp" (exponential decay)

for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  segment <- st_read(file_path)
  segment <- segment %>%
    select(-env_impact, -env_impact_sqrt)
  
  d <- units::drop_units(st_distance(segment, heavy_industry))
  
  w <- as.numeric(st_area(heavy_industry))
  w <- w / sum(w)
  
  segment$env_impact <- as.numeric((1 / (d + 1)) %*% w)
  
  segment$env_impact <- (segment$env_impact - min(segment$env_impact)) /
    (max(segment$env_impact) - min(segment$env_impact))
  segment$env_impact_km <- 
    (rank(segment$env_impact, ties.method = "average") - 1) /
    (sum(!is.na(segment$env_impact)) - 1)
  st_write(segment, file_path, delete_layer = TRUE)
}


# calculate pedestrian density
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  pedestrian_path <- paste0("05_data_output/pedestrian_network/", city_name, ".gpkg")
  segment <- st_read(file_path)
  pedestrian <- st_read(pedestrian_path)
  int <- st_intersection(segment %>% select(id), 
                         pedestrian) 
  int <- int %>%
    mutate(length = as.numeric(st_length(geom)))
  int <- int %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(total_length = sum(length))
  segment <- segment %>%
    left_join(int, by = "id") %>%
    mutate(
      total_length = ifelse(is.na(total_length), 0, total_length),
      pedestrian_density = total_length / area_m2
    )
  st_write(segment, file_path, delete_layer = TRUE)
}


# calculate shoreline length
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  water_path <- paste0("01_data/01_port_city/water_shape/", city_name, ".gpkg")
  segment <- st_read(file_path)
  water <- st_read(water_path)
  
  segment_boundary <- st_boundary(segment)
  bank_lines <- st_intersection(segment_boundary, water)
  bank_lines$bank_length <- st_length(bank_lines)
  bank_lines$bank_length <- as.numeric(bank_lines$bank_length)
  bank_sum <- bank_lines %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(bank_length = sum(bank_length))
  segment <- segment %>%
    left_join(bank_sum, by = "id")
  segment$bank_length[is.na(segment$bank_length)] <- 0
  segment$waterfront_ratio <- segment$bank_length / st_length(st_boundary(segment))
  
  st_write(segment, file_path, delete_layer = TRUE)
}



# combine and plot histogram of area
all_polys <- list()
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  x <- st_read(file_path)
  all_polys[[city_name]] <- x
}
all_polys <- bind_rows(all_polys)

head(all_polys)

all_polys_selected <- all_polys %>%
  select(id, area_m2, osr, l, fsi, mxi, env_impact_km, gsi, amenity_count, amenity_density, 
         amenity_density_t, area_reachable_center, reachable_ratio_center, ndvi_mean, area_reachable, 
         reachable_ratio, center_percentile, pedestrian_density, env_impact, total_length, 
         geom, osr_log, towards_idx, longitudinal_idx)

boxplot(all_polys_selected$towards_idx,
        main = "Boxplot of pedestrian density")

boxplot(all_polys_selected$longitudinal_idx,
        main = "Boxplot of pedestrian density")

boxplot(all_polys_selected$pedestrian_density,
        main = "Boxplot of pedestrian density")

boxplot(all_polys_selected$waterfront_ratio,
        main = "Boxplot of pedestrian density")


hist(
  all_polys$ndvi_mean,
  breaks = 30,
  main = "average ndvis",
  xlab = "average ndvi"
)

boxplot(all_polys$ndvi_mean,
        main = "Boxplot of NDVI")

hist(
  all_polys$env_impact_km,
  breaks = 30,
  main = "environmental impact",
  xlab = "environmental impact"
)

boxplot(all_polys$env_impact_km,
        main = "Boxplot of env impact")


hist(
  all_polys$env_impact_log,
  breaks = 30,
  main = "environmental impact",
  xlab = "environmental impact"
)

boxplot(all_polys$env_impact_log,
        main = "Boxplot of env impact log")


hist(
  all_polys$amenity_density_t,
  breaks = 30,
  main = "amenity density",
  xlab = "amenity density"
)

boxplot(all_polys$amenity_density_t,
        main = "Boxplot of amenity density")

hist(
  all_polys$fsi,
  breaks = 30,
  main = "fsi",
  xlab = "fsi"
)

boxplot(all_polys$fsi,
        main = "Boxplot of fsi")

hist(
  all_polys$gsi,
  breaks = 30,
  main = "gsi",
  xlab = "gsi"
)

boxplot(all_polys$gsi,
        main = "Boxplot of gsi")


hist(
  all_polys$osr,
  breaks = 30,
  main = "osr",
  xlab = "osr"
)

boxplot(all_polys$osr,
        main = "Boxplot of osr")


hist(
  all_polys$osr_log,
  breaks = 30,
  main = "osr",
  xlab = "osr"
)

boxplot(all_polys_selected$osr_log,
        main = "Boxplot of osr log")


hist(
  all_polys$l,
  breaks = 30,
  main = "l",
  xlab = "l"
)

boxplot(all_polys$l,
        main = "Boxplot of l")

hist(
  all_polys$mxi,
  breaks = 30,
  main = "mxi",
  xlab = "mxi"
)

boxplot(all_polys$mxi,
        main = "Boxplot of mxi")

hist(
  all_polys$public_ratio,
  breaks = 30,
  main = "public space ratio",
  xlab = "public space ratio"
)

boxplot(all_polys$public_ratio,
        main = "Boxplot of public ratio")

hist(
  all_polys$public_ratio_log,
  breaks = 30,
  main = "osr",
  xlab = "osr"
)

boxplot(all_polys$public_ratio_log,
        main = "Boxplot of public ratio log")

hist(
  all_polys$reachable_ratio,
  breaks = 30,
  main = "reachability",
  xlab = "reachability"
)

boxplot(all_polys$reachable_ratio,
        main = "Boxplot of reachable ratio")

hist(
  all_polys$reachable_ratio_center,
  breaks = 30,
  main = "reachability from urban center",
  xlab = "reachability from urban center"
)

boxplot(all_polys$reachable_ratio_center,
        main = "Boxplot of reachable ratio center")

hist(
  all_polys$center_log,
  breaks = 15,
  main = "reachability from urban center",
  xlab = "reachability from urban center"
)

boxplot(all_polys$center_log,
        main = "Boxplot of reachable ratio center log")

hist(
  all_polys$center_percentile,
  breaks = 15,
  main = "reachability from urban center",
  xlab = "reachability from urban center"
)

boxplot(all_polys$center_percentile,
        main = "Boxplot of reachable ratio center percentile")




st_write(all_polys_selected, "05_data_output/segment_gathered/260408.gpkg", append = FALSE)
