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

for (city_name in city_list) {
  segment_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  segment <- st_read(segment_path)
  
  segment_path_0 <- paste0("05_data_output/segment_a2/", city_name, ".gpkg")
  segment_0 <- st_read(segment_path_0) %>%
    select(amenity_density_t, id) %>%
    st_drop_geometry()
  
  segment <- segment %>%
    left_join(segment_0, by = "id")
  st_write(segment, segment_path, delete_layer = TRUE)
}


# calculate towards connectivity and longitudinal connectivity
for (city_name in city_list) {
  pedestrian_path <- paste0("05_data_output/pedestrian_network/", city_name, ".gpkg")
  pedestrian <- st_read(pedestrian_path)
  waterway_all <- st_read("01_data/02_nl_topography/pdok/waterway.gpkg")
  segment_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  segment <- st_read(segment_path)
  
  
  # get the relevant waterway
  buffer <- segment %>%
    st_union() %>%
    st_collection_extract("POLYGON") %>%
    st_as_sf() %>%
    st_buffer(300)
  waterway <- st_intersection(waterway_all, buffer)
  waterway_unified <- st_union(waterway) %>%
    st_line_merge() %>%
    st_as_sf()
  
  # split the waterway into segments of 500m 
  waterway_points <- st_line_sample(waterway, density = 1/50) %>% 
    st_cast("POINT")
  
  # 2. Function to create a perpendicular blade using trigonometry
  generate_perp_blade <- function(point, line_sf, length = 100) {
    # 1. Get the coordinates as a simple numeric vector
    # st_coordinates returns a matrix; we take the first row [1, ]
    coords_p <- st_coordinates(point)
    px <- coords_p[1, "X"]
    py <- coords_p[1, "Y"]
    
    # 2. Find the nearest segment of the waterway to get the tangent
    nearest_seg_idx <- st_nearest_feature(point, line_sf)
    line_coords <- st_coordinates(line_sf[nearest_seg_idx, ])
    
    # 3. Calculate the river's local angle
    # We use the first and last points of that specific segment
    angle_rad <- atan2(line_coords[nrow(line_coords), "Y"] - line_coords[1, "Y"], 
                       line_coords[nrow(line_coords), "X"] - line_coords[1, "X"])
    
    # 4. Perpendicular angle (90 degrees / pi/2 radians)
    perp_angle <- angle_rad + (pi / 2)
    
    # 5. Project the endpoints of the blade
    half_len <- length / 2
    
    # Calculate X and Y separately to avoid "non-conformable" errors
    p1_x <- px + (half_len * cos(perp_angle))
    p1_y <- py + (half_len * sin(perp_angle))
    
    p2_x <- px - (half_len * cos(perp_angle))
    p2_y <- py - (half_len * sin(perp_angle))
    
    # 6. Return as a LINESTRING
    return(st_linestring(rbind(c(p1_x, p1_y), c(p2_x, p2_y))))
  }
  
  # Apply the fixed function
  # Use lapply to create the list of blades, then wrap in sfc
  blades_list <- lapply(1:length(waterway_points), function(i) {
    generate_perp_blade(waterway_points[i], waterway, length = 100)
  })
  
  blades_sfc <- st_sfc(blades_list, crs = st_crs(waterway))
  all_blades <- st_union(blades_sfc)
  
  waterway_split <- st_split(waterway_unified, all_blades) %>% 
    st_collection_extract("LINESTRING") %>%
    st_as_sf()
  
  
  # find the towards and longitudinal roads
  pedestrian_int <- st_intersection(pedestrian, segment) %>%
    select(fclass, id, geom) %>%
    st_cast("LINESTRING") %>%
    mutate(road_uid = row_number())
  
  nearest_idx <- st_nearest_feature(pedestrian_int, waterway_split)
  
  pedestrian_mapped <- pedestrian_int %>%
    mutate(
      near_water_idx = nearest_idx,
      # Pull the ID of the waterway for easy checking
      waterway_ref_id = waterway_split$id[nearest_idx], 
      # Calculate the literal distance for verification
      dist_to_water = as.numeric(st_distance(geom, waterway_split[nearest_idx, ], by_element = TRUE))
    )
  
  # Ensure no non-linestrings are sneaking in
  pedestrian_mapped <- pedestrian_mapped %>%
    st_cast("LINESTRING") %>%
    filter(!st_is_empty(geom))
  
  calc_angle <- function(g) {
    if (st_is_empty(g)) return(NA_real_)
    
    # Get coordinates matrix (X, Y, L1)
    coords <- st_coordinates(g)
    
    # WE MUST SPECIFY BOTH ROW AND COLUMN
    # Row 1, Col 1 is X | Row 1, Col 2 is Y
    start_x <- coords[1, 1]
    start_y <- coords[1, 2]
    
    # Row 'Last', Col 1 is X | Row 'Last', Col 2 is Y
    end_x   <- coords[nrow(coords), 1]
    end_y   <- coords[nrow(coords), 2]
    
    # Now atan2 receives two single numbers, and returns one single number
    angle <- atan2(end_y - start_y, end_x - start_x) * 180 / pi
    
    return(as.numeric(angle))
  }
  
  pedestrian_analysis <- pedestrian_mapped %>%
    mutate(
      # map_dbl ensures we get a numeric vector back. 
      # If calc_angle returns a list or NULL, this will error helpfully.
      r_angle = map_dbl(st_geometry(.), calc_angle) %% 180,
      
      # We do the same for the waterway segments
      w_angle = map_dbl(st_geometry(waterway_split[near_water_idx, ]), calc_angle) %% 180,
      
      # Perpendicular Logic
      diff = abs(r_angle - w_angle),
      acute_angle = ifelse(diff > 90, 180 - diff, diff),
      
      # Connectivity logic
      is_towards = (acute_angle >= 45 & acute_angle <= 90),
      is_longitudinal = (acute_angle >= 0 & acute_angle <= 20),
      # weight_towards = ifelse(is_towards, pmax(0, 1 - (dist_to_water / 500)), 0),
      # weighted_contribution_towards = weight_towards * as.numeric(st_length(geom)),
      # weight_longitudinal = ifelse(is_longitudinal, pmax(0, 1 - (dist_to_water / 500)), 0),
      # weighted_contribution_longitudinal = weight_longitudinal * as.numeric(st_length(geom)),
      road_length = as.numeric(st_length(geom))
    )
  
  
  # pedestrian_towards <- pedestrian_analysis %>%
  #   filter(is_towards == TRUE)
  # pedestrian_longitudinal <- pedestrian_analysis %>%
  #   filter(is_longitudinal == TRUE)
  # plot(pedestrian_towards)
  # plot(pedestrian_longitudinal)
  
  
  # 2. Aggregate by segment ID
  density_index <- pedestrian_analysis %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(
      towards_length = sum(road_length[is_towards == TRUE], na.rm = TRUE),
      longitudinal_length = sum(road_length[is_longitudinal == TRUE], na.rm = TRUE)
    ) 
  
  segment <- segment %>%
    left_join(density_index, by = "id") %>%
    mutate(towards_idx = ifelse(is.na(towards_length), 0, towards_length / area_m2),
           longitudinal_idx = ifelse(is.na(longitudinal_length), 0, longitudinal_length / area_m2))
  
  st_write(segment, segment_path, delete_layer = TRUE)
}


# calculate average building age
for (city_name in city_list) {
  building_path <- paste0("05_data_output/pand/", city_name, ".gpkg")
  buildings <- st_read(building_path)
  segment_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  segment <- st_read(segment_path)
  
  buildings_with_id <- st_join(buildings, segment["id"], join = st_intersects)
  current_year <- 2026
  buildings_age <- buildings_with_id %>%
    mutate(
      # Ensure built_year is numeric
      built_year = as.numeric(bouwjaar),
      # Calculate age
      age = current_year - built_year
    ) %>%
    # Remove buildings with invalid years (e.g., in the future or NA)
    filter(!is.na(age), age >= 0)
  age_summary <- buildings_age %>%
    st_drop_geometry() %>% # Drop geometry for faster aggregation
    group_by(id) %>%
    summarise(
      avg_building_age = mean(age, na.rm = TRUE)
    )
  
  segment <- segment %>%
    left_join(age_summary, by = "id") %>%
    mutate(
      avg_building_age = ifelse(is.na(avg_building_age), 0, avg_building_age)
    )
  st_write(segment, segment_path, delete_layer = TRUE)
}


# calculate the average NDWI
for (city_name in city_list) {
  raster_path <- paste0("01_data/01_port_city/satellite_mnwir/", city_name,".tif")
  raster <- rast(raster_path)
  file_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  segment <- st_read(file_path)
  segment_buffered <- st_buffer(segment, 50)
  
  satellite_clipped <- crop(raster, segment_buffered) %>%
    mask(segment_buffered)
  
  green <- satellite_clipped[["B3"]]
  swir <- satellite_clipped[["B11"]]
  mndwi <- (green - swir) / (green + swir)
  crs(mndwi) <- "EPSG:28992"
  
  segment$avg_ndwi <- exact_extract(mndwi, segment, 'mean')
  segment$water_prop <- exact_extract(mndwi, segment, function(values, coverage_fraction) {
    sum(values > 0.2 & !is.na(values)) / length(values)
  })
  st_write(segment, file_path, delete_layer = TRUE)
}


# calculate waterway length
for (city_name in city_list) {
  waterway_all <- st_read("01_data/02_nl_topography/pdok/waterway.gpkg") %>%
    st_union() %>%
    st_line_merge() %>%
    st_as_sf()
  segment_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  segment <- st_read(segment_path) %>%
    select(-waterway_length, -waterway_ratio)
  
  segment_buffered <- st_buffer(segment, 300)
  waterway_relevant <- st_intersection(waterway_all, segment_buffered) %>%
    mutate(waterway_length = as.numeric(st_length(x))) %>%
    select(id, waterway_length) %>%
    st_drop_geometry()
  
  # segment <- segment %>%
  #   left_join(waterway_relevant, by = "id") %>%
  #   mutate(perimeter_m = st_perimeter(geom),
  #          waterway_ratio = ifelse(is.na(waterway_length / perimeter_m), 0, waterway_length / perimeter_m))
  
  segment <- segment %>%
    left_join(waterway_relevant, by = "id") %>%
    mutate(waterway_length = ifelse(is.na(waterway_length), 0, waterway_length))
  
  st_write(segment, segment_path, delete_layer = TRUE)
}



# flood risk
flood_risk <- rast("01_data/02_nl_topography/liwo/flood_risk.tif")

for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  segment <- st_read(file_path)
  
  flood_clipped <- crop(flood_risk, segment) %>%
    mask(segment)
  segment$avg_flood_risk <- exact_extract(flood_clipped, segment, 
                                          fun = function(values, 
                                                         coverage_fraction) {
                                            mean(values, na.rm = TRUE)})
  st_write(segment, file_path, delete_layer = TRUE)
}


# NO2 pollution
no2_pollution <- rast("01_data/02_nl_topography/liwo/no2.tif")
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  segment <- st_read(file_path)
  no2_clipped <- crop(no2_pollution, segment) %>%
    mask(segment)
  segment$avg_no2_pollution <- exact_extract(no2_clipped, segment, 
                                             fun = function(values, 
                                                            coverage_fraction) {
                                               mean(values, na.rm = TRUE)})
  st_write(segment, file_path, delete_layer = TRUE)
}



# interface barrier index
highway_list <- c("primary_link", "primary", "secondary", "secondary_link", "motorway",
                  "motorway_link")
gelderland_railway <- st_read("01_data/02_nl_topography/OSM/gelderland/gis_osm_railways_free_1.shp")
gelderland_highway <- st_read("01_data/02_nl_topography/OSM/gelderland/gis_osm_roads_free_1.shp") %>%
  filter(fclass %in% highway_list)
limburg_railway <- st_read("01_data/02_nl_topography/OSM/limburg/gis_osm_railways_free_1.shp")
limburg_highway <- st_read("01_data/02_nl_topography/OSM/limburg/gis_osm_roads_free_1.shp") %>%
  filter(fclass %in% highway_list)
noord_brabant_railway <- st_read("01_data/02_nl_topography/OSM/noord-brabant/gis_osm_railways_free_1.shp")
noord_brabant_highway <- st_read("01_data/02_nl_topography/OSM/noord-brabant/gis_osm_roads_free_1.shp") %>%
  filter(fclass %in% highway_list)
utrecht_railway <- st_read("01_data/02_nl_topography/OSM/utrecht/gis_osm_railways_free_1.shp")
utrecht_highway <- st_read("01_data/02_nl_topography/OSM/utrecht/gis_osm_roads_free_1.shp") %>%
  filter(fclass %in% highway_list)
zuid_holland_railway <- st_read("01_data/02_nl_topography/OSM/zuid-holland/gis_osm_railways_free_1.shp")
zuid_holland_highway <- st_read("01_data/02_nl_topography/OSM/zuid-holland/gis_osm_roads_free_1.shp") %>%
  filter(fclass %in% highway_list)

railways <- bind_rows(gelderland_railway, utrecht_railway, noord_brabant_railway, 
                      zuid_holland_railway, limburg_railway) %>%
  filter(tunnel == "F") %>%
  select(fclass, geometry)
highways <- bind_rows(gelderland_highway, utrecht_highway, zuid_holland_highway,
                      noord_brabant_highway, limburg_highway) %>%
  filter(tunnel == "F") %>%
  select(fclass, geometry)

urban_barrier <- bind_rows(railways, highways) %>%
  st_transform(crs = 28992)

for (city_name in city_list) {
  segment_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  segment <- st_read(segment_path)
  segment <- segment %>%
    select(-barrier_length_sum, -barrier_ratio)
  segment_buffered <- st_buffer(segment, 30)
  
  urban_barrier_relevant <- st_intersection(urban_barrier, segment_buffered) %>%
    mutate(barrier_length = as.numeric(st_length(geometry))) %>%
    select(id, barrier_length) %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(
      barrier_length_sum = sum(barrier_length, na.rm = TRUE)
    )
  
  segment <- segment %>%
    left_join(urban_barrier_relevant, by = "id") %>%
    mutate(barrier_ratio = ifelse(is.na(barrier_length_sum / perimeter_m), 0, barrier_length_sum / perimeter_m))
  
  st_write(segment, segment_path, delete_layer = TRUE)
}


# island index
corridor <- st_read("05_data_output/segment_gathered/260414_means.gpkg") %>%
  mutate(cluster_corridor = cluster) %>%
  select(id, cluster_corridor) %>%
  st_drop_geometry()
segment <- st_read("05_data_output/segment_gathered/260408.gpkg") %>%
  select(-urbanity_index, -island_index, -neighbor_urbanity, -cluster_corridor)
segment <- segment %>%
  left_join(corridor, by = "id")
segment <- segment %>%
  mutate(urbanity_index = case_when(
    cluster_corridor == 1 ~  1.0,
    cluster_corridor == 5 ~  0.6,
    cluster_corridor == 2 ~  0.2,
    cluster_corridor == 4 ~ -0.2,
    cluster_corridor == 6 ~ -0.6,
    cluster_corridor == 3 ~ -1.0,
    TRUE         ~ NA_real_ # Safety catch
  ))

# # 1. Create a neighbors list (Queen contiguity: sharing a vertex or edge)
# nb <- poly2nb(segment, queen = TRUE)
# 
# # 2. Assign weights (style = "W" means row-standardized/averaging)
# # zero.policy = TRUE handles segments with NO neighbors (actual islands)
# lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
# 
# # 3. Calculate the "Spatial Lag" (The average urbanity of the neighbors)
# segment$neighbor_urbanity <- lag.listw(lw, segment$urbanity_index, zero.policy = TRUE)
# 
# # 4. Calculate the Island Index (Absolute difference)
# segment <- segment %>%
#   mutate(island_index = urbanity_index - neighbor_urbanity)
# 
# # 5. Handle actual islands (segments with 0 neighbors)
# # If it has no neighbors, we can argue it is the ultimate 'island'
# segment$island_index[is.na(segment$island_index)] <- 1

# 1. Find which segments touch each other
# This creates a list of pairs (row indices)
intersections <- st_intersects(segment, segment)

# 2. Calculate the length of the shared borders
# We iterate through the intersection list to get the actual geometry of the edges
edges <- st_intersection(segment, segment) %>%
  filter(id != id.1) %>% # Remove self-intersections
  mutate(edge_length = as.numeric(st_length(geom))) %>%
  st_drop_geometry() %>%
  select(id, id.1, edge_length)

# 3. Calculate the weighted neighbor urbanity
# Join the edge lengths with the urbanity of the neighbor (id.1)
df_weighted_nb <- edges %>%
  left_join(segment %>% st_drop_geometry() %>% select(id, urbanity_index), by = c("id.1" = "id")) %>%
  rename(neighbor_urbanity_val = urbanity_index) %>%
  group_by(id) %>%
  # Row-standardize: Weight = (this edge) / (total perimeter shared with neighbors)
  mutate(weight = edge_length / sum(edge_length)) %>%
  # Calculate the weighted average
  summarise(weighted_neighbor_urbanity = sum(neighbor_urbanity_val * weight))

# 4. Join back to the original data
segment <- segment %>%
  left_join(df_weighted_nb, by = "id") %>%
  mutate(
    # Recalculate the Island Index with the new weighted average
    island_index_weighted = urbanity_index - weighted_neighbor_urbanity
  )

st_write(segment, "05_data_output/segment_gathered/260408.gpkg", delete_layer = TRUE)



# island index
rudifun_zh <- st_read("01_data/02_nl_topography/rudifun/zuid_holland.gpkg")
rudifun_zh <- rudifun_zh %>%
  select(geom, FSI_24, GSI_24, OSR_24, L_24, MXI_24)
#city_list <- c("Alblasserdam", "Dordrecht", "Gorinchem", "Hendrik-Ido-Ambacht",
#               "Papendrecht", "Ridderkerk", "Sliedrecht", "Zwijndrecht")


rudifun_g <- st_read("01_data/02_nl_topography/rudifun/gelderland.gpkg")
rudifun_g <- rudifun_g %>%
  select(geom, FSI_24, GSI_24, OSR_24, L_24, MXI_24)
#city_list <- c("Arnhem", "Nijmegen", "Tiel", "Wageningen")


rudifun_nb <- st_read("01_data/02_nl_topography/rudifun/noord_brabant.gpkg")
rudifun_nb <- rudifun_nb %>%
  select(geom, FSI_24, GSI_24, OSR_24, L_24, MXI_24)
#city_list <- c("Breda", "Eindhoven", "Geertruidenberg", "Helmond", "Hertogenbosch", "Meierijstad", "Oosterhout", "Oss", "Roosendaal", "Tilburg", "Waalwijk")

rudifun_l <- st_read("01_data/02_nl_topography/rudifun/limburg.gpkg")
rudifun_l <- rudifun_l %>%
  select(geom, FSI_24, GSI_24, OSR_24, L_24, MXI_24)
# city_list <- c("Gennep", "Roermond", "Sittard-Geleen", "Venlo")

rudifun_u <- st_read("01_data/02_nl_topography/rudifun/utrecht.gpkg")
rudifun_u <- rudifun_u %>%
  select(geom, FSI_24, GSI_24, OSR_24, L_24, MXI_24)
#city_list <- c("Nieuwegein", "Utrecht", "Vijfheerenlanden")

rudifun_combined <- bind_rows(rudifun_g, rudifun_l, rudifun_nb, rudifun_u, rudifun_zh)
rudifun_combined <- rudifun_combined %>%
  mutate(across(c(FSI_24, GSI_24, MXI_24), ~as.numeric(scale(.x)), .names = "z_{.col}"))
rudifun_combined <- rudifun_combined %>%
  mutate(part_urbanity = (z_MXI_24 * 0.5) + (z_FSI_24 * 0.3) + (z_GSI_24 * 0.2))
file_path <- "05_data_output/segment_gathered/260408.gpkg"
segments_sf <- st_read(file_path)
# segments_with_data <- st_interpolate_aw(rundifun_combined[c("FSI_24", "GSI_24", "MXI_24")], 
#                                         segments_sf, 
#                                         extensive = FALSE)
# segments_with_data$id <- segments_sf$id[as.numeric(rownames(segments_with_data))]
# 
# segments_final <- segments_sf %>%
#   left_join(st_drop_geometry(segments_with_data), by = "id")
segments_sf <- segments_sf %>%
  mutate(across(c(fsi, gsi, mxi), ~as.numeric(scale(.x)), .names = "z_{.col}"),
         urbanity_seg = (z_mxi * 0.5) + (z_fsi * 0.3) + (z_gsi * 0.2))
# Ensure both layers are in the same Projected CRS (e.g., UTM)
rudifun_combined <- st_transform(rudifun_combined, st_crs(segments_sf))
all_buffers <- st_buffer(segments_sf, 500)

# 2. Global Intersection
# This creates a new layer where blocks are clipped by every buffer
# 'suppressWarnings' hides the 'attributes assumed constant' message
intersected_context <- suppressWarnings(st_intersection(rudifun_combined, all_buffers))

# 3. Calculate Area and Weighted Urbanity
# We must use 'tmp_id' to know which segments the pieces belong to
context_summary <- intersected_context %>%
  mutate(part_area = as.numeric(st_area(geom))) %>%
  # Filter out the 'self' overlap (if your blocks and segments overlap)
  # You can skip this if rudifun_combined is strictly 'outside' segments
  group_by(id) %>% 
  summarise(
    context_urbanity_500m = sum(part_urbanity * part_area, na.rm = TRUE) / sum(part_area, na.rm = TRUE)
  ) %>%
  st_drop_geometry()

# 4. Join back to original segments
segments_sf <- segments_sf %>%
  left_join(context_summary, by = "id")
segments_sf <- segments_sf %>%
  mutate(
    urbanity_contrast = urbanity_seg - context_urbanity_500m
  )
boxplot(segments_sf$urbanity_contrast)

st_write(segments_sf, "05_data_output/segment_gathered/260421.gpkg", delete_layer = TRUE)

# segment <- st_read("05_data_output/segment_gathered/260421.gpkg") %>%
#   select(-urbanity_contrast, -urbanity_seg, -context_urbanity_500m, -z_mxi, -z_fsi, -z_gsi)
#st_write(segment, "05_data_output/segment_gathered/260421.gpkg", delete_layer = TRUE)

# a <- st_read("05_data_output/segment_gathered/260324.gpkg") %>%
#   select(id, center_percentile) %>%
#   st_drop_geometry()
# segment <- st_read("05_data_output/segment_gathered/260421.gpkg")
# segment <- segment %>%
#   left_join(a, by = "id")


# industrial ratio
segment_all <- st_read("05_data_output/segment_gathered/260421.gpkg")

for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
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
  st_write(segment, file_path, delete_layer = TRUE)
}

all_polys <- all_polys %>%
  select(industrial_land_ratio, id) %>%
  st_drop_geometry()
segment_all <- segment_all %>%
  left_join(all_polys, by = "id")
st_write(segment_all, "05_data_output/segment_gathered/260421.gpkg", delete_layer = TRUE)



# convexity index
segment <- st_read("05_data_output/segment_gathered/260421.gpkg")
segment <- segment %>%
  mutate(
    hull = st_convex_hull(geom),
    convexity_index = as.numeric(st_area(geom)) / as.numeric(st_area(hull))
  ) %>%
  select(-hull)
st_write(segment, "05_data_output/segment_gathered/260421.gpkg", delete_layer = TRUE)


# center reachability
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  reachable_path <- paste0("05_data_output/reachable_area_center/", city_name, ".gpkg")
  segment <- st_read(file_path)
  segment <- segment %>%
    select(-area_reachable_center, -center_reachability, -reachable_ratio_center)
  reachable <- st_read(reachable_path)
  int <- st_intersection(segment, reachable) %>%
    select(id, geom) %>%
    mutate(area_reachable_center = as.numeric(st_area(.))) %>%
    st_drop_geometry()
  segment <- segment %>%
    left_join(int, by = "id") %>%
    mutate(
      area_reachable_center = ifelse(is.na(area_reachable_center), 0, area_reachable_center),
      reachable_ratio_center = area_reachable_center / area_m2,
      center_reachability = ntile(reachable_ratio_center, 5)
    )
  st_write(segment, file_path, delete_layer = TRUE)
}

boxplot(all_polys$reachable_ratio_center)
boxplot(all_polys$center_reachability)
all_polys <- all_polys %>%
  select(id, waterway_length) %>%
  st_drop_geometry()
segment <- st_read("05_data_output/segment_gathered/260421.gpkg") %>%
  select(-waterway_length)
segment <- segment %>%
  left_join(all_polys, by = "id")
st_write(segment, "05_data_output/segment_gathered/260421.gpkg", delete_layer = TRUE)





# longitudinal density close to the waterway
for (city_name in city_list) {
  pedestrian_path <- paste0("05_data_output/pedestrian_network/", city_name, ".gpkg")
  pedestrian <- st_read(pedestrian_path)
  waterway_all <- st_read("01_data/02_nl_topography/pdok/waterway.gpkg")
  segment_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  
  segment <- st_read(segment_path)
  
  core_polygon <- segment %>%
    st_union() %>%
    st_collection_extract("POLYGON") %>%
    st_as_sf() %>%
    st_buffer(-100)
  
  border_strip <- segment %>%
    st_union() %>%
    st_collection_extract("POLYGON") %>%
    st_as_sf() %>%
    st_difference(core_polygon)
  
  # get the relevant waterway
  buffer <- segment %>%
    st_union() %>%
    st_collection_extract("POLYGON") %>%
    st_as_sf() %>%
    st_buffer(1000)
  waterway <- st_intersection(waterway_all, buffer)
  
  waterway_buffer <- st_buffer(waterway, 500)
  border_strip <- st_intersection(border_strip, waterway_buffer) %>%
    st_union() %>%
    st_collection_extract("POLYGON") %>%
    st_as_sf()
  
  waterway_unified <- st_union(waterway) %>%
    st_line_merge() %>%
    st_as_sf()
  
  # split the waterway into segments of 500m 
  waterway_points <- st_line_sample(waterway, density = 1/50) %>% 
    st_cast("POINT")
  
  # 2. Function to create a perpendicular blade using trigonometry
  generate_perp_blade <- function(point, line_sf, length = 100) {
    # 1. Get the coordinates as a simple numeric vector
    # st_coordinates returns a matrix; we take the first row [1, ]
    coords_p <- st_coordinates(point)
    px <- coords_p[1, "X"]
    py <- coords_p[1, "Y"]
    
    # 2. Find the nearest segment of the waterway to get the tangent
    nearest_seg_idx <- st_nearest_feature(point, line_sf)
    line_coords <- st_coordinates(line_sf[nearest_seg_idx, ])
    
    # 3. Calculate the river's local angle
    # We use the first and last points of that specific segment
    angle_rad <- atan2(line_coords[nrow(line_coords), "Y"] - line_coords[1, "Y"], 
                       line_coords[nrow(line_coords), "X"] - line_coords[1, "X"])
    
    # 4. Perpendicular angle (90 degrees / pi/2 radians)
    perp_angle <- angle_rad + (pi / 2)
    
    # 5. Project the endpoints of the blade
    half_len <- length / 2
    
    # Calculate X and Y separately to avoid "non-conformable" errors
    p1_x <- px + (half_len * cos(perp_angle))
    p1_y <- py + (half_len * sin(perp_angle))
    
    p2_x <- px - (half_len * cos(perp_angle))
    p2_y <- py - (half_len * sin(perp_angle))
    
    # 6. Return as a LINESTRING
    return(st_linestring(rbind(c(p1_x, p1_y), c(p2_x, p2_y))))
  }
  
  # Apply the fixed function
  # Use lapply to create the list of blades, then wrap in sfc
  blades_list <- lapply(1:length(waterway_points), function(i) {
    generate_perp_blade(waterway_points[i], waterway, length = 100)
  })
  
  blades_sfc <- st_sfc(blades_list, crs = st_crs(waterway))
  all_blades <- st_union(blades_sfc)
  
  waterway_split <- st_split(waterway_unified, all_blades) %>% 
    st_collection_extract("LINESTRING") %>%
    st_as_sf()
  
  
  # find the towards and longitudinal roads
  pedestrian_int <- st_intersection(pedestrian, segment) %>%
    select(fclass, id, geom) %>%
    st_cast("LINESTRING") %>%
    mutate(road_uid = row_number())
  
  nearest_idx <- st_nearest_feature(pedestrian_int, waterway_split)
  
  pedestrian_mapped <- pedestrian_int %>%
    mutate(
      near_water_idx = nearest_idx,
      # Pull the ID of the waterway for easy checking
      waterway_ref_id = waterway_split$id[nearest_idx], 
      # Calculate the literal distance for verification
      dist_to_water = as.numeric(st_distance(geom, waterway_split[nearest_idx, ], by_element = TRUE))
    )
  
  # Ensure no non-linestrings are sneaking in
  pedestrian_mapped <- pedestrian_mapped %>%
    st_cast("LINESTRING") %>%
    filter(!st_is_empty(geom))
  
  calc_angle <- function(g) {
    if (st_is_empty(g)) return(NA_real_)
    
    # Get coordinates matrix (X, Y, L1)
    coords <- st_coordinates(g)
    
    # WE MUST SPECIFY BOTH ROW AND COLUMN
    # Row 1, Col 1 is X | Row 1, Col 2 is Y
    start_x <- coords[1, 1]
    start_y <- coords[1, 2]
    
    # Row 'Last', Col 1 is X | Row 'Last', Col 2 is Y
    end_x   <- coords[nrow(coords), 1]
    end_y   <- coords[nrow(coords), 2]
    
    # Now atan2 receives two single numbers, and returns one single number
    angle <- atan2(end_y - start_y, end_x - start_x) * 180 / pi
    
    return(as.numeric(angle))
  }
  
  pedestrian_analysis <- pedestrian_mapped %>%
    mutate(
      # map_dbl ensures we get a numeric vector back. 
      # If calc_angle returns a list or NULL, this will error helpfully.
      r_angle = map_dbl(st_geometry(.), calc_angle) %% 180,
      
      # We do the same for the waterway segments
      w_angle = map_dbl(st_geometry(waterway_split[near_water_idx, ]), calc_angle) %% 180,
      
      # Perpendicular Logic
      diff = abs(r_angle - w_angle),
      acute_angle = ifelse(diff > 90, 180 - diff, diff),
      
      # Connectivity logic
      is_longitudinal = (acute_angle >= 0 & acute_angle <= 20)
    )
  
  pedestrian_longitudinal <- pedestrian_analysis %>%
    filter(is_longitudinal == TRUE)
  waterfront_path <- st_intersection(pedestrian_longitudinal, border_strip) %>%
    mutate(road_length = as.numeric(st_length(geom)))
  
  
  # 2. Aggregate by segment ID
  density_index <- waterfront_path %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(
      waterfront_length = sum(road_length, na.rm = TRUE)
    ) 
  
  segment <- segment %>%
    left_join(density_index, by = "id") %>%
    mutate(waterfront_path_idx = ifelse(is.na(waterfront_length), 0, waterfront_length / area_m2))
  
  st_write(segment, segment_path, delete_layer = TRUE)
}


# angular choice accessibility index

for (city_name in city_list) {
  segment_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  angular_choice_path <- paste0("05_data_output/angular_choice/", city_name, ".gpkg")
  segment <- st_read(segment_path)
  angular_choice <- st_read(angular_choice_path) %>%
    select(-id)
  
  road_intersection <- st_intersection(angular_choice, segment)
  road_intersection <- road_intersection %>%
    mutate(int_length = as.numeric(st_length(.)))
  road_intersection <- road_intersection %>%
    mutate(weighted_value = ACw800S * int_length)
  accessibility_df <- road_intersection %>%
    group_by(id) %>% # Use your unique ID column
    summarise(
      total_weighted_choice = sum(weighted_value, na.rm = TRUE),
      total_length = sum(int_length, na.rm = TRUE)
    ) %>%
    mutate(accessibility_index = total_weighted_choice / total_length)
  segment <- segment %>%
    left_join(st_drop_geometry(accessibility_df), by = "id")
  
  st_write(segment, segment_path, delete_layer = TRUE)
}


# island index using only fsi











all_polys <- all_polys %>%
  st_drop_geometry() %>%
  select(accessibility_index, id)

boxplot(all_polys$accessibility_index)

segment <- st_read("05_data_output/segment_gathered/260428.gpkg")
segment <- segment %>%
  left_join(all_polys, by = "id")
st_write(segment, "05_data_output/segment_gathered/260428.gpkg", append = FALSE)

# gather the segments
all_polys <- list()
for (city_name in city_list) {
  file_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  x <- st_read(file_path)
  all_polys[[city_name]] <- x
}
all_polys <- bind_rows(all_polys)
all_polys <- bind_rows(all_polys) %>%
  filter(id != "Nieuwegein_14",
         id != "Utrecht_30",
         id != "Hertogenbosch_20",
         id != "Dordrecht_22",
         id != "Utrecht_13")


boxplot(all_polys$area_m2,
        main = "Boxplot of area")
boxplot(all_polys$barrier_ratio,
        main = "Boxplot of barrier ratio")

all_polys <- all_polys %>%
  mutate(shape_compactness = (4 * pi * area_m2) / (perimeter_m ^ 2))

boxplot(all_polys$amenity_density_t,
        main = "Boxplot of amenity density")
boxplot(all_polys$avg_flood_risk,
        main = "Boxplot of flood risk")
boxplot(all_polys$avg_no2_pollution,
        main = "Boxplot of no2 pollution")
boxplot(all_polys$towards_idx,
        main = "Boxplot of towards connectivity")
boxplot(all_polys$longitudinal_idx,
        main = "Boxplot of longitudinal connectivity")


top3_waterway_ratio <- all_polys %>%
  slice_max(waterway_ratio, n = 3, with_ties = FALSE)



boxplot(all_polys$waterway_ratio,
        main = "Boxplot of waterway ratio")

st_write(all_polys, "05_data_output/segment_gathered/260408.gpkg", append = FALSE)
