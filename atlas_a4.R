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
library(cluster)
library(mclust)
library(stringr)
library(ggpattern)
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

waterway_all <- st_read("01_data/02_nl_topography/pdok/waterway.gpkg")
port_cities <- st_read("05_data_output/city_selection/city_selection.gpkg")
port_cities[22, "statnaam"] <- "Hertogenbosch"
nation <- st_union(port_cities) %>%
  st_as_sf()
#pand <- st_read("01_data/01_port_city/BAG/bag-light.gpkg")
city_list <- c("Alblasserdam", "Arnhem", "Breda", "Dordrecht", "Eindhoven", 
               "Geertruidenberg", "Gennep", "Gorinchem", "Helmond", "Hendrik-Ido-Ambacht", "Hertogenbosch",
               "Meierijstad", "Nieuwegein", "Nijmegen", "Oosterhout", "Oss", "Papendrecht", "Ridderkerk", 
               "Roermond", "Roosendaal", "Sittard-Geleen", "Sliedrecht", "Tiel", "Tilburg", "Venlo", "Utrecht",
               "Vijfheerenlanden", "Waalwijk", "Wageningen", "Zwijndrecht")
all_segment <- st_read("05_data_output/segment_gathered/260502_means.gpkg") %>%
  select(id, geom) %>%
  filter(id != "Utrecht_1") %>%
  filter(id != "Utrecht_15") %>%
  filter(id != "Utrecht_14")
# cluster_result <- st_read("05_data_output/segment_gathered/260502_means.gpkg") %>%
#   st_drop_geometry() %>%
#   filter(id != "Utrecht_1") %>%
#   filter(id != "Utrecht_15") %>%
#   filter(id != "Utrecht_14")
# pand_relevant <- st_intersection(pand, all_segment)

# cluster_colors <- c(
#   "1" = "#2f2f2f", 
#   "2" = "#b2df8a", 
#   "3" = "#1f78b4", 
#   "4" = "#ff7f00", 
#   "5" = "#fdbf6f"  
# )
all_segment_landuse <- st_read("05_data_output/segment_gathered/landuse_means.gpkg") %>%
  filter(id != "Utrecht_1") %>%
  filter(id != "Utrecht_15") %>%
  filter(id != "Utrecht_14") %>%
  st_drop_geometry()

# plot the clustering result
for (city_name in city_list) {
  file_name <- paste0("03_presentation/A4_pre/atlas/", city_name, ".jpg")
  segment_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  segment <- st_read(segment_path)
  municipality <- port_cities %>%
    filter(statnaam == city_name)
  bbox <- st_bbox(municipality)
  bbox_polygon <- st_as_sfc(bbox) %>%
    st_as_sf()
  
  segment_cluster <- left_join(segment, cluster_result, by = "id") %>%
    select(id, cluster)
  
  segment_cluster_1 <- segment_cluster %>%
    filter(cluster == 1)
  pand_cluster_1 <- pand_relevant %>%
    st_intersection(segment_cluster_1)
  segment_cluster_3 <- segment_cluster %>%
    filter(cluster == 3)
  pand_cluster_3 <- pand_relevant %>%
    st_intersection(segment_cluster_3)
  
  p <- ggplot() +
    geom_sf(data = waterway_all, colour = "black", linewidth = 1) +
    geom_sf(data = municipality, color = "darkred", linewidth = 0.1, linetype = "dashed", fill = NA) +
    geom_sf(data = segment_cluster,  aes(color = factor(cluster)), linewidth = 0.2, 
            linetype = "dashed", fill = NA) +
    scale_fill_manual(values = cluster_colors) +
    geom_sf(data = pand_relevant, fill = "black", alpha = 0.1, color = NA) +
    geom_sf(data = segment_cluster_3, color = "black", linewidth = 0.2, fill = "white") +
    geom_sf(data = segment_cluster_1, color = "white", fill = "black", linewidth = 0.2) +
    geom_sf(data = pand_cluster_1, color = NA, fill = "white") +
    geom_sf(data = pand_cluster_3, color = NA, fill = "black") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(), 
      legend.position = "none"
    ) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             expand = FALSE, datum = st_crs(28992))
  
  xlim <- bbox["xmax"] - bbox["xmin"]
  ylim <- bbox["ymax"] - bbox["ymin"]
  ggsave(file_name, p, width = 12, height = 12, dpi = 300)
}

# plot the landuse structure
for (city_name in city_list) {
  file_name <- paste0("03_presentation/A4_pre/landuse_structure/", city_name, ".jpg")
  segment_path <- paste0("05_data_output/segment_a3/", city_name, ".gpkg")
  segment <- st_read(segment_path) %>%
    select(id, geom)
  municipality <- port_cities %>%
    filter(statnaam == city_name)
  bbox <- st_bbox(municipality)
  bbox_polygon <- st_as_sfc(bbox) %>%
    st_as_sf()
  
  segment_landuse <- left_join(segment, all_segment_landuse, by = "id") %>%
    select(id, urbanity_ratio, industrial_land_ratio)
  segment_industrial <- segment_landuse %>%
    filter(industrial_land_ratio >= 0.5)
  
  segment_urban <- segment_landuse %>%
    filter(urbanity_ratio >= 0.5)
  segment_other <- segment_landuse %>%
    filter(urbanity_ratio < 0.5 | industrial_land_ratio < 0.5)
  
  
  p <- ggplot() +
    geom_sf(data = municipality, color = "darkred", linewidth = 0.1, linetype = "dashed", fill = NA) +
    geom_sf(data = segment_other, color = "black", linewidth = 0.1, linetype = "dashed", fill = "white") +
    geom_sf(data = segment_urban, color = "black", linewidth = 0.2, fill = "white") +
    geom_sf_pattern(data = segment_industrial, color = "black", linewidth = 0.2, 
                    pattern = "stripe",          # Options: 'stripe', 'crosshatch', 'circle', etc.
                    pattern_fill = "black",      # Color of the stripes
                    pattern_angle = 45,          # Angle of the stripes (45 degrees)
                    pattern_density = 0.005,       # Thickness of the stripes
                    pattern_spacing = 0.005,      # Distance between stripes
                    fill = "white") +
    geom_sf(data = waterway_all, colour = "black", linewidth = 0.5) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(), 
      legend.position = "none"
    ) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             expand = FALSE, datum = st_crs(28992))
  
  xlim <- bbox["xmax"] - bbox["xmin"]
  ylim <- bbox["ymax"] - bbox["ymin"]
  ggsave(file_name, p, width = 6, height = 6, dpi = 300)
}


# plot the network
for (city_name in city_list) {
  file_name <- paste0("03_presentation/A4_pre/network/", city_name, ".jpg")
  municipality <- port_cities %>%
    filter(statnaam == city_name)
  bbox <- st_bbox(nation)
  bbox_polygon <- st_as_sfc(bbox) %>%
    st_as_sf()
  
  p <- ggplot() +
    geom_sf(data = waterway_all, colour = "black", linewidth = 0.3) +
    geom_sf(data = nation, color = "darkred", linewidth = 0.2, linetype = "dashed", fill = NA) +
    geom_sf(data = municipality, color = NA, fill = "black") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(), 
      legend.position = "none"
    ) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             expand = FALSE, datum = st_crs(28992))
  
  xlim <- bbox["xmax"] - bbox["xmin"]
  ylim <- bbox["ymax"] - bbox["ymin"]
  ggsave(file_name, p, width = 12, height = 12, dpi = 300)
}










