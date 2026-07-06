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
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))


version <- "260317_2"
result_path <- paste0("05_data_output/segment_gathered/", version, "_means.gpkg")
cluster_result <- st_read(result_path)
cluster_result_relevant <- cluster_result %>%
  select(id, fsi, mxi, env_impact_km, gsi, amenity_density_t, ndvi_mean, 
         reachable_ratio, center_percentile, pedestrian_density, cluster) %>%
  filter(!is.na(cluster)) %>%
  filter(id != "Dordrecht_17" & id != "Oosterhout_12" & id != "Oosterhout_20")


# get the box plot of variables
cluster_result_relevant_long_1 <- cluster_result_relevant %>%
  pivot_longer(
    cols = where(is.numeric) & !all_of("cluster"),
    names_to = "variable",
    values_to = "value"
  )

ggplot(cluster_result_relevant_long_1, aes(x = factor(cluster), y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y")



# get the average value table
analysis <- cluster_result_relevant %>%
  filter(!is.na(cluster)) %>%                 # 1. drop NA clusters
  group_by(cluster) %>%                       # 2. group by cluster
  summarise(
    n = n(),   # number of entries in each cluster
    across(
      where(is.numeric), 
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

analysis_number <- analysis %>%
  st_drop_geometry()

write.csv(analysis_number, "05_data_output/correlation_matrix/260317_2_result_analysis_means_6.csv", row.names = FALSE)


# get the five most representative segments
cluster_result_relevant_wg <- cluster_result_relevant %>%
  st_drop_geometry()
X_scaled <- cluster_result_relevant_wg %>%
  select(where(is.numeric), -cluster) %>%
  scale()
X_scaled_df <- as.data.frame(X_scaled) %>%
  mutate(
    id = cluster_result_relevant_wg$id,
    cluster = cluster_result_relevant_wg$cluster
  )
centers_scaled <- X_scaled_df %>%
  group_by(cluster) %>%
  summarise(across(-id, mean), .groups = "drop")

long <- X_scaled_df %>%
  pivot_longer(
    -c(id, cluster),
    names_to = "variable",
    values_to = "value"
  )

centers_long <- centers_scaled %>%
  pivot_longer(
    -cluster,
    names_to = "variable",
    values_to = "center_value"
  )

dist_per_segment <- long %>%
  left_join(centers_long,
            by = c("cluster", "variable")) %>%
  mutate(diff2 = (value - center_value)^2) %>%
  group_by(id, cluster) %>%
  summarise(
    dist_to_center = sqrt(sum(diff2)),
    .groups = "drop"
  )

closest_5_segments <- dist_per_segment %>%
  group_by(cluster) %>%
  slice_min(dist_to_center, n = 3) %>%
  ungroup()


centers <- cluster_result_relevant %>%        
  group_by(cluster) %>%                       # group by cluster
  summarise(
    across(
      where(is.numeric), 
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

closest_5_segments_sf <- closest_5_segments %>%
  left_join(
    cluster_result_relevant %>% select(id, geom),
    by = "id"
  ) %>%
  st_as_sf()

dsn <- paste0("05_data_output/segment_gathered/", version, "_closest.gpkg")
st_write(closest_5_segments_sf, dsn, delete_layer = TRUE)


# get the three most deviated segments
cluster_result_relevant_wg <- cluster_result_relevant %>%
  st_drop_geometry()
X_scaled <- cluster_result_relevant_wg %>%
  select(where(is.numeric), -cluster) %>%
  scale()
X_scaled_df <- as.data.frame(X_scaled) %>%
  mutate(
    id = cluster_result_relevant_wg$id,
    cluster = cluster_result_relevant_wg$cluster
  )
centers_scaled <- X_scaled_df %>%
  group_by(cluster) %>%
  summarise(across(-id, mean), .groups = "drop")

long <- X_scaled_df %>%
  pivot_longer(
    -c(id, cluster),
    names_to = "variable",
    values_to = "value"
  )

centers_long <- centers_scaled %>%
  pivot_longer(
    -cluster,
    names_to = "variable",
    values_to = "center_value"
  )

dist_per_segment <- long %>%
  left_join(centers_long,
            by = c("cluster", "variable")) %>%
  mutate(diff2 = (value - center_value)^2) %>%
  group_by(id, cluster) %>%
  summarise(
    dist_to_center = sqrt(sum(diff2)),
    .groups = "drop"
  )

closest_5_segments <- dist_per_segment %>%
  group_by(cluster) %>%
  slice_max(dist_to_center, n = 3) %>%
  ungroup()
centers <- cluster_result_relevant %>%        
  group_by(cluster) %>%                       # group by cluster
  summarise(
    across(
      where(is.numeric), 
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

closest_5_segments_sf <- closest_5_segments %>%
  left_join(
    cluster_result_relevant %>% select(id, geom),
    by = "id"
  ) %>%
  st_as_sf()
dsn <- paste0("05_data_output/segment_gathered/", version, "_farthest.gpkg")
st_write(closest_5_segments_sf, dsn, delete_layer = TRUE)


# compare cluster result
version_1 <- "260303_3"
result_path_1 <- paste0("05_data_output/segment_gathered/", version_1, ".gpkg")
cluster_result_1 <- st_read(result_path_1)

version_2 <- "260303_6"
result_path_2 <- paste0("05_data_output/segment_gathered/", version_2, ".gpkg")
cluster_result_2 <- st_read(result_path_2)

cluster_result_2_fixed <- cluster_result_2 %>%
  mutate(
    cluster = recode(cluster,
                     `1` = 1L,
                     `3` = 2L,
                     `4` = 3L,
                     `7` = 4L,
                     `6` = 5L,
                     `5` = 6L,
                     `2` = 7L
    )
  )



c1 <- cluster_result_1 %>%
  st_drop_geometry() %>%
  select(id, cluster_1 = cluster)

c2 <- cluster_result_2_fixed %>%
  st_drop_geometry() %>%
  select(id, cluster_2 = cluster)
# c2 <- c2 %>%
#   mutate(
#     cluster_2 = case_when(
#       cluster_2 == 2 ~ 4L,
#       cluster_2 == 4 ~ 2L,
#       TRUE ~ cluster_2
#     )
#   )

cmp <- c1 %>%
  inner_join(c2, by = "id")
cmp <- cmp %>%
  filter(!is.na(cluster_1), !is.na(cluster_2))
cmp %>%
  summarise(
    total = n(),
    different = sum(cluster_1 != cluster_2),
    same = sum(cluster_1 == cluster_2)
  )
cmp %>%
  count(cluster_1, cluster_2)

mismatched_sf <- cmp %>%
  filter(cluster_1 != cluster_2) %>%
  left_join(
    cluster_result_1 %>% select(id, geom),
    by = "id"
  ) %>%
  st_as_sf()

dsn <- paste0("05_data_output/segment_gathered/", version, "_mismatched.gpkg")
st_write(mismatched_sf, dsn, delete_layer = TRUE)
