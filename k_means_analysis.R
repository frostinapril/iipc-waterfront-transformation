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
library(factoextra)
library(cluster)
library(fmsb)
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))


version <- "260502"
result_path <- paste0("05_data_output/segment_gathered/", version, "_means.gpkg")
# my_colors <- c("1" = "#b2df8a", "2" = "#2f2f2f", "3" = "#fdbf6f", "4" = "#ff7f00",
#                "5" = "#a6cee3", "6" = "#00204d", "7" = "#ff6546")
# my_colors <- c("1" = "#175045", "2" = "#2f2f2f", "3" = "#ff7f00", "4" = "#b2df8a",
#                "5" = "#fdbf6f", "6" = "#1f78b4")
my_colors <- c("1" = "#2f2f2f", "2" = "#b2df8a", "3" = "#1f78b4", "4" = "#ff7f00", 
               "5" = "#fdbf6f")
#my_colors <- c("1" = "#1f78b4", "2" = "#ff7f00", "3" = "#b2df8a", "4" = "#2f2f2f")
#my_colors <- c("1" = "#b2df8a", "2" = "#2f2f2f", "3" = "#a6cee3", "4" = "#ff7f00")
#my_colors <- c("1" = "#b2df8a", "2" = "#2f2f2f", "3" = "#a6cee3")
cluster_result <- st_read(result_path)
cluster_result <- cluster_result %>%
  mutate(
    fsi = ifelse(is.na(fsi) , 0, fsi),
    #gsi = ifelse(is.na(gsi) , 0, gsi),
    mxi = ifelse(is.na(mxi) , 0, mxi)
  )
cluster_result_relevant <- cluster_result %>%
  select(id, fsi, center_reachability, ndvi_mean, waterway_ratio, mxi, 
         accessibility_index, cluster) %>%
  filter(!is.na(cluster))

# cluster_result_pheno <- cluster_result %>%
#   select(id, cluster, amenity_density_t, industrial_land_ratio,
#          residential_land_ratio, amenity_density_t, towards_idx, waterfront_path_idx) %>%
#   filter(!is.na(cluster))


# scattered_industrial <- cluster_result_relevant %>%
#   filter(cluster == 2 & industrial_land_ratio > 0.3)
# st_write(scattered_industrial, "05_data_output/segment_gathered/260426_scattered.gpkg", delete_layer = TRUE)



cluster_clean <- cluster_result_relevant %>%
  st_drop_geometry() %>%
  select(-id, -cluster) %>%
  mutate(across(everything(), ~ as.numeric(scale(.))))

X_scaled_df <- as.data.frame(cluster_clean) %>%
  mutate(
    id = cluster_result_relevant$id,
    cluster = cluster_result_relevant$cluster
  )

centers_scaled <- X_scaled_df %>%
  group_by(cluster) %>%
  summarise(
    across(where(is.numeric) & -id, mean), 
    .groups = "drop"
  )

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
  left_join(centers_long, by = c("cluster", "variable")) %>%
  mutate(diff2 = (value - center_value)^2) %>%
  group_by(id, cluster) %>%
  summarise(
    dist_to_center = sqrt(sum(diff2)),
    .groups = "drop"
  )

closest_5_segments <- dist_per_segment %>%
  group_by(cluster) %>%
  slice_min(dist_to_center, n = 5, with_ties = FALSE) %>%
  ungroup()

closest_5_segments_sf <- closest_5_segments %>%
  left_join(
    cluster_result_relevant %>% select(id), # sf preserves geom automatically here
    by = "id"
  ) %>%
  st_as_sf()

dsn_closest <- paste0("05_data_output/segment_gathered/", version, "_closest.gpkg")
st_write(closest_5_segments_sf, dsn_closest, delete_layer = TRUE)


# PCA analysis
# MAHALANOBIS ANALYSIS
morph_vars <- c("fsi", "center_reachability", "mxi", "ndvi_mean", "waterway_ratio", 
                "accessibility_index")
# 2. Function to calculate Mahalanobis for a data frame
calc_mahalanobis <- function(df) {
  # 1. Drop geometry and filter for numeric columns ONLY
  # This prevents the 'x must be numeric' error
  numeric_data <- df %>% 
    st_drop_geometry() %>% 
    select(all_of(morph_vars)) %>%
    select(where(is.numeric))
  
  # 2. Calculate the distance
  d2 <- mahalanobis(numeric_data, 
                    colMeans(numeric_data), 
                    cov(numeric_data))
  
  # 3. Add result back to the original dataframe
  df$mahalanobis_dist <- d2
  return(df)
}

cluster_clean_mahalanobis <- cluster_result %>%
  group_by(cluster) %>%
  group_split() %>%
  lapply(calc_mahalanobis) %>%
  bind_rows()

# 4. Set the Threshold
# We use the Chi-Square distribution (p = 0.975 for 95% confidence interval)
# df = number of morphological variables used
cutoff <- qchisq(0.975, df = length(morph_vars))

# 5. Filter for Outliers
# Points with distance > cutoff are multivariate outliers
cluster_clean_mahalanobis <- cluster_clean_mahalanobis %>%
  mutate(is_outlier = mahalanobis_dist > cutoff)

# 6. Final Data for Silhouette Calculation
# Use this dataset for your silhouette analysis
df_for_silhouette <- cluster_clean_mahalanobis %>% 
  filter(is_outlier == FALSE)

silhouette_clean <- df_for_silhouette %>%
  st_drop_geometry() %>%
  select(fsi, center_reachability, ndvi_mean, waterway_ratio, mxi, accessibility_index) %>%
  mutate(across(everything(), ~ as.numeric(scale(.))))

pca_res <- prcomp(silhouette_clean, scale. = TRUE)
fviz_pca_ind(pca_res,
             label = "none", # Hide labels if too crowded
             habillage = df_for_silhouette$cluster, # Color by cluster
             addEllipses = TRUE, 
             palette = my_colors) + 
  labs(title = "PCA: Cluster Separation")
fviz_pca_var(pca_res, col.var = "black", repel = TRUE) +
  labs(title = "PCA: Determining Factors")
fviz_contrib(pca_res, choice = "var", axes = 2, fill = "steelblue")
fviz_contrib(pca_res, choice = "var", axes = 1, fill = "steelblue")



df_long <- cluster_result_relevant %>%
  st_drop_geometry() %>%
  mutate(cluster = as.factor(cluster)) %>%
  pivot_longer(cols = where(is.numeric), 
               names_to = "Variable", 
               values_to = "Value")
ggplot(df_long, aes(x = cluster, y = Value, fill = cluster)) +
  geom_boxplot(alpha = 0.7) +
  #geom_jitter(width = 0.2, alpha = 0.5, size = 1) + # Add points to see distribution
  facet_wrap(~ Variable, scales = "free_y") + # Each variable gets its own y-axis scale
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  labs(title = "Variable Distribution by Cluster",
       x = "Cluster ID",
       y = "Value") +
  theme(legend.position = "none")

df_long_pheno <- cluster_result_pheno %>%
  st_drop_geometry() %>%
  mutate(cluster = as.factor(cluster)) %>%
  pivot_longer(cols = where(is.numeric), 
               names_to = "Variable", 
               values_to = "Value")
ggplot(df_long_pheno, aes(x = cluster, y = Value, fill = cluster)) +
  geom_boxplot(alpha = 0.7) +
  #geom_jitter(width = 0.2, alpha = 0.5, size = 1) + # Add points to see distribution
  facet_wrap(~ Variable, scales = "free_y") + # Each variable gets its own y-axis scale
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  labs(title = "Variable Distribution by Cluster",
       x = "Cluster ID",
       y = "Value") +
  theme(legend.position = "none")



# 1. Prepare storage for results
results <- data.frame(k = 1:10, wss = numeric(10), sil = numeric(10))

# 2. Loop through k = 1 to 10
# Note: Silhouette cannot be calculated for k=1
for (i in 1:10) {
  km_test <- kmeans(silhouette_clean, centers = i, nstart = 25)
  results$wss[i] <- km_test$tot.withinss
  
  if (i > 1) {
    sil_test <- silhouette(km_test$cluster, dist(silhouette_clean))
    results$sil[i] <- summary(sil_test)$avg.width
  }
}
ggplot(results, aes(x = k, y = wss)) +
  geom_line() + geom_point() +
  labs(title = "Elbow Method", y = "Total Within-Cluster SS") +
  theme_minimal()
ggplot(results[-1, ], aes(x = k, y = sil)) +
  geom_line(color = "blue") + geom_point(color = "blue") +
  labs(title = "Silhouette Method", y = "Average Silhouette Width") +
  theme_minimal()


# STARPLOT ANALYSIS
# 1. Prepare your data: Group by cluster and calculate the MEAN of each variable
# Assuming your clusters are in a column called 'cluster_id'
cluster_profiles <- cluster_result %>%
  filter(!is.na(cluster)) %>%
  st_drop_geometry() %>%
  group_by(cluster) %>%
  summarise(
    across(c(residential_land_ratio, amenity_density_t, industrial_land_ratio, 
             towards_idx, waterfront_path_idx), mean)
  )

# 2. Normalize/Scale the profiles so they fit on the chart
# We apply scale() to the columns, then convert to a data frame
scaled_profiles <- cluster_profiles %>%
  mutate(across(-cluster, ~scale(.)[,1]))

# 3. fmsb requires a data frame with MIN and MAX rows at the top
# Let's set min to -2 and max to 2 (the range of z-scores)
max_min <- data.frame(
  cluster = c("max", "min"), residential_land_ratio = c(2, -2), 
  amenity_density_t = c(2, -2), industrial_land_ratio = c(2, -2), 
  waterfront_path_idx = c(2, -2), towards_idx = c(2, -2)
)

# Combine and remove the ID column for plotting
plot_data <- rbind(max_min, scaled_profiles) %>% select(-cluster)

# 4. Plotting (Example for Cluster 1)
# Note: You usually need to loop through this to plot all clusters or overlay them
radarchart(plot_data[c(1, 2, 3),], # Plotting max, min, and Cluster 1
           title="Cluster 1 Low Urbanity Agglomeration",
           pcol="#2f2f2f", plwd=2, cglcol="grey", cglty=1)

radarchart(plot_data[c(1, 2, 4),], # Plotting max, min, and Cluster 2
           title="Cluster 2 Green",
           pcol="#b2df8a", plwd=2, cglcol="grey", cglty=1)

radarchart(plot_data[c(1, 2, 5),], # Plotting max, min, and Cluster 3
           title="Cluster 3 High Potential Vibrant Waterfront",
           pcol="#1f78b4", plwd=2, cglcol="grey", cglty=1)

radarchart(plot_data[c(1, 2, 6),], # Plotting max, min, and Cluster 4
           title="Cluster 4 Urban Core",
           pcol="#ff7f00", plwd=2, cglcol="grey", cglty=1)

radarchart(plot_data[c(1, 2, 7),], # Plotting max, min, and Cluster 5
           title="Cluster 5 Average Urban Context",
           pcol="#fdbf6f", plwd=2, cglcol="grey", cglty=1)

# Define semi-transparent colors (r, g, b, alpha)
# The 0.2 is the transparency (alpha)
# fill_cols <- c(rgb(1, 0.5, 0, 0.2), rgb(0.2, 0.2, 0.2, 0.2), 
#                rgb(0.7, 0.9, 0.5, 0.2), rgb(0.1, 0.4, 0.7, 0.2))
border_cols <- c("#2f2f2f", "#b2df8a", "#1f78b4", "#ff7f00", "#fdbf6f")

# Plot all data at once
radarchart(plot_data, 
           pcol = border_cols, 
           pfcol = NA, 
           plwd = 3, 
           cglcol = "grey", 
           axistype = 1)

# Add a clean legend
legend(x = 1.3, y = 1.3, 
       legend = c("Low Urbanity Agglomeration", "Green", "High Potential Vibrant Waterfront", 
                  "Urban Core", "Average Urban Context"), 
       col = border_cols, lty = 1, lwd = 3, bty = "n", cex = 0.8)



# FIND THE LANDUSE OUTLIERS
cluster_norms <- cluster_result %>%
  group_by(cluster) %>%
  summarise(
    mean_ind = mean(industrial_land_ratio, na.rm = TRUE),
    sd_ind = sd(industrial_land_ratio, na.rm = TRUE),
    mean_density = mean(fsi, na.rm = TRUE),
    sd_density = sd(fsi, na.rm = TRUE),
    mean_res = mean(residential_land_ratio, na.rm = TRUE),
    sd_res = sd(residential_land_ratio, na.rm = TRUE),
    mean_amen = mean(amenity_density_t, na.rm = TRUE),
    sd_amen = sd(amenity_density_t, na.rm = TRUE),
    mean_longitudinal = mean(longitudinal_idx, na.rm = TRUE),
    sd_longitudinal = sd(longitudinal_idx, na.rm = TRUE),
    mean_ndvi = mean(ndvi_mean, na.rm = TRUE),
    sd_ndvi = sd(ndvi_mean, na.rm = TRUE),
    mean_island = mean(urbanity_contrast, na.rm = TRUE),
    sd_island = sd(urbanity_contrast, na.rm = TRUE),
    mean_lateral = mean(towards_idx, na.rm = TRUE),
    sd_lateral = sd(towards_idx, na.rm = TRUE)
  ) %>%
  st_drop_geometry()

cluster_result <- cluster_result %>%
  left_join(cluster_norms, by = "cluster") %>%
  # 3. Calculate the Dissonance Score (Z-score)
  mutate(dissonance_ind = (industrial_land_ratio - mean_ind) / sd_ind,
         dissonance_res = (residential_land_ratio - mean_res) / sd_res,
         dissonance_density = (fsi - mean_density) / sd_density,
         dissonance_amen = (amenity_density_t - mean_amen) / sd_amen,
         dissonance_longitudinal = (longitudinal_idx - mean_longitudinal) / sd_longitudinal,
         dissonance_island = (urbanity_contrast - mean_island) / sd_island,
         dissonance_ndvi = (ndvi_mean - mean_ndvi) / sd_ndvi,
         dissonance_lateral = (towards_idx - mean_lateral) / sd_lateral)

function_unbalance_ind <- cluster_result %>%
  filter(cluster %in% c(3, 4, 5) & dissonance_ind > 1.5) %>%
  select(id) %>%
  mutate(type = "ind_too_much")

function_unbalance_res <- cluster_result %>%
  filter(cluster == 1 & dissonance_res > 1.0) %>%
  select(id) %>%
  mutate(type = "res_bad_performance")

amen_lack <- cluster_result %>%
  filter(cluster == 3 & dissonance_amen < -1.0) %>%
  select(id) %>%
  mutate(type = "amen_lack")

lateral_lack <- cluster_result %>%
  filter(cluster == 3 & dissonance_lateral < -1.0) %>%
  select(id) %>%
  mutate(type = "lateral_lack")

density_lack <- cluster_result %>%
  filter(cluster == 3 & dissonance_density < -1.0) %>%
  select(id) %>%
  mutate(type = "density_lack")


longitudinal_lack <- cluster_result %>%
  filter(cluster == 3 & dissonance_longitudinal < -1.0) %>%
  select(id) %>%
  mutate(type = "longitudinal_lack")

# site_counts <- potential_sites %>%
#   group_by(cluster) %>%
#   summarise(count = n())
# print(site_counts)

unbalanced <- bind_rows(function_unbalance_ind, amen_lack, function_unbalance_res,
                        lateral_lack, longitudinal_lack, density_lack)

dsn_unbalanced <- paste0("05_data_output/segment_gathered/", version, "_unbalanced.gpkg")
st_write(unbalanced, dsn_unbalanced, delete_layer = TRUE)











# UNBALANCED SEGMENTS
waterfront <- cluster_result %>%
  filter(cluster == 3)
mxi_threshold <- quantile(waterfront$mxi, 0.5)
low_mxi <- waterfront %>%
  filter(mxi < mxi_threshold & industrial_land_ratio > 0.5)
dsn_lowmxi <- paste0("05_data_output/segment_gathered/", version, "_lowmxi.gpkg")
st_write(low_mxi, dsn_lowmxi, delete_layer = TRUE)

ind_threshold <- quantile(cluster_result$industrial_land_ratio, 0.75)
urb_threshold <- quantile(cluster_result$urbanity_index, 0.75)
unbalanced <- cluster_result %>%
  filter((cluster == 1 & urbanity_land_ratio >= urb_threshold) | (cluster == 3 & industrial_land_ratio >= ind_threshold))
dsn_unbalanced <- paste0("05_data_output/segment_gathered/", version, "_unbalanced.gpkg")
st_write(unbalanced, dsn_unbalanced, delete_layer = TRUE)

# URBAN GROWTH TREND
distance_threshold <- quantile(cluster_result$center_percentile, 0.75)
probable <- cluster_result %>%
  filter(cluster == 2 & center_percentile >= distance_threshold)
dsn_probable <- paste0("05_data_output/segment_gathered/", version, "_probable.gpkg")
st_write(probable, dsn_probable, delete_layer = TRUE)
