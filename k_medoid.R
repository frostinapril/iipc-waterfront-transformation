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
library(mclust)
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))


version <- "260428"
file_path <- paste0("05_data_output/segment_gathered/260421.gpkg")
dsn <- paste0("05_data_output/segment_gathered/", version, "_medoid.gpkg")
# my_colors <- c("1" = "#b2df8a", "2" = "#2f2f2f", "3" = "#fdbf6f", "4" = "#ff7f00",
#                "5" = "#a6cee3", "6" = "#00204d", "7" = "#ff6546")
# my_colors <- c("1" = "#b2df8a", "2" = "#2f2f2f", "3" = "#fdbf6f", "4" = "#ff7f00",
#                "5" = "#a6cee3", "6" = "#00204d")
my_colors <- c("1" = "#b2df8a", "2" = "#2f2f2f", "3" = "#fdbf6f", "4" = "#ff7f00",
               "5" = "#a6cee3")
#my_colors <- c("1" = "#b2df8a", "2" = "#2f2f2f", "3" = "#a6cee3", "4" = "#ff7f00")
#my_colors <- c("1" = "#b2df8a", "2" = "#2f2f2f", "3" = "#a6cee3")





segment <- st_read(file_path)

variable <- segment %>%
  st_drop_geometry() %>%
  select(id, urbanity_seg, center_reachability, towards_idx, waterway_ratio, urbanity_contrast, 
         industrial_land_ratio, ndvi_mean)

variable <- variable %>%
  mutate(across(-id, ~ ifelse(is.infinite(.), NA, .))) %>%
  na.omit()
x_scaled <- scale(variable %>% select(-id))
set.seed(1)
km <- pam(x_scaled, k = 5)
cluster_tbl <- tibble(
  id = variable$id,
  cluster = km$clustering 
)
segment <- segment %>%
  left_join(cluster_tbl, by = "id")
st_write(segment, dsn, delete_layer = TRUE)

cluster_result <- st_read(dsn)
cluster_result_relevant <- cluster_result %>%
  select(id, urbanity_seg, center_reachability, towards_idx, waterway_ratio,
         urbanity_contrast, ndvi_mean, industrial_land_ratio, cluster) %>%
  filter(!is.na(cluster))

cluster_clean <- cluster_result_relevant %>%
  st_drop_geometry() %>%
  select(-id, -cluster) %>%
  mutate(across(everything(), ~ as.numeric(scale(.))))

pca_res <- prcomp(cluster_clean, scale. = TRUE)
fviz_pca_ind(pca_res,
             label = "none", # Hide labels if too crowded
             habillage = cluster_result_relevant$cluster, # Color by cluster
             addEllipses = TRUE, 
             palette = my_colors) + 
  labs(title = "PCA: Cluster Separation")
fviz_pca_var(pca_res, col.var = "black", repel = TRUE) +
  labs(title = "PCA: Determining Factors")
fviz_contrib(pca_res, choice = "var", axes = 2, fill = "steelblue")



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


# 1. Prepare storage
results <- data.frame(k = 1:10, objective = numeric(10), sil = numeric(10))

# 2. Loop through k = 2 to 10 
# (PAM requires at least 2 clusters to calculate Silhouette)
for (i in 2:10) {
  km_test <- pam(cluster_clean, k = i)
  
  # Use the 'objective' value for the Elbow Method
  results$objective[i] <- km_test$objective["build"]
  
  # Silhouette info is built into the pam object
  results$sil[i] <- km_test$silinfo$avg.width
}

# Plotting the Elbow (using Objective function)
ggplot(results, aes(x = k, y = objective)) +
  geom_line() + geom_point() +
  labs(title = "K-medoids Elbow Method", y = "Objective Function Value") +
  theme_minimal()

# Plotting Silhouette
ggplot(results[-1, ], aes(x = k, y = sil)) +
  geom_line(color = "blue") + geom_point(color = "blue") +
  labs(title = "K-medoids Silhouette Method", y = "Average Silhouette Width") +
  theme_minimal()

