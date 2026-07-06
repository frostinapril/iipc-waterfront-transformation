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
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))


city_list <- c("Alblasserdam", "Arnhem", "Breda", "Dordrecht", "Eindhoven", 
               "Geertruidenberg", "Gennep", "Gorinchem", "Helmond", "Hendrik-Ido-Ambacht", "Hertogenbosch",
               "Meierijstad", "Nieuwegein", "Nijmegen", "Oosterhout", "Oss", "Papendrecht", "Ridderkerk", 
               "Roermond", "Roosendaal", "Sittard-Geleen", "Sliedrecht", "Tiel", "Tilburg", "Utrecht", 
               "Venlo", "Vijfheerenlanden", "Waalwijk", "Wageningen", "Zwijndrecht")

version <- "260502"
file_path <- paste0("05_data_output/segment_gathered/260428.gpkg")
dsn <- paste0("05_data_output/segment_gathered/", version, "_means.gpkg")



# segment <- segment %>%
#   mutate(
#     access_constrained = (towards_idx) / (1 + barrier_ratio),
#     shape_compactness = (4 * pi * area) / (perimeter^2)
#   )
#st_write(segment, file_path, delete_layer = TRUE)

# dsn_csv <- paste0("05_data_output/correlation_matrix/", version, "_means.csv")
# segment_wg <- st_read(dsn)
# segment_csv <- segment_wg %>%
#   st_drop_geometry() %>%
#   select(id, fsi, mxi, env_impact_km, gsi, amenity_density_t, ndvi_mean, 
#          reachable_ratio, center_percentile, pedestrian_density)
# select(id, fsi, mxi, env_impact_km, gsi, pedestrian_density, center_percentile, 
#        reachable_ratio, amenity_density_t)    #260414
# write.csv(segment_csv, dsn_csv, append = FALSE)
# dsn_2 <- paste0("05_data_output/segment_gathered/", version, "_probability.gpkg")
# dsn_3 <- paste0("05_data_output/segment_gathered/", version, "_probability_fuzzy.gpkg")
# cluster_result_relevant <- cluster_result %>%
#   select(id, ndvi_mean, towards_idx, avg_no2_pollution, avg_flood_risk, waterway_ratio, 
#          area_m2, cluster)


segment <- st_read(file_path)
# segment <- segment %>%
#   select(-cluster.x, -cluster.y)
# top3_osr <- segment %>%
#   slice_max(osr_log, n = 3, with_ties = FALSE)

# segment_wo <- segment %>%
#   filter(id != "Dordrecht_17" & id != "Oosterhout_20" & id != "Oosterhout_12")


# k_means
variable <- segment %>%
  st_drop_geometry() %>%
  select(id, fsi, center_reachability, ndvi_mean, waterway_ratio, mxi, accessibility_index)
variable <- variable %>%
  mutate(
    fsi = ifelse(is.na(fsi) , 0, fsi),
    #gsi = ifelse(is.na(gsi) , 0, gsi),
    mxi = ifelse(is.na(mxi) , 0, mxi)
  )
variable <- variable %>%
  mutate(across(-id, ~ ifelse(is.infinite(.), NA, .))) %>%
  na.omit()
# variable <- variable %>%
#   filter(id != "Dordrecht_17" & id != "Oosterhout_20" & id != "Oosterhout_12")
x_scaled <- scale(variable %>% select(-id))
# x_weighted <- x_scaled %>%
#   as.data.frame() %>% # This is the missing link!
#   # No need to scale again, it's already scaled
#   mutate(
#     towards_idx = towards_idx * 2,
#     longitudinal_idx = longitudinal_idx * 2
#   )


set.seed(1)
km <-
  kmeans(
    x_scaled,
    centers = 5,
    nstart = 200
  )
cluster_tbl <- tibble(
  id = variable$id,
  cluster = km$cluster
)
segment <- segment %>%
  left_join(cluster_tbl, by = "id")
st_write(segment, dsn, delete_layer = TRUE)

wcss <- numeric(10)  # to store WCSS for k = 1:10
for(k in 1:10){
  km <- kmeans(x_scaled, centers = k, nstart = 200)
  wcss[k] <- km$tot.withinss
}

# Plot
elbow_df <- data.frame(k = 1:10, WCSS = wcss)

ggplot(elbow_df, aes(x = k, y = WCSS)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 4, linetype = "dashed", color = "red") + # <- highlight best k
  labs(title = "Elbow Method", x = "Number of clusters", y = "Within-Cluster SS")




# k-median
# variable <- segment_wo %>%
#   st_drop_geometry() %>%
#   select(id, fsi, osr_log, mxi, env_impact_log, gsi, l, 
#          amenity_density_t, public_ratio_log, reachable_ratio, center_log)
# variable <- variable %>%
#   mutate(across(-id, ~ ifelse(is.infinite(.), NA, .))) %>%
#   na.omit()
# x_scaled <- scale(variable %>% select(-id))
# set.seed(123)
# k <- 5  # number of clusters
# kmed_result <- pam(x_scaled, k = k, metric = "manhattan")
# cluster_tbl <- tibble(
#   id = variable$id,
#   cluster = kmed_result$clustering
# )
# segment <- segment %>%
#   left_join(cluster_tbl, by = "id")
# st_write(segment, dsn, delete_layer = TRUE)
# 
# # plot elbow line
# wss <- numeric(10)  # store WSS for k = 1..10
# k.max <- 10
# 
# for (k in 1:k.max) {
#   set.seed(123)
#   km <- kmeans(x_scaled, centers = k, nstart = 25)
#   wss[k] <- km$tot.withinss
# }
# 
# elbow_df <- data.frame(k = 1:10, WCSS = wss)
# 
# ggplot(elbow_df, aes(x = k, y = WCSS)) +
#   geom_point() +
#   geom_line() +
#   geom_vline(xintercept = 5, linetype = "dashed", color = "red") + # <- highlight best k
#   labs(title = "Elbow Method", x = "Number of clusters", y = "Within-Cluster SS")


# k probability
variable <- segment %>%
  st_drop_geometry() %>%
  select(id, fsi, mxi, env_impact_km, gsi, amenity_density_t, ndvi_mean, 
         reachable_ratio, center_percentile, pedestrian_density)
variable <- variable %>%
  mutate(
    fsi = ifelse(is.na(fsi) , 0, fsi),
    gsi = ifelse(is.na(gsi) , 0, gsi),
    mxi = ifelse(is.na(mxi) , 0, mxi)
  )
variable <- variable %>%
  mutate(across(-id, ~ ifelse(is.infinite(.), NA, .))) %>%
  na.omit()
variable <- variable %>%
  filter(id != "Dordrecht_16" & id != "Oosterhout_25" & id != "Oosterhout_16")

vars_scaled <- scale(variable %>% select(-id))
model <- Mclust(vars_scaled, G = 5)

prob_df <- as.data.frame(model$z)
colnames(prob_df) <- paste0("prob_", 1:ncol(prob_df))

cluster_tbl <- tibble(
  id = variable$id,
  cluster = model$classification
) %>%
  bind_cols(prob_df)







segment <- segment %>%
  left_join(cluster_tbl, by = "id")

prob_cols <- grep("^prob_", names(segment), value = TRUE)
segment <- segment %>%
  rowwise() %>%
  mutate(uncertainty = 1 - max(c_across(all_of(prob_cols)), na.rm = TRUE)) %>%
  ungroup()

st_write(segment, dsn_2, delete_layer = TRUE)


model <- Mclust(vars_scaled, G = 9)
table(model$classification)

plot(model, what = "BIC")

segment_with_cluster <- segment %>%
  filter(!is.na(cluster))


ggplot(segment_with_cluster, aes(x = uncertainty)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Cluster Uncertainty", x = "Uncertainty", y = "Count")
summary(segment_with_cluster$uncertainty)
quantile(segment_with_cluster$uncertainty, probs = seq(0, 1, 0.1))

fuzzy_threshold <- 0.3

segment <- segment %>%
  mutate(
    fuzzy_zone = ifelse(uncertainty >= fuzzy_threshold, TRUE, FALSE)
  )
st_write(segment, dsn_3, delete_layer = TRUE)




# correlation matrix calculation
variable_without_id <- variable %>%
  select(-id)
M <- cor(variable_without_id, use = "pairwise.complete.obs")
M_df <- as.data.frame(M)
M_df <- cbind(Variable = rownames(M_df), M_df)
write.csv(M_df, "05_data_output/correlation_matrix/260317.csv", row.names = FALSE)

# convert matrix to long format
M_long <- M_df %>%
  select(-Variable) %>%
  tibble::rownames_to_column(var = "Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")

# plot heatmap
p <- ggplot(M_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix", x = "", y = "")
p

# remove diagonal (self-correlation = 1)
M_no_diag <- M
diag(M_no_diag) <- 0

# count strong correlations per variable
strong_threshold <- 0.6
strong_counts <- apply(M_no_diag, 1, function(x) sum(abs(x) >= strong_threshold))

# convert to a data frame and order
strong_summary <- data.frame(
  Variable = names(strong_counts),
  StrongCorCount = strong_counts
) |> 
  arrange(desc(StrongCorCount))
write.csv(strong_summary, "05_data_output/correlation_matrix/260224_strong_correlation.csv", row.names = FALSE)
