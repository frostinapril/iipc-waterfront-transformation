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
library(clustMixType)
library(factoextra)
library(FactoMineR)
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

file_path <- "05_data_output/port_network_info/figures.gpkg"
cities <- st_read(file_path)

cities_selected <- cities %>%
  select(expansion_capacity, diversity, scale, sector_gdp_trend, 
         port_gdp_trend, multi_modal_connection, dynamics_ratio, geom)
str(cities_selected)

cities_selected_clean <- cities_selected %>%
  st_drop_geometry() %>%
  mutate(across(where(is.character), as.factor))
cities_selected_clean <- cities_selected_clean %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))


# 1. Prepare a vector to store the costs (within-cluster distance)
costs <- numeric(8) 

# 2. Run a loop for k = 1 to 8
# We use nstart > 1 to ensure we find a stable cost for each k
for (i in 1:8) {
  k_test <- kproto(cities_selected_clean, k = i, nstart = 5, verbose = FALSE, lambda = 10)
  costs[i] <- k_test$tot.withinss
}

# 3. Create a data frame for plotting
elbow_data <- data.frame(k = 1:8, cost = costs)

# 4. Plot the results
ggplot(elbow_data, aes(x = k, y = cost)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Elbow Method for K-Prototypes",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Distance") +
  theme_minimal()


#k_res_heavy_cat <- kproto(cities_selected_clean, k = 3, nstart = 5, lambda = 10)
#cities$cluster_2 <- as.factor(k_res_heavy_cat$cluster)
# clprofiles(k_res_heavy_cat, cities_selected_clean)



set.seed(123) # For reproducibility
suggested_lambda <- lambdaest(cities_selected_clean)
print(suggested_lambda)

k_res <- kproto(cities_selected_clean, k = 3, nstart = 5, lambda = suggested_lambda)
cities$cluster <- as.factor(k_res$cluster)
clprofiles(k_res, cities_selected_clean)



# 1. Run FAMD on your clustered data (excluding the 'cluster' column itself)
res.famd <- FAMD(cities_selected_clean, graph = FALSE)

# 2. Visualize Variable Contributions
# This shows you which variables have the most 'power' in the model
fviz_famd_var(res.famd, repel = TRUE)

# 3. See how the clusters align with the variables
fviz_famd_ind(res.famd, 
              habillage = cities$cluster, # Color by your kproto clusters
              addEllipses = TRUE)






st_write(cities, file_path, delete_layer = TRUE)
