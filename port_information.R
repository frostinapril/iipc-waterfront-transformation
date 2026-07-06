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
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

city_list <- c("Alblasserdam", "Arnhem", "Breda", "Dordrecht", "Eindhoven", 
               "Geertruidenberg", "Gennep", "Gorinchem", "Helmond", "Hendrik-Ido-Ambacht", "'s-Hertogenbosch",
               "Meierijstad", "Nieuwegein", "Nijmegen", "Oosterhout", "Oss", "Papendrecht", "Ridderkerk", 
               "Roermond", "Roosendaal", "Sittard-Geleen", "Sliedrecht", "Tiel", "Tilburg", "Utrecht", 
               "Venlo", "Vijfheerenlanden", "Waalwijk", "Wageningen", "Zwijndrecht")

data_2024 <- read.csv("01_data/00_context_information/01_the_Netherlands/CBS/2024.csv", 
                 skip = 3,
                 sep = ";",
                 header = TRUE,
                 stringsAsFactors = FALSE) %>%
  select(-X, -X.1, -X.2)

data_2023 <- read.csv("01_data/00_context_information/01_the_Netherlands/CBS/2023.csv", 
                 skip = 3,
                 sep = ";",
                 header = TRUE,
                 stringsAsFactors = FALSE) %>%
  select(-X)

municipality <- st_read("01_data/02_nl_topography/pdok/municipality.gpkg")

lookup_2 <- data.frame(
  csv_name = c("Bergen (L)", "Gravenhage, 's-", "Hengelo (O)", "Hertogenbosch, 's-",
               "Ijsselstein", "Middelburg", "Stein", "Súdwest Fryslân", "Voorne aan zee"),
  gpk_name = c("Bergen (L.)", "'s-Gravenhage", "Hengelo (O.)", "'s-Hertogenbosch",
               "IJsselstein", "Middelburg (Z.)", "Stein (L.)", "Súdwest-Fryslân", "Voorne aan Zee")
)


lookup <- data.frame(csv_name = "Hertogenbosch, 's-", gpk_name = "'s-Hertogenbosch")
data_2023 <- data_2023 %>%
  left_join(lookup, by = c("Gemeentenaam" = "csv_name"))
data_2024 <- data_2024 %>%
  left_join(lookup, by = c("Gemeentenaam" = "csv_name"))
data_2023$gpk_name <- ifelse(is.na(data_2023$gpk_name), data_2023$Gemeentenaam, data_2023$gpk_name)
data_2024$gpk_name <- ifelse(is.na(data_2024$gpk_name), data_2024$Gemeentenaam, data_2024$gpk_name)

data_2023_listed <- data_2023 %>%
  filter(gpk_name %in% city_list) %>%
  mutate(Type.Haven_2023 = Type.Haven,
         Dominantie_2023 = Dominantie,
         Dominante.NST_2023 = Dominante.NST) %>%
  select(-Type.Haven, -Dominantie, -Dominante.NST, -gpk_name)

data_2024_listed <- data_2024 %>%
  filter(gpk_name %in% city_list)

data_listed <- data_2024_listed %>%
  left_join(data_2023_listed, by = "Gemeentenaam")

data_listed_sf <- municipality %>%
  right_join(data_listed, by = c("statnaam" = "gpk_name")) %>%
  select(statnaam, Type.Haven, Dominantie, Dominantie_2023, Dominante.NST, Dominante.NST_2023, geom)

data_listed_sf[16, "Type.Haven"] <- "Multifunctionele agrohaven"  #Wageningen
data_listed_sf[27, "Type.Haven"] <- "Multifunctionele containerhaven" #Venlo
data_listed_sf[3, "Type.Haven"] <- "Industriehaven" #Hendrik-Ido-Ambacht
data_listed_sf[6, "Type.Haven"] <- "Industriehaven"    #Sliedrecht
data_listed_sf[25, "Type.Haven"] <- "Grote zand- en grindhaven" #Gennep
data_listed_sf[25, "Type.Haven"] <- "Zand- en Grindhaven" #Gennep


data_listed_sf <- data_listed_sf %>%
  mutate(port_type_trend = case_when(
    Type.Haven == "Containerhaven" ~ -0.29,
    Type.Haven == "Industriehaven"     ~ 0.35,
    Type.Haven == "Kleine Zand- en Grindhaven"   ~ -0.29,
    Type.Haven == "Multifunctionele containerhaven"   ~ 0.33,
    Type.Haven == "Multifunctionele Zand- en Grindhaven"   ~ 0.26,
    Type.Haven == "Multifunctionele industriehaven"   ~ 0.08,
    Type.Haven == "Zand- en Grindhaven"   ~ 0.25,
    Type.Haven == "Multifunctionele agrohaven"   ~ 0.52,
    Type.Haven == "Grote zand- en grindhaven"   ~ 0.25,
    Type.Haven == "Grote multifunctionele binnenhaven"   ~ 0.23,
    TRUE                         ~ 0.00  # Default value if no match is found
  ))

unique(data_listed_sf$Dominante.NST)
unique(data_listed_sf$Type.Haven)
unique(data_listed_sf$statnaam)

space_capacity <- c(0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0,
                    1, 0, 1, 1, 1, 1, 1, 1, 1, 1)
data_listed_sf <- data_listed_sf %>%
  mutate(expansion_capacity = space_capacity)

data_listed_sf <- data_listed_sf %>%
  mutate(Dominante.NST = case_when(
    Dominante.NST == 1 ~ "agriculture",
    Dominante.NST == 3     ~ "metal ores",
    Dominante.NST == 4   ~ "food and beverage",
    Dominante.NST == 7   ~ "coke and refined petroleum",
    Dominante.NST == 8   ~ "chemical and rubber",
    Dominante.NST == 19   ~ "other",
    TRUE                         ~ "other"  # Default value if no match is found
  ))

provinces <- c(
  "Zuid-Holland", "Zuid-Holland", "Zuid-Holland", "Zuid-Holland", "Zuid-Holland", # 1-5
  "Zuid-Holland", "Zuid-Holland", "Utrecht",      "Zuid-Holland", "Noord-Brabant", # 6-10
  "Noord-Brabant", "Noord-Brabant", "Gelderland",  "Gelderland",   "Gelderland",    # 11-15
  "Gelderland",   "Utrecht",      "Utrecht",      "Noord-Brabant", "Noord-Brabant", # 16-20
  "Noord-Brabant", "Noord-Brabant", "Noord-Brabant", "Noord-Brabant", "Limburg",       # 21-25
  "Limburg",       "Limburg",       "Noord-Brabant", "Limburg",       "Noord-Brabant"  # 26-30
)

data_listed_sf <- data_listed_sf %>%
  mutate(province = provinces)


data_listed_sf <- data_listed_sf %>%
  mutate(binnenhaven_monitor = case_when(
    statnaam == "Sittard-Geleen" ~ 0.23,
    statnaam == "Nijmegen" ~ 1.21,
    statnaam == "Oss" ~ -0.06,
    statnaam == "Utrecht" ~ -0.06,
    statnaam == "Wageningen" ~ 0.39,
    statnaam == "Venlo" ~ 0.22,
    statnaam == "Meierijstad" ~ 0.26,
    statnaam == "Dordrecht" ~ 0.59,
    statnaam == "Zwijndrecht" ~ -0.06,
    statnaam == "Hendrik-Ido-Ambacht" ~ -0.24,
    statnaam == "Sliedrecht" ~ 0.03,
    statnaam == "Tilburg" ~ -0.22,
    statnaam == "Alblasserdam" ~ -0.49,
    statnaam == "Gennep" ~ 0.07,
    TRUE                         ~ NA_real_  # Default value if no match is found
  ))


data_listed_sf <- data_listed_sf %>%
  mutate(diversity = case_when(
    Type.Haven %in% c("Containerhaven", "Industriehaven", "Kleine Zand- en Grindhaven",
                      "Zand- en Grindhaven") ~ "Specialized",
    Type.Haven %in% c("Multifunctionele containerhaven", "Multifunctionele Zand- en Grindhaven", 
                      "Multifunctionele industriehaven", "Multifunctionele agrohaven",
                      "Grote multifunctionele binnenhaven") ~ "Multifunctional"
  ))

data_listed_sf <- data_listed_sf %>%
  mutate(scale = case_when(
    Type.Haven %in% c("Kleine Zand- en Grindhaven") ~ 1,
    Type.Haven %in% c("Containerhaven", "Industriehaven", "Zand- en Grindhaven", 
                      "Multifunctionele containerhaven", "Multifunctionele Zand- en Grindhaven", 
                      "Multifunctionele industriehaven", "Multifunctionele agrohaven") ~ 2,
    Type.Haven == "Grote multifunctionele binnenhaven" ~ 3
  ))



# city_list_2 <- c("Alblasserdam", "Dordrecht", "Hendrik-Ido-Ambacht", "Papendrecht", "Ridderkerk", 
#                "Sliedrecht", "Zwijndrecht", "Vijfheerenlanden", "Gorinchem", "Geertruidenberg",
#                "Oosterhout", "Waalwijk", "Arnhem", "Nijmegen", "Tiel", "Wageningen", 
#                "Utrecht", "Nieuwegein", "Breda", "Eindhoven", "Helmond", "'s-Hertogenbosch",
#                "Oss", "Tilburg", "Gennep", "Roermond", "Venlo", "Roosendaal", "Sittard-Geleen", "Meierijstad")

gdp_trend <- c(0.15, 0.09, 0.09, -0.46, 0.15, 0.09, 0.09, -0.8, 0.15, 0.22, -0.28, 
               -0.28, 0.15, 0.15,0.18, -0.04, 0.09, 0.09, -0.28, 0.25, 0.25, -0.28, 
               0.25, 0.15, -0.19, -0.07, 0.2, -0.28, 0.2, 0.15)

data_listed_sf <- data_listed_sf %>%
  mutate(sector_gdp_trend = gdp_trend)


data_listed_sf <- data_listed_sf %>%
  mutate(port_gdp_trend = coalesce(binnenhaven_monitor, port_type_trend))

multi_modal <- c(0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 
                 1, 0, 0, 0, 1, 1, 1, 0)

data_listed_sf <- data_listed_sf %>%
  mutate(multi_modal_connection = multi_modal)


population <- read.csv("01_data/00_context_information/01_the_Netherlands/CBS/population_dynamics.csv", 
                      skip = 3,
                      sep = ";",
                      header = TRUE,
                      stringsAsFactors = FALSE) %>%
  select(-Topic, -X)

population <- population %>%
  filter(!is.na(X2019)) %>%
  mutate(dynamics = X2024 -X2019)

population <- population %>%
  mutate(dynamics_ratio = dynamics /X2019)

population[25, "Regions"] <- "Utrecht"

data_listed_sf <- data_listed_sf %>%
  left_join(population, by = c("statnaam" = "Regions")) %>%
  select(-X2019, -X2024, -dynamics)







# read the gdp data
regional_gdp <- read.csv("01_data/00_context_information/01_the_Netherlands/regional_gdp_sector.csv", 
                      skip = 3,
                      sep = ";",
                      header = TRUE,
                      stringsAsFactors = FALSE) %>%
  select(-X, -X.1, -Sector.branches..SIC.2008.)
str(regional_gdp)

# 1. Truncate the existing column names (removes the ".3")
clean_names <- substr(colnames(regional_gdp), 1, nchar(colnames(regional_gdp)) - 2)

# 2. Combine with the first row of data (e.g., "Gelderland")
colnames(regional_gdp) <- paste(clean_names, unlist(regional_gdp[1, ]), sep = "_")

# 3. Remove that first row from the data
regional_gdp <- regional_gdp[-1, ]
rownames(regional_gdp) <- NULL
regional_gdp <- head(regional_gdp, -1)

regional_gdp[] <- lapply(regional_gdp, as.numeric)

# 1. Initialize the results vector
final_results <- numeric(ncol(regional_gdp))

# 2. Outer Loop (Columns)
for (i in 1:ncol(regional_gdp)) {
  
  # Pull the column as a simple vector using [[ ]]
  current_column <- regional_gdp[[i]] 
  
  # Start the multiplication factor at 1
  running_product <- 1
  
  # 3. Inner Loop (Rows 1 to 4)
  for (j in 1:4) {
    # Math: (1 + r/100)
    # Since current_column is a vector, current_column[j] is just one number
    running_product <- running_product * (1 + (current_column[j] / 100))
  }
  
  # 4. Final step: Subtract 1 and turn back to percentage
  final_results[i] <- round((running_product - 1), 2)
}

# Add the new row to the data
regional_gdp_modified <- rbind(regional_gdp, final_results)

write.csv(regional_gdp_modified, dsn_csv, append = FALSE)


data_listed_sf <- st_read(file_path) %>%
  select(-cluster, -cluster_2)
data_listed_sf[17, "multi_modal_connection"] <- 0 #Gennep
data_listed_sf <- data_listed_sf %>%
  mutate(expansion_capacity = expansion_capacity * 5)

dsn <- "05_data_output/port_network_info/figures.gpkg"
dsn_csv <- "05_data_output/port_network_info/2020_2024_gdp.csv"

st_write(data_listed_sf, dsn, delete_layer = TRUE)
