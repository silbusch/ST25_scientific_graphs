# Packages
library(sf)
library(terra)
library(landscapemetrics)
library(dplyr)
library(tidyr)

# Load Data (Old Path)

setwd("C:/Users/Duck/Documents/Studium/EAGLE/2_semester/4_scientific_graphs/Exam")

unstructured <-st_read("C:/Users/Duck/Documents/Studium/EAGLE/2_semester/4_scientific_graphs/Exam/NewCairo_unstructured.gpkg")

structured <-st_read("C:/Users/Duck/Documents/Studium/EAGLE/2_semester/4_scientific_graphs/Exam/NewCairo_structured.gpkg")

# Merge Datasets
unstructured$type <- 1
structured$type <- 2

#Setting ID for every Polygon
merged_data$poly_id <- seq_len(nrow(merged_data))

#
# merged_data_long <- merged_data %>% st_drop_geometry() %>%
#   pivot_longer(cols = -c(type, poly_id), names_to="metric", values_to = "value")

# rasterisation with cell size of 5m
raster <- rast(ext(merged_data), resolution = 5)

# raster with type-values
r_type <- rasterize(merged_data, raster, field = "type")

# raster with ID-values
r_polyid <- rasterize(merged_data, raster, field = "poly_id")

plot(r_polyid)
plot(r_type)