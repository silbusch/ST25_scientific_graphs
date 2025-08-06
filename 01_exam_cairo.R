# ----usefuul for me ----
# in Terminal (Tools --> Terminal)
# git grep -n TODO

# Packages
library(sf)
library(terra)
library(landscapemetrics)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(concaveman)

library(sysfonts)
library(showtextdb)
library(showtext)
font_add_google("Source Sans 3", "Source Sans 3")
showtext_auto()

# ---- Load Data (Old Path) ----
# TODO: New path

setwd("C:/Users/Duck/Documents/Studium/EAGLE/2_semester/4_scientific_graphs/Exam")

unstructured <-st_read("C:/Users/Duck/Documents/Studium/EAGLE/2_semester/4_scientific_graphs/Exam/NewCairo_unstructured.gpkg")

structured <-st_read("C:/Users/Duck/Documents/Studium/EAGLE/2_semester/4_scientific_graphs/Exam/NewCairo_structured.gpkg")

# Merge Datasets
unstructured$type <- 1
structured$type <- 2

merged_data <- rbind(unstructured, structured)
#Setting ID for every Polygon
merged_data$poly_id <- seq_len(nrow(merged_data))

#
# merged_data_long <- merged_data %>% st_drop_geometry() %>%
#pivot_longer(cols = -c(type, poly_id), names_to="metric", values_to = "value")

################################################################################

# ggplot()+
#   geom_sf(data=merged_data)
# 
# 
# ######################
# # rasterisation with cell size of 5m
# raster <- rast(ext(merged_data), resolution = 5)
# 
# # raster with type-values
# r_type <- rasterize(merged_data, raster, field = "type")
# 
# # raster with ID-values
# r_polyid <- rasterize(merged_data, raster, field = "poly_id")
# 
# plot(r_polyid)
# plot(r_type)
################################################################################
################################################################################
#### CLACLULATION PART##########################################################
################################################################################
################################################################################
#---- Number of buildings in the buffer zone of each building ------------------

# Calculating the centroid for every building
centroid_1 <- st_centroid(merged_data)
plot(centroid_1[4], pch=20)

# 50m buffer
buffer_50m <- st_buffer(centroid_1, dist = 50)

# Area of Buffer
buffer_area_50m <- pi * 50^2

# Counting the number of centroids within the buffer for each point
count_neighbour_50 <- st_intersects(buffer_50m, centroid_1)

# Checking that buildings are not mixed up
count_neighbour_50[[5]]
merged_data$poly_id[count_neighbour_50[[5]]]

# Adding new column ("-1" to not include the respective point itself)
centroid$neighbour_50m <- lengths(count_neighbour_50) - 1



# # Map 
# plot_50m <- ggplot(centroid)+ 
#   geom_sf(aes(color=neighbour_50m), size = 1)+
#   scale_color_viridis_c(option = "plasma", name = "Neighbours within 50 m distance") +
#   labs(
#     title = "Neighbours 50 m",
#   )
# 
# plot_100m <- ggplot(centroid)+ 
#   geom_sf(aes(color=neighbour_100m), size = 1)+
#   scale_color_viridis_c(option = "plasma", name = "Neighbours within 100 m distance") +
#   labs(
#     title = "Neighbours 100 m",
#   )
# plot_50m + plot_100m

#---- Delimiting the study area ------------------------------------------------
# Roads should not be considered "outside the study area"

# getting EVERY corner coordinate of the buildings
corner <- st_coordinates(merged_data) %>%
  as.data.frame() %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(merged_data))

# concave area around the building cluster....so I don´t have the huge area around
border <- concaveman(corner)

plot(border)

#---- Border for both areas ----------------------------------------------------

merged_type1 <- merged_data[merged_data$type == 1, ]
merged_type2 <- merged_data[merged_data$type == 2, ]

#Edges of Buildings
coords_type1 <- st_coordinates(merged_type1) %>%
  as.data.frame() %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(merged_data))

coords_type2 <- st_coordinates(merged_type2) %>%
  as.data.frame() %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(merged_data))

# surrounding
border_type1 <- concaveman(coords_type1)
border_type2 <- concaveman(coords_type2)

border_2 <- rbind(border_type1, border_type2)
plot(border_2)

# ---- Filtering buffers -------------------------------------------------------

#TODO: Noch zu viele variablen, teilweise doppelt und durcheinander, muss aufräumen
centroid <- centroid_1
# Empty columns
centroid$neighbour_50m_filtered <- NA
centroid$neighbour_50m_outside_estimate <- NA
centroid$building_area_full <- NA
centroid$building_area_outside_estimate <- NA
centroid$building_area_intersect <- NA
centroid$buffer_area_inside <- NA
centroid$buffer_area_outside <- NA
centroid$area_density_inside <- NA
centroid$area_outside_estimate <- NA
centroid$building_area_estimated_total <- NA

#TODO: Variablen besser benennen und einheitlicher
for (i in seq_len(nrow(buffer_50m))) {
  # BBuffer current centroid 
  buffer_i <- buffer_50m[i, ]
  # addint all buildings touching the buffer[i] to the list
  neighbour_ids <- count_neighbour_50[[i]]
  buildings_all_50 <- merged_data[neighbour_ids, ]
  
  # Possibly unnecessary loop, but currently too scared to remove it
  if (nrow(buildings_all_50) == 0) next
  
  #Area for each building
  building_area <- st_area(buildings_all_50)
  # clipping buildings with buffer
  building_intersect <- st_intersection(buildings_all_50, buffer_i)
  #remaining area of each building after clipping
  area_intersect <- st_area(building_intersect)
  # adding amount of building area of each buffer to column
  centroid$building_area_intersect[i] <- sum(as.numeric(area_intersect))
  centroid$building_area_full[i] <- sum(as.numeric(area_intersect))
  # percentage of the remaining area of each building
  building_area_intersect_percentage <- as.numeric((area_intersect / building_area) * 100)
  
  #---- Counting Neighbours ----- 
  # Only keeping buildings whose area is at least 50% within the buffer zone, but without the Building itself
  neighbours_filtered <- neighbour_ids[building_area_intersect_percentage >= 50]
  neighbours_filtered <- neighbours_filtered[neighbours_filtered != i]
  # adding amount of neighbours to column
  centroid$neighbour_50m_filtered[i] <- length(neighbours_filtered)
  
  #---- Lalalala ----
  # total buffer area (always the same)
  # TODO: Als Variable abspeichern, anstatt imemr neu zu berechnen
  buffer_total_area <- st_area(buffer_i)
  # Calculating area of buffer inside border
  buffer_inside_border <- st_area(st_intersection(buffer_i, border))
  centroid$buffer_area_inside[i] <- as.numeric(buffer_inside_border)
  #Calculating area of buffer outside border
  buffer_outside_border <- max(0, as.numeric(buffer_total_area - buffer_inside_border))
  centroid$buffer_area_outside[i] <- buffer_outside_border
  
  #TODO: if durch for ersetzen
  if (as.numeric(buffer_inside_border) > 0) {
    #---- Number of Neighbours ----
    # Building density
    neighbour_density <- centroid$neighbour_50m_filtered[i] / as.numeric(buffer_inside_border)
    # Estimating density outside border
    centroid$neighbour_50m_outside_estimate[i] <- neighbour_density * buffer_outside_border
    
    #---- Area Density ----
    # Dichte der gesamten Gebäude(m²) pro m² Fläche
    area_density_inside <- centroid$building_area_full[i] / as.numeric(buffer_inside_border)
    centroid$area_density_inside[i] <- area_density_inside
    
    centroid$area_outside_estimate[i] <- area_density_inside * buffer_outside_border
    #total estimated building area in buffer
    centroid$building_area_estimated_total[i] <- centroid$building_area_full[i] + centroid$area_outside_estimate[i]
    
    
  } else {
    centroid$neighbour_50m_outside_estimate[i] <- NA
    centroid$building_area_outside_estimate[i] <- NA
  }
  #----Standard deviation within a 50 m buffer of the distance
  if (length(neighbours_filtered) > 0) {
    filtered_buildings <- merged_data[neighbours_filtered, ]
    avg_area <- mean(as.numeric(st_area(filtered_buildings)))
    centroid$avg_neighbour_area[i] <- avg_area
  } else {
    centroid$avg_neighbour_area[i] <- NA
  }
  neighbour_ids <- count_neighbour_50[[i]]
  neighbour_ids <- neighbour_ids[neighbour_ids != i]
  
  if (length(neighbour_ids) >= 3) {
    center_i <- st_coordinates(centroid[i, ])
    neighbour_coords <- st_coordinates(centroid[neighbour_ids, ])
    dists <- sqrt((neighbour_coords[,1] - center_i[1])^2 + (neighbour_coords[,2] - center_i[2])^2)
    centroid$distance_sd_neighbours[i] <- sd(dists)
  } else {
    centroid$distance_sd_neighbours[i] <- NA
  }
  }
  
  centroid$distance_sd_neighbours <- dist_sd
}

# rounded estimated neighbours
centroid$neighbour_50m_total_estimate <- centroid$neighbour_50m_filtered + centroid$neighbour_50m_outside_estimate
centroid$neighbour_50m_total_estimate_rounded <- round(centroid$neighbour_50m_total_estimate)

#rounded estimated building area density
centroid$building_density_total <- centroid$building_area_estimated_total / buffer_area_50m
centroid$building_density_total_rounded <- round(merged_data$building_density_total, 2)

# left join
merged_data <- merged_data %>%
  left_join(
    centroid %>%
      st_drop_geometry() %>%
      select(poly_id, neighbour_50m_total_estimate_rounded, building_density_total_rounded, building_density_total),
    by = "poly_id"
  )

merged_data <- merged_data %>%
  left_join(
    centroid %>%
      st_drop_geometry() %>%
      select(poly_id, distance_sd_neighbours, neighbour_50m),
    by = "poly_id"
  )

merged_data$distance_sd_neighbours_round <- round(as.numeric(merged_data$distance_sd_neighbours), 3)


ggplot(merged_data, aes(x = factor(building_density_total_rounded))) +
  geom_bar(fill = viridisLite::viridis(1, option = "plasma")) +
  scale_x_discrete(breaks = seq(0, 1, by = 0.1))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Same Loop but only buildings whose buffer is at least 95% within the border are used.

centroid_edge <- centroid_1

centroid_edge$neighbour_50m_filtered <- NA
centroid_edge$neighbour_50m_outside_estimate <- NA
centroid_edge$building_area_full <- NA
centroid_edge$building_area_outside_estimate <- NA
centroid_edge$building_area_intersect <- NA
centroid_edge$buffer_area_inside <- NA
centroid_edge$buffer_area_outside <- NA
centroid_edge$area_density_inside <- NA
centroid_edge$area_outside_estimate <- NA
centroid_edge$building_area_estimated_total <- NA
centroid_edge$avg_neighbour_area <- NA
centroid_edge$distance_sd_neighbours <- NA
centroid_edge$inside_border <- NA

for (i in seq_len(nrow(buffer_50m))) {
  
  buffer_i <- buffer_50m[i, ]
  neighbour_ids <- count_neighbour_50[[i]]
  buildings_all_50 <- merged_data[neighbour_ids, ]
  
  if (nrow(buildings_all_50) == 0) next
  
  # Buffer coverage check
  buffer_total_area <- st_area(buffer_i)
  buffer_inside_border <- st_area(st_intersection(buffer_i, border_2))
  
  share_inside <- as.numeric(buffer_inside_border) / as.numeric(buffer_total_area)
  centroid_edge$inside_border[i] <- ifelse(share_inside >= 0.95, 1, 0)
  
  if (share_inside < 0.95) next
  
  building_area <- st_area(buildings_all_50)
  building_intersect <- st_intersection(buildings_all_50, buffer_i)
  area_intersect <- st_area(building_intersect)
  
  centroid_edge$building_area_intersect[i] <- sum(as.numeric(area_intersect))
  centroid_edge$building_area_full[i] <- sum(as.numeric(area_intersect))
  
  building_area_intersect_percentage <- as.numeric((area_intersect / building_area) * 100)
  
  neighbours_filtered <- neighbour_ids[building_area_intersect_percentage >= 50]
  neighbours_filtered <- neighbours_filtered[neighbours_filtered != i]
  centroid_edge$neighbour_50m_filtered[i] <- length(neighbours_filtered)
  
  centroid_edge$buffer_area_inside[i] <- as.numeric(buffer_inside_border)
  buffer_outside_border <- max(0, as.numeric(buffer_total_area - buffer_inside_border))
  centroid_edge$buffer_area_outside[i] <- buffer_outside_border
  
  if (as.numeric(buffer_inside_border) > 0) {
    neighbour_density <- centroid_edge$neighbour_50m_filtered[i] / as.numeric(buffer_inside_border)
    centroid_edge$neighbour_50m_outside_estimate[i] <- neighbour_density * buffer_outside_border
    area_density_inside <- centroid_edge$building_area_full[i] / as.numeric(buffer_inside_border)
    centroid_edge$area_density_inside[i] <- area_density_inside
    centroid_edge$area_outside_estimate[i] <- area_density_inside * buffer_outside_border
    centroid_edge$building_area_estimated_total[i] <- centroid_edge$building_area_full[i] + centroid_edge$area_outside_estimate[i]
  } else {
    centroid_edge$neighbour_50m_outside_estimate[i] <- NA
    centroid_edge$building_area_outside_estimate[i] <- NA
  }
  
  if (length(neighbours_filtered) > 0) {
    filtered_buildings <- merged_data[neighbours_filtered, ]
    avg_area <- mean(as.numeric(st_area(filtered_buildings)))
    centroid_edge$avg_neighbour_area[i] <- avg_area
  } else {
    centroid_edge$avg_neighbour_area[i] <- NA
  }
  
  neighbour_ids <- neighbour_ids[neighbour_ids != i]
  if (length(neighbour_ids) >= 3) {
    center_i <- st_coordinates(centroid[i, ])
    neighbour_coords <- st_coordinates(centroid[neighbour_ids, ])
    dists <- sqrt((neighbour_coords[,1] - center_i[1])^2 + (neighbour_coords[,2] - center_i[2])^2)
    centroid_edge$distance_sd_neighbours[i] <- sd(dists)
  } else {
    centroid_edge$distance_sd_neighbours[i] <- NA
  }
}

# Join results to merged_data_edge
merged_data_edge <- merged_data %>%
  left_join(
    centroid_edge %>%
      st_drop_geometry() %>%
      select(poly_id, neighbour_50m_filtered, area_density_inside, avg_neighbour_area, distance_sd_neighbours, inside_border),
    by = "poly_id"
  )

# individual Building area
merged_data$building_area_individual <- round(as.numeric(st_area(merged_data)))

centroid_edge <- centroid_edge %>%
  left_join(
    merged_data %>% st_drop_geometry() %>% select(poly_id, building_area_individual, neighbour_50m),
    by = "poly_id"
  )
# classify
merged_data$building_area_class <- floor(merged_data$building_area / 50) 

# How many Buildings per class?
c <- merged_data %>%
  count(building_area_class)

# # classify
# breaks <- seq(0, 4488 , by = 100)
# 
# ggplot(merged_data, aes(x = factor(building_area), fill = factor(building_area))) +
#   geom_bar() +
#   scale_fill_viridis_d(option = "H")+
#   facet_wrap(~ type, labeller = as_labeller(c("1" = "Unstructured urban space", "2" = "Structured urban space")))+
#   theme(
#     legend.position = "none")

##### Chain buildings and area #################################################
library(igraph)

# Matrix of touching building
touch_list <- st_touches(merged_data)

graph <- graph_from_adj_list(touch_list, mode = "all")

components <- components(graph)

merged_data$group_id <- components$membership

#solo buildings
deg <- degree(graph)
merged_data$group_id[deg == 0] <- 0

#solor buildings become value 0
merged_data$group_id[merged_data$group_id != 0] <- merged_data$group_id[merged_data$group_id != 0] + 1

#Counting Buildings oer group_id and adding them in new column "group_count"
merged_data <- merged_data %>%
  add_count(group_id, name = "group_count")

#Calculating area per group
merged_data <- merged_data %>%
  group_by(group_id) %>%
  mutate(group_area = sum(building_area_individual, na.rm = TRUE)) %>%
  ungroup()

# Keeping area for solo buildings
merged_data <- merged_data %>%
  mutate(
    group_area_adjusted = if_else(
      group_id == 0,
      building_area_individual,
      group_area
    )
  )

#-----Calculating Indizes -----------------------------------------------------------------
#area of Buffer'
buffer_area_50m
# Building Area within Buffer
centroid_edge$building_area_intersect
#Area of individual Buildings
centroid_edge$building_area_individual


#....Surrounding index---dont know...-------------------------------------------
building_area_individual <- centroid_edge$building_area_individual
building_area_buffer <- centroid_edge$building_area_intersect
free_area <- buffer_area_50m - centroid_edge$building_area_intersect
buffer_area_50m

# dont knowif this could be intresting
centroid_edge$surrounding_index <-  round(as.numeric(((building_area_individual/building_area_buffer)* (1*(free_area/buffer_area_50m)))),2)

#---- Density-Index ------------------------------------------------------------
building_area_individual
building_area_buffer

centroid_edge$building_area_density_index <- round(as.numeric((building_area_individual/building_area_buffer)),2)

#---- Building dominanz ----------------------------------------------
#Mixing relative area with neighbour density

# A high value means that a big Buildings is in a high density environment
# mean value means that the a big building is in a loose neighbourhood or a small building in a high density neighbourhood
# small value: small Building with view neighbours --> less spatial dominanz
building_area_intersect <- centroid_edge$building_area_intersect
neighbours_count <- centroid_edge$neighbour_50m_filtered

centroid_edge$neighbour_building_dominanz <- round((building_area_individual /building_area_intersect) *neighbours_count,2)


#---- Left join indices --------------------------------------------------------

merged_data_edge <- merged_data_edge %>%
  left_join(
    centroid_edge %>%
      st_drop_geometry() %>%
      select(poly_id, surrounding_index, building_area_density_index, neighbour_building_dominanz),
    by = "poly_id"
  )

p_map <- ggplot() +
  geom_sf(data = merged_data_edge, aes(fill = surrounding_index), color = NA)+
  theme(
    legend.position = "none"
  )
################################################################################
################################################################################
#### PLOTTING PART #############################################################
################################################################################
################################################################################

#### Connected Buildings #######################################################
#Used Dataset: merged_data'
#Used Parameters:
#
#
#
#
#---- Map -----

#---- Matrix Plot ----
library(cowplot)

#no solo buildings
grouped_data <- merged_data %>%
  filter(group_id != 0)

#classify data in three groups with same range
classify_minmax <- function(x, n = 3) {
  breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
  cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
}


non_solo_buildings <- grouped_data %>%
  mutate(
    area_class = classify_minmax(group_area_adjusted),
    count_class = classify_minmax(group_count),
    group_matrix = paste0(area_class, "-", count_class)
  )

# Checking classes
print_class_breaks <- function(x, n = 3, varname = "") {
  breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
  cat(paste0("Breaks for ", varname, ":\n"))
  print(breaks)
  invisible(breaks)
}

print_class_breaks(non_solo_buildings$group_area_adjusted, n = 3, varname = "group_area_adjusted")
print_class_breaks(non_solo_buildings$group_count, n = 3, varname = "group_count")

#own category for solo buildings
solo_buildings <- solo_buildings %>%
  mutate(group_matrix = "solo")

#combine
merged_data_classified <- bind_rows(non_solo_buildings, solo_buildings)

# colors
bi_colors <- c(
  "1-1" = "#e9e9e9ff", "2-1" = "#e9dda9ff", "3-1" = "#e8ac00ff",
  "1-2" = "#adcbd5ff", "2-2" = "#acaaadff", "3-2" = "#a96100ff",
  "1-3" = "#4c97b6ff", "2-3" = "#336081ff", "3-3" = "#000000",
  "solo" = "#4F0000"
)


#map

p_map <- ggplot() +
  geom_sf(data = merged_data_classified, aes(fill = group_matrix), color = NA) +
  scale_fill_manual(values = bi_colors, name = "Group Composition") +
  ##############theme_minimal() +
  coord_sf() +
  labs(title = "Building Connectivity and Area",
       subtitle = "Group size vs. Group area (excluding solo buildings)") +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color = "#F2F2DE", size = 25, face = "bold"),
    plot.subtitle = element_text(color = "#F2F2DE", size = 16),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
p_map

#matrix legend
legend_df <- expand.grid(
  area_class = 1:3,
  count_class = 1:3
) %>%
  mutate(
    group_matrix = paste0(area_class, "-", count_class),
    fill = bi_colors[group_matrix]
  )

legend_plot <- ggplot(legend_df, aes(x = area_class, y = count_class, fill = fill)) +
  geom_tile() +
  scale_fill_identity() +
  scale_x_continuous(breaks = 1:3, labels = c("Small\n[101-3380 m²]", "Medium\n3380-6660 m²]", "Large\n[6660-9940 m²]")) +
  scale_y_continuous(breaks = 1:3, labels = c("Low\n[2-15]", "Medium\n[16-29]", "High\n[30-43]")) +
  labs(x = "Group Area", y = "Group Size") +
  coord_equal() +
  theme(
    axis.title = element_text(size = 10, family = "Source Sans 3", color = "#F2F2DE", face="bold"),
    axis.title = element_text(hjust=0.5),
    axis.text  = element_text(size = 8, family = "Source Sans 3", color = "#F2F2DE"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA)
  )

# combine map and legend
combined_plot <- ggdraw() +
  draw_plot(p_map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend_plot, x = 0.03, y = 0.05, width = 0.3, height = 0.3)


combined_plot

showtext_opts(dpi = 600)

ggsave("cairo_connected_buildings.png", combined_plot , width = 24, height = 18, units = "cm", dpi = 600)

######## Same map but more classes #############################################
#remotes::install_github("nowosad/colorblindcheck")
library(colorblindcheck)
library(cowplot)
library(biscale)

library(pals)
bivcol = function(pal){
  tit = substitute(pal)
  pal = pal()
  ncol = length(pal)
  image(matrix(seq_along(pal), nrow = sqrt(ncol)),
        axes = FALSE, 
        col = pal, 
        asp = 1)
  mtext(tit)
}
bivcol(stevens.bluered)
pal_fun <- biscale::bi_pal(pal = "PurpleOr", dim = 4, flip_axes = T, rotate_pal = F)
colors_vector <- pal_fun()
print(colors_vector)

#no solo buildings
grouped_data <- merged_data %>%
  filter(group_id != 0)

#classify data in three groups with same range
classify_minmax_4 <- function(x, n = 4) {
  breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
  cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
}


non_solo_buildings_4 <- grouped_data %>%
  mutate(
    area_class = classify_minmax(group_area_adjusted),
    count_class = classify_minmax(group_count),
    group_matrix = paste0(area_class, "-", count_class)
  )

# Checking classes
print_class_breaks_4 <- function(x, n = 4, varname = "") {
  breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
  cat(paste0("Breaks for ", varname, ":\n"))
  print(breaks)
  invisible(breaks)
}

print_class_breaks_4(non_solo_buildings$group_area_adjusted, n = 4, varname = "group_area_adjusted")
print_class_breaks_4(non_solo_buildings$group_count, n = 4, varname = "group_count")

#own category for solo buildings
solo_buildings <- solo_buildings %>%
  mutate(group_matrix = "solo")

#combine
merged_data_classified <- bind_rows(non_solo_buildings, solo_buildings)

# colors
bi_colors_4 <- c(
  "1-1" = "#e9e9e9ff", "2-1" = "#A89DB9", "3-1" = "#7E6A9F", "4-1"="#563787",
  "1-2" = "#D3AF95", "2-2" = "#A88283", "3-2" = "#7E5771", "4-2"="#562D5F",
  "1-3" = "#D28753", "2-3" = "#A86448", "3-3" = "#7E433E", "4-3"="#552335",
  "1-4" = "#D25601", "2-4" = "#A84001", "3-4" = "#7E2B01", "4-4"="#551601",
  "solo" = "red"
)
bi_colors_4 <- c(
"1-1" ="#D9D9D9",
"2-1"= "#D3BBA5",
"3-1" ="#C38D6E",
"4-1" ="#BF6015",
"1-2" ="#B7A6C2",
"2-2" ="#B0919D",
"3-2" ="#A15E57",
"4-2" ="#9A3B10",
"1-3" ="#9272B2",
"2-3" ="#8A5D8D",
"3-3" ="#7B2C47",
"4-3" ="#741000",
"1-4" ="#6C63AC",
"2-4" ="#654E87",
"3-4" ="#561C41",
"4-4" ="#4F0000",
"solo" = "red"
)


bi_colors <- c(
  "1-1" = "#D3D3D3", "2-1" = "#e9dda9ff", "3-1" = "#e8ac00ff",
  "1-2" = "#adcbd5ff", "2-2" = "#acaaadff", "3-2" = "#a96100ff",
  "1-3" = "#4c97b6ff", "2-3" = "#336081ff", "3-3" = "#000000",
  "solo" = "red"
)
#map

p_map_4 <- ggplot() +
  geom_sf(data = merged_data_classified, aes(fill = group_matrix), color = NA) +
  scale_fill_manual(values = bi_colors_4, name = "Group Composition") +
  theme_minimal() +
  coord_sf() +
  labs(title = "Building Connectivity and Area",
       subtitle = "Group size vs. Group area (excluding solo buildings)") +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color = "#F2F2DE", size = 25, face = "bold"),
    plot.subtitle = element_text(color = "#F2F2DE", size = 16),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
p_map

#matrix legend
legend_df_4 <- expand.grid(
  area_class = 1:4,
  count_class = 1:4
) %>%
  mutate(
    group_matrix = paste0(area_class, "-", count_class),
    fill = bi_colors_4[group_matrix]
  )



legend_plot_4 <- ggplot(legend_df_4, aes(x = area_class, y = count_class, fill = fill)) +
  geom_tile() +
  scale_fill_identity() +
  scale_x_continuous(breaks = 1:4, labels = c("Small\n[101-3380 m²]", "Medium\n3380-6660 m²]", "Large\n[6660-9940 m²]", "....")) +
  scale_y_continuous(breaks = 1:4, labels = c("Low\n[2-15]", "Medium\n[16-29]", "High\n[30-43]", "...")) +
  labs(x = "Group Area", y = "Group Size") +
  coord_equal() +
  theme(
    axis.title = element_text(size = 10, family = "Source Sans 3", color = "#F2F2DE", face="bold", hjust = 0.5),
    axis.text  = element_text(size = 8, family = "Source Sans 3", color = "#F2F2DE"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA)
  )

# combine map and legend
combined_plot_4 <- ggdraw() +
  draw_plot(p_map_4, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend_plot_4, x = 0.03, y = 0.05, width = 0.3, height = 0.3)


combined_plot_4

showtext_opts(dpi = 600)

ggsave("cairo_connected_buildings_4.png", combined_plot_4 , width = 24, height = 18, units = "cm", dpi = 600)

################################################################################
#---- Map: Nieghbours Edge -----------------------------------------------------
bbox <- st_bbox(merged_data_edge)
buffer_y <- (bbox["ymax"] - bbox["ymin"]) * 0.50

merged_data_edge$fill_group <- ifelse(
  merged_data_edge$inside_border == 0,
  "Buildings excluded from analysis",
  as.character(merged_data_edge$neighbour_50m_filtered)
)

levels_sorted <- sort(as.numeric(unique(merged_data_edge$fill_group[merged_data_edge$fill_group != "Buildings excluded from analysis"])))
levels_sorted <- c(as.character(levels_sorted), "Buildings excluded from analysis")
merged_data_edge$fill_group <- factor(merged_data_edge$fill_group, levels = levels_sorted)

viridis_colors <- viridis::viridis(length(levels(merged_data_edge$fill_group)) - 1, option = "H")
fill_colors <- c("Buildings excluded from analysis" = "grey30", setNames(viridis_colors, levels(merged_data_edge$fill_group)[levels(merged_data_edge$fill_group) != "Buildings excluded from analysis"]))

#---------------
map_neighbours_edge <- ggplot() +
  geom_sf(
    data = merged_data_edge,
    aes(fill = fill_group),
    color = NA
  ) +
  scale_fill_manual(
    values = fill_colors,
    name = "Number of neighbours"
  ) +
  coord_sf(
    ylim = c(bbox["ymin"] - buffer_y, bbox["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Number of buildings within a radius of 50 metres"
  ) +
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color = "#F2F2DE", size = 100, face = "bold"),
    plot.subtitle = element_text(color = "#F2F2DE", size = 70),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    
  )


map_neighbours_edge

#---- Barplot: Nieghbours Edge -------------------------------------------------

# Excluding Buildings where inside_border == 0
filtered_data <- merged_data_edge %>% 
  filter(inside_border == 1)

bar_neighbours_edge <- ggplot(filtered_data, aes(x = factor(neighbour_50m_filtered), fill = factor(neighbour_50m_filtered))) +
  geom_bar() +
  scale_fill_viridis_d(option = "H") +
  labs(
    x = "Number of individual neighbours within a radius of 50 metres",
    y = "Count",
    fill = "Neighbours"
  ) +
  facet_wrap(~ type, labeller = as_labeller(c("1" = "Unstructured urban space", "2" = "Structured urban space"))) +
  scale_x_discrete(breaks = as.character(seq(0, 54, 5))) +
  scale_y_continuous(breaks = seq(0, 500, 100)) +
  geom_hline(
    yintercept = seq(0, 500, 100),
    color = "#F2F2DE",
    linetype = "dotted",
    linewidth = 0.3
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Source Sans 3"),
    axis.text.x = element_text(margin = margin(t = 0, unit = "pt"), angle = 0, vjust = 1, hjust = 0.5, size = 40, face = "bold", colour = "#F2F2DE"),
    axis.text.y = element_text(color = "#F2F2DE", family = "Source Sans 3", size = 40, face = "bold"),
    axis.title.x = element_text(color = "#F2F2DE", family = "Source Sans 3", size = 50),
    axis.title.y = element_text(color = "#F2F2DE", angle = 90, family = "Source Sans 3", size = 50),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(color = "#F2F2DE", linewidth = 0.3),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold", color = "#F2F2DE", family = "Source Sans 3", size = 60),
    panel.spacing = unit(0.7, "cm")
  )

bar_neighbours_edge

#---- Combine ------------------------------------------------------------------

combined_neighbour_edge <- map_neighbours_edge +
  inset_element(bar_neighbours_edge, left= 0.05, right= 0.95, bottom = 0, top = 0.30)

ggsave("cairo_neighbours_buffer50_without_edge.png", combined_neighbour_edge , width = 30, height = 18, units = "cm", dpi = 600)


#---- calculating the orientation of each building -----------------------------
# get_orientation <- function(polygon) {
#   lines <- st_cast(polygon, "LINESTRING")
#   coords <- st_coordinates(lines)[, 1:2]
#   
#   diffs <- diff(coords)
#   lengths <- sqrt(diffs[,1]^2 + diffs[,2]^2)
#   
#   if (length(lengths) == 0) return(NA)  # falls ein Fehler auftritt
#   
#   i_max <- which.max(lengths)
#   
#   dx <- diffs[i_max, 1]
#   dy <- diffs[i_max, 2]
#   
#   angle <- atan2(dy, dx) * (180 / pi)
#   if (angle < 0) angle <- angle + 360
#   return(angle)
# }
# 
# merged_data_poly <- st_cast(merged_data, "POLYGON")
# 
# merged_data_poly$orientation_angle <- sapply(1:nrow(merged_data_poly), function(i) {
#   get_orientation(merged_data_poly[i, ])
# })
# 
# # Divide angles
# angle_to_direction <- function(angle) {
#   directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
#   breaks <- seq(-22.5, 360, by = 45)
#   angle_mod <- (angle + 22.5) %% 360
#   return(directions[findInterval(angle_mod, breaks)])
# }
# 
# merged_data_poly$orientation_direction <- sapply(merged_data_poly$orientation_angle, angle_to_direction)

#----Orientatidifferent ------------------
get_orientation <- function(polygon) {
  lines <- st_cast(polygon, "LINESTRING")
  coords <- st_coordinates(lines)[, 1:2]
  
  diffs <- diff(coords)
  lengths <- sqrt(diffs[,1]^2 + diffs[,2]^2)
  
  if (length(lengths) == 0) return(NA)
  
  i_max <- which.max(lengths)
  
  dx <- diffs[i_max, 1]
  dy <- diffs[i_max, 2]
  
  angle <- atan2(dy, dx) * (180 / pi)
  if (angle < 0) angle <- angle + 360
  return(angle)
}

merged_data_poly <- st_cast(merged_data, "POLYGON")

merged_data_poly$orientation_angle <- sapply(1:nrow(merged_data_poly), function(i) {
  get_orientation(merged_data_poly[i, ])
})


angle_to_axis <- function(angle) {
  if (is.na(angle)) return(NA)
  
  angle_mod <- angle %% 180  
  
  if (angle_mod < 22.5 || angle_mod >= 157.5) {
    return("North-South")
  } else if (angle_mod >= 67.5 && angle_mod < 112.5) {
    return("East-West")
  } else if (angle_mod >= 22.5 && angle_mod < 67.5) {
    return("Northeast-Southwest")
  } else if (angle_mod >= 112.5 && angle_mod < 157.5) {
    return("Northwest-Southeast")
  } else {
    return(NA)
  }
}

merged_data_poly$orientation_axis <- sapply(merged_data_poly$orientation_angle, angle_to_axis)

merged_data <- merged_data %>%
  left_join(
    merged_data_poly %>%
      st_drop_geometry() %>%
      select(poly_id, orientation_axis),
    by = "poly_id"
  )


#---- Map ----------------------------------------------------------------------
bbox <- st_bbox(merged_data)
buffer_y <- (bbox["ymax"] - bbox["ymin"]) * 0.50

orientation_colors <- c(
  "N"        = "#FF0000",   
  "NE"       = "#FFA500",   
  "E"        = "#FFFF00",   
  "SE"       = "#00FF00",   
  "S"        = "#00FFFF",   
  "SW"       = "#0000FF",   
  "W"        = "#8A2BE2",   
  "NW"       = "#FF1493",   
  "N_end"    = "#FF0000"
)

map_orientation <- ggplot() +
  geom_sf(data = merged_data_poly, aes(fill = factor(orientation_direction)), color = NA) +
  scale_fill_manual(values = orientation_colors) +
  coord_sf(
    ylim = c(bbox["ymin"] - buffer_y, bbox["ymax"]),
    expand = FALSE)+
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Primary building orientation by dominant axis"
  )+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color="#F2F2DE", size = 100, face= "bold"),
    plot.subtitle = element_text(color="#F2F2DE", size = 70),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # more space for barplot
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"),
  )
map_orientation

map_sd_different <- ggplot() +
  geom_sf(data = merged_data, aes(fill = factor(orientation_axis)), color = NA) +
  scale_fill_viridis_d(option = "D") +
  coord_sf(
    ylim = c(bbox["ymin"] - buffer_y, bbox["ymax"]),
    expand = FALSE)+
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Variation in distances to neighbors (within 50m) used as a proxy for spatial order"
  )+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color="#F2F2DE", size = 100, face= "bold"),
    plot.subtitle = element_text(color="#F2F2DE", size = 70),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # more space for barplot
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"),
  )
map_sd_different

bar_orientation <- ggplot(merged_data, aes(x = factor(orientation_axis), fill = factor(orientation_axis))) +
  geom_bar() +
  scale_fill_viridis_d(option = "H") +
  labs(
    x = "Orientation Axis",
    y = "Count",
    fill = "Type"
  ) +
  facet_wrap(~ type, labeller = as_labeller(c("1" = "Unstructured urban space", "2" = "Structured urban space"))) +
  # scale_x_discrete(breaks=as.character(seq(0,54,5)))+
  # scale_y_continuous(breaks = seq(0, 500, 100))+
  # geom_hline(
  #   yintercept = seq(0, 500, 100),
  #   color = "#F2F2DE",
  #   linetype = "dotted",
  #   linewidth = 0.3 
  #)+
  theme(
    legend.position = "none"#,
    # text = element_text(family = "Source Sans 3"),
    # axis.text.x = element_text(margin= margin(t=0, unit="pt"), angle = 0, vjust = 1, hjust = 0.5, size = 40, face = "bold",
    #                            colour = "#F2F2DE",
    #                            family = "Source Sans 3"),
    # axis.text.y = element_text(color = "#F2F2DE", family = "Source Sans 3", size= 40, face = "bold"),
    # axis.title.x = element_text(color = "#F2F2DE", family = "Source Sans 3", size= 50),
    # axis.title.y = element_text(color = "#F2F2DE", angle = 90, family = "Source Sans 3", size= 50),
    # panel.background = element_rect(fill = NA, color = NA),
    # plot.background = element_rect(fill = NA, color = NA),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    # axis.ticks.y = element_blank(),
    # axis.ticks = element_line(color = "#F2F2DE", linewidth =0.3),
    # # Changing header with strip.
    # strip.background = element_blank(),
    # strip.text = element_text(hjust=0, face="bold", color="#F2F2DE", family="Source Sans 3", size=60),
    # panel.spacing = unit(0.7, "cm")
  )

bar_orientation

 #### Standard Deviation distance 50m ##########################################
#---- Map ----------------------------------------------------------------------
bbox <- st_bbox(merged_data)
buffer_y <- (bbox["ymax"] - bbox["ymin"]) * 0.50

map_sd <- ggplot() +
  geom_sf(data = merged_data, aes(fill = factor(distance_sd_neighbours_round)), color = NA) +
  scale_fill_viridis_d(option = "H") +
  coord_sf(
    ylim = c(bbox["ymin"] - buffer_y, bbox["ymax"]),
    expand = FALSE)+
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Variation in distances to neighbors (within 50m) used as a proxy for spatial order"
  )+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color="#F2F2DE", size = 100, face= "bold"),
    plot.subtitle = element_text(color="#F2F2DE", size = 70),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # more space for barplot
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"),
  )
map_sd

map_sd_2 <- ggplot() +
  geom_sf(data = merged_data, aes(fill = factor(distance_sd_neighbours)), color = NA) +
  scale_fill_viridis_d(option = "H") +
  coord_sf(
    ylim = c(bbox["ymin"] - buffer_y, bbox["ymax"]),
    expand = FALSE)+
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Variation in distances to neighbors (within 50m) used as a proxy for spatial order"
  )+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color="#F2F2DE", size = 100, face= "bold"),
    plot.subtitle = element_text(color="#F2F2DE", size = 70),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # more space for barplot
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"),
  )
map_sd_2

#---- Creating Barplot ---------------------------------------------------------
ggplot(merged_data, aes(x = factor(distance_sd_neighbours_round), fill = factor(distance_sd_neighbours))) +
  geom_bar() +
  facet_wrap(~ type, labeller = as_labeller(c("1" = "Unstructured urban space", "2" = "Structured urban space")))+
  theme(
    legend.position ="none"
  )


bar_sd <- ggplot(merged_data, aes(x = factor(distance_sd_neighbours_round), fill = factor(distance_sd_neighbours_round))) +
  geom_bar() +
  scale_fill_viridis_d(option = "H") +
  labs(
    x = "Building proximity variation",
    y = "Count",
    fill = "Type"
  ) +
  facet_wrap(~ type, labeller = as_labeller(c("1" = "Unstructured urban space", "2" = "Structured urban space"))) +
  # scale_x_discrete(breaks=as.character(seq(0,54,5)))+
  # scale_y_continuous(breaks = seq(0, 500, 100))+
  # geom_hline(
  #   yintercept = seq(0, 500, 100),
  #   color = "#F2F2DE",
  #   linetype = "dotted",
  #   linewidth = 0.3 
  #)+
  theme(
    legend.position = "none"#,
    # text = element_text(family = "Source Sans 3"),
    # axis.text.x = element_text(margin= margin(t=0, unit="pt"), angle = 0, vjust = 1, hjust = 0.5, size = 40, face = "bold",
    #                            colour = "#F2F2DE",
    #                            family = "Source Sans 3"),
    # axis.text.y = element_text(color = "#F2F2DE", family = "Source Sans 3", size= 40, face = "bold"),
    # axis.title.x = element_text(color = "#F2F2DE", family = "Source Sans 3", size= 50),
    # axis.title.y = element_text(color = "#F2F2DE", angle = 90, family = "Source Sans 3", size= 50),
    # panel.background = element_rect(fill = NA, color = NA),
    # plot.background = element_rect(fill = NA, color = NA),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    # axis.ticks.y = element_blank(),
    # axis.ticks = element_line(color = "#F2F2DE", linewidth =0.3),
    # # Changing header with strip.
    # strip.background = element_blank(),
    # strip.text = element_text(hjust=0, face="bold", color="#F2F2DE", family="Source Sans 3", size=60),
    # panel.spacing = unit(0.7, "cm")
  )

bar_sd



#### Estimated Neighbours ######################################################
#---- Map ----------------------------------------------------------------------
bbox <- st_bbox(merged_data)
buffer_y <- (bbox["ymax"] - bbox["ymin"]) * 0.50

map_neighbour_50_2 <- ggplot() +
  geom_sf(data = merged_data, aes(fill = factor(neighbour_50m_total_estimate_rounded)), color = NA) +
  scale_fill_viridis_d(option = "H") +
  coord_sf(
    ylim = c(bbox["ymin"] - buffer_y, bbox["ymax"]),
    expand = FALSE)+
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Number of neighbouring buildings within a radius of 50 metres"
  )+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color="#F2F2DE", size = 100, face= "bold"),
    plot.subtitle = element_text(color="#F2F2DE", size = 70),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # more space for barplot
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"),
  )

#---- Creating Barplot ---------------------------------------------------------


bar_50_2 <- ggplot(merged_data, aes(x = factor(building_density_total_rounded), fill = factor(building_density_total_rounded))) +
  geom_bar() +
  scale_fill_viridis_d(option = "H") +
  labs(
    x = "Number of (estimated) buildings within a radius of 50 metres",
    y = "Count",
    fill = "Type"
  ) +
  facet_wrap(~ type, labeller = as_labeller(c("1" = "Unstructured urban space", "2" = "Structured urban space"))) +
  scale_x_discrete(breaks=as.character(seq(0,54,5)))+
  scale_y_continuous(breaks = seq(0, 500, 100))+
  geom_hline(
    yintercept = seq(0, 500, 100),
    color = "#F2F2DE",
    linetype = "dotted",
    linewidth = 0.3 
  )+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    axis.text.x = element_text(margin= margin(t=0, unit="pt"), angle = 0, vjust = 1, hjust = 0.5, size = 40, face = "bold",
                               colour = "#F2F2DE",
                               family = "Source Sans 3"),
    axis.text.y = element_text(color = "#F2F2DE", family = "Source Sans 3", size= 40, face = "bold"),
    axis.title.x = element_text(color = "#F2F2DE", family = "Source Sans 3", size= 50),
    axis.title.y = element_text(color = "#F2F2DE", angle = 90, family = "Source Sans 3", size= 50),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(color = "#F2F2DE", linewidth =0.3),
    # Changing header with strip.
    strip.background = element_blank(),
    strip.text = element_text(hjust=0, face="bold", color="#F2F2DE", family="Source Sans 3", size=60),
    panel.spacing = unit(0.7, "cm")
  )

bar_50_2

#---- Combine BArplots and maps ------------------------------------------------

combined_map_bar_2 <- map_neighbour_50_2 +
  inset_element(bar_50_2, left = 0.10, right= 0.90, bottom = 0, top = 0.30)
#inset_element(cairo_plot, left = 0.1, right= 0.30, bottom = 0.05, top = 0.35)
combined_map_bar_2


ggsave("test_2_estimated_neighbours.png", combined_map_bar_2, width = 30, height = 18, units = "cm", dpi = 600)



#### Desnity ###################################################################
#--- Map 3 - Density estimated -----------------------

bbox <- st_bbox(merged_data)
buffer_y <- (bbox["ymax"] - bbox["ymin"]) * 0.50

map_density <- ggplot() +
  geom_sf(data = merged_data, aes(fill = factor(building_density_total)), color = NA) +
  scale_fill_viridis_d(option = "H") +
  coord_sf(
    ylim = c(bbox["ymin"] - buffer_y, bbox["ymax"]),
    expand = FALSE)+
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Number of building density within a radius of 50 metres"
  )+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color="#F2F2DE", size = 100, face= "bold"),
    plot.subtitle = element_text(color="#F2F2DE", size = 70),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # more space for barplot
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"),
  )

ggplot()+
  geom_sf(data = merged_data, aes(fill = factor(building_density_total)), color = NA)
#---- Creating Barplot ---------------------------------------------------------


bar_density <- ggplot(merged_data, aes(x = factor(building_density_total), fill = factor(building_density_total))) +
  geom_bar() +
  scale_fill_viridis_d(option = "H") +
  labs(
    x = "Building denisty within a radius of 50 metres",
    y = "Count",
    fill = "Type"
  ) +
  facet_wrap(~ type, labeller = as_labeller(c("1" = "Unstructured urban space", "2" = "Structured urban space"))) +
  #scale_x_discrete(breaks=as.character(seq(0,54,5)))+
  #scale_y_continuous(breaks = seq(0, 500, 100))+
  # geom_hline(
  #   yintercept = seq(0, 500, 100),
  #   color = "#F2F2DE",
  #   linetype = "dotted",
  #   linewidth = 0.3 
  #)+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    axis.text.x = element_text(margin= margin(t=0, unit="pt"), angle = 0, vjust = 1, hjust = 0.5, size = 40, face = "bold",
                               colour = "#F2F2DE",
                               family = "Source Sans 3"),
    axis.text.y = element_text(color = "#F2F2DE", family = "Source Sans 3", size= 40, face = "bold"),
    axis.title.x = element_text(color = "#F2F2DE", family = "Source Sans 3", size= 50),
    axis.title.y = element_text(color = "#F2F2DE", angle = 90, family = "Source Sans 3", size= 50),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(color = "#F2F2DE", linewidth =0.3),
    # Changing header with strip.
    strip.background = element_blank(),
    strip.text = element_text(hjust=0, face="bold", color="#F2F2DE", family="Source Sans 3", size=60),
    panel.spacing = unit(0.7, "cm")
  )

bar_density

#---- Combine BArplots and maps ------------------------------------------------

combined_map_bar_2 <- map_neighbour_50_2 +
  inset_element(bar_50_2, left = 0.10, right= 0.90, bottom = 0, top = 0.30)
#inset_element(cairo_plot, left = 0.1, right= 0.30, bottom = 0.05, top = 0.35)
combined_map_bar_2


ggsave("test_2_estimated_neighbours.png", combined_map_bar_2, width = 30, height = 18, units = "cm", dpi = 600)


################################################################################
#### Individual Building Area ##################################################
################################################################################


ggplot() +
  geom_sf(data = merged_data, aes(fill = factor(building_area)), color = NA) +
  scale_fill_viridis_d(option = "H")+
  theme(
    legend.position = "none")

#---- waffle Chart -------------------------------------------------------------
#TODO: Waffle Chart is noch wrong

library(viridis)

# classify building area in 100-steps
merged_data <- merged_data %>%
  mutate(
    building_area_class = cut(
      building_area,
      breaks = seq(0, max(building_area, na.rm = TRUE) + 100, by = 100),
      include.lowest = TRUE,
      right = FALSE
    )
  )

merged_data <- merged_data %>%
  arrange(building_area) %>%
  mutate(
    id = row_number(),
    x = (id - 1) %% 100,
    y = -((id - 1) %/% 100)
  )

ggplot(merged_data, aes(x = x, y = y, fill = building_area)) +
  geom_tile(color = "white", size = 0.1) +
  coord_equal() +
  scale_fill_viridis_d(option = "H", name = "Building Size (m²)") +
  theme_void(base_family = "Source Sans 3") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(title = "Waffle Chart: Sorted by Building Size")


#---- Creating Map -------------------------------------------------------------
bbox <- st_bbox(merged_data)
buffer_y <- (bbox["ymax"] - bbox["ymin"]) * 0.50

map_area <- ggplot() +
  geom_sf(data = merged_data, aes(fill = factor(building_area)), color = NA) +
  scale_fill_viridis_d(option = "H") +
  coord_sf(
    ylim = c(bbox["ymin"] - buffer_y, bbox["ymax"]),
    expand = FALSE)+
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Distribution of building sizes"
  )+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color="#F2F2DE", size = 100, face= "bold"),
    plot.subtitle = element_text(color="#F2F2DE", size = 70),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # more space for barplot
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"),
  )

map_area

#---- Creating Barplot ---------------------------------------------------------
library(ggbreak)

bar_area <- ggplot(merged_data, aes(x = factor(building_area), fill = factor(building_area))) +
  geom_bar() +
  scale_fill_viridis_d(option = "H") +
  labs(
    x = "Building area [m²]",
    y = "Count",
    fill = "Type"
  ) +
  facet_wrap(~ type, labeller = as_labeller(c("1" = "Unstructured urban space", "2" = "Structured urban space"))) +
  # scale_x_discrete(breaks=as.character(seq(0,54,5)))+
  # scale_y_continuous(breaks = seq(0, 500, 100))+
  # geom_hline(
  #   yintercept = seq(0, 500, 100),
  #   color = "#F2F2DE",
  #   linetype = "dotted",
  #   linewidth = 0.3)+
  theme(
    legend.position = "none"
    # text = element_text(family = "Source Sans 3"),
    # axis.text.x = element_text(margin= margin(t=0, unit="pt"), angle = 0, vjust = 1, hjust = 0.5, size = 40, face = "bold",
    #                            colour = "#F2F2DE",
    #                            family = "Source Sans 3"),
    # axis.text.y = element_text(color = "#F2F2DE", family = "Source Sans 3", size= 40, face = "bold"),
    # axis.title.x = element_text(color = "#F2F2DE", family = "Source Sans 3", size= 50),
    # axis.title.y = element_text(color = "#F2F2DE", angle = 90, family = "Source Sans 3", size= 50),
    # panel.background = element_rect(fill = NA, color = NA),
    # plot.background = element_rect(fill = NA, color = NA),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    # axis.ticks.y = element_blank(),
    # axis.ticks = element_line(color = "#F2F2DE", size = 0.3),
    # # Changing header with strip.
    # strip.background = element_blank(),
    # strip.text = element_text(hjust=0, face="bold", color="#F2F2DE", family="Source Sans 3", size=60),
    # panel.spacing = unit(0.7, "cm")
  )

bar_area
# most common size for unstructured buildings is 120m² n= 67
bar_area_scale <- bar_area + 
  scale_y_break(c(70, 665), scales = 0.3)+
  annotate("segment", x = 0.5, xend = Inf, y = 70, yend = 70,
           linetype = "dashed", color = "red", linewidth = 0.5)

bar_area_scale
#------------

combined_map_area <- map_area +
  inset_element(bar_area, left = 0.10, right= 0.90, bottom = 0, top = 0.30)
#inset_element(cairo_plot, left = 0.1, right= 0.30, bottom = 0.05, top = 0.35)
combined_map_area


ggsave("cairo_building_area.png", combined_map_area, width = 15, height = 18, units = "cm", dpi = 600)
################################################################################
#### Cluster ##########################################################
################################################################################


library(dbscan)

# Useless cluster
coords <- st_coordinates(centroid)

clusters <- dbscan(coords, eps = 50, minPts = 4)


centroid$cluster <- as.factor(clusters$cluster)

ggplot(centroid) +
  geom_sf(aes(color = cluster)) +
  scale_fill_viridis_d(option = "H")+
  theme_minimal()

################################################################################
#### Building density ##########################################################
################################################################################

map_density_estimated <- ggplot() +
  geom_sf(data = merged_data, aes(fill = factor(building_density_total.y)), color = NA) +
  scale_fill_viridis_d(option = "H") +
  coord_sf(
    ylim = c(bbox["ymin"] - buffer_y, bbox["ymax"]),
    expand = FALSE)+
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Building density Index"
  )+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color="#F2F2DE", size = 100, face= "bold"),
    plot.subtitle = element_text(color="#F2F2DE", size = 70),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # more space for barplot
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"),
  )

map_density_estimated

#################
################
################
#---- Creating Map -------------------------------------------------------------
bbox <- st_bbox(merged_data)
buffer_y <- (bbox["ymax"] - bbox["ymin"]) * 0.50

map_neighbour_50 <- ggplot() +
  geom_sf(data = merged_data, aes(fill = factor(neighbour_50m_filtered)), color = NA) +
  scale_fill_viridis_d(option = "H") +
  coord_sf(
    ylim = c(bbox["ymin"] - buffer_y, bbox["ymax"]),
    expand = FALSE)+
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Number of neighbouring buildings within a radius of 50 metres"
  )+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color="#F2F2DE", size = 100, face= "bold"),
    plot.subtitle = element_text(color="#F2F2DE", size = 70),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # more space for barplot
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"),
    )

map_neighbour_50

#---- Creating Barplot ---------------------------------------------------------


bar_50 <- ggplot(merged_data, aes(x = factor(neighbour_50m_filtered), fill = factor(neighbour_50m_filtered))) +
  geom_bar() +
  scale_fill_viridis_d(option = "H") +
  labs(
    x = "Number of buildings within a radius of 50 metres",
    y = "Count",
    fill = "Type"
  ) +
  facet_wrap(~ type, labeller = as_labeller(c("1" = "Unstructured urban space", "2" = "Structured urban space"))) +
  scale_x_discrete(breaks=as.character(seq(0,54,5)))+
  scale_y_continuous(breaks = seq(0, 500, 100))+
  geom_hline(
    yintercept = seq(0, 500, 100),
    color = "#F2F2DE",
    linetype = "dotted",
    linewidth = 0.3 
  )+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    axis.text.x = element_text(margin= margin(t=0, unit="pt"), angle = 0, vjust = 1, hjust = 0.5, size = 40, face = "bold",
                               colour = "#F2F2DE",
                               family = "Source Sans 3"),
    axis.text.y = element_text(color = "#F2F2DE", family = "Source Sans 3", size= 40, face = "bold"),
    axis.title.x = element_text(color = "#F2F2DE", family = "Source Sans 3", size= 50),
    axis.title.y = element_text(color = "#F2F2DE", angle = 90, family = "Source Sans 3", size= 50),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(color = "#F2F2DE", size = 0.3),
    # Changing header with strip.
    strip.background = element_blank(),
    strip.text = element_text(hjust=0, face="bold", color="#F2F2DE", family="Source Sans 3", size=60),
    panel.spacing = unit(0.7, "cm")
  )

bar_50

#---- density plot --------
# merged_data %>%
#   filter(neighbour_50m_filtered>-1)%>%
#   ggplot(aes(x=neighbour_50m_filtered))+
#     geom_density()+
#     scale_fill_viridis_d(option = "plasma")

#---- Overview map -------------------------------------------------------------
library(osmdata)

cairo_boundary <- getbb("Cairo, Egypt") %>% 
  opq() %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  add_osm_feature(key = "name", value = "Cairo") %>%
  osmdata_sf()

cairo_outline <- cairo_boundary$osm_multipolygons

# Bounding box around Buildings
box_buildings <- st_bbox(merged_data) %>% st_as_sfc() %>% st_sf()

# Borders of Cairo
cairo <- getbb("Cairo, Egypt", format_out = "sf_polygon")

# Plotting Cairo Map with bounding box of Buildings
cairo_plot <- ggplot() +
  geom_sf(data = cairo, fill = "#1a1a1a", color = "#F2F2DE")+
  geom_sf(data = box_buildings, fill = NA, color = "red", size = 1)+
  theme_void()+
  labs(
    title= "Cairo Governorate"
  )+
  theme(
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    text = element_text(family = "Source Sans 3", colour = "#F2F2DE", face = "bold")
  )

cairo_plot
#---- Combine BArplots and maps ------------------------------------------------

combined_map_bar <- map_neighbour_50 +
  inset_element(bar_50, left = 0.10, right= 0.90, bottom = 0, top = 0.30)
  #inset_element(cairo_plot, left = 0.1, right= 0.30, bottom = 0.05, top = 0.35)
combined_map_bar


ggsave("test.png", combined_map_bar, width = 30, height = 18, units = "cm", dpi = 600)


#---- Area Buildings -----------------------------------------------------------

study_area <- st_union(merged_data)
plot(study_area)


#---- TODO ---------------------------------------------------------------------
# TODO: Map graph
# TODO: BAreplot counting Graph
# TODO: Combine bareplot and Map
# TODO: Thinking about art der darstellung barplot.....smooth?
# TODO: Kleine Karte Cairo mit markierung der Stelle