# ----usefuul for me ----
# in Terminal (Tools --> Terminal)
# git grep -n TODO
#*******************************************************************************
#**** PACKAGES *****************************************************************
#*******************************************************************************
library(sf)
library(terra)
library(landscapemetrics)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(concaveman)
library(igraph)

library(sysfonts)
library(showtextdb)
library(showtext)

# Loading Font for Graphs
font_add_google("Source Sans 3", "Source Sans 3")
showtext_auto()

#*******************************************************************************
#**** LOADING DATA *************************************************************
#*******************************************************************************

# TODO: New path

setwd("C:/Users/Duck/Documents/Studium/EAGLE/2_semester/4_scientific_graphs/Exam")

unstructured <-st_read("C:/Users/Duck/Documents/Studium/EAGLE/2_semester/4_scientific_graphs/Exam/NewCairo_unstructured.gpkg")

structured <-st_read("C:/Users/Duck/Documents/Studium/EAGLE/2_semester/4_scientific_graphs/Exam/NewCairo_structured.gpkg")

#*******************************************************************************
#**** PREPARE DATA *************************************************************
#*******************************************************************************
# Merge Datasets
unstructured$type <- 1
structured$type <- 2

merged_data <- rbind(unstructured, structured)
#Setting ID for every Polygon
merged_data$poly_id <- seq_len(nrow(merged_data))

#Check if metric coordinate system
st_crs(merged_data)

#*******************************************************************************
#**** NEIGHBOURS IN 50m BUFFER *************************************************
#*******************************************************************************
#---- Number of buildings in the buffer zone of each building ------------------

# Calculating the centroid for every building
centroid <- st_centroid(merged_data)

# 50m buffer
buffer_50m <- st_buffer(centroid, dist = 50)
# Area of Buffer
buffer_area_50m <- pi * 50^2

# Counting the number of centroids within the buffer for each point
count_neighbour_50 <- st_intersects(buffer_50m, centroid)

# Checking that buildings are not mixed up
count_neighbour_50[[5]]
merged_data$poly_id[count_neighbour_50[[5]]]

# Adding new column ("-1" to not include the respective point itself)
centroid$neighbour_50m <- lengths(count_neighbour_50) - 1

#*******************************************************************************
#**** BOUNDARIES OF AOI ********************************************************
#*******************************************************************************
# Roads should not be considered "outside the study area"
# getting EVERY corner coordinate of the buildings
coords_type1 <- st_coordinates(unstructured) %>%
  as.data.frame() %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(unstructured))

coords_type2 <- st_coordinates(structured) %>%
  as.data.frame() %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(structured))

# surrounding
border_type1 <- concaveman(coords_type1)
border_type2 <- concaveman(coords_type2)

#border_2 <- rbind(border_type1, border_type2)
border_2 <- st_union(border_type1, border_type2)
plot(border_2)

#*******************************************************************************
#**** ENVIRONMENT BASED METRICS ************************************************
#*******************************************************************************

centroid_edge <- centroid

# Buffer_area
buffer_total_area <- pi * 50^2

# New columns
centroid_edge$neighbour_50m_filtered <- NA
centroid_edge$building_area_intersect <- NA
centroid_edge$avg_neighbour_area <- NA
centroid_edge$distance_sd_neighbours <- NA
centroid_edge$include_in_analysis <- NA

for (i in seq_len(nrow(buffer_50m))) {
  
  buffer_i <- buffer_50m[i, ]
  neighbour_ids <- count_neighbour_50[[i]]
  buildings_all_50 <- merged_data[neighbour_ids, ]
  
  # scip, if no neighbour
  if (nrow(buildings_all_50) == 0) next
  
  # Area of buffer inside border_2
  buffer_inside <- st_intersection(buffer_i, border_2)
  buffer_inside_area <- st_area(buffer_inside)
  #precentage of buffer-area inside border
  share_inside <- as.numeric(buffer_inside_area) / as.numeric(buffer_total_area)
  
  # Only continoue with current [i] if buffer-area min. 95% within border_2
  if (share_inside < 0.95) {
    centroid_edge$include_in_analysis[i] <- 0
    next
  } else {
    centroid_edge$include_in_analysis[i] <- 1
  }
  
  # calculating whole buffer build up area
  building_area <- st_area(buildings_all_50)
  building_intersect <- st_intersection(buildings_all_50, buffer_i)
  area_intersect <- st_area(building_intersect)
  
  # Saving Build-Up Area in [i]
  centroid_edge$building_area_intersect[i] <- sum(as.numeric(area_intersect))
  
  # For neighbour count:: keeping only neighbours with min. 50% area inside buffer
  clipped_pct <- as.numeric((area_intersect / building_area) * 100)
  filtered_ids <- neighbour_ids[clipped_pct >= 50 & neighbour_ids != i]
  centroid_edge$neighbour_50m_filtered[i] <- length(filtered_ids)
  
  # Calculating average neighbouring area
  if (length(filtered_ids) > 0) {
    filtered <- merged_data[filtered_ids, ]
    centroid_edge$avg_neighbour_area[i] <- mean(as.numeric(st_area(filtered)))
  }
    #Standard deviation of neighbour distances with at least three neighbours
  if (length(filtered_ids) >= 3) {
    #coordinated current building[i]
    center <- st_coordinates(centroid_edge[i, ])
    #coordinated filtered neighbours of current building[i]
    neighbours_coords <- st_coordinates(centroid_edge[filtered_ids, ])
    #calculating euclidean distance between current building and his filtered neighbours
    dists <- sqrt((neighbours_coords[, 1] - center[1])^2 +
                    (neighbours_coords[, 2] - center[2])^2)
    #Standard deviation of distance
    centroid_edge$distance_sd_neighbours[i] <- sd(dists)
  }
}

# Join new columns to polygones merged_data
merged_data_edge <- merged_data %>%
  left_join(
    centroid_edge %>%
      st_drop_geometry() %>%
      select(poly_id, neighbour_50m_filtered, building_area_intersect, avg_neighbour_area, distance_sd_neighbours, include_in_analysis),
    by = "poly_id"
  )

#*******************************************************************************
#**** CALCULATING MORE INDICES *************************************************
#*******************************************************************************

#---- Density-Index ------------------------------------------------------------
building_area_individual
building_area_buffer

merged_data_edge$building_area_density_index <- round(as.numeric((merged_data_edge$building_area_individual/merged_data_edge$building_area_intersect)),2)

#---- Building dominanz ----------------------------------------------
#Mixing relative area with neighbour density

# A high value means that a big Buildings is in a high density environment
# mean value means that the a big building is in a loose neighbourhood or a small building in a high density neighbourhood
# small value: small Building with view neighbours --> less spatial dominanz

merged_data_edge$neighbour_building_dominanz <- 
  round(((merged_data_edge$building_area_individual /merged_data_edge$building_area_intersect) *merged_data_edge$neighbour_50m_filtered),2)

#....Surrounding index---dont know...-------------------------------------------
# dont knowif this could be intresting
merged_data_edge$surrounding_index <-  round(as.numeric(((merged_data_edge$building_area_individual/merged_data_edge$building_area_intersect)* (1*((buffer_total_area - merged_data_edge$building_area_intersect)/buffer_total_area)))),2)

#---- JOIN ---------------------------------------------------------------------

merged_data_edge <- merged_data_edge %>%
  left_join(
    centroid_edge %>%
      st_drop_geometry() %>%
      select(poly_id, surrounding_index, building_area_density_index, neighbour_building_dominanz),
    by = "poly_id"
  )
#*******************************************************************************
#**** BUILDING CLUSTERS ********************************************************
#*******************************************************************************
merged_data_edge$building_area_individual <- st_area(merged_data_edge)
merged_data_edge$building_area_individual <- as.numeric(st_area(merged_data_edge))


# Matrix of touching building
touch_list <- st_touches(merged_data_edge)

graph <- graph_from_adj_list(touch_list, mode = "all")

components <- components(graph)

merged_data_edge$group_id <- components$membership

#solo buildings
deg <- degree(graph)
merged_data_edge$group_id[deg == 0] <- 0

#solor buildings become value 0
merged_data_edge$group_id[merged_data_edge$group_id != 0] <- merged_data_edge$group_id[merged_data_edge$group_id != 0] + 1

#Counting Buildings oer group_id and adding them in new column "group_count"
merged_data_edge <-merged_data_edge %>%
  add_count(group_id, name = "group_count")

#Calculating area per group
merged_data_edge <- merged_data_edge %>%
  group_by(group_id) %>%
  mutate(group_area = sum(building_area_individual, na.rm = TRUE)) %>%
  ungroup()

# Keeping area for solo buildings
merged_data_edge <- merged_data_edge %>%
  mutate(
    group_area_adjusted = as.numeric(if_else(
      group_id == 0,
      building_area_individual,
      group_area
    ))
  )

#save everything as numeric value and round to whole numbers
merged_data_edge <- merged_data_edge %>%
  mutate(
    building_area_individual = round(as.numeric(building_area_individual), 0),
    group_area = round(as.numeric(group_area), 0),
    group_area_adjusted = round(as.numeric(group_area_adjusted), 0),
    building_area_intersect = round(as.numeric(building_area_intersect), 0),
    avg_neighbour_area = round(as.numeric(avg_neighbour_area), 0),
    distance_sd_neighbours = round(as.numeric(distance_sd_neighbours), 0)
  )

#*******************************************************************************
#**** PLOTTING AREA ************************************************************
#*******************************************************************************

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