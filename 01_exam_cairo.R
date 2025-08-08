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
library(cowplot)

library(sysfonts)
library(showtextdb)
library(showtext)

# Loading Font for Graphs
font_add_google("Source Sans 3", "Source Sans 3")
showtext_auto()

#*******************************************************************************
#**** LOADING DATA *************************************************************
#*******************************************************************************

# setwd("C:/Users/Duck/Documents/Studium/EAGLE/2_semester/4_scientific_graphs/Exam")
# unstructured <-st_read("C:/Users/Duck/Documents/Studium/EAGLE/2_semester/4_scientific_graphs/Exam/NewCairo_unstructured.gpkg")
# structured <-st_read("C:/Users/Duck/Documents/Studium/EAGLE/2_semester/4_scientific_graphs/Exam/NewCairo_structured.gpkg")

setwd("Set the path where the graphics should be saved")

unstructured <-st_read("C:/set your path/NewCairo_unstructured.gpkg")

structured <-st_read("C:/set your path/NewCairo_structured.gpkg")

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
  
  # For neighbour count: keeping only neighbours with min. 50% area inside buffer
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
    #Coordinated current building[i]
    center <- st_coordinates(centroid_edge[i, ])
    #coordinated filtered neighbours of current building[i]
    neighbours_coords <- st_coordinates(centroid_edge[filtered_ids, ])
    # Calculating euclidean distance between current building and his filtered neighbours
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

#---- AREA----------------------------------------------------------------------

#Individual Building area
merged_data_edge$building_area_individual <- st_area(merged_data_edge)
merged_data_edge$building_area_individual <- as.numeric(st_area(merged_data_edge))

#*******************************************************************************
#**** CALCULATING MORE INDICES *************************************************
#*******************************************************************************

#---- Dataset without edge -------------------------------------------------
#creating a dataset without the edge
merged_data_clean <- merged_data_edge %>%
  filter(include_in_analysis == 1)

# join all missing columns
merged_data_clean <- merged_data_edge %>%
  filter(include_in_analysis == 1) %>%
  left_join(
    merged_data_edge %>%
      st_drop_geometry() %>%
      select(poly_id, building_area_intersect),  # nur was du brauchst
    by = "poly_id"
  )


#---- Density-Index ------------------------------------------------------------

merged_data_clean$building_area_density_index <- round(as.numeric((merged_data_clean$building_area_individual / merged_data_clean$building_area_intersect.x)),2)

#---- Building dominanz --------------------------------------------------------
# Mixing relative area with neighbour density
# High value: the building dominates its surroundings in terms of area
#Low value: Building fits in with the surrounding area in terms of surface area

merged_data_clean$neighbour_building_dominanz <- 
  round(((merged_data_clean$building_area_individual /merged_data_clean$building_area_intersect.x) *merged_data_clean$neighbour_50m_filtered),2)

#....Surrounding index---dont know...-------------------------------------------
# dont know if this could be intresting
merged_data_clean$surrounding_index <-  round(as.numeric(((merged_data_clean$building_area_individual/merged_data_clean$building_area_intersect.x)* (1*((buffer_total_area -merged_data_clean$building_area_intersect.x)/buffer_total_area)))),2)

#---- JOIN ---------------------------------------------------------------------

merged_data_edge <- merged_data_edge %>%
  left_join(
    merged_data_clean %>%
      st_drop_geometry() %>%
      select(poly_id, building_area_density_index, neighbour_building_dominanz,surrounding_index),
    by = "poly_id"
  )
#*******************************************************************************
#**** BUILDING CLUSTERS ********************************************************
#*******************************************************************************

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

################################################################################
#**** NUMBER OF NEIGHBOURS WITHIN A RADIUS OF 50m ******************************
#################################################################################
# More plotting space under Polygones
bbox <- st_bbox(merged_data_edge)
buffer_y <- (bbox["ymax"] - bbox["ymin"]) * 0.50

# Preparing the edge of the AOI, so labeling buildings at the edge new
merged_data_edge$fill_group <- ifelse(
  merged_data_edge$include_in_analysis == 0,
  "Buildings excluded from analysis",
  as.character(merged_data_edge$neighbour_50m_filtered)
)

# sort all values ascending, except for edge buildings
levels_sorted <- sort(as.numeric(unique(merged_data_edge$fill_group[merged_data_edge$fill_group != "Buildings excluded from analysis"])))
levels_sorted <- c(as.character(levels_sorted), "Buildings excluded from analysis")
#transform to factor, for better usage in plot
merged_data_edge$fill_group <- factor(merged_data_edge$fill_group, levels = levels_sorted)

#prepare color sheme for all buildings
viridis_colors <- viridis::viridis(length(levels(merged_data_edge$fill_group)) - 1, option = "H")
fill_colors <- c("Buildings excluded from analysis" = "grey30", setNames(viridis_colors, levels(merged_data_edge$fill_group)[levels(merged_data_edge$fill_group) != "Buildings excluded from analysis"]))

#---- MAP ----------------------------------------------------------------------

map_neighbours_edge <- ggplot() +
  geom_sf(
    data = merged_data_edge, aes(fill = fill_group), color = NA
  ) +
  scale_fill_manual(values = fill_colors, name = "Number of neighbours"
  ) +
  coord_sf(
    ylim = c(bbox["ymin"] - buffer_y, bbox["ymax"]), expand = FALSE
  ) +
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Number of neighbouring buildings within a radius of 50 m"
  ) +
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color = "#F2F2DE", size = 18, face = "bold"),
    plot.subtitle = element_text(color = "#F2F2DE", size = 14),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )

#---- Barplot ------------------------------------------------------------------

# Excluding Buildings where inside_border == 0
filtered_data <- merged_data_edge %>% 
  filter(include_in_analysis == 1)

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
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    axis.text.x = element_text(margin = margin(t = 0, unit = "pt"), angle = 0, vjust = 1, hjust = 0.5, size = 10, face = "bold", colour = "#F2F2DE"),
    axis.text.y = element_text(color = "#F2F2DE", family = "Source Sans 3", size = 10, face = "bold"),
    axis.title.x = element_text(color = "#F2F2DE", family = "Source Sans 3", size = 10),
    axis.title.y = element_text(color = "#F2F2DE", angle = 90, family = "Source Sans 3", size = 10),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(color = "#F2F2DE", linewidth = 0.3),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold", color = "#F2F2DE", family = "Source Sans 3", size = 10),
    panel.spacing = unit(0.7, "cm")
  )

bar_neighbours_edge

#---- Combine ------------------------------------------------------------------

combined_neighbour_edge <- map_neighbours_edge +
  inset_element(bar_neighbours_edge, left= 0.05, right= 0.95, bottom = 0, top = 0.30)

ggsave("cairo_neighbours_buffer50_without_edge.png", combined_neighbour_edge , width = 15, height = 18, units = "cm", dpi = 600)


################################################################################
#**** PATCHES ******************************************************************
################################################################################

#no solo buildings
grouped_data <- merged_data_edge %>%
  filter(group_id != 0)

#classify data in three groups with same range
classify_minmax <- function(x, n = 3) {
  breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
  cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
}

# classify with function
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
solo_buildings <- merged_data_edge %>%
  filter(group_id == 0) %>%
  mutate(group_matrix = "solo")

solo_buildings <- solo_buildings %>%
  mutate(group_matrix = "solo")

#combine
merged_data_classified <- bind_rows(non_solo_buildings, solo_buildings)

# colors
bi_colors <- c(
  "1-1" = "#c5c5c5", "2-1" = "#7fb2c6",  "3-1" = "#1f6d99", 
  "1-2" = "#d5b96d","2-2" = "#7d7d7d","3-2" = "#234f71",
  "1-3" = "#d99100",   "2-3" = "#7b4000", "3-3" = "#000000", 
  "solo" = "#e6b8b7"
)

#---- Map ---------------------------------------------------------------------- 
patches_map <- ggplot() +
  geom_sf(data = merged_data_classified, aes(fill = group_matrix), color = NA) +
  scale_fill_manual(values = bi_colors, name = "Group Composition") +
  coord_sf() +
  labs(title = "Building Connectivity and Area",
       subtitle = "Group size vs. group area (excluding single buildings)") +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color = "grey20", size = 25, face = "bold"),
    plot.subtitle = element_text(color = "grey20", size = 15),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
patches_map

#---- Matrix plot --------------------------------------------------------------

#create matrix grid
matrix_plot <- expand.grid(
  area_class = 1:3,
  count_class = 1:3
) %>%
  mutate(
    group_matrix = paste0(area_class, "-", count_class),
    fill = bi_colors[group_matrix]
  )

patches_matrix <- ggplot(matrix_plot, aes(x = area_class, y = count_class, fill = fill)) +
  geom_tile() +
  scale_fill_identity() +
  scale_x_continuous(breaks = 1:3, labels = c("Small\n[101-3380 m²]", "Medium\n3380-6660 m²]", "Large\n[6660-9940 m²]")) +
  scale_y_continuous(breaks = 1:3, labels = c("Low\n[2-15]", "Medium\n[16-29]", "High\n[30-43]")) +
  labs(x = "Group Area", y = "Group Size") +
  coord_equal() +
  theme(
    axis.title = element_text(size = 10, family = "Source Sans 3", color = "grey20", face="bold"),
    axis.text  = element_text(size = 8, family = "Source Sans 3", color = "grey20", hjust=0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.ticks.length = unit(0, "pt"),
    axis.ticks = element_blank(),
  )

patches_matrix

#---- Combine ------------------------------------------------------------------
patches_plot <- ggdraw() +
  draw_plot(patches_map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(patches_matrix, x = 0.03, y = 0.05, width = 0.3, height = 0.3)


patches_plot

showtext_opts(dpi = 600)

ggsave("cairo_patches.png", patches_plot , width = 24, height = 18, units = "cm", dpi = 600)

################################################################################
#**** PATCHES WITH QUANTILES ***************************************************
################################################################################

# classify with quantiles
classify_quantiles <- function(x, n = 3) {
  breaks_q <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
  cut(x, breaks = breaks_q, include.lowest = TRUE, labels = FALSE)
}

non_solo_buildings_q <- grouped_data %>%
  mutate(
    area_class_q = classify_quantiles(group_area_adjusted),
    count_class_q = classify_quantiles(group_count),
    group_matrix_q = paste0(area_class_q, "-", count_class_q)
  )

solo_buildings_q <- merged_data_edge %>%
  filter(group_id == 0) %>%
  mutate(group_matrix_q = "solo")

merged_data_classified_q <- bind_rows(non_solo_buildings_q, solo_buildings_q)

#checking quantiles
print_class_breaks <- function(x, n = 3, varname = "") {
  breaks <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
  cat(paste0("Quantile breaks for ", varname, ":\n"))
  print(round(breaks, 0))
  invisible(breaks)
}
print_class_breaks(non_solo_buildings_q$group_area_adjusted, n = 3, varname = "group_area_adjusted")
print_class_breaks(non_solo_buildings_q$group_count, n = 3, varname = "group_count")

bi_colors_q <- c(
  "1-1" = "#c5c5c5", "2-1" = "#7fb2c6",  "3-1" = "#1f6d99", 
  "1-2" = "#d5b96d", "2-2" = "#7d7d7d",  "3-2" = "#234f71",
  "1-3" = "#d99100", "2-3" = "#7b4000",  "3-3" = "#000000", 
  "solo" = "#e6b8b7"
)

#---- Map ----------------------------------------------------------------------
patches_map_q <- ggplot() +
  geom_sf(data = merged_data_classified_q, aes(fill = group_matrix_q), color = NA) +
  scale_fill_manual(values = bi_colors_q, name = "Group Composition") +
  coord_sf() +
  labs(
    title = "Building Connectivity and Area",
    subtitle = "Group size vs. group area (excluding single buildings)"
  ) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color = "grey20", size = 25, face = "bold"),
    plot.subtitle = element_text(color = "grey20", size = 15),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

patches_map_q

#---- Matrix plot --------------------------------------------------------------
matrix_plot_q <- expand.grid(
  area_class_q = 1:3,
  count_class_q = 1:3
) %>%
  mutate(
    group_matrix_q = paste0(area_class_q, "-", count_class_q),
    fill = bi_colors_q[group_matrix_q]
  )

patches_matrix_q <- ggplot(matrix_plot_q, aes(x = area_class_q, y = count_class_q, fill = fill)) +
  geom_tile() +
  scale_fill_identity() +
  scale_x_continuous(breaks = 1:3, labels = c("Small\n[101-1362 m²]", "Medium\n1363-2587 m²]", "Large\n[2588-9940 m²]")) +
  scale_y_continuous(breaks = 1:3, labels = c("Low\n[2-8]", "Medium\n[9-15]", "High\n[16-43]")) +
  labs(x = "Group Area", y = "Group Size") +
  coord_equal() +
  theme(
    axis.title = element_text(size = 10, family = "Source Sans 3", color = "grey20", face = "bold"),
    axis.text = element_text(size = 8, family = "Source Sans 3", color = "grey20"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.ticks.length = unit(0, "pt"),
    axis.ticks = element_blank()
  )

#---- Combine ------------------------------------------------------------------

patches_plot_q <- ggdraw() +
  draw_plot(patches_map_q, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(patches_matrix_q, x = 0.03, y = 0.05, width = 0.3, height = 0.3)

showtext_opts(dpi = 600)

ggsave("cairo_patches_quantiles.png", patches_plot_q, width = 24, height = 18, units = "cm", dpi = 600)

################################################################################
#**** AVG NEIGHBOUR AREA *******************************************************
################################################################################
#removing NA
merged_data_edge <- merged_data_edge %>%
  filter(!is.na(avg_neighbour_area))

min_val <- floor(min(merged_data_edge$avg_neighbour_area) / 50) * 50
max_val <- ceiling(max(merged_data_edge$avg_neighbour_area) / 50) * 50

# steps
breaks <- seq(min_val, max_val, by = 25)

#removing [ and )
labels_pretty <- paste(
  head(breaks, -1),
  tail(breaks, -1),
  sep = "-"
)

merged_data_edge$avg_neighbour_area_class <- cut(
  merged_data_edge$avg_neighbour_area,
  breaks = breaks,
  include.lowest = TRUE,
  right = FALSE,
  labels = labels_pretty
)

merged_data_edge$avg_neighbour_area_class <- factor(
  merged_data_edge$avg_neighbour_area_class,
  levels = sort(unique(merged_data_edge$avg_neighbour_area_class))
)

viridis_colors_area <- viridis::viridis(
  length(levels(merged_data_edge$avg_neighbour_area_class)),
  option = "H"
)
fill_colors_area <- setNames(
  viridis_colors_area,
  levels(merged_data_edge$avg_neighbour_area_class)
)

#---- Map ----------------------------------------------------------------------

map_neighbours_area <- ggplot() +
  geom_sf(
    data = merged_data_edge,
    aes(fill = avg_neighbour_area_class),
    color = NA
  ) +
  scale_fill_manual(
    values = fill_colors_area,
    name = "Average\nNeighbour Area (m²)",
    drop = FALSE
  ) +
  coord_sf(
    ylim = c(bbox["ymin"] - buffer_y *2, bbox["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Average building size within a 50 m radius of a building"
  ) +
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color = "#F2F2DE", size = 14, face = "bold"),
    plot.subtitle = element_text(color = "#F2F2DE", size = 10),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
    

map_neighbours_area

#---- Barplot ------------------------------------------------------------------

# Excluding Buildings where inside_border == 0
filtered_data <- merged_data_edge %>% 
  filter(include_in_analysis == 1)

bar_neighbours_area <- ggplot(filtered_data,aes(x = avg_neighbour_area_class, fill = avg_neighbour_area_class)) +
  geom_bar() +
  scale_fill_manual(values = fill_colors_area, guide = "none", drop = FALSE) +
  scale_x_discrete(labels = function(x) {
    lbls <- as.character(x)
    lbls[seq_along(lbls) %% 5 != 1] <- ""
    lbls
  }) +
  facet_wrap(~ type,
    labeller = as_labeller(c("1"="Unstructured urban space","2"="Structured urban space")),
    scales = "free_y"
  ) +
  scale_y_continuous(breaks = scales::breaks_width(100), expand = expansion(mult = c(0, 0.05))
  )+
  labs(x = "Average building size [m²]", y = "Count") +
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    axis.text.x = element_text(margin = margin(t = 0, unit = "pt"),
      colour = "#F2F2DE", size = 5, face = "bold",
      angle = 90, vjust = 0.5
    ),
    axis.text.y = element_text(color = "#F2F2DE", size = 5, face = "bold"),
    axis.title.x = element_text(color = "#F2F2DE", size = 8),
    axis.title.y = element_text(color = "#F2F2DE", size = 8, angle = 90),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid.major.y = element_line(color = "#F2F2DE", linetype = "dotted", linewidth = 0.1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold", color = "#F2F2DE", size = 8),
    axis.ticks = element_blank(),
  )

bar_neighbours_area

#---- Combine ------------------------------------------------------------------

combined_neighbour_area <- map_neighbours_area +
  inset_element(bar_neighbours_area, left= 0.05, right= 0.95, bottom = 0, top = 0.50)

ggsave("cairo_avg_building_area.png", combined_neighbour_area , width = 15, height = 18, units = "cm", dpi = 600)


################################################################################
#**** Density_dominance ********************************************************
################################################################################

#---- MAP ----------------------------------------------------------------------

map_dominance <- ggplot() +
  geom_sf(
    data = merged_data_edge, aes(fill = building_area_density_index), color = NA
  ) +
  scale_fill_viridis_c(option = "H", name = "Density Index", na.value = "#444444") +
  coord_sf(
    ylim = c(bbox["ymin"] - buffer_y, bbox["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Dominance of a building within 50 m – measured as\nthe ratio of its area to the built-up area around it"
  ) +
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color = "#F2F2DE", size = 18, face = "bold"),
    plot.subtitle = element_text(color = "#F2F2DE", size = 11),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    
  )

map_dominance

#---- Barplot ------------------------------------------------------------------

# Excluding Buildings where inside_border == 0
filtered_data <- merged_data_edge %>% 
  filter(include_in_analysis == 1)

bar_dominance <- ggplot(merged_data_edge, aes(x = factor(building_area_density_index),  fill = factor(building_area_density_index))) +
  geom_bar() +
  scale_fill_viridis_d(option = "H") +
  scale_x_discrete(breaks = seq(0,1, 0.1)) +
  labs(
    x = "Building dominance index",
    y = "Count",
  ) +
  facet_wrap(
    ~ type,
    labeller = as_labeller(c("1" = "Unstructured urban space", "2" = "Structured urban space")),
    scales = "free_y")+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    axis.text.x = element_text(margin = margin(t = 0, unit = "pt"), angle = 0, vjust = 1, hjust = 0.5, size = 10, face = "bold", colour = "#F2F2DE"),
    axis.text.y = element_text(color = "#F2F2DE", family = "Source Sans 3", size = 10, face = "bold"),
    axis.title.x = element_text(color = "#F2F2DE", family = "Source Sans 3", size = 10),
    axis.title.y = element_text(color = "#F2F2DE", angle = 90, family = "Source Sans 3", size = 10),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold", color = "#F2F2DE", family = "Source Sans 3", size = 10),
    panel.grid.major.y = element_line(color = "#F2F2DE", linetype = "dotted", linewidth = 0.3)
  )


bar_dominance

#---- Combine ------------------------------------------------------------------

combined_dominance <- map_dominance +
  inset_element(bar_dominance, left= 0.05, right= 0.95, bottom = 0, top = 0.30)

ggsave("cairo_building_area_dominance.png", combined_dominance , width = 15, height = 18, units = "cm", dpi = 600)