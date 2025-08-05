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
#---- Number of buildings in the buffer zone of each building ------------------

# Calculating the centroid for every building
centroid <- st_centroid(merged_data)
plot(centroid[4], pch=20)

# 50m buffer
buffer_50m <- st_buffer(centroid, dist = 50)

buffer_area_50m <- pi * 50^2

# Counting the number of centroids within the buffer for each point
count_neighbour_50 <- st_intersects(buffer_50m, centroid)

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

# ---- Filtering buffers -------------------------------------------------------

#TODO: Noch zu viele variablen, teilweise doppelt und durcheinander, muss aufräumen

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
    centroid$area_density_inside[i] <- centroid$building_area_full[i] / as.numeric(buffer_inside_border)
    centroid$area_outside_estimate[i] <- area_density_inside * buffer_outside_border
    #total estimated building area in buffer
    centroid$building_area_estimated_total[i] <- centroid$building_area_full[i] + centroid$area_outside_estimate[i]
    
    
  } else {
    centroid$neighbour_50m_outside_estimate[i] <- NA
    centroid$building_area_outside_estimate[i] <- NA
  }
  
  if (length(neighbours_filtered) > 0) {
    filtered_buildings <- merged_data[neighbours_filtered, ]
    avg_area <- mean(as.numeric(st_area(filtered_buildings)))
    centroid$avg_neighbour_area[i] <- avg_area
  } else {
    centroid$avg_neighbour_area[i] <- NA
  }
}

centroid$neighbour_50m_total_estimate <- centroid$neighbour_50m_filtered + centroid$neighbour_50m_outside_estimate
centroid$neighbour_50m_total_estimate_rounded <- round(centroid$neighbour_50m_total_estimate)


centroid$building_density_total <- centroid$building_area_estimated_total / buffer_area_50m
centroid$building_density_total_rounded <- round(merged_data$building_density_total, 2)



merged_data <- merged_data %>%
  left_join(
    centroid %>%
      st_drop_geometry() %>%
      select(poly_id, neighbour_50m_total_estimate_rounded, building_density_total_rounded),
    by = "poly_id"
  )

ggplot(merged_data, aes(x = factor(building_density_total_rounded))) +
  geom_bar(fill = viridisLite::viridis(1, option = "plasma")) +
  scale_x_discrete(breaks = seq(0, 1, by = 0.1))



#--- Map 2 -----------------------
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
    x = "Number of estimated buildings within a radius of 50 metres",
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

###############################################################################
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
################################################################################
# individual Building area
merged_data$building_area <- round(as.numeric(st_area(merged_data)))

# classify
breaks <- seq(0, 4488 , by = 100)

ggplot(merged_data, aes(x = factor(building_area), fill = factor(building_area))) +
  geom_bar() +
  scale_fill_viridis_d(option = "H")+
  theme(
    legend.position = "none")

ggplot() +
  geom_sf(data = merged_data, aes(fill = factor(building_area)), color = NA) +
  scale_fill_viridis_d(option = "H")+
  theme(
    legend.position = "none")
#---- waffle Chart ----
####

library(viridis)

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

ggplot(merged_data, aes(x = x, y = y, fill = building_area_class)) +
  geom_tile(color = "white", size = 0.1) +
  coord_equal() +
  scale_fill_viridis_d(option = "H", name = "Building Size (m²)") +
  theme_void(base_family = "Source Sans 3") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(title = "Waffle Chart: Sorted by Building Size")


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