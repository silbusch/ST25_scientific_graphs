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

# rasterisation with cell size of 5m
raster <- rast(ext(merged_data), resolution = 5)

# raster with type-values
r_type <- rasterize(merged_data, raster, field = "type")

# raster with ID-values
r_polyid <- rasterize(merged_data, raster, field = "poly_id")

plot(r_polyid)
plot(r_type)


################################################################################
#---- How many Buildings touch a building? ----
################################################################################

## Count touches for each polygon in unstructured
touch_matrix <- st_touches(unstructured)

unstructured$touch_count <- factor(lengths(st_touches(unstructured)))

head(unstructured$touch_count)

## Count touches for each polygon in structured
touch_matrix <- st_touches(structured)

structured$touch_count <- factor(lengths(st_touches(structured)))

head(structured$touch_count)

# Histogramm for unstruc and struc
#adding column with information unstruc or struc, so I know which row correspond to which class
structured$typ <- "struc"
unstructured$typ <- "unstruc"

combined <- rbind(structured[, c("touch_count", "typ")],
                  unstructured[, c("touch_count", "typ")])

# histogram
ggplot(combined, aes(x = touch_count, fill = typ)) +
  geom_bar(position = "dodge") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.2,
    size = 3,
    color = "black"
  )+
  labs(title = "Number of neighbouring buildings", 
       x = "Count Neighbours", 
       y = "Count Buildings",
       fill = "...")

# barplot (legend)
bar_data <- combined %>%
  count(touch_count, typ) %>%
  arrange(touch_count, typ) %>%
  group_by(touch_count) %>%
  mutate(
    ymin = cumsum(n) - n,
    ymax = cumsum(n)
  )

# no separation
bar_data <- bar_data %>%
  mutate(typ = factor(typ, levels = c("unstruc", "struc")))

# filtering all zero neighbors for struc
highlight_struc <- bar_data %>%
  filter(touch_count == 0, typ == "struc")

# Adding a color to YlGnBu-Palette (cause n=10)
palette <- c(
  "#FFFFD9","#EDF8B1","#C7E9B4","#7FCDBB","#41B6C4","#1D91C0","#225EA8",
  "#253494","#081D58", "black"
)

bar <- ggplot(bar_data, aes(x = factor(touch_count), y = n, fill = factor(touch_count))) +
  geom_col(position = "stack", color = NA) +
  geom_rect(
    data = highlight_struc,
    aes(
      xmin = as.numeric(factor(touch_count)) - 0.4,
      xmax = as.numeric(factor(touch_count)) + 0.4,
      ymin = ymin,
      ymax = ymax
    ),
    inherit.aes = FALSE,
    color = "darkred",
    fill = NA,
    linewidth = 1.3
  ) +
  scale_fill_manual(values = palette, guide="none") +
  labs(
    x = "Neighbouring Buildings in Contact",
    y = "Count",
    fill = "Type"
  ) +
  scale_y_continuous(breaks = seq(0, 2500, 500))+
  geom_hline(
    yintercept = seq(0, 2500, 500),
    color = "white",
    linetype = "dotted",
    linewidth = 0.3 )+
  theme(
    text = element_text(family = "Source Sans 3"),
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, size = 10, 
                               colour = "#F2F2DE",
                               family = "Source Sans 3"),
    axis.text.y = element_text(color = "#F2F2DE", family = "Source Sans 3"),
    axis.title.x = element_text(color = "#F2F2DE", family = "Source Sans 3", face = "bold"),
    axis.title.y = element_text(color = "#F2F2DE", angle = 90, family = "Source Sans 3", face = "bold"),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),                       
    panel.grid.minor = element_blank()
  )+
  annotate(
    x=1,
    y=450,
    geom="text",
    label="East side",
    color="darkred",
    angle=90,
    size = 4,
    fontface = "bold"
  )+
  coord_cartesian(clip = "off")

bar

#Creating the Map of Buildings
map_counts_buildings <- ggplot() +
  geom_sf(data = combined, aes(fill = factor(touch_count)), color = NA) +
  scale_fill_manual(
    values = palette,
    guide = "none",
    breaks = 0:9
  ) +
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Number of neighbouring buildings in contact"
  )+
  theme(
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color="#F2F2DE", size = 25),
    plot.subtitle = element_text(color="#F2F2DE", size = 18),
    plot.background = element_rect(fill = "grey30", color = NA),
    plot.margin = margin(t = 5, l = 1, unit = "mm"),)

map_counts_buildings

#Combine Barplot and Map

combined_map_bar <- map_counts_buildings +
  inset_element(bar, left = 0.01, right= 0.30, bottom = 0.05, top = 0.35)

# horrible map
combined_map_bar

################################################################################
#---- Next Stuff ----

# Calculating area
merged_data$area_m2 <-st_area(merged_data)
# Saving as numeric, cause its sometimes easier to use
merged_data$area_m2_num <- as.numeric(st_area(merged_data))

#ggplot(merged_data)+ 
#  geom_sf(aes(fill=area_m2_num), color=NA)+
#  scale_fill_viridis_c(option = "plasma") 


# Calculating the centroid for every building
centroid <- st_centroid(merged_data)
plot(centroid[4], pch=20)

# 50m buffer
buffer_50m <- st_buffer(centroid, dist = 50)
buffer_100m <- st_buffer(centroid, dist = 100)

# Counting the number of centroids within the buffer for each point
count_neighbour_50 <- st_intersects(buffer_50m, centroid)

# Checking that buildings are not mixed up
count_neighbour_50[[5]]
merged_data$poly_id[count_neighbour_50[[5]]]

count_neighbour_100 <- st_intersects(buffer_100m, centroid)

# Adding new column ("-1" to not include the respective point itself)
centroid$neighbour_50m <- lengths(count_neighbour_50) - 1
centroid$neighbour_100m <- lengths(count_neighbour_100) - 1


# Map 
plot_50m <- ggplot(centroid)+ 
  geom_sf(aes(color=neighbour_50m), size = 1)+
  scale_color_viridis_c(option = "plasma", name = "Neighbours within 50 m distance") +
  labs(
    title = "Neighbours 50 m",
  )

plot_100m <- ggplot(centroid)+ 
  geom_sf(aes(color=neighbour_100m), size = 1)+
  scale_color_viridis_c(option = "plasma", name = "Neighbours within 100 m distance") +
  labs(
    title = "Neighbours 100 m",
  )
plot_50m + plot_100m

# ---- Filtering buffers ----
# Using only buildings with at least 50% of their area within buffers 

# Empty vector to enter the counted neighbours in the for loop below
neighbour_count_50m_filtered <- integer(nrow(centroid))

for (i in seq_len(nrow(buffer_50m))) {
  # BBuffer current centroid 
  buffer_i <- buffer_50m[i, ]
  
  # All centroids within the buffer (including current one)
  neighbour <- count_neighbour_50[[i]]
  all_polygone <- merged_data[neighbour, ]
  
  # Calculating intersection
  intersect <- st_intersection(all_polygone, buffer_i)
  
  # Calculate total area and potential intersection area
  area <- st_area(all_polygone)
  area_intersect <- st_area(intersect)
  
  # Calculating percentage share
  area_precentage <- as.numeric((area_intersect / area)*100)
  
  # Only keeping Buildings with more at least 50% of the area within the buffer
  neighbours_filtered <- neighbour[area_precentage >= 50]
  
  #  excluding centroid[i] from counting
  neighbours_filtered <- neighbours_filtered[neighbours_filtered != i]
  
  # Counting
  neighbour_count_50m_filtered[i] <- length(neighbours_filtered)
}

centroid$neighbour_50m_filtered <- neighbour_count_50m_filtered

# Join Counts to merged_data without geometry
merged_data <- merged_data %>%
  left_join(centroid %>% st_drop_geometry() %>% select(poly_id, neighbour_50m_filtered),
    by = "poly_id"
  )

# TODO: Habe jetzt zwei mal Fläche berechnet, muss eines noch raus schmeißen
# TODO: Map graph
# TODO: BAreplot counting Graph
# TODO: Combine bareplot and Map
# TODO: Thinking about art der darstellung barplot.....smooth?

#---- Creating Map ----

map_neighbour_50 <- ggplot() +
  geom_sf(data = merged_data, aes(fill = factor(neighbour_50m_filtered)), color = NA) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Urban Contrasts in Cairo’s Building Structure",
    subtitle = "Number of neighbouring buildings within a radius of 50 metres"
  )+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    plot.title = element_text(color="#F2F2DE", size = 25),
    plot.subtitle = element_text(color="#F2F2DE", size = 18),
    plot.background = element_rect(fill = "grey30", color = NA),
    plot.margin = margin(t = 5, l = 1, unit = "mm"),)

map_neighbour_50

bar_50 <- ggplot(merged_data, aes(x = factor(neighbour_50m_filtered), fill = factor(neighbour_50m_filtered))) +
  geom_bar() +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    x = "Neighbouring Buildings within a radius of 50 m",
    y = "Count",
    fill = "Type"
  ) +
  facet_wrap(~ type, labeller = as_labeller(c("1" = "Unstructured", "2" = "Structured"))) +
  scale_x_discrete(breaks=as.character(seq(0,54,5)))+
  scale_y_continuous(breaks = seq(0, 500, 100))+
  geom_hline(
    yintercept = seq(0, 500, 100),
    color = "white",
    linetype = "dotted",
    linewidth = 0.3 
  )+
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans 3"),
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, size = 10,
                               colour = "#F2F2DE",
                               family = "Source Sans 3"),
    axis.text.y = element_text(color = "#F2F2DE", family = "Source Sans 3"),
    axis.title.x = element_text(color = "#F2F2DE", family = "Source Sans 3", face = "bold"),
    axis.title.y = element_text(color = "#F2F2DE", angle = 90, family = "Source Sans 3", face = "bold"),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#---- Combine BArplots and map ----
combined_map_bar <- map_neighbour_50 +
  inset_element(bar_50, left = 0.01, right= 0.30, bottom = 0.05, top = 0.35)
combined_map_bar
  