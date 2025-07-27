# Packages
library(sf)
library(terra)
library(landscapemetrics)
library(dplyr)
library(tidyr)
library(ggplot2)

library(showtext)
font_add_google("Source Sans 3", "Source Sans 3")
showtext_auto()

# Load Data (Old Path)

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

# rasterisation with cell size of 5m
raster <- rast(ext(merged_data), resolution = 5)

# raster with type-values
r_type <- rasterize(merged_data, raster, field = "type")

# raster with ID-values
r_polyid <- rasterize(merged_data, raster, field = "poly_id")

plot(r_polyid)
plot(r_type)


################################################################################
### How many Buildings touch a building? ###
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
  theme_fancy_map()+
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
combined_map_bar