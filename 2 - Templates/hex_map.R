# Separate script to run a basic choropleth first?...

options(pkgType = "binary")
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  here, # File path referencing
  data.table, # Fast reading/writing
  janitor, # Rounding
  dplyr, # General data manipulation
  tidyr, # More general data manipulation
  ggplot2, # General plotting
  sf, # Geospatial mapping
  scales, # Commas for legend
  stringr, # str_detect()
  knitr, # include_graphics()
  cowplot, # Extra plotting functions
  geogrid,
  geofacet
)

df_measure <- fread(here("1 - Data/example_data", "example_data_lad.csv")) %>% 
  tibble()

shape_one <- read_sf(here("1 - Data/shapefiles/LADs", "LAD_DEC_2021_UK_BFC.shp")) %>% 
  rename(area_code = LAD21CD)

df_measure_shape <- left_join(shape_one, df_measure, by = "area_code")

shape_one_england <- function(df) {df %>% filter(str_detect(area_code, "^E"))}

df_measure_shape %>% 
  tibble() %>% 
  shape_one_england %>% 
  filter(is.na(measure)) %>% 
  select(area_code, measure)

source(here("2 - Templates", "extra_scripts", "scale_quintile.R"))

df_grouped <- scale_quintile(
  round_to = 100, # Denomination to round to
  decimal_places = 0 # Decimal places to round to (0 for count data)
)

# Check legend labels look correct
levels(df_grouped$fill_grouped)

fill_palette <- c(
  "#294011", # Q5 (Highest values)
  "#4C721D", # Q4
  "#589325", # Q3
  "#88D147", # Q2
  "#D7EFC3", # Q1 (Lowest values)
  "grey80" # For missing data
)

names(fill_palette) <- levels(df_grouped$fill_grouped)
fill_scale_final <- scale_fill_manual(values = fill_palette)

# Test legend colours are correct, along with general distribution of the measure
df_grouped %>% 
  ggplot(aes(1, measure, fill = fill_grouped)) + 
  geom_jitter(
    height = 0,
    shape = 21, 
    size = 1.5,
    stroke = 0,
    colour = "transparent"
  ) + 
  theme_minimal() + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) + 
  guides(fill = guide_legend(override.aes = list(size = 10, shape = 22))) + 
  fill_scale_final

boundary_colour <- "black"

p_map <- df_grouped %>%
  shape_one_england() %>%
  ggplot() +
  geom_sf(
    aes(fill = fill_grouped), 
    size = 0.1,
    colour = boundary_colour
  ) +
  fill_scale_final +
  coord_sf(expand = FALSE, clip = "off") +
  labs(
    title = "Chart title goes here",
    fill = "Legend title goes here",
    caption = "Caption / data source details can go down here."
  ) +
  theme_void(base_size = 18, base_family = "sans") +
  theme(legend.position = c(0.84, 0.93),
        plot.margin = margin(0, 10, 10, 10),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot")


# Hex/square map, note that assign_polygons() can take a while to run. 
# Only use if less than 500 areas. I.e. not for MSOA, or LSOA.

new_cells_hex <- calculate_grid(shape = df_grouped %>% shape_one_england(), 
                                grid_type = "hexagonal", seed = 3)
plot(new_cells_hex)
df_grouped_hex <- assign_polygons(shape = df_grouped %>% shape_one_england(), 
                             new_polygons = new_cells_hex)

new_cells_square <- calculate_grid(shape = df_grouped %>% shape_one_england(), 
                                   grid_type = "regular", seed = 3)
plot(new_cells_square)
df_grouped_square <- assign_polygons(shape = df_grouped %>% shape_one_england(), 
                                     new_cells_square)


df_grouped_square %>%
  ggplot() +
  geom_sf(
    aes(fill = fill_grouped), 
    size = 0.1,
    colour = boundary_colour
  ) + 
  fill_scale_final +
  coord_sf(expand = FALSE, clip = "off") +
  labs(
    title = "Chart title goes here",
    fill = "Legend title goes here",
    caption = "Caption / data source details can go down here."
  ) +
  theme_void(base_size = 18, base_family = "sans") +
  theme(
    legend.position = c(0.84, 0.93),
        plot.margin = margin(0, 10, 10, 10),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot"
    )


# Maybe use the premade hex maps for MSOA, LAD, then make a set of other grids?



