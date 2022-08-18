Static choropleth map example
================

The example code below can be used to create a static choropleth map of
England, with optional zoomed in areas for London, the north east and
north west.

Install and load packages:

``` r
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  here, # File path referencing
  data.table, # Fast reading/writing
  janitor, # Rounding
  dplyr, # general data manipulation
  ggplot2, # General plotting
  sf, # Geospatial mapping
  scales, # Commas for legend
  stringr, # str_detect()
  RColorBrewer, # colour palettes
  knitr, # include_graphics()
  cowplot # draw_plot()
)
```

  
Load in shapefiles for England. In this example we’ll use Middle Layer
Super Output Areas (MSOA) and Local Authority Districts (LAD).  
MSOA -
<https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-full-clipped-bfc-ew-v3/explore?location=52.775763%2C-2.489527%2C7.39>  
LAD -
<https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2021-uk-bfc>  
`shape_one` is the area to be filled with colour, whilst `shape_two`
will provide the boundary lines.

``` r
shape_one <- read_sf(here("1 - Data/shapefiles/MSOAs", "Middle_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Clipped__BFC__EW_V3.shp")) %>% 
  rename(area_code = MSOA11CD)

shape_two <- read_sf(here("1 - Data/shapefiles/LADs", "LAD_DEC_2021_UK_BFC.shp")) %>% 
  rename(area_code = LAD21CD)
```

  
Read in example data. Replace with your own data using `area_code` and
`measure`:

``` r
df_measure <- fread("https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage&format=csv") %>% 
  distinct() %>% 
  group_by(areaCode) %>% 
  filter(date == max(date)) %>% 
  rename(area_code = areaCode,
         measure = cumVaccinationFirstDoseUptakeByVaccinationDatePercentage)
```

  
Join shapefile for `shape_one` with your data:

``` r
# join shapefile with data
df_measure_shape <- left_join(shape_one, df_measure, by = "area_code")
```

  
Function to automatically generate quintiles for the fill legend:

``` r
scale_gen <- function(round_to = 1, small = 1, decimal_places = 0) {
  
  # calculate quintile cut off values, and round them to avoid interpolation 
  # beyond lowest denomination in original data. 
  measure_q <- quantile(df_measure_shape$measure, 
                        c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                        na.rm = TRUE) %>% 
    round_half_up(digits = decimal_places)
  
  # round middle quintiles to nearest round_to 
  measure_q[[2]] <- round_half_up(measure_q[[2]]/round_to)*round_to
  measure_q[[3]] <- round_half_up(measure_q[[3]]/round_to)*round_to
  measure_q[[4]] <- round_half_up(measure_q[[4]]/round_to)*round_to
  measure_q[[5]] <- round_half_up(measure_q[[5]]/round_to)*round_to
  
  df_q <- df_measure_shape %>% 
    mutate(
      fill_q = factor(
        case_when(
          measure < measure_q[[2]] ~ paste0(comma(measure_q[[1]], accuracy = round_to), " - ", comma(measure_q[[2]] - small, accuracy = round_to), sep = ""),
          measure >= measure_q[[2]] & measure < measure_q[[3]] ~ paste0(comma(measure_q[[2]], accuracy = round_to), " - ", comma(measure_q[[3]] - small, accuracy = round_to), sep = ""),
          measure >= measure_q[[3]] & measure < measure_q[[4]] ~ paste0(comma(measure_q[[3]], accuracy = round_to), " - ", comma(measure_q[[4]] - small, accuracy = round_to), sep = ""),
          measure >= measure_q[[4]] & measure < measure_q[[5]] ~ paste0(comma(measure_q[[4]], accuracy = round_to), " - ", comma(measure_q[[5]] - small, accuracy = round_to), sep = ""),
          measure >= measure_q[[5]] ~ paste0(comma(measure_q[[5]], accuracy = round_to), " - ", comma(measure_q[[6]], accuracy = round_to), sep = ""),
          is.na(measure) ~ "Missing data"
        ),
        levels = c(
          paste0(comma(measure_q[[5]], accuracy = round_to), " - ", comma(measure_q[[6]], accuracy = round_to), sep = ""),
          paste0(comma(measure_q[[4]], accuracy = round_to), " - ", comma(measure_q[[5]] - small, accuracy = round_to), sep = ""),
          paste0(comma(measure_q[[3]], accuracy = round_to), " - ", comma(measure_q[[4]] - small, accuracy = round_to), sep = ""),
          paste0(comma(measure_q[[2]], accuracy = round_to), " - ", comma(measure_q[[3]] - small, accuracy = round_to), sep = ""),
          paste0(comma(measure_q[[1]], accuracy = round_to), " - ", comma(measure_q[[2]] - small, accuracy = round_to), sep = ""),
          "Missing data"
        ),
        ordered = TRUE)
    )
  
  return(df_q)
  
}
```

  
Set quintile parameters (dependant on the type of data you’re using):  
**round\_to**: denomination to round legend values to (not including min
or max).  
**small**: smallest rightmost digit in measure (1 for count data). Used
in the creation of labels to avoid overlapping quantiles.  
**decimal\_places**: decimal places to round data to in next step (0 for
count data).

``` r
df_q <- scale_gen(round_to = 0.1, small = 0.1, decimal_places = 1)
```

  
Make fill scale using the quintiles:

``` r
fill_palette_q <- c(rev(brewer.pal(5, name = "Blues")), "grey80")
names(fill_palette_q) <- c(levels(df_q$fill_q))
fill_scale_q <- scale_fill_manual(values = fill_palette_q)

df_map <- df_q %>% mutate(fill_final = fill_q)
fill_scale_final <- fill_scale_q

fill_palette_q
```

    ##  89.9 - 94.7  86.9 - 89.8  82.2 - 86.8  73.2 - 82.1  38.6 - 73.1 Missing data 
    ##    "#08519C"    "#3182BD"    "#6BAED6"    "#BDD7E7"    "#EFF3FF"     "grey80"

  
Plot choropleth map of England. You can change the text in `labs`.

``` r
p_map <- df_map %>%
  filter(str_detect(area_code, "^E")) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry,
              fill = fill_final),
          colour = NA) +
  geom_sf(data = subset(shape_two, str_detect(area_code, "^E")),
          aes(geometry = geometry),
          fill = NA,
          colour = "black",
          size = 0.1) +
  fill_scale_final +
  coord_sf(expand = FALSE,
           clip = "off") +
  labs(
    title = "Chart title goes here",
    fill = "Legend title goes here",
    caption = "Caption / data source details can go down here."
  ) +
  theme_void(base_size = 18,
             base_family = "sans") +
  theme(legend.position = c(0.84, 0.93),
        plot.margin = margin(0, 10, 10, 10),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot")

ggsave(p_map, dpi = 300, width = 12, height = 14, units = "in",
       filename = here("2 - Templates", "output_vis", "choropleth_2area.jpeg"))
```

![](output_vis/choropleth_2area.jpeg) 
Include zoomed in areas for Greater London, North West England. and
North East England. Firstly, here’s a function to create a zoomed in
area of the map:

``` r
zoom_plot <- function(title_text, x1, x2, y1, y2) {
  df_map %>% 
    filter(str_detect(area_code, "^E")) %>% 
    ggplot() + 
    geom_sf(aes(geometry = geometry,
                fill = fill_final),
            colour = NA) + 
    geom_sf(data = subset(shape_two, str_detect(area_code, "^E")),
            aes(geometry = geometry),
            fill = NA,
            colour = "black",
            size = 0.1) +
    labs(title = title_text) + 
    fill_scale_final + 
    xlim(x1, x2) +
    ylim(y1, y2) +
    coord_sf(expand = FALSE, 
             clip = "on") + 
    theme_void() + 
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0))
}
```

  
Now we can add in descriptions and coordinates of the areas of interest.

``` r
p_nee <- zoom_plot("North East England", 408000, 480000, 505000, 595000)
p_nwe <- zoom_plot("North West England", 320000, 410000, 375000, 425000)
p_glondon <- zoom_plot("Greater London", 500000, 565000, 155000, 201500)
```

  
Finally, we use cowplot to plot the different areas together.

``` r
p_map_zoom <- ggdraw() + 
  draw_plot(p_map, 0, 0, 1, 1) + 
  draw_plot(p_nee, 0.025, 0.608, 0.25, 0.4) + 
  draw_plot(p_nwe, 0.025, 0.466, 0.35, 0.18) + 
  draw_plot(p_glondon, 0.025, 0.248, 0.30, 0.20)

ggsave(p_map_zoom, dpi = 300, width = 12, height = 14, units = "in",
       filename = here("2 - Templates", "output_vis", "choropleth_2area_zoom.jpeg"))
```

![](output_vis/choropleth_2area_zoom.jpeg)
