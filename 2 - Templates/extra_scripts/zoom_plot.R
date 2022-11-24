# Create zoomed version of a map - currently only to be used within the 
# choropleth templates

zoom_plot <- function(title_text, coords) {
  df_grouped %>% 
    shape_one_england() %>% 
    ggplot() + 
    geom_sf(
      aes(fill = fill_grouped),
      colour = NA
    ) + 
    geom_sf(
      data = shape_two_england(shape_two),
      fill = NA,
      colour = boundary_colour,
      size = 0.1
    ) +
    labs(title = title_text) + 
    fill_scale_final + 
    xlim(coords[1], coords[2]) +
    ylim(coords[3], coords[4]) +
    coord_sf(
      expand = FALSE, 
      clip = "on"
      ) + 
    theme_void() + 
    theme(
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 0)
    )
}