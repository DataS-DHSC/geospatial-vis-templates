# Create zoomed version of a map

zoom_plot <- function(title_text, coords) {
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
    xlim(coords[1], coords[2]) +
    ylim(coords[3], coords[4]) +
    coord_sf(expand = FALSE, 
             clip = "on") + 
    theme_void() + 
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0))
}