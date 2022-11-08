# `scale_quintile()` splits your measures into three equally sized groups, combines those into a bivariate scale with nine groups, then creates a legend to explain it.

scale_bivariate <- function(
  df, 
  measure_a_label = "Higher measure a", 
  measure_b_label = "Higher measure b"
) {
  
  bivariate_levels <- c(
    "low a, low b",
    "low a, med b",
    "low a, high b",
    "med a, low b",
    "med a, med b",
    "med a, high b",
    "high a, low b",
    "high a, med b",
    "high a, high b"
  )
  
  # Split each measure into 3 quantiles
  measure_grouped_a <- quantile(df$measure_a, 
                                c(0, 1/3, 2/3, 1), 
                                na.rm = TRUE)
  
  measure_grouped_b <- quantile(df$measure_b, 
                                c(0, 1/3, 2/3, 1), 
                                na.rm = TRUE)
  
  # Give each pair of measures a bivariate grouping
  df_grouped <- df %>% 
    mutate(
      fill_grouped = factor(
        case_when(
          measure_a < measure_grouped_a[[2]] & measure_b < measure_grouped_b[[2]] ~ "low a, low b",
          measure_a < measure_grouped_a[[2]] & measure_b >= measure_grouped_b[[2]] & measure_b < measure_grouped_b[[3]] ~ "low a, med b",
          measure_a < measure_grouped_a[[2]] & measure_b >= measure_grouped_b[[3]] ~ "low a, high b",
          
          measure_a >= measure_grouped_a[[2]] & measure_a < measure_grouped_a[[3]] & measure_b < measure_grouped_b[[2]] ~ "med a, low b",
          measure_a >= measure_grouped_a[[2]] & measure_a < measure_grouped_a[[3]] & measure_b >= measure_grouped_b[[2]] & measure_b < measure_grouped_b[[3]] ~ "med a, med b",
          measure_a >= measure_grouped_a[[2]] & measure_a < measure_grouped_a[[3]] & measure_b >= measure_grouped_b[[3]] ~ "med a, high b",
          
          measure_a >= measure_grouped_a[[3]] & measure_b < measure_grouped_b[[2]] ~ "high a, low b",
          measure_a >= measure_grouped_a[[3]] & measure_b >= measure_grouped_b[[2]] & measure_b < measure_grouped_b[[3]] ~ "high a, med b",
          measure_a >= measure_grouped_a[[3]] & measure_b >= measure_grouped_b[[3]] ~ "high a, high b",
          
          is.na(measure_a) | is.na(measure_b) ~ "Missing data"
        ),
        levels = c(bivariate_levels, "Missing data"),
        ordered = TRUE)
    )
  
  # Match the factor with the specified bivariate palette
  names(bivariate_palette) <- levels(df_grouped$fill_grouped)
  fill_scale_final <<- scale_fill_manual(values = bivariate_palette)
  

  # Make Legend graphic
  legend_bivariate <<- tibble(
    x = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    y = c(1:3, 1:3, 1:3),
    bivariate_grouping = factor(bivariate_levels)
  ) %>% 
    ggplot(aes(x, y, fill = bivariate_grouping)) + 
    geom_tile() + 
    fill_scale_final + 
    labs(
      x = paste(measure_a_label, sprintf("\u2794")),
      y = paste(measure_b_label, sprintf("\u2794")),
    ) + 
    coord_fixed() + 
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = 18, family = "sans")
    )
  
  return(df_grouped)
  
}
