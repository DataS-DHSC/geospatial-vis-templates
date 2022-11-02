# https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/

measure_grouped_a <- quantile(df_measure_shape$measure_a, 
                            c(0, 1/3, 2/3, 1), 
                            na.rm = TRUE)

measure_grouped_b <- quantile(df_measure_shape$measure_b, 
                              c(0, 1/3, 2/3, 1), 
                              na.rm = TRUE)

df_grouped <- df_measure_shape %>% 
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
      levels = c(
        "low a, low b",
        "low a, med b",
        "low a, high b",
        "med a, low b",
        "med a, med b",
        "med a, high b",
        "high a, low b",
        "high a, med b",
        "high a, high b",
        "Missing data"
      ),
      ordered = TRUE)
  )

names(bivariate_palette) <- levels(df_grouped$fill_grouped)
fill_scale_bivariate <- scale_fill_manual(values = bivariate_palette)
