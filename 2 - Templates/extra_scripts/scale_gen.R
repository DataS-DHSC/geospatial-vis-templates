# Automatically generate quintiles for a fill legend

scale_gen <- function(round_to = 1, decimal_places = 0) {
  
  # calculate quintile cut off values, and round them to avoid interpolation 
  # beyond lowest denomination in original data. 
  measure_grouped <- quantile(df_measure_shape$measure, 
                        c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                        na.rm = TRUE) %>% 
    round_half_up(digits = decimal_places)
  
  # round middle quintiles to nearest round_to 
  measure_grouped[[2]] <- round_half_up(measure_grouped[[2]]/round_to)*round_to
  measure_grouped[[3]] <- round_half_up(measure_grouped[[3]]/round_to)*round_to
  measure_grouped[[4]] <- round_half_up(measure_grouped[[4]]/round_to)*round_to
  measure_grouped[[5]] <- round_half_up(measure_grouped[[5]]/round_to)*round_to
  
  df_grouped <- df_measure_shape %>% 
    mutate(
      fill_grouped = factor(
        case_when(
          measure < measure_grouped[[2]] ~ paste0(comma(measure_grouped[[1]], accuracy = round_to), " - ", comma(measure_grouped[[2]] - round_to, accuracy = round_to), sep = ""),
          measure >= measure_grouped[[2]] & measure < measure_grouped[[3]] ~ paste0(comma(measure_grouped[[2]], accuracy = round_to), " - ", comma(measure_grouped[[3]] - round_to, accuracy = round_to), sep = ""),
          measure >= measure_grouped[[3]] & measure < measure_grouped[[4]] ~ paste0(comma(measure_grouped[[3]], accuracy = round_to), " - ", comma(measure_grouped[[4]] - round_to, accuracy = round_to), sep = ""),
          measure >= measure_grouped[[4]] & measure < measure_grouped[[5]] ~ paste0(comma(measure_grouped[[4]], accuracy = round_to), " - ", comma(measure_grouped[[5]] - round_to, accuracy = round_to), sep = ""),
          measure >= measure_grouped[[5]] ~ paste0(comma(measure_grouped[[5]], accuracy = round_to), " - ", comma(measure_grouped[[6]], accuracy = round_to), sep = ""),
          is.na(measure) ~ "Missing data"
        ),
        levels = c(
          paste0(comma(measure_grouped[[5]], accuracy = round_to), " - ", comma(measure_grouped[[6]], accuracy = round_to), sep = ""),
          paste0(comma(measure_grouped[[4]], accuracy = round_to), " - ", comma(measure_grouped[[5]] - round_to, accuracy = round_to), sep = ""),
          paste0(comma(measure_grouped[[3]], accuracy = round_to), " - ", comma(measure_grouped[[4]] - round_to, accuracy = round_to), sep = ""),
          paste0(comma(measure_grouped[[2]], accuracy = round_to), " - ", comma(measure_grouped[[3]] - round_to, accuracy = round_to), sep = ""),
          paste0(comma(measure_grouped[[1]], accuracy = round_to), " - ", comma(measure_grouped[[2]] - round_to, accuracy = round_to), sep = ""),
          "Missing data"
        ),
        ordered = TRUE)
    )
  
  return(df_grouped)
  
}