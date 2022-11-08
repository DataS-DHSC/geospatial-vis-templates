# `scale_quintile()` splits your measure into five equally sized groups, rounds each value up to the nearest `round_to`, whilst keeping the numbers at a specified number of `decimal_places`.
# Below are some examples uses of `scale_quintile()` with different continous or count data, showing the labels it creates.
# 
# scale_quintile(df, case_count, round_to = 100, decimal_places = 0)
# "89,200 - 368,200"
# "55,700 - 89,100"
# "43,100 - 55,600"
# "33,600 - 43,000"
# "6,200 - 33,500"
# 
# scale_quintile(df, estimate, round_to = 0.005, decimal_places = 3)
# "0.055 - 0.075"
# "0.045 - 0.050"
# "0.035 - 0.040"
# "0.020 - 0.030"
# "0.005 - 0.015"
# 
# scale_quintile(df, proportion, round_to = 1, decimal_places = 0)
# "88 - 100"
# "58 - 87"
# "35 - 56"
# "19 - 34"
# "0 - 18"


scale_quintile <- function(df, measure, round_to, decimal_places) {
  
  # Calculate quintile cut off values, and round them.
  measure_grouped <- quantile(
    df$measure, 
    c(0, 0.2, 0.4, 0.6, 0.8, 1), 
    na.rm = TRUE
  ) %>% 
    round_half_up(digits = decimal_places)
  
  # Round groups up to nearest round_to
  measure_grouped[[1]] <- round_half_up(measure_grouped[[1]]/round_to)*round_to
  measure_grouped[[2]] <- round_half_up(measure_grouped[[2]]/round_to)*round_to
  measure_grouped[[3]] <- round_half_up(measure_grouped[[3]]/round_to)*round_to
  measure_grouped[[4]] <- round_half_up(measure_grouped[[4]]/round_to)*round_to
  measure_grouped[[5]] <- round_half_up(measure_grouped[[5]]/round_to)*round_to
  measure_grouped[[6]] <- round_half_up(measure_grouped[[6]]/round_to)*round_to
  
  df_grouped <- df %>% 
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
