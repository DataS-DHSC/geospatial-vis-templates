# Colour palette accessibility checker

options(pkgType = "binary")
if (!require("pacman")) install.packages("pacman")

p_load(
  tidyverse,
  janitor,
  savonliquide,
  gt,
  scales
)


check_palette <- function(p) {
  
  palette <- tolower(p)
  
  palette_wbg <- c(
    "#ffffff", # white
    "#000000", # black
    "#999999", # grey60, commonly used for missing data
    "#cccccc", # grey80, commonly used for missing data
    palette
  )
  
  ratios <- sapply(palette_wbg, function(a)
    sapply(palette_wbg, function(b)
      check_contrast_raw(a,b)$ratio)) %>% 
    data.frame() %>% 
    clean_names() %>% 
    rownames_to_column("compare_to")
  
  table <- ratios %>% 
    pivot_longer(cols = -compare_to) %>% 
    mutate(
      value = as.numeric(value),
      name = str_replace(name, "x_", "#"),
      new_name = factor(case_when(
        name == "#ffffff" ~ "White",
        name == "#000000" ~ "Black",
        name == "#999999" ~ "grey60",
        name == "#cccccc" ~ "grey80",
        TRUE ~ name
      ),
      levels = c("White", "Black", "grey60", "grey80", palette),
      ordered = TRUE
      ),
      Colours = factor(case_when(
        compare_to == "#ffffff" ~ "White",
        compare_to == "#000000" ~ "Black",
        compare_to == "#999999" ~ "grey60",
        compare_to == "#cccccc" ~ "grey80",
        TRUE ~ compare_to
      ),
      levels = c("White", "Black", "grey60", "grey80", palette),
      ordered = TRUE
      )
    ) %>% 
    select(-name, -compare_to) %>% 
    pivot_wider(
      names_from = new_name, 
      values_from = value
    )
  
  table %>% 
    gt() %>% 
    data_color(
      columns = names(table)[-1],
      colors = col_numeric(
        palette = c("#D7EFC3", "#88D147"),
        domain = c(3, 21),
        na.color = "#000000"
      )
    ) %>% 
    suppressWarnings()
  
}

# DHSC_accessible_5_g <- c("#294011", "#4C721D", "#589325", "#88D147", "#D7EFC3")
# check_palette(DHSC_accessible_5)
