# Colour palette accessibility checker (work in progress)

options(pkgType = "binary")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  janitor,
  savonliquide
)


check_palette <- function(p) {
  # choose the palette to check here
  palette <- tolower(p)
  palette_wbg <- c(
    "#ffffff", # white
    "#000000", # black
    "#999999", # grey60, commonly used for missing data
    "#CCCCCC", # grey80, commonly used for missing data
    palette
  )
  
  test <- sapply(palette_wbg, function(a)
    sapply(palette_wbg, function(b)
      check_contrast_raw(a,b)$ratio)) %>% 
    data.frame() %>% 
    clean_names() %>% 
    rownames_to_column("compare_to")
  
  test %>% 
    pivot_longer(cols = -compare_to) %>% 
    mutate(
      value = as.numeric(value),
      name = factor(
        str_replace(name, "x_", "#"),
        levels = c(palette_wbg),
        ordered = TRUE
      )
    ) %>% 
    filter(value >= 3) %>% 
    pivot_wider(names_from = name, values_from = value)
  
}

DHSC_accessible_5 <- c("#512698", "#E57200", "#1A9CCB", "#2F4D14", "#D06DCE")
DHSC_accessible_5_g <- c("#294011", "#4C721D", "#589325", "#88D147", "#D7EFC3")
viridis_quintile <- c("#440154", "#3b528b", "#21918c", "#5ec962", "#fde725")
ibm_palette <- c("#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000")

check_palette(DHSC_accessible_5_g)

