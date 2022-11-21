# Script for crudely converting data from one shapefile to another, even if 
# areas don't perfectly overlap.

# Currently, this function only gives the weighted average from overlapping 
# areas. So this wouldn't work for summing up count data from a group of 
# smaller areas up to one biggest area, for example.

options(pkgType = "binary")
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  sf,
  dplyr
)

shape_convert <- function(old_shape, new_shape, type = "mean") {
  
  intersect_pct <- st_intersection(old_shape, new_shape) %>% 
    mutate(intersect_area = st_area(.)) %>%
    select(
      area_code,
      area_code.1,
      intersect_area
    ) %>%
    st_drop_geometry()
  
  old_shape_joined <- old_shape %>% 
    mutate(county_area = st_area(old_shape)) %>% 
    merge(intersect_pct, by = "area_code", all.x = TRUE)
  
  converted_measure <- old_shape_joined %>% 
    mutate(prop_inter = as.numeric(intersect_area/county_area)) %>% 
    st_drop_geometry() %>% 
    select(
      area_code,
      area_code.1,
      prop_inter,
      measure
    ) %>% 
    group_by(area_code.1) %>% 
    mutate(
      prop_of_prop_inter = prop_inter/sum(prop_inter, na.rm = TRUE),
      check_100 = sum(prop_of_prop_inter),
      measure_weighted = case_when(
        type == "mean" ~ measure*prop_of_prop_inter,
        type == "sum" ~ measure*prop_inter
      ),
      measure_new = sum(measure_weighted, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    select(
      area_code.1,
      measure_new
    ) %>% 
    distinct()
  
  new_shape_measure <- left_join(
    new_shape, 
    converted_measure, 
    by = c("area_code" = "area_code.1")
  ) %>% 
    rename(measure = measure_new)
  
  return(new_shape_measure)
}
