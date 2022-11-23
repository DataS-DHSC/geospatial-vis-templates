# shape_convert() takes a measure with shape or point geography (old_shape) and 
# a new shape to convert the measure to (new_shape).

# This function creates a new measure column calculated as a weighted average 
# from overlapping areas when type = "mean", and sums up the weighted count when 
# type = "sum".

# weight = "none", take the unweighted sum/mean of all points or areas that 
# overlap.

# weight = "weighting", weight the sum/mean of all points or areas that overlap 
# by a given measure.

# weight = "overlap", take the sum/mean of all areas that overlap, weighted by
# the % overlap of old and new areas.

# weight = "weighting and overlap", take the sum/mean of all areas that overlap, 
# weighted by the % overlap of old and new areas plus a given measure.

# Your weighting column should be included in old_shape, if required.


options(pkgType = "binary")
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  sf,
  dplyr
)

shape_convert <- function(old_shape, new_shape, weight, type) {
  
  # Calculate the area of overlap between old and new areas
  intersect_pct <<- st_intersection(old_shape, new_shape) %>% 
    mutate(
      intersect_area = st_area(.),
      weighting = ifelse(str_detect(weight, "weighting"), weighting, 1)
      ) %>%
    select(
      area_code,
      area_code.1,
      intersect_area,
      weighting
    ) %>%
    st_drop_geometry() %>% 
    mutate(intersect_area = ifelse(weight == "none", 1, intersect_area))
  
  # Join the overlap data back to old shape to compare with original area
  old_shape_joined <<- old_shape %>% 
    mutate(county_area = ifelse(weight == "none", 1, st_area(old_shape))) %>% 
    select(-contains("weighting")) %>% # so we don't end up with two of the same column
    merge(intersect_pct, by = "area_code", all.x = TRUE)
  
  # Calculate % area overlap, then aggregate measure based on this
  converted_measure <- old_shape_joined %>% 
    mutate(
      prop_inter = case_when(
        weight == "none" ~ 1, 
        weight == "weighting" ~ weighting, 
        weight == "overlap" ~ as.numeric(intersect_area/county_area), 
        weight == "weighting and overlap" ~ weighting + as.numeric(intersect_area/county_area) 
      )
    ) %>% 
    st_drop_geometry() %>% 
    select(
      area_code,
      area_code.1,
      prop_inter,
      weighting,
      measure
    ) %>% 
    group_by(area_code) %>% 
    mutate(
      prop_of_prop_inter_old = prop_inter/sum(prop_inter, na.rm = TRUE),
      check_100_old = sum(prop_of_prop_inter_old),
    ) %>% 
    group_by(area_code.1) %>% 
    mutate(
      prop_of_prop_inter_new = prop_inter/sum(prop_inter, na.rm = TRUE),
      check_100_new = sum(prop_of_prop_inter_new),
      measure_weighted = case_when(
        type == "mean" ~ measure*prop_of_prop_inter_new,
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
  
  # Join back to shapefile geometry
  new_shape_measure <- left_join(
    new_shape, 
    converted_measure, 
    by = c("area_code" = "area_code.1")
  ) %>% 
    rename(measure = measure_new)
  
  return(new_shape_measure)
}
