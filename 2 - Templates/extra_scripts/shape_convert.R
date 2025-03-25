options(pkgType = "binary")
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  sf,
  dplyr
)

#' Convert a measure from one geography to another
#'
#' This function takes a spatial dataset (`old_shape`) containing a measure
#' and converts it into a new spatial dataset (`new_shape`).
#' The conversion can be unweighted, or weighted based on area overlap, an
#' external measure (e.g., population), or both.
#'
#' @param old_shape An `sf` object representing the original spatial dataset
#' with measures. It must contain an `area_code` column and, if required, a
#' weighting column.
#' @param new_shape An `sf` object representing the target spatial dataset to
#' which the measure should be converted. It must contain an `area_code` column.
#' @param weight A character string specifying the weighting method:
#'   - `"none"`: No weighting, taking a simple mean or sum.
#'   - `"weighting"`: Weights the measure based on an external measure.
#'   - `"overlap"`: Weights based on the percentage overlap between `old_shape`
#'   and `new_shape`.
#'   - `"weighting and overlap"`: Combines both external weighting and
#'   percentage overlap.
#' @param type A character string to indicate whether we want the new area
#' measure to be calculated as `"mean"` or `"sum"` or the old area measure.
#' @param use_missing Logical. If `TRUE`, any missing measure data will not be
#' included in the new measure. This is not recommended.
#' @param extra_cols Logical. If `TRUE`, the function returns additional columns
#' used to calculate the new data. This is for sense checking purposes.

shape_convert <- function(
    old_shape,
    new_shape,
    weight = c("none", "weighting", "overlap", "weighting and overlap"),
    type = c("sum", "mean"),
    use_missing = FALSE,
    extra_cols = FALSE
) {
  
  # Pick first value as default for type
  type = match.arg(type)
  
  # Check expected columns are present
  if (!("area_code" %in% colnames(old_shape))) {
    stop("`old_shape` needs a `area_code` column. Function stopped.")
  }
  
  if (!("weighting" %in% colnames(old_shape) & str_detect(weight, "weighting"))) {
    stop("`old_shape` needs a `weighting` column. Function stopped.")
  }
  
  # Warn if user tries to use missing values
  if (use_missing) {
    warning("Including NA values is not recommended. Use extra_cols = FALSE and check for bad aggregations.")
  }
  
  # Ensure they use the same coordinate reference system (CRS)
  if (st_crs(old_shape) != st_crs(new_shape)) {
    new_shape <- st_transform(new_shape, st_crs(old_shape))
  }
  
  # Calculate the area of overlap between old and new areas
  intersect_pct <- st_intersection(old_shape, new_shape) %>%
    mutate(intersect_area = as.numeric(st_area(.))) %>%
    rename(
      area_code_old = area_code,
      area_code_new = area_code.1,
    ) %>%
    select(
      area_code_old,
      area_code_new,
      intersect_area,
      weighting
    ) %>%
    st_drop_geometry() %>%
    # If we want to disregard area overlap, set the intersect area to 1.
    mutate(
      intersect_area = case_when(
        weight == "none" ~ 1,
        TRUE ~ intersect_area
      )
    )
  
  # If we don't want a weighting, make a weighting column of 1
  if (!str_detect(weight, "weighting")) {
    intersect_pct <- intersect_pct %>% mutate(weighting = 1)
  }
  
  # Join the overlap data back to old shape to compare with original area
  old_shape_joined <- old_shape %>%
    mutate(
      old_area = case_when(
        weight == "none" ~ 1,
        TRUE ~ as.numeric(st_area(old_shape))
      )
    ) %>%
    st_drop_geometry() %>%
    select(-weighting) %>% # just need one
    left_join(intersect_pct, by = c("area_code" = "area_code_old")) %>%
    rename(area_code_old = area_code) %>%
    select(
      area_code_old,
      area_code_new,
      old_area,
      intersect_area,
      weighting,
      measure
    )
  
  # Calculate % area overlap, then aggregate measure based on this
  converted_measure <- old_shape_joined %>%
    # Convert weighting into % weighting in old area.
    # E.g. If using population as weighting, for each old area, this would give
    # you the % out of the total population in all old areas that intersect with
    # the new area.
    group_by(area_code_new) %>%
    mutate(weighting_sum = case_when(
       !use_missing ~ sum(weighting),
       use_missing ~ sum(weighting, na.rm = TRUE)
    )
    ) %>%
    group_by(area_code_old) %>%
    mutate(weighting_as_prop = weighting / weighting_sum) %>%
    ungroup() %>%
    mutate(
      # Calculate the proportion of the old area intersects with the new area.
      prop_old_in_new = intersect_area / old_area,
    ) %>%
    group_by(area_code_old) %>%
    mutate(
      # Calculate a scalar for the measure based the weight choice.
      conversion_scalar_part = case_when(
        weight == "none" ~ 1,
        weight == "weighting" ~ weighting_as_prop,
        weight == "overlap" ~ prop_old_in_new,
        weight == "weighting and overlap" ~ weighting_as_prop * prop_old_in_new
      ),
      
      # - If old_shape is areas, make the scalar add to 1 for each new area.
      # This makes it easier to calculate measure_new in the next step.
      # - If old_shape is points, weight == "none", and we want the mean, then
      # we treat it the same as with areas.
      # - If old_shape is points, weight == "none", and we want the sum
      # (aggregated into new area), then leave the conversion_scalar_part as 1
      # so it can stack.
      conversion_scalar = case_when(
        weight != "none" ~ conversion_scalar_part / sum(conversion_scalar_part),
        weight == "none" & type == "mean" ~ conversion_scalar_part / sum(conversion_scalar_part),
        weight == "none" & type == "sum" ~ conversion_scalar_part
      )
    ) %>%
    ungroup() %>%
    group_by(area_code_new) %>%
    mutate(
      # First, apply scalar to each old measure.
      measure_new_part = measure * conversion_scalar,
      
      # Create new weighted measure by summing or averaging these parts.
      # This structure feels quite confusing now, but the output data looks ok.
      measure_new = case_when(
        type == "mean" & !use_missing ~ mean(measure_new_part),
        type == "sum" & !use_missing ~ sum(measure_new_part),
        type == "mean" & use_missing ~ mean(measure_new_part, na.rm = TRUE),
        type == "sum" & use_missing ~ sum(measure_new_part, na.rm = TRUE)
      ),
      
      # Get a count of old areas that were used to make the new area
      count_overlap = n()
    ) %>%
    ungroup() %>%
    select(
      area_code_new,
      area_code_old,
      old_area,
      intersect_area,
      prop_old_in_new,
      weighting,
      conversion_scalar,
      measure,
      measure_new,
      measure_new_part,
      count_overlap
    ) %>%
    ungroup() %>%
    distinct()
  
  if (!extra_cols) {
    converted_measure <- converted_measure %>%
      select(area_code_new, measure_new, count_overlap) %>%
      distinct()
  }
  
  # Join back to shapefile geometry
  new_shape_measure <- left_join(
    new_shape,
    converted_measure,
    by = c("area_code" = "area_code_new")
  ) %>%
    rename(measure = measure_new)
  
  return(new_shape_measure)
}
