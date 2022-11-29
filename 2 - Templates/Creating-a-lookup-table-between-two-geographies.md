Creating a lookup table between two geographies
================

In the walkthrough below, we will match up 533 Westminster Parliamentary
Constituencies (WPC) with 42 Integrated Care Board (ICB) areas to create
a simple lookup file. This will be calculated by seeing in which ICB
areas the WPC centroid points lie.  
  
Note: Many geographies already have lookups provided so check first on
the [ONS Open Geography Portal](https://geoportal.statistics.gov.uk/).  
  
The :red\_circle: symbol is used where you may need to edit code,
download something, or make a choice before running the next code
chunk.  
  
Firstly, install and load the following packages.

``` r
options(pkgType = "binary")
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  here,  # File path referencing
  readr, # Reading data
  dplyr, # General data manipulation
  sf     # Geospatial mapping
)
```

  
:red\_circle: Download your own shapefiles from the [ONS Open Geography
Portal](https://geoportal.statistics.gov.uk/) and read them in below.  
  
In this example we will load in Westminster Parliamentary Constituencies
(WPC) and Integrated Care Board (ICB).

``` r
shape_icb <- read_sf(here("1 - Data", "shapefiles", "ICBs", 
                          "Integrated_Care_Boards_(July_2022)_EN_BFC.shp"))

shape_wpc <- read_sf(here("1 - Data", "shapefiles", "WPCs", 
                          "Westminster_Parliamentary_Constituencies_(Dec_2021)_UK_BFC.shp"))
```

  
If we wanted to match based on percentage overlap of each area, and/or
use a weighting to convert a measure too, we’d use the [shape\_convert
function](https://github.com/DataS-DHSC/geospatial-vis-templates/tree/master/2%20-%20Templates/extra_scripts/shape_convert.R).  
  
Here we will just match based on whether a WPC centroid is within an ICB
or not. To do this we need to drop the WPC shape geometry and use the
centroid points instead.

``` r
points_wpc <- shape_wpc %>% 
  st_drop_geometry() %>% # Drop the geometry for the area polygons.
  st_as_sf(coords = c("LONG", "LAT"), crs = 4326) # Set the centroid points as the geometry.
```

  
As the Coordinate Reference System (CRS) is not exactly the same between
the WPC points and ICB areas, we need to make them match.

``` r
points_wpc_match <- points_wpc %>% 
  st_transform(crs = st_crs(shape_icb)) # Give the point geometry the CRS of the ICB areas.
```

  
Now we use `st_intersection` match up ICB areas and WPC centroid points.
As we just want a lookup table, we drop the geometry using
`st_drop_geometry` and just `select` the area names and codes of
interest.  
  
:red\_circle: If you have used different areas, you will need to swap
out the names and codes here.

``` r
intersect_pct <- st_intersection(points_wpc_match, shape_icb) %>% 
  st_drop_geometry() %>% 
  select(
    PCON21CD,
    PCON21NM,
    ICB22CD,
    ICB22NM
  )
```

  
Quick check that no centroid point is in more than one area. This should
say TRUE.

``` r
intersect_pct %>% 
  count(PCON21CD, ICB22CD, sort = TRUE) %>% 
  pull(n) %>% 
  max() == 1
```

    ## [1] TRUE

  
Write the new lookup table to a csv file.  
  
:red\_circle: Change the file name and path to suit.

``` r
write_csv(intersect_pct, "WPC_212021_ICB_072022_lookup.csv")
```
