library(sf) 
library(ggplot2)
library(dplyr) 
library(purrr)
library(readr)
library(tidyr)
library(jsonlite)


#read in trusts
trusts <- read_json("https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?PrimaryRoleId=RO197&Limit=1000")
trusts <- bind_rows(trusts) %>% filter(Status == "Active")

#functions to get lon and lat from open api
get_lon <- function(postcode){
  tryCatch(
    {
      api_response <- read_json(paste0("https://api.postcodes.io/postcodes/", postcode))[2]
      lon <- unlist(api_response)[7]
      return(lon)
    },
    error = function(n){
      message("no lat found")
      return(NA)
    }
  )
  
}


get_lat <- function(postcode){
  tryCatch(
    {
      api_response <- read_json(paste0("https://api.postcodes.io/postcodes/", postcode))[2]
      lat <- unlist(api_response)[8]
      return(lat)
    },
    error = function(cond){
      message("no lat found")
      return(NA)
    }
  )
  
}


#get lon and lat for trusts
lat <- map_chr(.x = trusts$PostCode, .f = get_lat)
lon <- map_chr(.x = trusts$PostCode, .f = get_lon)

trusts$lat = as.numeric(lat)
trusts$lon = as.numeric(lon)


#get icb shapefile - downloaded from ons
icb_boundaries <- read_sf(httr::content(
  httr::GET(url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_July_2022_EN_BGC_v2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"), as = "text"))

#filter single ICB - here Chester
single_ICB <- icb_boundaries %>% filter(ICB22CD == "E54000008")


#find which trusts are within ICB boundaries
#turn lats and lons into a geometry
trusts_in_icb <- trusts %>%
  mutate(trust_lat = lat, trust_lon = lon) %>%
  filter(!is.na(lat)) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = st_crs(4326))

#find which trusts intersect with boundaries
trusts_in_icb <- trusts_in_icb %>% mutate(
  intersection = as.integer(st_intersects(st_transform(trusts_in_icb),st_transform(single_ICB))))

#filter only trusts that intersect
trusts_in_icb <- trusts_in_icb %>% filter(intersection == 1)

#plot trusts in ICB
ggplot() +
  geom_sf(data = single_ICB, aes(geometry = geometry)) +
  geom_point(data = trusts_in_icb, aes(trust_lon, trust_lat))

