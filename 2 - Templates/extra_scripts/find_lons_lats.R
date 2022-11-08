library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

#get lons and lats
#These two functions are the same, but one gets the lat item from the api response, and one gets the lon

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

#example with trusts
#from the ods api, retrieve some trust postcodes to test.
example_trusts <- read_json("https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?PrimaryRoleId=RO197")
example_trusts <-  as_tibble(example_trusts) %>% unnest_wider(Organisations)
  
#for each postcode find the lat and lon
#this will produce NAs for some of the inactive trusts where they no longer exist. this should be caught by the try catch.
lat <- map_chr(.x = example_trusts$PostCode, .f = get_lat)
lon <- map_chr(.x = example_trusts$PostCode, .f = get_lon)

#add the lats and lons to the trust df
example_trusts$lat = as.numeric(lat)
example_trusts$lon = as.numeric(lon)





