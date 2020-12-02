library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sf)
library(rgeos)

#==========================================================================================================
#Data files for points locations
#==========================================================================================================

#Read carceral facility Shapefile
pb <- st_read("data-clean/Prison_Boundaries/Prison_Boundaries.shp", stringsAsFactors = FALSE)

#Thanks Ben!
#Convert carceral facilities to match (larger) FRS fac data set Coordinate Reference System
pb_sf <- st_transform(pb, crs = 4269)

#Reduce carceral facilities from polygons to points (centroids) to reduces distance calculation times
pb_sf <- st_transform(pb_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269

#Convert the unique ID for carceral facilities to be a string
pb_sf$FID <- as.character(pb_sf$FID)
pb_crs <- st_crs(pb_sf) #get the CRS for carceral facility centroids
pb_sf <- sf::st_transform(pb_sf, "+proj=longlat + datum=WGS84")

#Read military bases data file
mil <- read.csv("data-clean/military_bases.csv", stringsAsFactors = FALSE) 
mil_sf <- st_as_sf(mil, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)
mil_sf <- sf::st_transform(mil_sf, "+proj=longlat + datum=WGS84")

#Read brownfield data file and convert to sf
bf <- read.csv("data-clean/brownfields.csv", stringsAsFactors = FALSE)
bf <- bf %>% filter(!is.na(LATITUDE83))
bf_sf <- st_as_sf(bf, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)
bf_sf <- sf::st_transform(bf_sf, "+proj=longlat + datum=WGS84")

#Read airport data file and convert to sf
ap <- read.csv("data-clean/airports.csv", stringsAsFactors = FALSE) 
ap_sf <- st_as_sf(ap, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)
ap_sf <- sf::st_transform(ap_sf, "+proj=longlat + datum=WGS84")

#Read superfund sites data file and convert to sf
sfs <- read.csv("data-clean/sf.csv", stringsAsFactors = FALSE) 
sfs <- sfs %>% filter(!is.na(LATITUDE83))
sfs_sf <- st_as_sf(sfs, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)
sfs_sf <- sf::st_transform(sfs_sf, "+proj=longlat + datum=WGS84")

#Read TRI sites data file and convert to sf
tri <- read.csv("data-clean/tri.csv", stringsAsFactors = FALSE) 
tri <- tri %>% filter(!is.na(X12..LATITUDE))
tri_sf <- st_as_sf(tri, coords = c("X13..LONGITUDE", "X12..LATITUDE"), crs = pb_crs, na.fail = FALSE)
tri_sf <- sf::st_transform(tri_sf, "+proj=longlat + datum=WGS84")

#==========================================================================================================
#Supplemental data files
#==========================================================================================================
#state_codes.csv contains full state names, state abbreviations, and census codes for each state, allowing conversion between the variables in each data file
#Ben: added ' ' to fix Windows column name issue from this .csv, caused by 'Byte Order Mark'
#per https://stackoverflow.com/questions/24568056/rs-read-csv-prepending-1st-column-name-with-junk-text/24568505
codes <- read.csv("data-clean/state_codes.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE) 
codes$STATE_NUM <- as.character(codes$STATE_NUM) %>% #convert state census codes to characters because they will eventually be text in file names
  str_pad(2, pad = "0") #add a leading zero to ensure all state numbers are two digits

#There are census tract shapefiles for each state stored in the project files. To reduce reading times, these are not all read in by default but on a state-by-state basis when a user selects a state. Below we add two columns with the path to the zip of each state's shapefile, along with the file itself.
codes <- codes %>%
  mutate(TRACT_ZIP = paste("data-clean/tracts/tl_2019_", STATE_NUM, "_tract.zip", sep = "")) %>% #create new column in codes dataframe with location of each census tract shapfile zip directory by filling the state census codes into the path
  mutate(TRACT_FOLDER = paste("data-clean/tracts/tl_2019_", STATE_NUM, "_tract/", sep = "")) %>% #create new column in codes dataframe with location of each census tract shapfile zip directory by filling the state census codes into the path
  mutate(TRACT_FILE = paste("tl_2019_", STATE_NUM, "_tract.shp", sep = "")) #create new column in codes dataframe with location of each census tract shapefile name

#==========================================================================================================
#Data files for calculations
#==========================================================================================================
#census_tract_data <- read.csv("data-clean/census_tract_data.csv", stringsAsFactors = FALSE)
#pb_with_census_tracts <- read.csv("data-clean/prisons_with_census_tracts.csv", stringsAsFactors = FALSE)
pb_with_facility_distances <- read.csv("data-clean/prisons_with_facility_distances.csv", stringsAsFactors = FALSE)

#Until we verify census tract join, importing brownfield with census tracts separately. Eventually will replace code above to only import this file. 
#cls <- c(GEOID="character", STATEFP="character", COUNTYFP="character", TRACTCE="character")
#bf_with_census_tracts <- read.csv("data-clean/brownfields_with_census_tracts.csv", colClasses=cls, stringsAsFactors = FALSE)
