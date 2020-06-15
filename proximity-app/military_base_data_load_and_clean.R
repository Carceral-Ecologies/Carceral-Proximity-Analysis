library(sf)
library(tidyverse)

#Read two military site shapefiles
mil <- st_read("installations_ranges/MILITARY_INSTALLATIONS_RANGES_TRAINING_AREAS_PT.shp", stringsAsFactors = FALSE)
mil2 <- st_read("installations_ranges/MILITARY_INSTALLATIONS_RANGES_TRAINING_AREAS_BND.shp", stringsAsFactors = FALSE)

#Bind the two military site shapefiles
mil <- rbind(mil, mil2)

rm(mil2)

#Convert military sites to match (larger) FRS fac data set Coordinate Reference System
mil_sf <- st_transform(mil, crs = 4269)

rm(mil)

#Reduce military sites from polygons to points (centroids) to reduces distance calculation times
mil_sf <- st_transform(mil_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269

#state_codes.csv contains full state names, state abbreviations, and census codes for each state, allowing conversion between the variables in each data file
#Ben: added ' ' to fix Windows column name issue from this .csv, caused by 'Byte Order Mark'
#per https://stackoverflow.com/questions/24568056/rs-read-csv-prepending-1st-column-name-with-junk-text/24568505
codes <- read.csv("state_codes.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE) 
codes$STATE_NUM <- as.character(codes$STATE_NUM) %>% #convert state census codes to characters because they will eventually be text in file names
  str_pad(2, pad = "0") #add a leading zero to ensure all state numbers are two digits

#There are census tract shapefiles for each state stored in the project files. To reduce reading times, these are not all read in by default but on a state-by-state basis when a user selects a state. Below we add two columns with the path to the zip of each state's shapefile, along with the file itself.
codes <- codes %>%
  mutate(TRACT_ZIP = paste("tracts/tl_2019_", STATE_NUM, "_tract.zip", sep = "")) %>% #create new column in codes dataframe with location of each census tract shapfile zip directory by filling the state census codes into the path
  mutate(TRACT_FOLDER = paste("tracts/tl_2019_", STATE_NUM, "_tract/", sep = "")) %>% #create new column in codes dataframe with location of each census tract shapfile zip directory by filling the state census codes into the path
  mutate(TRACT_FILE = paste("tl_2019_", STATE_NUM, "_tract.shp", sep = "")) #create new column in codes dataframe with location of each census tract shapefile name

mil_sf <- mil_sf %>% left_join(codes, by = "STATE_TERR") #Add the state abbreviation to military site data frame, which only contains the fill state name.
mil_sf <- mil_sf %>% rowid_to_column("ID")

st_write(mil_sf,  "military_bases.csv", layer_options = "GEOMETRY=AS_XY")