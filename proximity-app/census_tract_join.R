library(tidyverse)
library(sf)

pb <- st_read("Prison_Boundaries/Prison_Boundaries.shp", stringsAsFactors = FALSE)

#Convert prisons to match (larger) FRS fac data set Coordinate Reference System
pb_sf <- st_transform(pb, crs = 4269)

#Reduce prisons from polygons to points (centroids) to reduces distance calculation times
pb_sf <- st_transform(pb_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269

pb_crs <- st_crs(pb_sf) #get the CRS for prison centroids

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

#Read brownfield data file and convert to sf
bf <- read.csv("brownfields.csv", stringsAsFactors = FALSE)
bf_sf <- st_as_sf(bf, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)

#Initiate list for each census tract state shape fil
state_list = list()

#Filter out codes that don't have associated census tract shapefiles
codes <- codes %>%
  filter(STATE_CODE != "MH" & STATE_CODE != "FM" & STATE_CODE != "PW")

for (x in codes$STATE_CODE) {
  
  codes_filtered <- codes %>%
    filter(STATE_CODE == x) #Filter codes df to selected state
  
  tracts <- st_read(paste(codes_filtered$TRACT_FOLDER, codes_filtered$TRACT_FILE, sep = ""), stringsAsFactors = FALSE) #read shapefile 
  
  tracts <- st_transform(tracts, crs = 4269)
  tracts <- tracts %>% rename(CENSUS_NAME = NAME) #NAME variable appears in both pb_sf and tracts; rename in tracts to differentiate
  
  tracts$x <- x  
  state_list[[x]] <- tracts # add to your list
}

#Bind census tract shapefiles for all states
census_tracts <- do.call(rbind, state_list) 

#Join if the point location of prison/brownfield is within the census tract polygon
bf_in_tract <- st_join(bf_sf, census_tracts, join = st_within) 
pb_in_tract <- st_join(pb_sf, census_tracts, join = st_within)

#write files
write.csv(bf_in_tract,  "brownfields_with_census_tracts.csv")
write.csv(pb_in_tract,  "prisons_with_census_tracts.csv")


