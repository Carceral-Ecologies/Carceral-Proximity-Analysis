library(tidyverse)
library(sf)
library(rgeos)

pb <- st_read("data-clean/Prison_Boundaries/Prison_Boundaries.shp", stringsAsFactors = FALSE)

#Convert prisons to match (larger) FRS fac data set Coordinate Reference System
pb_sf <- st_transform(pb, crs = 4269)

#Reduce prisons from polygons to points (centroids) to reduces distance calculation times
pb_sf <- st_transform(pb_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269

pb_crs <- st_crs(pb_sf) #get the CRS for prison centroids

#Read airports data file and convert to sf
ap <- read.csv("data-clean/airports.csv", stringsAsFactors = FALSE) 
ap_sf <- st_as_sf(ap, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)

#Read military bases data file
mil <- st_read("data-clean/military_bases.csv", stringsAsFactors = FALSE) 
mil_sf <- st_as_sf(mil, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)

#Read superfund sites data file and convert to sf
sfs <- read.csv("data-clean/sf.csv", stringsAsFactors = FALSE) 
sfs <- sfs %>% filter(!is.na(LONGITUDE83))
sfs_sf <- st_as_sf(sfs, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)

#Read TRI sites data file and convert to sf
tri <- read.csv("data-clean/tri.csv", stringsAsFactors = FALSE) 
tri <- tri %>% filter(!is.na(X12..LATITUDE))
tri_sf <- st_as_sf(tri, coords = c("X13..LONGITUDE", "X12..LATITUDE"), crs = pb_crs, na.fail = FALSE)

#Rename ID columns so they all match
ap_sf <- ap_sf %>% rename(ID = X.1)
#mil_sf <- mil_sf %>% rename(ID = ID) Already set to ID
mil_sf$ID <- as.numeric(mil_sf$ID)
sfs_sf <- sfs_sf %>% rename(ID = X)
tri_sf <- tri_sf %>% rename(ID = X)

#==========================================================================================================
#This function will find the FACILITIES within a specified distance to a prison and calculate their DISTANCES to the prison. 
#It will return a list of two vectors: the IDs of FACILITIES within proximity to prison and their DISTANCES in meters
#==========================================================================================================
calculateInProximitySorted <- function(prison, site, prox) {
  print(paste("Starting FID:", as.character(prison), ",", site))
  #Check which type of site the calculation will be run for in order to select the df we will be calculating based on. 
  if (site == "sfs") {
    toxic_site_sf <- sfs_sf
  }
  else if (site == "ap") {
    toxic_site_sf <- ap_sf
  }
  else if (site == "mil") {
    toxic_site_sf <- mil_sf
  }
  else {
    toxic_site_sf <- tri_sf
  }
  
  #Checks which FACILITIES from the selected site type df (stored in toxic_site_sf) are within the specified distance to the selected prison.  
  in_proximity <- st_is_within_distance(pb_sf[pb_sf$FID == prison,], toxic_site_sf, dist = prox, sparse = TRUE)

  #Sets the name of the vector of FACILITIES within proximity (stored in the in_proximity list) to FACILITIES
  names(in_proximity) <- c("FACILITIES") 
  
  #Convert row indexes to object ID column
  in_proximity$FACILITIES <- sapply(in_proximity[[1]], function(x){toxic_site_sf[as.numeric(rownames(toxic_site_sf)) == x,]$ID})
  print(in_proximity$FACILITIES)
  
  #Calculates the distance from each object within proximity to the specified prison and stores in vector
  DISTANCES <- sapply(in_proximity[[1]], function(x){st_distance(pb_sf[pb_sf$FID == prison,], toxic_site_sf[toxic_site_sf$ID == x,])})

  #Appends DISTANCES to the in_proximity list
  in_proximity$DISTANCES <- DISTANCES 

  #Get the indexes of DISTANCES when ordered
  index <- order(in_proximity$DISTANCES)

  #Sort both FACILITIES and DISTANCES vector based on ordered distance index
  in_proximity$FACILITIES <- in_proximity$FACILITIES[index]
  in_proximity$DISTANCES <- in_proximity$DISTANCES[index]

  #Returns a list with both a vector of the FACILITIES within proximity (FACILITIES) and a vector of their DISTANCES to the prison (DISTANCES)
  return(in_proximity) 
}

#==========================================================================================================
#We start by creating an empty list below. 
#This will become a list of prisons, each containing two lists: 
#FACILITIES within proximity and the DISTANCES of those FACILITIES to the prison. 
#To create this list, for every FID in the prisons data frame, we are going to run the calculateInProximitySorted function, 
#specifying the FID to run it on, the data frame to check FACILITIES within proximity on (airports, superfunds, etc) 
#and the distance within which we should check FACILITIES in proximity. 
#Once we have returned a list of the FACILITIES within proximity and their DISTANCES, 
#we will append this returned list to the end of the empty list we initialized, 
#and name the returned list with the FID (so that we can reference the prison later).
#==========================================================================================================

ap_distances <- list() #Initialize airport distance list
mil_distances <- list() #Initialize military base distance list
sfs_distances <- list() #Initialize superfund site distance list
tri_distances <- list() #Initialize TRI site distance list

for (x in pb_sf$FID) {
  #For given FID, calculate the FACILITIES within 5000 meters, along with their DISTANCES to the prison. This will return a list of two vectors: FACILITIES and DISTANCES. Append this list to x_distances. 
  ap_distances[[length(ap_distances)+1]] <- calculateInProximitySorted(x, "ap", 5000) 
  mil_distances[[length(mil_distances)+1]] <- calculateInProximitySorted(x, "mil", 5000) 
  sfs_distances[[length(sfs_distances)+1]] <- calculateInProximitySorted(x, "sfs", 5000) 
  tri_distances[[length(tri_distances)+1]] <- calculateInProximitySorted(x, "tri", 5000) 
  
  #Set the name of the new list in ap_distances to the FID so that we may reference the prison later. 
  names(ap_distances)[length(ap_distances)] <- x 
  names(mil_distances)[length(mil_distances)] <- x 
  names(sfs_distances)[length(sfs_distances)] <- x 
  names(tri_distances)[length(tri_distances)] <- x  
}

reformat_distance_list_into_df <- function(site, facility_type) {
  
  if (site == "sfs") {
    x_distances <- sfs_distances
    print("success sfs")
  }
  else if (site == "ap") {
    x_distances <- ap_distances
    print("success ap")
  }
  else if (site == "mil") {
    x_distances <- mil_distances
    print("success mil")
  }
  else {
    x_distances <- tri_distances
    print("success tri")
  }
  
  #NOTE: I'm convinced that there is a cleaner way to reformat the data below. However, the below works. 
  
  #x_distances is a list of prisons, which themselves are a list of two named lists (FACILITIES and DISTANCES). 
  #This line will unlist the first nest, so we will now have a list of the values in FACILITIES and DISTANCES. 
  #The names of each nested list in the parent list will be [prisonFID].FACILITIES or [prisonFID].DISTANCES
  x_distances_unlist <- unlist(x_distances, recursive = FALSE) 
  
  #This line will bind the values in each list in x_distances_unlist as a new row in a dataframe. 
  #The names of each list in x_distances_unlist will become the rownames in the x_distances_df 
  #and a ".[index]" will be appended to the end of each rowname when there is more than one FACILITIES/DISTANCES for a given prison. 
  #Because we have sorted the facilities by their proximity in both FACILITIES/DISTANCES, 
  #this .[index] will indicate the order of the nearness to the facility. 
  #We will later call it the proximity index. 
  x_distances_df <- do.call(rbind, lapply(x_distances_unlist, data.frame)) 
  
  #This line will reformat our dataframe.
  x_distances_df_1 <- 
    x_distances_df %>%
    tibble::rownames_to_column("TYPE") %>% #set the rownames to a column called type
    separate(TYPE, into = c("FID", "VALUE_TYPE", "PROXIMITY_INDEX"), sep = "[.]", extra = "merge", fill = "right") %>% #separate the new column by the "." into the prison ID, value type (DISTANCES or FACILITIES), and the proximity index. e.g. 1001.DISTANCES.1 will be separated into three columns - the prison ID, the value type, and the proximity index (in this case the closest to the prison). If there is only one item in the list, there will be not proximity index appended.
    mutate(PROXIMITY_INDEX = as.numeric(PROXIMITY_INDEX)) %>% 
    mutate(PROXIMITY_INDEX = replace_na(PROXIMITY_INDEX, 1)) %>% #the closest facility will have a proximity index of NA. This is because ".[index]" was only added to the rowname above when there was more than one facility/value in the x_distances$FACILITIES/DISTANCES. Here we will set the NA to 1, indicating the closest prison
    spread(VALUE_TYPE, X..i..) %>% #make the value type (FACILITIES/DISTANCES) column names versus separate rows
    mutate(FACILITY_TYPE = facility_type) %>% #create a new column indicating the type of facility
    mutate(FID = as.numeric(FID))
  
  return(x_distances_df_1)
}

mil_distances_df <- reformat_distance_list_into_df("mil", "Military Base")
sfs_distances_df <- reformat_distance_list_into_df("sfs", "Superfund Site")
ap_distances_df <- reformat_distance_list_into_df("ap", "Airport")
tri_distances_df <- reformat_distance_list_into_df("tri", "TRI Facility")

facility_distances_df <- rbind(mil_distances_df, sfs_distances_df, ap_distances_df, tri_distances_df)

pb_sf_with_facility_distances <- pb_sf %>%
  left_join(facility_distances_df, by = "FID")

st_write(pb_sf_with_facility_distances,  "data-clean/prisons_with_facility_distances.csv")

