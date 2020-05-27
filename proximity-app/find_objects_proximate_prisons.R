library(tidyverse)
library(sf)
library(rgeos)

pb <- st_read("Prison_Boundaries/Prison_Boundaries.shp", stringsAsFactors = FALSE)

#Convert prisons to match (larger) FRS fac data set Coordinate Reference System
pb_sf <- st_transform(pb, crs = 4269)

#Reduce prisons from polygons to points (centroids) to reduces distance calculation times
pb_sf <- st_transform(pb_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269

pb_crs <- st_crs(pb_sf) #get the CRS for prison centroids

#Read airports data file and convert to sf
ap <- read.csv("airports.csv", stringsAsFactors = FALSE) 
ap_sf <- st_as_sf(ap, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)

#==========================================================================================================
#This function will find the objects within a specified distance to a prison and calculate their distances to the prison. 
#It will return a list of two vectors: the IDs of objects within proximity to prison and their distances in meters
#==========================================================================================================
calculateInProximitySorted <- function(prison, site, prox) {
  print(paste("Starting FID:", as.character(prison)))
  #Check which type of site the calculation will be run for in order to select the df we will be calculating based on. 
  #if (site == "sfs")
  #toxic_site_sf <- sfs_sf
  #else if (site == "ap")
  toxic_site_sf <- ap_sf
  #else 
  #toxic_site_sf <- mil_sf
  
  #Checks which objects from the selected site type df (stored in toxic_site_sf) are within the specified distance to the selected prison.  
  in_proximity <- st_is_within_distance(pb_sf[pb_sf$FID == prison,], toxic_site_sf, dist = prox, sparse = TRUE)
  
  #Sets the name of the vector of objects within proximity (stored in the in_proximity list) to objects
  names(in_proximity) <- c("objects") 
  
  #Calculates the distance from each object within proximity to the specified prison and stores in vector
  distances <- sapply(in_proximity[[1]], function(x){st_distance(pb_sf[pb_sf$FID == prison,], ap_sf[ap_sf$X.1 == x,])})
  
  #Appends distances to the in_proximity list
  in_proximity$distances <- distances 
  
  #Get the indexes of distances when ordered
  index <- order(in_proximity$distances)
  
  #Sort both objects and distances vector based on ordered distance index
  in_proximity$objects <- in_proximity$objects[index]
  in_proximity$distances <- in_proximity$distances[index]
  
  #Returns a list with both a vector of the objects within proximity (objects) and a vector of their distances to the prison (distances)
  return(in_proximity) 
}

#==========================================================================================================
#We start by creating an empty list below. 
#This will become a list prisons, each containing two lists: 
#objects within proximity and the distances of those objects to the prison. 
#To create this list, for every FID in the prisons data frame, we are going to run the calculateInProximity function, 
#specifying the FID to run it on, the data frame to check objects within proximity on (airports, superfunds, etc) 
#and the distance within which we should check objects in proximity. 
#Once we have returned a list of the objects within proximity and their distances, 
#we will append this returned list to the end of the empty list we initialized, 
#and name the returned list with the FID (so that we can reference the prison later).
#==========================================================================================================

ap_distances <- list() #Initialize airport distance list

for (x in pb_sf$FID) {
  #For given FID, calculate the objects within 10000 meters, along with their distances to the prison. This will return a list of two vectors: objects and distances. Append this list to ap_distances. 
  ap_distances[[length(ap_distances)+1]] <- calculateInProximitySorted(x, "ap", 5000) 
  
  #Set the name of the new list in ap_distances to the FID so that we may reference the prison later. 
  names(ap_distances)[length(ap_distances)] <- x 
}

#==========================================================================================================
#These functions will return the airports and distances from ap_distances for the given prison at the given index.
#==========================================================================================================

get_airport <- function(prison, index) {
  #Get prison from named ap_distances list
  prison_in_ap_distances <- ap_distances[[as.character(prison)]] 
  
  if (length(prison_in_ap_distances$objects) < index)
    return(0)
  else #Return airport ID at designated index from list
    return(prison_in_ap_distances$objects[index])
}

get_airport_dist <- function(prison, index) {
  #Get prison from named ap_distances list
  prison_in_ap_distances <- ap_distances[[as.character(prison)]]
  
  if (length(prison_in_ap_distances$distances) < index)
    return(0)
  else #Return airport ID at designated index from list
    return(prison_in_ap_distances$distances[index])
}

pb_sf_with_airports <- pb_sf %>% group_by(FID) %>%
  mutate(
    airport_1 = get_airport(FID, 1), 
    airport_1_dist = get_airport_dist(FID, 1),
    airport_2 = get_airport(FID, 2),
    airport_2_dist = get_airport_dist(FID, 2),
    airport_3 = get_airport(FID, 3),
    airport_3_dist = get_airport_dist(FID, 3),
    airport_4 = get_airport(FID, 4), 
    airport_4_dist = get_airport_dist(FID, 4),
    airport_5 = get_airport(FID, 5), 
    airport_5_dist = get_airport_dist(FID, 5)
  ) %>%
  ungroup()


st_write(pb_sf_with_airports,  "pb_sf_with_airports.csv")
