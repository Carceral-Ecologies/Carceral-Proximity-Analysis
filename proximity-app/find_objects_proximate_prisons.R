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

#Read military bases data file
mil <- st_read("military_bases.csv", stringsAsFactors = FALSE) 
mil_sf <- st_as_sf(mil, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)

#Read superfund sites data file and convert to sf
sfs <- read.csv("sf.csv", stringsAsFactors = FALSE) 
sfs <- sfs %>% filter(!is.na(LONGITUDE83))
sfs_sf <- st_as_sf(sfs, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)

#Rename ID columns so they all match
ap_sf <- ap_sf %>% rename(ID = X.1)
#mil_sf <- mil_sf %>% rename(ID = ID) Already set to ID
mil_sf$ID <- as.numeric(mil_sf$ID)
sfs_sf <- sfs_sf %>% rename(ID = X)

#==========================================================================================================
#This function will find the objects within a specified distance to a prison and calculate their distances to the prison. 
#It will return a list of two vectors: the IDs of objects within proximity to prison and their distances in meters
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
  else {
    toxic_site_sf <- mil_sf
  }
  
  #Checks which objects from the selected site type df (stored in toxic_site_sf) are within the specified distance to the selected prison.  
  in_proximity <- st_is_within_distance(pb_sf[pb_sf$FID == prison,], toxic_site_sf, dist = prox, sparse = TRUE)

  #Sets the name of the vector of objects within proximity (stored in the in_proximity list) to objects
  names(in_proximity) <- c("objects") 
  
  #Convert row indexes to object ID column
  in_proximity$objects <- sapply(in_proximity[[1]], function(x){toxic_site_sf[as.numeric(rownames(toxic_site_sf)) == x,]$ID})
  print(in_proximity$objects)
  
  #Calculates the distance from each object within proximity to the specified prison and stores in vector
  distances <- sapply(in_proximity[[1]], function(x){st_distance(pb_sf[pb_sf$FID == prison,], toxic_site_sf[toxic_site_sf$ID == x,])})

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
#This will become a list of prisons, each containing two lists: 
#objects within proximity and the distances of those objects to the prison. 
#To create this list, for every FID in the prisons data frame, we are going to run the calculateInProximitySorted function, 
#specifying the FID to run it on, the data frame to check objects within proximity on (airports, superfunds, etc) 
#and the distance within which we should check objects in proximity. 
#Once we have returned a list of the objects within proximity and their distances, 
#we will append this returned list to the end of the empty list we initialized, 
#and name the returned list with the FID (so that we can reference the prison later).
#==========================================================================================================

ap_distances <- list() #Initialize airport distance list
mil_distances <- list() #Initialize military base distance list
sfs_distances <- list() #Initialize superfund site distance list

for (x in pb_sf$FID) {
  #For given FID, calculate the objects within 5000 meters, along with their distances to the prison. This will return a list of two vectors: objects and distances. Append this list to x_distances. 
  #ap_distances[[length(ap_distances)+1]] <- calculateInProximitySorted(x, "ap", 5000) 
  mil_distances[[length(mil_distances)+1]] <- calculateInProximitySorted(x, "mil", 5000) 
  #sfs_distances[[length(sfs_distances)+1]] <- calculateInProximitySorted(x, "sfs", 5000) 
  
  #Set the name of the new list in ap_distances to the FID so that we may reference the prison later. 
  #names(ap_distances)[length(ap_distances)] <- x 
  names(mil_distances)[length(mil_distances)] <- x 
  #names(sfs_distances)[length(sfs_distances)] <- x 
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

get_military <- function(prison, index) {
  #Get prison from named mil_distances list
  prison_in_mil_distances <- mil_distances[[as.character(prison)]] 
  
  if (length(prison_in_mil_distances$objects) < index)
    return(0)
  else #Return military ID at designated index from list
    return(prison_in_mil_distances$objects[index])
}

get_military_dist <- function(prison, index) {
  #Get prison from named mil_distances list
  prison_in_mil_distances <- mil_distances[[as.character(prison)]]
  
  if (length(prison_in_mil_distances$distances) < index)
    return(0)
  else #Return military ID at designated index from list
    return(prison_in_mil_distances$distances[index])
}

get_superfund <- function(prison, index) {
  #Get prison from named sfs_distances list
  prison_in_sfs_distances <- sfs_distances[[as.character(prison)]] 
  
  if (length(prison_in_sfs_distances$objects) < index)
    return(0)
  else #Return superfund ID at designated index from list
    return(prison_in_sfs_distances$objects[index])
}

get_superfund_dist <- function(prison, index) {
  #Get prison from named sfs_distances list
  prison_in_sfs_distances <- sfs_distances[[as.character(prison)]]
  
  if (length(prison_in_sfs_distances$distances) < index)
    return(0)
  else #Return superfund ID at designated index from list
    return(prison_in_sfs_distances$distances[index])
}

pb_sf_with_object_distances <- pb_sf %>% group_by(FID) %>%
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
    airport_5_dist = get_airport_dist(FID, 5),
    military_1 = get_military(FID, 1),
    military_1_dist = get_military_dist(FID, 1),
    military_2 = get_military(FID, 2),
    military_2_dist = get_military_dist(FID, 2),
    military_3 = get_military(FID, 3),
    military_3_dist = get_military_dist(FID, 3),
    military_4 = get_military(FID, 4),
    military_4_dist = get_military_dist(FID, 4),
    military_5 = get_military(FID, 5),
    military_5_dist = get_military_dist(FID, 5),
    military_6 = get_military(FID, 6),
    military_6_dist = get_military_dist(FID, 6),
    military_7 = get_military(FID, 7),
    military_7_dist = get_military_dist(FID, 7),
    military_8 = get_military(FID, 8),
    military_8_dist = get_military_dist(FID, 8),
    military_9 = get_military(FID, 9),
    military_9_dist = get_military_dist(FID, 9),
    military_10 = get_military(FID, 10),
    military_10_dist = get_military_dist(FID, 10),
    military_11 = get_military(FID, 11),
    military_11_dist = get_military_dist(FID, 11),
    military_12 = get_military(FID, 12),
    military_12_dist = get_military_dist(FID, 12),
    military_13 = get_military(FID, 13),
    military_13_dist = get_military_dist(FID, 13),
    military_14 = get_military(FID, 14),
    military_14_dist = get_military_dist(FID, 14),
    military_15 = get_military(FID, 15),
    military_15_dist = get_military_dist(FID, 15),
    military_16 = get_military(FID, 16),
    military_16_dist = get_military_dist(FID, 16),
    military_17 = get_military(FID, 17),
    military_17_dist = get_military_dist(FID, 17),
    military_18 = get_military(FID, 18),
    military_18_dist = get_military_dist(FID, 18),
    military_19 = get_military(FID, 19),
    military_19_dist = get_military_dist(FID, 19),
    military_20 = get_military(FID, 20),
    military_20_dist = get_military_dist(FID, 20),
    superfund_1 = get_superfund(FID, 1),
    superfund_1_dist = get_superfund_dist(FID, 1),
    superfund_2 = get_superfund(FID, 2),
    superfund_2_dist = get_superfund_dist(FID, 2),
    superfund_3 = get_superfund(FID, 3),
    superfund_3_dist = get_superfund_dist(FID, 3),
    superfund_4 = get_superfund(FID, 4),
    superfund_4_dist = get_superfund_dist(FID, 4),
    superfund_5 = get_superfund(FID, 5),
    superfund_5_dist = get_superfund_dist(FID, 5),
    superfund_6 = get_superfund(FID, 6),
    superfund_6_dist = get_superfund_dist(FID, 6),
    superfund_7 = get_superfund(FID, 7),
    superfund_7_dist = get_superfund_dist(FID, 7),
    superfund_8 = get_superfund(FID, 8),
    superfund_8_dist = get_superfund_dist(FID, 8),
    superfund_9 = get_superfund(FID, 9),
    superfund_9_dist = get_superfund_dist(FID, 9),
    superfund_10 = get_superfund(FID, 10),
    superfund_10_dist = get_superfund_dist(FID, 10),
    superfund_11 = get_superfund(FID, 11),
    superfund_11_dist = get_superfund_dist(FID, 11),
    superfund_12 = get_superfund(FID, 12),
    superfund_12_dist = get_superfund_dist(FID, 12),
    superfund_13 = get_superfund(FID, 13),
    superfund_13_dist = get_superfund_dist(FID, 13),
    superfund_14 = get_superfund(FID, 14),
    superfund_14_dist = get_superfund_dist(FID, 14),
    superfund_15 = get_superfund(FID, 15),
    superfund_15_dist = get_superfund_dist(FID, 15),
    superfund_16 = get_superfund(FID, 16),
    superfund_16_dist = get_superfund_dist(FID, 16),
    superfund_17 = get_superfund(FID, 17),
    superfund_17_dist = get_superfund_dist(FID, 17),
    superfund_18 = get_superfund(FID, 18),
    superfund_18_dist = get_superfund_dist(FID, 18),
    superfund_19 = get_superfund(FID, 19),
    superfund_19_dist = get_superfund_dist(FID, 19),
    superfund_20 = get_superfund(FID, 20),
    superfund_20_dist = get_superfund_dist(FID, 20),
    superfund_21 = get_superfund(FID, 21),
    superfund_21_dist = get_superfund_dist(FID, 21),
    superfund_22 = get_superfund(FID, 22),
    superfund_22_dist = get_superfund_dist(FID, 22),
    superfund_23 = get_superfund(FID, 23),
    superfund_23_dist = get_superfund_dist(FID, 23),
    superfund_24 = get_superfund(FID, 24),
    superfund_24_dist = get_superfund_dist(FID, 24),
    superfund_25 = get_superfund(FID, 25),
    superfund_25_dist = get_superfund_dist(FID, 25),
    superfund_26 = get_superfund(FID, 26),
    superfund_26_dist = get_superfund_dist(FID, 26),
    superfund_27 = get_superfund(FID, 27),
    superfund_27_dist = get_superfund_dist(FID, 27),
    superfund_28 = get_superfund(FID, 28),
    superfund_28_dist = get_superfund_dist(FID, 28),
    superfund_29 = get_superfund(FID, 29),
    superfund_29_dist = get_superfund_dist(FID, 29),
    superfund_30 = get_superfund(FID, 30),
    superfund_30_dist = get_superfund_dist(FID, 30),
    superfund_31 = get_superfund(FID, 31),
    superfund_31_dist = get_superfund_dist(FID, 31),
    superfund_32 = get_superfund(FID, 32),
    superfund_32_dist = get_superfund_dist(FID, 32),
    superfund_33 = get_superfund(FID, 33),
    superfund_33_dist = get_superfund_dist(FID, 33),
    superfund_34 = get_superfund(FID, 34),
    superfund_34_dist = get_superfund_dist(FID, 34),
    superfund_35 = get_superfund(FID, 35),
    superfund_35_dist = get_superfund_dist(FID, 35),
    superfund_36 = get_superfund(FID, 36),
    superfund_36_dist = get_superfund_dist(FID, 36),
    superfund_37 = get_superfund(FID, 37),
    superfund_37_dist = get_superfund_dist(FID, 37),
    superfund_38 = get_superfund(FID, 38),
    superfund_38_dist = get_superfund_dist(FID, 38),
    superfund_39 = get_superfund(FID, 39),
    superfund_39_dist = get_superfund_dist(FID, 39),
    superfund_40 = get_superfund(FID, 40),
    superfund_40_dist = get_superfund_dist(FID, 40),
    superfund_41 = get_superfund(FID, 41),
    superfund_41_dist = get_superfund_dist(FID, 41),
    superfund_42 = get_superfund(FID, 42),
    superfund_42_dist = get_superfund_dist(FID, 42),
    superfund_43 = get_superfund(FID, 43),
    superfund_43_dist = get_superfund_dist(FID, 43),
    superfund_44 = get_superfund(FID, 44),
    superfund_44_dist = get_superfund_dist(FID, 44),
    superfund_45 = get_superfund(FID, 45),
    superfund_45_dist = get_superfund_dist(FID, 45),
    superfund_46 = get_superfund(FID, 46),
    superfund_46_dist = get_superfund_dist(FID, 46),
    superfund_47 = get_superfund(FID, 47),
    superfund_47_dist = get_superfund_dist(FID, 47),
    superfund_48 = get_superfund(FID, 48),
    superfund_48_dist = get_superfund_dist(FID, 48),
    superfund_49 = get_superfund(FID, 49),
    superfund_49_dist = get_superfund_dist(FID, 49),
    superfund_50 = get_superfund(FID, 50),
    superfund_50_dist = get_superfund_dist(FID, 50),
    superfund_51 = get_superfund(FID, 51),
    superfund_51_dist = get_superfund_dist(FID, 51),
    superfund_52 = get_superfund(FID, 52),
    superfund_52_dist = get_superfund_dist(FID, 52),
    superfund_53 = get_superfund(FID, 53),
    superfund_53_dist = get_superfund_dist(FID, 53),
    superfund_54 = get_superfund(FID, 54),
    superfund_54_dist = get_superfund_dist(FID, 54),
    superfund_55 = get_superfund(FID, 55),
    superfund_55_dist = get_superfund_dist(FID, 55),
    superfund_56 = get_superfund(FID, 56),
    superfund_56_dist = get_superfund_dist(FID, 56),
    superfund_57 = get_superfund(FID, 57),
    superfund_57_dist = get_superfund_dist(FID, 57),
    superfund_58 = get_superfund(FID, 58),
    superfund_58_dist = get_superfund_dist(FID, 58),
    superfund_59 = get_superfund(FID, 59),
    superfund_59_dist = get_superfund_dist(FID, 59),
    superfund_60 = get_superfund(FID, 60),
    superfund_60_dist = get_superfund_dist(FID, 60)
  ) %>%
  ungroup()


st_write(pb_sf_with_object_distances,  "prisons_with_object_distances.csv")
