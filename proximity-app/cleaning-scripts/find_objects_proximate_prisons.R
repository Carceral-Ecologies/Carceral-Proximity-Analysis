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
    separate(TYPE, into = c("FID", "VALUE_TYPE", "PROXIMITY_INDEX"), sep = "[.]", extra = "merge", fill = "right") %>% #separate the new column by the "." into the prison ID, value type (DISTANCES or FACILITIES), and the proximity index. e.g. 1001.DISTANCES.1 will be separated into three columns - the prison ID, the value type, and the proximity index (in this case the second closest to the prison)
    mutate(PROXIMITY_INDEX = replace_na(PROXIMITY_INDEX, 0)) %>% #the closest facility will have a proximity index of NA. This is because ".[index]" was only added to the rowname above when there was more than one facility/value in the x_distances$FACILITIES/DISTANCES. Here we will set the NA to 0, indicating the closest prison
    mutate(PROXIMITY_INDEX = as.numeric(PROXIMITY_INDEX) + 1) %>% #The proximity index starts as zero, but instead we would like it to start at one. 
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

#Everything below was a previous attempt to reformat the dataframe and has been replaced by the code above. 

#==========================================================================================================
#These functions will return the airports and DISTANCES from ap_distances for the given prison at the given index.
#==========================================================================================================

# get_airport <- function(prison, index) {
#   #Get prison from named ap_distances list
#   prison_in_ap_distances <- ap_distances[[as.character(prison)]] 
#   
#   if (length(prison_in_ap_distances$FACILITIES) < index)
#     return(0)
#   else #Return airport ID at designated index from list
#     return(prison_in_ap_distances$FACILITIES[index])
# }
# 
# get_airport_dist <- function(prison, index) {
#   #Get prison from named ap_distances list
#   prison_in_ap_distances <- ap_distances[[as.character(prison)]]
#   
#   if (length(prison_in_ap_distances$DISTANCES) < index)
#     return(0)
#   else #Return airport ID at designated index from list
#     return(prison_in_ap_distances$DISTANCES[index])
# }
# 
# get_military <- function(prison, index) {
#   #Get prison from named mil_distances list
#   prison_in_mil_distances <- mil_distances[[as.character(prison)]] 
#   
#   if (length(prison_in_mil_distances$FACILITIES) < index)
#     return(0)
#   else #Return military ID at designated index from list
#     return(prison_in_mil_distances$FACILITIES[index])
# }
# 
# get_military_dist <- function(prison, index) {
#   #Get prison from named mil_distances list
#   prison_in_mil_distances <- mil_distances[[as.character(prison)]]
#   
#   if (length(prison_in_mil_distances$DISTANCES) < index)
#     return(0)
#   else #Return military ID at designated index from list
#     return(prison_in_mil_distances$DISTANCES[index])
# }
# 
# get_superfund <- function(prison, index) {
#   #Get prison from named sfs_distances list
#   prison_in_sfs_distances <- sfs_distances[[as.character(prison)]] 
#   
#   if (length(prison_in_sfs_distances$FACILITIES) < index)
#     return(0)
#   else #Return superfund ID at designated index from list
#     return(prison_in_sfs_distances$FACILITIES[index])
# }
# 
# get_superfund_dist <- function(prison, index) {
#   #Get prison from named sfs_distances list
#   prison_in_sfs_distances <- sfs_distances[[as.character(prison)]]
#   
#   if (length(prison_in_sfs_distances$DISTANCES) < index)
#     return(0)
#   else #Return superfund ID at designated index from list
#     return(prison_in_sfs_distances$DISTANCES[index])
# }
# 
# get_tri <- function(prison, index) {
#   #Get prison from named tri_distances list
#   prison_in_tri_distances <- tri_distances[[as.character(prison)]] 
#   
#   if (length(prison_in_tri_distances$FACILITIES) < index)
#     return(0)
#   else #Return tri ID at designated index from list
#     return(prison_in_tri_distances$FACILITIES[index])
# }
# 
# get_tri_dist <- function(prison, index) {
#   #Get prison from named tri_distances list
#   prison_in_tri_distances <- tri_distances[[as.character(prison)]]
#   
#   if (length(prison_in_tri_distances$DISTANCES) < index)
#     return(0)
#   else #Return tri ID at designated index from list
#     return(prison_in_tri_distances$DISTANCES[index])
# }
# 
# pb_sf_with_object_distances <- pb_sf %>% group_by(FID) %>%
#   mutate(
#     airport_1 = get_airport(FID, 1),
#     airport_1_dist = get_airport_dist(FID, 1),
#     airport_2 = get_airport(FID, 2),
#     airport_2_dist = get_airport_dist(FID, 2),
#     airport_3 = get_airport(FID, 3),
#     airport_3_dist = get_airport_dist(FID, 3),
#     airport_4 = get_airport(FID, 4),
#     airport_4_dist = get_airport_dist(FID, 4),
#     airport_5 = get_airport(FID, 5),
#     airport_5_dist = get_airport_dist(FID, 5),
#     military_1 = get_military(FID, 1),
#     military_1_dist = get_military_dist(FID, 1),
#     military_2 = get_military(FID, 2),
#     military_2_dist = get_military_dist(FID, 2),
#     military_3 = get_military(FID, 3),
#     military_3_dist = get_military_dist(FID, 3),
#     military_4 = get_military(FID, 4),
#     military_4_dist = get_military_dist(FID, 4),
#     military_5 = get_military(FID, 5),
#     military_5_dist = get_military_dist(FID, 5),
#     military_6 = get_military(FID, 6),
#     military_6_dist = get_military_dist(FID, 6),
#     military_7 = get_military(FID, 7),
#     military_7_dist = get_military_dist(FID, 7),
#     military_8 = get_military(FID, 8),
#     military_8_dist = get_military_dist(FID, 8),
#     military_9 = get_military(FID, 9),
#     military_9_dist = get_military_dist(FID, 9),
#     military_10 = get_military(FID, 10),
#     military_10_dist = get_military_dist(FID, 10),
#     military_11 = get_military(FID, 11),
#     military_11_dist = get_military_dist(FID, 11),
#     military_12 = get_military(FID, 12),
#     military_12_dist = get_military_dist(FID, 12),
#     military_13 = get_military(FID, 13),
#     military_13_dist = get_military_dist(FID, 13),
#     military_14 = get_military(FID, 14),
#     military_14_dist = get_military_dist(FID, 14),
#     military_15 = get_military(FID, 15),
#     military_15_dist = get_military_dist(FID, 15),
#     military_16 = get_military(FID, 16),
#     military_16_dist = get_military_dist(FID, 16),
#     military_17 = get_military(FID, 17),
#     military_17_dist = get_military_dist(FID, 17),
#     military_18 = get_military(FID, 18),
#     military_18_dist = get_military_dist(FID, 18),
#     military_19 = get_military(FID, 19),
#     military_19_dist = get_military_dist(FID, 19),
#     military_20 = get_military(FID, 20),
#     military_20_dist = get_military_dist(FID, 20),
#     superfund_1 = get_superfund(FID, 1),
#     superfund_1_dist = get_superfund_dist(FID, 1),
#     superfund_2 = get_superfund(FID, 2),
#     superfund_2_dist = get_superfund_dist(FID, 2),
#     superfund_3 = get_superfund(FID, 3),
#     superfund_3_dist = get_superfund_dist(FID, 3),
#     superfund_4 = get_superfund(FID, 4),
#     superfund_4_dist = get_superfund_dist(FID, 4),
#     superfund_5 = get_superfund(FID, 5),
#     superfund_5_dist = get_superfund_dist(FID, 5),
#     superfund_6 = get_superfund(FID, 6),
#     superfund_6_dist = get_superfund_dist(FID, 6),
#     superfund_7 = get_superfund(FID, 7),
#     superfund_7_dist = get_superfund_dist(FID, 7),
#     superfund_8 = get_superfund(FID, 8),
#     superfund_8_dist = get_superfund_dist(FID, 8),
#     superfund_9 = get_superfund(FID, 9),
#     superfund_9_dist = get_superfund_dist(FID, 9),
#     superfund_10 = get_superfund(FID, 10),
#     superfund_10_dist = get_superfund_dist(FID, 10),
#     superfund_11 = get_superfund(FID, 11),
#     superfund_11_dist = get_superfund_dist(FID, 11),
#     superfund_12 = get_superfund(FID, 12),
#     superfund_12_dist = get_superfund_dist(FID, 12),
#     superfund_13 = get_superfund(FID, 13),
#     superfund_13_dist = get_superfund_dist(FID, 13),
#     superfund_14 = get_superfund(FID, 14),
#     superfund_14_dist = get_superfund_dist(FID, 14),
#     superfund_15 = get_superfund(FID, 15),
#     superfund_15_dist = get_superfund_dist(FID, 15),
#     superfund_16 = get_superfund(FID, 16),
#     superfund_16_dist = get_superfund_dist(FID, 16),
#     superfund_17 = get_superfund(FID, 17),
#     superfund_17_dist = get_superfund_dist(FID, 17),
#     superfund_18 = get_superfund(FID, 18),
#     superfund_18_dist = get_superfund_dist(FID, 18),
#     superfund_19 = get_superfund(FID, 19),
#     superfund_19_dist = get_superfund_dist(FID, 19),
#     superfund_20 = get_superfund(FID, 20),
#     superfund_20_dist = get_superfund_dist(FID, 20),
#     superfund_21 = get_superfund(FID, 21),
#     superfund_21_dist = get_superfund_dist(FID, 21),
#     superfund_22 = get_superfund(FID, 22),
#     superfund_22_dist = get_superfund_dist(FID, 22),
#     superfund_23 = get_superfund(FID, 23),
#     superfund_23_dist = get_superfund_dist(FID, 23),
#     superfund_24 = get_superfund(FID, 24),
#     superfund_24_dist = get_superfund_dist(FID, 24),
#     superfund_25 = get_superfund(FID, 25),
#     superfund_25_dist = get_superfund_dist(FID, 25),
#     superfund_26 = get_superfund(FID, 26),
#     superfund_26_dist = get_superfund_dist(FID, 26),
#     superfund_27 = get_superfund(FID, 27),
#     superfund_27_dist = get_superfund_dist(FID, 27),
#     superfund_28 = get_superfund(FID, 28),
#     superfund_28_dist = get_superfund_dist(FID, 28),
#     superfund_29 = get_superfund(FID, 29),
#     superfund_29_dist = get_superfund_dist(FID, 29),
#     superfund_30 = get_superfund(FID, 30),
#     superfund_30_dist = get_superfund_dist(FID, 30),
#     superfund_31 = get_superfund(FID, 31),
#     superfund_31_dist = get_superfund_dist(FID, 31),
#     superfund_32 = get_superfund(FID, 32),
#     superfund_32_dist = get_superfund_dist(FID, 32),
#     superfund_33 = get_superfund(FID, 33),
#     superfund_33_dist = get_superfund_dist(FID, 33),
#     superfund_34 = get_superfund(FID, 34),
#     superfund_34_dist = get_superfund_dist(FID, 34),
#     superfund_35 = get_superfund(FID, 35),
#     superfund_35_dist = get_superfund_dist(FID, 35),
#     superfund_36 = get_superfund(FID, 36),
#     superfund_36_dist = get_superfund_dist(FID, 36),
#     superfund_37 = get_superfund(FID, 37),
#     superfund_37_dist = get_superfund_dist(FID, 37),
#     superfund_38 = get_superfund(FID, 38),
#     superfund_38_dist = get_superfund_dist(FID, 38),
#     superfund_39 = get_superfund(FID, 39),
#     superfund_39_dist = get_superfund_dist(FID, 39),
#     superfund_40 = get_superfund(FID, 40),
#     superfund_40_dist = get_superfund_dist(FID, 40),
#     superfund_41 = get_superfund(FID, 41),
#     superfund_41_dist = get_superfund_dist(FID, 41),
#     superfund_42 = get_superfund(FID, 42),
#     superfund_42_dist = get_superfund_dist(FID, 42),
#     superfund_43 = get_superfund(FID, 43),
#     superfund_43_dist = get_superfund_dist(FID, 43),
#     superfund_44 = get_superfund(FID, 44),
#     superfund_44_dist = get_superfund_dist(FID, 44),
#     superfund_45 = get_superfund(FID, 45),
#     superfund_45_dist = get_superfund_dist(FID, 45),
#     superfund_46 = get_superfund(FID, 46),
#     superfund_46_dist = get_superfund_dist(FID, 46),
#     superfund_47 = get_superfund(FID, 47),
#     superfund_47_dist = get_superfund_dist(FID, 47),
#     superfund_48 = get_superfund(FID, 48),
#     superfund_48_dist = get_superfund_dist(FID, 48),
#     superfund_49 = get_superfund(FID, 49),
#     superfund_49_dist = get_superfund_dist(FID, 49),
#     superfund_50 = get_superfund(FID, 50),
#     superfund_50_dist = get_superfund_dist(FID, 50),
#     superfund_51 = get_superfund(FID, 51),
#     superfund_51_dist = get_superfund_dist(FID, 51),
#     superfund_52 = get_superfund(FID, 52),
#     superfund_52_dist = get_superfund_dist(FID, 52),
#     superfund_53 = get_superfund(FID, 53),
#     superfund_53_dist = get_superfund_dist(FID, 53),
#     superfund_54 = get_superfund(FID, 54),
#     superfund_54_dist = get_superfund_dist(FID, 54),
#     superfund_55 = get_superfund(FID, 55),
#     superfund_55_dist = get_superfund_dist(FID, 55),
#     superfund_56 = get_superfund(FID, 56),
#     superfund_56_dist = get_superfund_dist(FID, 56),
#     superfund_57 = get_superfund(FID, 57),
#     superfund_57_dist = get_superfund_dist(FID, 57),
#     superfund_58 = get_superfund(FID, 58),
#     superfund_58_dist = get_superfund_dist(FID, 58),
#     superfund_59 = get_superfund(FID, 59),
#     superfund_59_dist = get_superfund_dist(FID, 59),
#     superfund_60 = get_superfund(FID, 60),
#     superfund_60_dist = get_superfund_dist(FID, 60),
#     superfund_61 = get_superfund(FID, 61),
#     superfund_61_dist = get_superfund_dist(FID, 61),
#     superfund_62 = get_superfund(FID, 62),
#     superfund_62_dist = get_superfund_dist(FID, 62),
#     superfund_63 = get_superfund(FID, 63),
#     superfund_63_dist = get_superfund_dist(FID, 63),
#     superfund_64 = get_superfund(FID, 64),
#     superfund_64_dist = get_superfund_dist(FID, 64),
#     superfund_65 = get_superfund(FID, 65),
#     superfund_65_dist = get_superfund_dist(FID, 65),
#     superfund_66 = get_superfund(FID, 66),
#     superfund_66_dist = get_superfund_dist(FID, 66),
#     superfund_67 = get_superfund(FID, 67),
#     superfund_67_dist = get_superfund_dist(FID, 67),
#     superfund_68 = get_superfund(FID, 68),
#     superfund_68_dist = get_superfund_dist(FID, 68),
#     superfund_69 = get_superfund(FID, 69),
#     superfund_69_dist = get_superfund_dist(FID, 69),
#     superfund_70 = get_superfund(FID, 70),
#     superfund_70_dist = get_superfund_dist(FID, 70),
#     tri_1 = get_tri(FID, 1),
#     tri_1_dist = get_tri_dist(FID, 1),
#     tri_2 = get_tri(FID, 2),
#     tri_2_dist = get_tri_dist(FID, 2),
#     tri_3 = get_tri(FID, 3),
#     tri_3_dist = get_tri_dist(FID, 3),
#     tri_4 = get_tri(FID, 4),
#     tri_4_dist = get_tri_dist(FID, 4),
#     tri_5 = get_tri(FID, 5),
#     tri_5_dist = get_tri_dist(FID, 5),
#     tri_6 = get_tri(FID, 6),
#     tri_6_dist = get_tri_dist(FID, 6),
#     tri_7 = get_tri(FID, 7),
#     tri_7_dist = get_tri_dist(FID, 7),
#     tri_8 = get_tri(FID, 8),
#     tri_8_dist = get_tri_dist(FID, 8),
#     tri_9 = get_tri(FID, 9),
#     tri_9_dist = get_tri_dist(FID, 9),
#     tri_10 = get_tri(FID, 10),
#     tri_10_dist = get_tri_dist(FID, 10),
#     tri_11 = get_tri(FID, 11),
#     tri_11_dist = get_tri_dist(FID, 11),
#     tri_12 = get_tri(FID, 12),
#     tri_12_dist = get_tri_dist(FID, 12),
#     tri_13 = get_tri(FID, 13),
#     tri_13_dist = get_tri_dist(FID, 13),
#     tri_14 = get_tri(FID, 14),
#     tri_14_dist = get_tri_dist(FID, 14),
#     tri_15 = get_tri(FID, 15),
#     tri_15_dist = get_tri_dist(FID, 15),
#     tri_16 = get_tri(FID, 16),
#     tri_16_dist = get_tri_dist(FID, 16),
#     tri_17 = get_tri(FID, 17),
#     tri_17_dist = get_tri_dist(FID, 17),
#     tri_18 = get_tri(FID, 18),
#     tri_18_dist = get_tri_dist(FID, 18),
#     tri_19 = get_tri(FID, 19),
#     tri_19_dist = get_tri_dist(FID, 19),
#     tri_20 = get_tri(FID, 20),
#     tri_20_dist = get_tri_dist(FID, 20),
#     tri_21 = get_tri(FID, 21),
#     tri_21_dist = get_tri_dist(FID, 21),
#     tri_22 = get_tri(FID, 22),
#     tri_22_dist = get_tri_dist(FID, 22),
#     tri_23 = get_tri(FID, 23),
#     tri_23_dist = get_tri_dist(FID, 23),
#     tri_24 = get_tri(FID, 24),
#     tri_24_dist = get_tri_dist(FID, 24),
#     tri_25 = get_tri(FID, 25),
#     tri_25_dist = get_tri_dist(FID, 25),
#     tri_26 = get_tri(FID, 26),
#     tri_26_dist = get_tri_dist(FID, 26),
#     tri_27 = get_tri(FID, 27),
#     tri_27_dist = get_tri_dist(FID, 27),
#     tri_28 = get_tri(FID, 28),
#     tri_28_dist = get_tri_dist(FID, 28),
#     tri_29 = get_tri(FID, 29),
#     tri_29_dist = get_tri_dist(FID, 29),
#     tri_30 = get_tri(FID, 30),
#     tri_30_dist = get_tri_dist(FID, 30),
#     tri_31 = get_tri(FID, 31),
#     tri_31_dist = get_tri_dist(FID, 31),
#     tri_32 = get_tri(FID, 32),
#     tri_32_dist = get_tri_dist(FID, 32),
#     tri_33 = get_tri(FID, 33),
#     tri_33_dist = get_tri_dist(FID, 33),
#     tri_34 = get_tri(FID, 34),
#     tri_34_dist = get_tri_dist(FID, 34),
#     tri_35 = get_tri(FID, 35),
#     tri_35_dist = get_tri_dist(FID, 35),
#     tri_36 = get_tri(FID, 36),
#     tri_36_dist = get_tri_dist(FID, 36),
#     tri_37 = get_tri(FID, 37),
#     tri_37_dist = get_tri_dist(FID, 37),
#     tri_38 = get_tri(FID, 38),
#     tri_38_dist = get_tri_dist(FID, 38),
#     tri_39 = get_tri(FID, 39),
#     tri_39_dist = get_tri_dist(FID, 39),
#     tri_40 = get_tri(FID, 40),
#     tri_40_dist = get_tri_dist(FID, 40),
#     tri_41 = get_tri(FID, 41),
#     tri_41_dist = get_tri_dist(FID, 41),
#     tri_42 = get_tri(FID, 42),
#     tri_42_dist = get_tri_dist(FID, 42),
#     tri_43 = get_tri(FID, 43),
#     tri_43_dist = get_tri_dist(FID, 43),
#     tri_44 = get_tri(FID, 44),
#     tri_44_dist = get_tri_dist(FID, 44),
#     tri_45 = get_tri(FID, 45),
#     tri_45_dist = get_tri_dist(FID, 45),
#     tri_46 = get_tri(FID, 46),
#     tri_46_dist = get_tri_dist(FID, 46),
#     tri_47 = get_tri(FID, 47),
#     tri_47_dist = get_tri_dist(FID, 47),
#     tri_48 = get_tri(FID, 48),
#     tri_48_dist = get_tri_dist(FID, 48),
#     tri_49 = get_tri(FID, 49),
#     tri_49_dist = get_tri_dist(FID, 49),
#     tri_50 = get_tri(FID, 50),
#     tri_50_dist = get_tri_dist(FID, 50),
#     tri_51 = get_tri(FID, 51),
#     tri_51_dist = get_tri_dist(FID, 51),
#     tri_52 = get_tri(FID, 52),
#     tri_52_dist = get_tri_dist(FID, 52),
#     tri_53 = get_tri(FID, 53),
#     tri_53_dist = get_tri_dist(FID, 53),
#     tri_54 = get_tri(FID, 54),
#     tri_54_dist = get_tri_dist(FID, 54),
#     tri_55 = get_tri(FID, 55),
#     tri_55_dist = get_tri_dist(FID, 55),
#     tri_56 = get_tri(FID, 56),
#     tri_56_dist = get_tri_dist(FID, 56),
#     tri_57 = get_tri(FID, 57),
#     tri_57_dist = get_tri_dist(FID, 57),
#     tri_58 = get_tri(FID, 58),
#     tri_58_dist = get_tri_dist(FID, 58),
#     tri_59 = get_tri(FID, 59),
#     tri_59_dist = get_tri_dist(FID, 59),
#     tri_60 = get_tri(FID, 60),
#     tri_60_dist = get_tri_dist(FID, 60)
#   ) %>%
#   ungroup()
# 
# 
# 
# 
# 
# 
#   
# 
# 
# 
# 
