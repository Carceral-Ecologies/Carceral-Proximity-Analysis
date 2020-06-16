library(sf)
library(tidyverse)

#Read airport CSV
ap <- read.csv("data-original/Aircraft_Landing_Facilities.csv", stringsAsFactors = FALSE) 

#Filter to only airports
ap <- ap %>%
  filter(fac_type == "AIRPORT")

write.csv(ap,  "data-clean/airports.csv")
