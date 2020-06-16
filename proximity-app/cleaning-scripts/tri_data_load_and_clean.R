library(sf)
library(tidyverse)

#Read TRI CSV
tri <- read.csv("data-original/TRI_2018_US.csv", stringsAsFactors = FALSE) 
options(scipen = 999) #Don't convert to scientific notation

#Calculate total releases across all chemicals for each facility
tri <- tri %>%
  mutate(X58..ON.SITE.RELEASE.TOTAL = ifelse(X43..UNIT.OF.MEASURE == "Grams", X58..ON.SITE.RELEASE.TOTAL / 454, X58..ON.SITE.RELEASE.TOTAL)) %>% #Convert units reported in Grams to Pounds
  group_by_at(vars(X1..YEAR:X13..LONGITUDE)) %>%
  summarize(ONSITE_RELEASES = sum(X58..ON.SITE.RELEASE.TOTAL, na.rm = TRUE)) %>%
  ungroup()

write.csv(tri,  "data-clean/tri.csv")

