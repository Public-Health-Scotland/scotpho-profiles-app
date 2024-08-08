# Code to add Police Divisions to the profiles tool

# Required because some mental health indicators (those from the Scottish Crime and Justice Survey) are provided at PD level.
# Police division shapefile: https://spatialdata.gov.scot/geonetwork/srv/api/records/4364af71-167a-4236-b5a0-bd4109913231
# This code creates a lookup file for PDs that is then appended to the lookup file for the other geographies.
# It also creates a shapefile for use in the app. 



############################.
## Filepaths ----
############################.
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/"
  shapefiles <- "/PHI_conf/ScotPHO/Profiles/Data/Shapefiles/"
} else  {
  lookups <- "//stats/ScotPHO/Profiles/Data/Lookups/"
  shapefiles <- "//stats/ScotPHO/Profiles/Data/Shapefiles/"
}

############################.
## Packages ----
############################.
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(rgdal) #for reading shapefiles
library(sf) # required for formatting shapefiles


###############################################.
## Create lookup  ----
###############################################.

# Reading in the police division shapefile:
pd_bound_orig <-readOGR(shapefiles, "SG_ScottishPoliceDivisions_2019") 

# The shapefile is too big for app, and we can't use rmapshaper to simplify it, so look to create by dissolving the council area shapefile:

# First, use the PD shapefile to make a PD lookup with PD codes and names. 
# Will use to add codes to the shapefile created below, 
# and also for adding to the geo_lookup the shiny app uses.
pd_lookup <- pd_bound_orig@data %>%
  select(code = PDivision, areaname = PDivName) %>%
  mutate(areatype = "Police division",
         parent_area = NA_character_,
         areaname_full = paste0(areaname, " - PD"))

# append the PD lookup to the main lookup used in the app
geo_lookup <- readRDS("shiny_app/data/profiles_geo_lookup.rds") %>% # geography lookup
  mutate(across(.cols = c('code', 'areaname', 'areatype', 'parent_area'),
                ~ as.character(.x))) %>% # convert to character before appending, to avoid factor mess
  rbind(pd_lookup) %>%
  arrange(code) %>% # keeps same area types together in the factor coding
  mutate(across(.cols = c('code', 'areaname', 'areatype', 'parent_area'), #convert back to factor
                ~ factor(.x)))

# Save back into the folder to be used by the shiny app
write_rds(geo_lookup,"shiny_app/data/profiles_geo_lookup.rds")
# has increased rds file size from 38 KB to 149 KB, not sure why, as only 13 rows of data from a 1.5KB df have been added.
# Had some compression been applied to the original lookup file?


###############################################.
## Create shapefile  ----
###############################################.

# Second, dissolve the CA shapefile into PDs:
# Read in the CA shapefile and recode the area_name column from CAs to PDs
ca_to_pd <- readRDS("shiny_app/data/CA_boundary.rds") %>%
# Recode the area_name column (manual lookup based on the info in the source web page)
  mutate(area_name = case_when(area_name %in% c("Argyll and Bute", "West Dunbartonshire") ~ "Argyll and West Dunbartonshire",
                        area_name %in% c("East Ayrshire","North Ayrshire","South Ayrshire") ~ "Ayrshire",
                        area_name %in% c("Dumfries and Galloway") ~ "Dumfries and Galloway",
                        area_name %in% c("City of Edinburgh") ~ "Edinburgh",
                        area_name %in% c("Fife") ~ "Fife",
                        area_name %in% c("Stirling","Falkirk","Clackmannanshire") ~ "Forth Valley",
                        area_name %in% c("Glasgow City","East Renfrewshire","East Dunbartonshire") ~ "Greater Glasgow",
                        area_name %in% c("Highland", "Orkney Islands","Shetland Islands","Eilean Siar"  ) ~ "Highlands and Islands",
                        area_name %in% c("North Lanarkshire", "South Lanarkshire") ~ "Lanarkshire",
                        area_name %in% c("West Lothian", "East Lothian", "Midlothian","Scottish Borders") ~ "The Lothians and Scottish Borders",
                        area_name %in% c("Aberdeen City", "Aberdeenshire", "Moray") ~ "North East",
                        area_name %in% c("Inverclyde", "Renfrewshire") ~ "Renfrewshire and Inverclyde",
                        area_name %in% c("Angus", "Dundee City", "Perth and Kinross") ~ "Tayside"
  )) %>%
  select(-code) %>%
  merge(y=pd_lookup, by.x="area_name", by.y = "areaname") %>% # add in the PD codes
  select(area_name, code)

# Dissolve the geometries in the same PD
PD_boundary <- ca_to_pd %>%
  group_by(area_name, code) %>%
  summarise() %>%
  mutate(area_name = as.factor(area_name), #to match other shapefiles
         code = as.factor(code)) 

# check it plots ok
plot(PD_boundary)

#Saving as rds as it is much faster to read

# First, back into the ScotPHO shapefiles folder
saveRDS(PD_boundary, paste0(shapefiles, "PD_boundary.rds"))

# Then into the folder to be used by the shiny app
write_rds(PD_boundary,"shiny_app/data/PD_boundary.rds")


##END