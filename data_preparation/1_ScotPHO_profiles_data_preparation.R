# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1_ScotPHO_profiles_data_preparation.R
# This script prepares the data behind the ScotPHO Profiles Tool
#
# There are a number of data files that profile tool app requires.
# Updating the tool will require updating one or more of the following data files
#
# Meta data files
# 1 - Technical document - excel file located within network directory holding all meta about indicators
# 2 - Geography lookup - details linking geography codes to geography names plus heirarchy of which areas are within others
# 3 - Shapefiles - used to generate maps within rank visualisations - updating these is rarely required
#
# Files containing indicator data (which parts of profiles tool depend on them)
# 4 - main data (summary/rank/trend tabs)
# 5 - SIMD data (SIMD/deprivation tab)
# 6 - population groups data (population group tab)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


######################################.
# Set up
######################################.

## Load libraries -----
library(dplyr) # data wrangling
library(openxlsx) # for reading in technical document / converting excel dates
library(readr) # for reading csv files
library(data.table) # for quickly combining multiple files
#library(scales) # for re-scaling measures
library(nanoparquet) # for writing parquet files
library(janitor) # for cleaning column names
library(assertthat) # for data validation tests
library(purrr) # for copying multiple files at once
library(tidyr) # for pivot longer
library(sf)


## Set file-paths
data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"
lookups <- paste0(data_folder, "Lookups/") 
shape_files <- paste0(data_folder, "Shapefiles/")
shiny_files <- paste0(data_folder, "Shiny Data") #folder which contains indicator data files ready for inclusion in live profiles tool
test_shiny_files <- paste0(data_folder, "Test Shiny Data") #folder which contains indicator data files under development and not considered ready for live tool
backups <- paste0(data_folder, "Backups/") #area for saving copies of historic files linked to shiny app in case we mess up a file and need to roll back


## Source function scripts
source("data_preparation/2_dataprep_functions.R") # generic functions 
source("data_preparation/2_dataprep_validation_tests.R") # validation checks

source("data_preparation/update_techdoc.R") # script to read in & format techdoc
source("data_preparation/update_shapefiles.R") # script to read in & format shapefiles
source("data_preparation/update_deprivation_data.R") # script to read in & format deprivation data
source("data_preparation/update_main_data.R") # script to read in and forma main data
source("data_preparation/update_popgroup_data.R") # script to read in and forma main data


###################################################.
## Update technical document ----
##################################################.

# Generates techdoc parquet file within shiny app data folder
# Also makes technical_doc visible in data pane
# which are required for processing the indicator data files below.
# Option:  to save a backup version of techdoc (set create_backup to TRUE)
# Option: to include indicators data labelled as test indicators in techdoc (set load test indicators to TRUE)

update_techdoc(load_test_indicators = FALSE, create_backup = FALSE)

# PLANNING ON UPDATING INDICATORS AND DEPLOYING THE APP? consider generating backup of techdoc. 
# update_techdoc(load_test_indicators = FALSE, create_backup = TRUE)


###################################################.
## Update geography lookups  ----
###################################################.

## Geography Lookup
# copy the main geography lookup to your local repo
file.copy(
  paste0(lookups, "Geography/opt_geo_lookup.rds"), # old file path - TO DO rename opt_geo_lookup to profiles_geo_lookup in looksups repo
  paste0("shiny_app/data/profiles_geo_lookup.rds"),  # copy to local shiny app data folder
  overwrite = TRUE
)

# open geography look from shiny app data folder 
# (required for matches on areanames, area type and parent geographies to geography codes in indicator datasets)
geography_lookup <- readRDS(
  file = paste0(lookups, "Geography/opt_geo_lookup.rds") ##TO DO rename opt_geo_lookup to profiles_geo_lookup in looksups repo
)



#########################################################################################.
## Update main dataset  ----
## information that populates summary/trend/rank tabs
#########################################################################################.

# switch to TRUE if including test indicators (note that you will also need to load test indicators in the update_techdoc function)
# create_backup - switch to true if deploying the live app with updated indicator datasets 

update_main_data(load_test_indicators = FALSE, create_backup = FALSE)

# run validation tests one by one 
# when a test is finished running, if it's passed 'TRUE' will print in the console
# otherwise pay attention to the message in the console detailing why the test has failed
TEST_no_missing_indicators(main_dataset) # compares indicators in dataset to active indicators in techdoc  
TEST_no_missing_geography_info(main_dataset) # all rows have valid geography code
TEST_no_missing_metadata(main_dataset) # checks for indicators with missing metdata (i.e. if failed to join with techdoc)
TEST_suppression_applied(main_dataset) # double checking suppression function wasn't skipped




#######################################################################################.
## Create geography nodes file 
#######################################################################################.


# get a distinct list of geography paths in the main dataset
main_dataset_geography_list <- main_dataset |>
  select(geo_path) |>
  distinct()

main_dataset <- main_dataset |>
  mutate(areatype = factor(areatype, levels = c("Scotland", 
                                                "Council area", 
                                                "Health board",
                                                "Alcohol & drug partnership",
                                                "HSC partnership",
                                                "HSC locality",
                                                "Intermediate zone",
                                                "Police division"))) |>
  arrange(areatype, parent_area, areaname)

# convert them into lists of parent/child nodes that can be used to create a 
# hierarchical geography filter in the data tab of the shiny app
main_dataset_geography_list <- create_geography_nodes(main_dataset_geography_list$geo_path)


# save file to be used in app
saveRDS(main_dataset_geography_list, "shiny_app/data/main_dataset_geography_nodes.rds")


###############################################################.
## Update deprivation data  ----
## i.e. indicator data split by SIMD quintiles.
##############################################################.

update_deprivation_data(load_test_indicators = FALSE, create_backup = FALSE)

## Decide which fields actually need to be fed into profiles tool - some are required for validation checks but not sure these are needed in app or have different names.

# run validation tests
TEST_no_missing_ineq_indicators(deprivation_dataset) # compares dataset to techdoc column 'inequality label' is not null 
TEST_no_missing_geography_info(deprivation_dataset) # all rows have valid geography code
TEST_no_missing_metadata(deprivation_dataset) # checks for dep indicators with no indicator name
TEST_suppression_applied(deprivation_dataset) # double checking suppression function wasn't skipped
TEST_inequalities_trends(deprivation_dataset) # checks if last deprivation indicator year is same as main profiles dataset max year (wont run until main data also in data prep)



########################################################################.
## Update population groups data  ----
## i.e. indicator data split by various different inequality groups.
########################################################################.

update_popgroup_data(load_test_indicators = FALSE, create_backup = FALSE)

# no validation tests currently written for population data.


############################################################.
## Update shape files  ----
############################################################.

# Shapefiles (used to generate maps presented in rank tab only)
# Note: this step is only really necessary if you are running this script for the first time
# OR if there have been updates to the shape files
update_shapefiles()


###############################################################.
## Remove objects  ----
###############################################################.

# remove objects from global environment before running shiny app 
rm(list = ls())

##END