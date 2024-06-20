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

# 1. Set up

## Load libraries -----

library(dplyr) # data wrangling
library(openxlsx) # for reading in technical document / converting excel dates
library(readr) # for reading csv files
library(data.table) # for quickly combining multiple files
#library(scales) # for re-scaling measures
library(arrow) # for writing parquet files
library(janitor) # for cleaning column names
library(assertthat) # for data validation tests
library(purrr) # for copying multiple files at once
library(tidyr) # for pivot longer


# 2. Set file-paths

data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"
lookups <- paste0(data_folder, "Lookups/") 
shape_files <- paste0(data_folder, "Shapefiles/")
shiny_files <- paste0(data_folder, "Shiny Data") #folder which contains indicator data files ready for inclusion in live profiles tool
test_shiny_files <- paste0(data_folder, "Test Shiny Data") #folder which contains indicator data files under development and not considered ready for live tool
backups <- paste0(data_folder, "Backups/") #area for saving copies of historic files linked to shiny app in case we mess up a file and need to roll back


# 3. Source function scripts

source("data_preparation/2_dataprep_functions.R") # generic functions 
source("data_preparation/2_dataprep_validation_tests.R") # validation checks

source("data_preparation/update_techdoc.R") # script to read in & format techdoc
source("data_preparation/update_shapefiles.R") # script to read in & format shapefiles
source("data_preparation/update_deprivation_data.R") # script to read in & format deprivation data




#########################################################################################.
## Update technical document ----
#########################################################################################.

# Generates techdoc parquet file within shiny app data folder
# Also makes profile_lookup and technical_doc dataframes visible in data pane
# which are required for processing the indicator data files below.
# Option:  to save a backup version of techdoc (set create_backup to TRUE)
# Option: to include indicators data labelled as test indicators in techdoc (set load test indicators to TRUE)

update_techdoc(load_test_indicators = FALSE, create_backup = FALSE)

# PLANNING ON UPDATING INDICATORS AND DEPLOYING THE APP? consider generating backup of techdoc. 
# update_techdoc(load_test_indicators = FALSE, create_backup = TRUE)



#########################################################################################.
## Update geography lookups  ----
#########################################################################################.

## Geography Lookup
# copy the main geography lookup to your local repo
file.copy(
  paste0(lookups, "Geography/opt_geo_lookup.rds"), # old file path - TO DO rename opt_geo_lookup to profiles_geo_lookup in looksups repo
  paste0("shiny_app/data/profiles_geo_lookup.rds"),  # copy to local shiny app data folder
  overwrite = TRUE
)

# open geography look from shiny app data folder (required for matches on areanames, area type and parent geographies to geography codes in indicator datasets)
geography_lookup <- readRDS(
  file = paste0(lookups, "Geography/opt_geo_lookup.rds") ##TO DO rename opt_geo_lookup to profiles_geo_lookup in looksups repo
)


## To DO: is there code to create the nodes?
## Geo nodes - need to add this in somewhere
#  rename prefix from opt something like 'profiles'geo_lookup its more explict what geo lookup is for. Ok to keep as just geo_lookup once inside the shiny app tho.


#########################################################################################.
## Update main dataset  ----
## information that populates summary/trend/rank tabs
#########################################################################################.


# TO DO:NEED TO WRITE THIS FUNCTION
#update_main_data()



#########################################################################################.
## Update deprivation data  ----
## i.e. indicator data split by SIMD quintiles.
#########################################################################################.

update_deprivation_data()

## TO DO figure out if validation tests # should these sit inside the data prep function? what happens if validation test fails inside a function
## Decide which fields actually need to be fed into profiles tool - some are required for validation checks but not sure these are needed in app or have different names.


TEST_no_missing_ineq_indicators(data_depr) # compares dataset to techdoc column 'inequality label' is not null 

TEST_no_missing_geography_info(data_depr) # all rows have valid geography code

TEST_no_missing_metadata(data_depr) # checks for dep indicators with no indicator name

TEST_suppression_applied(data_depr) # double checking suppression function wasn't skipped

TEST_inequalities_trends(data_depr) # checks if last deprivation indicator year is same as main profiles dataset max year (wont run until main data also in data prep)

# Haven't split inequalities data by profile yet



#########################################################################################.
## Update shape files  ----
#########################################################################################.

# Shapefiles (used to generate maps presented in rank tab only)
# Note: this step is only really necessary if you are running this script for the first time
# OR if there have been updates to the shape files
update_shapefiles()



##END


