# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script prepares the data behind indicators split by SIMD within the ScotPHO Profiles Tool
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Set up ----

## Load libraries -----

library(dplyr) # data wrangling
library(openxlsx) # for reading in technical document / converting excel dates
library(readr) # for reading csv files
#library(data.table) # for quickly combining multiple files
#library(scales) # for re-scaling measures
library(arrow) # for writing parquet files
library(janitor) # for cleaning column names
library(assertthat) # for data validation tests
library(purrr) # for copying multiple files at once
library(tidyr) # for pivot longer


## Set file-paths -----
data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"
lookups <- paste0(data_folder, "Lookups/") 
shape_files <- paste0(data_folder, "Shapefiles/")
shiny_files <- paste0(data_folder, "Shiny Data") #folder which contains indicator data files ready for inclusion in live profiles tool
test_shiny_files <- paste0(data_folder, "Test Shiny Data") #folder which contains indicator data files under development and not considered ready for live tool
backups <- paste0(data_folder, "Backups/") #area for saving copies of historic files linked to shiny app in case we mess up a file and need to roll back


## Source functions called within this script
source("data_preparation/prepare_techdoc.R")
source("data_preparation/prepare_shapefiles.R")



## Prepare technical document ----

# Generates techdoc parquet file within shiny app data folder
# Also generates profile_lookup and technical_doc dataframe which are required for processing the indicator data files below.
# Can also be used to save a backup version of techdoc
# Optional parameter allows inclusion of test indicator data

prepare_techdoc(load_test_indicators = FALSE, create_backup = FALSE)




## Shapefiles (optional - shape files rarely update - you )
#   Note: this step is only really necessary if you are running this script for the first time
#   OR if there have been updates to the shape files

prepare_shapefiles()


