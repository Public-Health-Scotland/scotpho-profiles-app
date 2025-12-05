# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data preparation script ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The ScotPHO profiles tool app requires a number of different data files to run.
# This script runs through the steps required to generate all data files that the app will need.

# It might help to know individual ScotPHO profiles indicator data files are prepared using code from a distinct github repositor
# (https://github.com/Public-Health-Scotland/scotpho-indicator-production).
# Distinct indicator data files are saved within a ScotPHO team network folder.
# This script contains steps that create lists of all indicators within these folders, reads in these files to create combined indicator data files.
# The combined data files are then saved to an individual users shiny app project folder. 
# Using a personal cloned version of the profiles tool shiny app is what enables different users to work with profiles tool app containing indicator data files
# or look-up files in different states.

# There are a number of data files that profile tool app requires to run:
# Meta data files
# 1 - Technical document - excel file located within network directory holding all meta about indicators
# 2 - Geography lookups - list of all geography codes and transaltion to areanames
#   - Geography hierarchy - heirarchy of parent/child geographies
#   - Shapefiles - used to generate maps within rank visualisations - updating these is rarely required

# Files containing indicator data (which parts of profiles tool depend on them)
# 3 - main data (summary/rank/trend tabs)
# 4 - population groups data (population group tab)
# 5 - SIMD data (SIMD/deprivation tab)


# If preparing data for deployment: - 
# Change 'file_snapshot' arguments to TRUE
# this will show you what files have been added/removed/updates since previous deployment
# and ensures a snapshot of the folders current state is taken, ready for the next analyst
# to compare when next deploying.

# If wanting to include test indicators: - 
# Change test_indicator arguments to TRUE
# This will also read in data from any 'Test Shiny Data' folders

# If only wanting to include data for a particular profile: - 
# Change profile_filter to profiles 3-letter capitalised abbreviation


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dependencies -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Load packages ----
library(dplyr)
library(data.table)
library(janitor)
library(openxlsx)
library(nanoparquet)
library(purrr) 
library(cli)
library(rlang)
library(tools)
library(utils)


## Source functions ----
## This step reads in variety of functions which complete the steps required to generate files required by shiny app
list.files("data_preparation/functions", full.names = TRUE) |> walk(~ source(.x))


## Filepaths ----

# scotpho network folder:
# bulk of metadata and indicator data sits in sub-folders within this folder
# (for indicators managed by scotpho analysts)
scotpho_folder <- "/PHI_conf/ScotPHO/Profiles/Data"

# collaboration network folder:
# subfolder in scotpho network folder where teams developing new indicators/profiles 
# save their data/metadata (when they are responsible for updating going forward)
collab_folder <- file.path(scotpho_folder, "Collaborations")

# project data folder (this will be local to your cloned repo):
# a sub-folder WITHIN this scotpho-profiles-app R project
# where the files generated in this script are saved
# ready to be read into the shiny app via the 'Global.R' script
project_folder <- file.path("shiny_app/data")


# create local project folder if doesn't already exist
if (!dir.exists (project_folder)){
  dir.create(project_folder)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1 - Prepare metadata lookup ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Technical Documents (techdocs) contain metadata for each indicator in the app
# This step reads in, formats and combines multiple techdocs to create one version for the app
# This file is used in the indicator metadata panels under each tab of the app 


# Read in scotpho's techdoc and other team/profile specific techdocs from collaboration sub-folders 
techdocs_list <- list(
  "scotpho" = prepare_techdoc(
    filepath = file.path(scotpho_folder,"Lookups/Technical_Document.xlsx"), 
    test_indicators = FALSE
    ),
  "climate" = prepare_techdoc(
    filepath = file.path(collab_folder,"Climate/Lookups/Climate_Technical_Document.xlsx"), 
    test_indicators = FALSE
    )
  )

# Function to update techdoc file in local shiny data folder 
update_techdoc(techdocs_list)

# remove objects from global env.
#rm(techdocs_list)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Prepare geography files ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# if you are running this script for the first time all functions need to be run
# after initial running only technically needs to be run if the geography lookups have been updated

# Update geography lookup
create_geography_lookup(folder = scotpho_folder)

# Update geographical hierarchy - derived from geography lookup
update_geography_hierachy(folder = scotpho_folder)

#Update the shape files used in the maps on rank tab 
update_shapefiles(folder=scotpho_folder)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Prepare main dataset ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This dataset is where the bulk of indicator data for the shiny app sits
# Every indicator in the app will have some data within this dataset
# Used across the 'summary', 'trend' and 'rank' tabs of the app


# combine files ending in '_shiny.csv from 'Shiny Data' folder
# within /PHI_conf/ScotPHO/Profiles/Data
scotpho_main <- create_dataset(
  dataset = "main", 
  folder = scotpho_folder,
  techdoc_name = "Technical_Document.xlsx", #name of scotpho team techdoc
  file_suffix = "_shiny.csv", 
  test_indicators = FALSE,
  profile_filter = NULL,
  file_snapshot = FALSE # set to TRUE if deploying
  )


# combine files ending in '_shiny.parquet' from 'Shiny Data'
# folder within /PHI_conf/ScotPHO/Profiles/Data/Collaborations/Climate
climate_main <- create_dataset(
  dataset = "main",
  folder = file.path(collab_folder,"Climate"),
  techdoc_name = "Climate_Technical_Document.xlsx", #name of climate team techdoc
  file_suffix = "_shiny.parquet",
  test_indicators = FALSE,
  profile_filter = NULL,
  file_snapshot = FALSE # set to TRUE if deploying
)

# combine all main datasets 
main_dataset <- rbind(scotpho_main, climate_main)


# save main dataset in this project data folder, ready to be used in shiny app
write_parquet(main_dataset, file = file.path(project_folder, "main_dataset.parquet"), compression = "zstd")

# clear global env.
rm(list = ls(pattern = "main"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Prepare popgroups dataset -----              
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This dataset only contains some indicators, where an extra file has been produced
# for that indicator containing some population group splits (such as age, sex etc.)
# Used for the 'population groups' tab of the app 


# combine files ending in 'popgrp.csv from 'Shiny Data' folder
# within /PHI_conf/ScotPHO/Profiles/Data
scotpho_popgrp <- create_dataset(
  dataset = "popgroup",
  folder = scotpho_folder,
  techdoc_name = "Technical_Document.xlsx", #name of scotpho team techdoc
  file_suffix = "popgrp.csv",
  test_indicators = FALSE,
  profile_filter = NULL,
  file_snapshot = FALSE # set to TRUE if deploying
)



# combine files ending in 'popgrp.parquet' from 'Shiny Data'
# folder within /PHI_conf/ScotPHO/Profiles/Data/Collaborations/Climate
climate_popgrp <- create_dataset(
  dataset = "popgroup",
  folder = file.path(collab_folder,"Climate"),
  techdoc_name = "Climate_Technical_Document.xlsx", #name of climate team techdoc
  file_suffix = "popgrp.parquet",
  test_indicators = FALSE,
  profile_filter = NULL,
  file_snapshot = FALSE # set to true if deploying
)


# combine all popgroup datasets
popgrp_dataset <- rbind(scotpho_popgrp, climate_popgrp)


# save popgroup dataset in this projects data folder, ready to be used in the app
write_parquet(popgrp_dataset, file = file.path(project_folder, "popgroup_dataset.parquet"), compression = "zstd")


# clear global env.
rm(list = ls(pattern = "popgrp"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: Prepare deprivation dataset -----              
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This dataset only contains some indicators, where an extra file has been produced
# for that indicator containing inequalities splits (i.e. SIMD, RII, SII, PAR)
# Used for the 'Deprivation' tab of the app 

# combine files ending in 'ineq.rds' from 'Shiny Data' folder
# within /PHI_conf/ScotPHO/Profiles/Data
depr_dataset <- create_dataset(
  dataset = "deprivation",
  folder = scotpho_folder,
  techdoc_name = "Technical_Document.xlsx", #name of scotpho team techdoc
  file_suffix = "ineq.rds",
  test_indicators = FALSE,
  profile_filter = NULL,
  file_snapshot = FALSE # set to true if deploying
)

# MM Nov 2025 - temporarily remove scotland decile data
# as we currently only publish quintiles
depr_dataset <- depr_dataset |>
  filter(quint_type != "sc_decile")


# save deprivation dataset in this project data folder, ready to be used in the app
write_parquet(depr_dataset, file = file.path(project_folder, "deprivation_dataset.parquet"), compression = "zstd")



# clear global env.
rm(list = ls())


#END


