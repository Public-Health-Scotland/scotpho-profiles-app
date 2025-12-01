# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data preparation script ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script prepares 4 files required for the dashboard:
# 3 indicator datasets and 1 metadata file

# Note there are also 8 geography files required to run the dashboard
# (one of which is needed to run this script)
# if you are running this script for the first time OR if the geography lookups have been updated
# then run the 'update geography lookups.R' script first to generate these files and save them in your project


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
list.files("data_preparation/functions", full.names = TRUE) |> walk(~ source(.x))


## Filepaths ----

# scotpho network folder:
# bulk of metadata and indicator data sits in sub-folders within this folder
# (for indicators managed by scotpho analysts)
scotpho_folder <- "/PHI_conf/ScotPHO/Profiles/Data"

# collaboration folder:
# subfolder in scotpho network folder where teams developing new indicators/profiles 
# save their data/metadata (when they are responsible for updating going forward)
collab_folder <- file.path(scotpho_folder, "Collaborations")


# collaboration sub-folders for specific teams/profiles:
climate_folder <- file.path(collab_folder, "Climate")


# project data folder:
# a sub-folder WITHIN this scotpho-profiles-app R project
# where the files generated in this script are saved
# ready to be read into the shiny app via the 'Global.R' script
project_folder <- file.path("shiny_app/data")


# create project folder if doesn't already exist
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
    filepath = file.path(scotpho_folder, "Lookups/Technical_Document.xlsx"), 
    test_indicators = FALSE
    ),
  "climate" = prepare_techdoc(
    filepath = file.path(climate_folder, "Lookups/Technical_Document.xlsx"), 
    test_indicators = FALSE
    )
  )


# combine techdocs to create one "master copy" of metadata 
# for all indicators in the dashboard
techdocs_combined <- list_rbind(techdocs_list)


# check if same ind_id exists in more than 1 techdoc: ind_ids should be 
# unique to each indicator, not just indicators within a specific techdocs!
duplicate_inds <- techdocs_combined |>
  group_by(ind_id) |>
  filter(n() > 1)


# remove columns not required
techdocs_combined <- techdocs_combined |>
  select(-c(supress_less_than, techdoc_path))


# if no duplicates found then save the metadata in this projects
# data folder ready to be used within the shiny app 
if (nrow(duplicate_inds) == 0){
  write_parquet(techdocs_combined, file = file.path(project_folder, "techdoc.parquet"))
}


# remove objects from global env.
rm(techdocs_list, techdocs_combined, duplicate_inds)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Prepare main dataset ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This dataset is where the bulk of indicator data for the shiny app sits
# Every indicator in the app will have some data within this dataset
# Used across the 'summary', 'trend' and 'rank' tabs of the app


# combine files ending in '_shiny.csv from 'Shiny Data' folder
# within /PHI_conf/ScotPHO/Profiles/Data
scotpho_main <- create_dataset(
  dataset = "main", 
  folder = scotpho_folder, 
  file_suffix = "_shiny.csv", 
  test_indicators = FALSE,
  profile_filter = NULL,
  file_snapshot = FALSE # set to TRUE if deploying
  )


# VE 2025: temporarily remove this indicator at HSC locality 
# level as still uses old codes
scotpho_main <- scotpho_main |>
  filter(!(indicator == "Children in low income families" &
             substr(code, 1, 3) == "S99"))



# combine files ending in '_shiny.parquet' from 'Shiny Data'
# folder within /PHI_conf/ScotPHO/Profiles/Data/Collaborations/Climate
climate_main <- create_dataset(
  dataset = "main",
  folder = climate_folder,
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
# Step 3: Prepare popgroups dataset -----              
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This dataset only contains some indicators, where an extra file has been produced
# for that indicator containing some population group splits (such as age, sex etc.)
# Used for the 'population groups' tab of the app 


# combine files ending in 'popgrp.csv from 'Shiny Data' folder
# within /PHI_conf/ScotPHO/Profiles/Data
scotpho_popgrp <- create_dataset(
  dataset = "popgroup",
  folder = scotpho_folder,
  file_suffix = "popgrp.csv",
  test_indicators = FALSE,
  profile_filter = NULL,
  file_snapshot = FALSE # set to TRUE if deploying
)



# combine files ending in 'popgrp.parquet' from 'Shiny Data'
# folder within /PHI_conf/ScotPHO/Profiles/Data/Collaborations/Climate
climate_popgrp <- create_dataset(
  dataset = "popgroup",
  folder = climate_folder,
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
# Step 4: Prepare deprivation dataset -----              
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This dataset only contains some indicators, where an extra file has been produced
# for that indicator containing inequalities splits (i.e. SIMD, RII, SII, PAR)
# Used for the 'Deprivation' tab of the app 

# combine files ending in 'ineq.rds' from 'Shiny Data' folder
# within /PHI_conf/ScotPHO/Profiles/Data
depr_dataset <- create_dataset(
  dataset = "deprivation",
  folder = scotpho_folder,
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



