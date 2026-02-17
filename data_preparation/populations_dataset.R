
### THIS SCRIPT SHOULD BE INTEGRATED INTO DATA PREP WHEN NEW VISUALISATION COMPLETED

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


# Filepaths 
pop_lookup <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Population/"


population_data_file <- readRDS(file=paste0(pop_lookup, 'testfile_population_breakdown_wide.rds'))


population_data <- population_data_file  |>
  mutate(age = case_when(age_grp == 1 ~ "0-4",
                         age_grp == 2   ~ "5-9",
                         age_grp == 3   ~ "10-14",
                         age_grp == 4   ~ "15-19",
                         age_grp == 5   ~ "20-24",
                         age_grp == 6   ~ "25-29",
                         age_grp == 7   ~ "30-34",
                         age_grp == 8   ~ "35-39",
                         age_grp == 9   ~ "40-44",
                         age_grp == 10  ~ "45-49",
                         age_grp == 11  ~ "50-54",
                         age_grp == 12  ~ "55-59",
                         age_grp == 13  ~ "60-64",
                         age_grp == 14  ~ "65-69",
                         age_grp == 15  ~ "70-74",
                         age_grp == 16  ~ "75-79",
                         age_grp == 17  ~ "80-84",
                         age_grp == 18  ~ "85-89",
                         age_grp == 19  ~ "90+", TRUE ~"other"))

# open geography look from shiny app data folder 
# (required for matches on areanames, area type and parent geographies to geography codes in indicator datasets)
geography_lookup <- readRDS(
  file = paste0(lookups, "Geography/opt_geo_lookup.rds") ##TO DO rename opt_geo_lookup to profiles_geo_lookup in looksups repo
)

population_data <- population_data  |> 
  left_join(geography_lookup, by = "code")


## save final file to local repo

write_parquet(population_data, "shiny_app/data/demographic_dataset", compression = "zstd")




##############
## simd data


deprivation_centile_file <- readRDS(paste0(data_folder, '/Test Shiny Data/testfile_population_by_simd_centiles.rds'))

geography_lookup <- readRDS(
  file = paste0(lookups, "Geography/opt_geo_lookup.rds") ##TO DO rename opt_geo_lookup to profiles_geo_lookup in looksups repo
)

deprivation_centile_file <- deprivation_centile_file |> 
  left_join(geography_lookup, by = "code") |>
  select(-geo_type,-simd_version)


deprivation_centile_file <- deprivation_centile_file|>
  select(-total_pop_all_ages,-total_pop_u26,-total_pop_working)

write_parquet(deprivation_centile_file, "shiny_app/data/demographic_simd_dataset", compression = "zstd")

