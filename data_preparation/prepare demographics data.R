
## THIS SCRIPT COULD BE INTEGRATED INTO DATA PREP SCRIPTS/FUNCTIONS WHEN NEW VISUALISATION COMPLETED

# Need to run main data prep script first to load libraries and filepaths
# profiles tool geo lookup should also be up to data to ensure corect geo labelling

#population_data_file <- readRDS(file=paste0(pop_lookup, 'testfile_population_breakdown_wide.rds'))
demographics_dataset <- readRDS(file=paste0(scotpho_folder, '/Lookups/Population/population_age_sex_breakdown_wide.rds')) |>
  # Add age decriptions to population data (should this be done in indicator production?)
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

# read in geography lookup from 'shiny_app/data' project in this folder 
geo_lookup_path <- "shiny_app/data/profiles_geo_lookup.rds"
geo_lookup <- readRDS(geo_lookup_path)

#match on geogrpahy names and parent areas 
demographics_dataset <- demographics_dataset |> 
  left_join(geo_lookup, by = "code")

## save final file to local repo
write_parquet(demographics_dataset, "shiny_app/data/demographic_dataset", compression = "zstd")




