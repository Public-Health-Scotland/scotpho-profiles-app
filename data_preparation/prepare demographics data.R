
## THIS SCRIPT COULD BE INTEGRATED INTO DATA PREP SCRIPTS/FUNCTIONS WHEN NEW VISUALISATION COMPLETED

# Need to run main data prep script first to load libraries and filepaths
# profiles tool geo lookup should also be up to data to ensure corect geo labelling


demographic_dataset <- read_parquet(file=paste0(scotpho_folder, "/Data to be checked/demographics_populations.parquet")) |>
  rename(age=age_label)

  
# read in geography lookup from 'shiny_app/data' project in this folder 
geo_lookup_path <- "shiny_app/data/profiles_geo_lookup.rds"
geo_lookup <- readRDS(geo_lookup_path)

#match on geogrpahy names and parent areas 
demographic_dataset <- demographic_dataset |> 
  left_join(geo_lookup, by = "code")

## save final file to local repo
write_parquet(demographic_dataset, "shiny_app/data/demographic_dataset", compression = "zstd")




