## THIS SCRIPT COULD BE INTEGRATED INTO DATA PREP SCRIPTS/FUNCTIONS WHEN NEW VISUALISATION COMPLETED

# Need to run main data prep script first to load libraries and filepaths
# profiles tool geo lookup should also be up to data to ensure corect geo labelling

# open the base indicator file generated in indicator production script ("demo_scotland.RDS" within branch called ve_dep_pop)

demographic_simd_data <-readRDS(file=paste0(scotpho_folder, '/Test Shiny Data/testfile_population_by_simd_centiles_long.rds'))

# read in geography lookup from 'shiny_app/data' project in this folder 
geo_lookup_path <- "shiny_app/data/profiles_geo_lookup.rds"
geo_lookup <- readRDS(geo_lookup_path)


demographic_simd_data<- demographic_simd_data|> 
  left_join(geo_lookup, by = "code") 


## save final file to local repo
write_parquet(demographic_simd_data, "shiny_app/data/demographic_simd_dataset.parquet", compression = "zstd")



rm(geo_lookup,demographic_simd_data)

demographic_simd_data <-readRDS(file=paste0(scotpho_folder, '/Test Shiny Data/testfile_population_by_simd_centiles_wide.rds'))

# read in geography lookup from 'shiny_app/data' project in this folder 
geo_lookup_path <- "shiny_app/data/profiles_geo_lookup.rds"
geo_lookup <- readRDS(geo_lookup_path)


demographic_simd_data<- demographic_simd_data|> 
  left_join(geo_lookup, by = "code") 


## save final file to local repo
write_parquet(demographic_simd_data, "shiny_app/data/demographic_simd_dataset_wide.parquet", compression = "zstd")
