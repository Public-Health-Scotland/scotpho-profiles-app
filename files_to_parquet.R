# read in rds files and re-save them as parquet files


library(arrow) # for saving as parquet


### read in RDS files

air_quality <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Climate Data/air_quality_shiny.rds")

climate_action <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Climate Data/climate_action_shiny.rds")

climate_problem <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Climate Data/climate_problem_shiny.rds")

maxtemp_annual <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Climate Data/maxtemp_annual_shiny.rds")

# change maxtemp_annual ind_id column type to number
maxtemp_annual <- maxtemp_annual |>
  mutate(ind_id = as.numeric(ind_id))

### save files as parquet

write_parquet(air_quality, "/PHI_conf/ScotPHO/Profiles/Data/Climate Data/air_quality_shiny.parquet")

write_parquet(climate_action, "/PHI_conf/ScotPHO/Profiles/Data/Climate Data/climate_action_shiny.parquet")

write_parquet(climate_problem, "/PHI_conf/ScotPHO/Profiles/Data/Climate Data/climate_problem_shiny.parquet")

write_parquet(maxtemp_annual, "/PHI_conf/ScotPHO/Profiles/Data/Climate Data/maxtemp_annual_shiny.parquet")