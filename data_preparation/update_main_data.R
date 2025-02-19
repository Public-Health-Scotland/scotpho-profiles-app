## FUNCTION: update_main_data.R ----


## PARAMETERS
# load_test_indicators : (default is FALSE) option to set to TRUE which reads indicators from the test shiny folder
# create_backup :  (default is FALSE ) option to set to TRUE when creating a distinct backup version desired e.g. when planning to deploy the app

update_main_data <- function(load_test_indicators = FALSE, create_backup = FALSE) {
  
  
## Find all separate indicator data files in the shiny data folder -----
indicator_data_files <- list.files(
  path = shiny_files, 
  pattern = "*_shiny.csv", 
  full.names = TRUE
)


## Combine into one dataset  -----
main_dataset <- combine_files(indicator_data_files)


## If test indicators are to be included then list files & combine files from test shiny folder
if (load_test_indicators == TRUE){
  
  ## Find new indicators data files in the test shiny data folder -----
  test_indicator_files <- list.files(
    path = test_shiny_files, 
    pattern = "*_shiny.csv", 
    full.names = TRUE
  )
  
  ## Combine into one dataset  -----
  test_indicator_dataset <- combine_files(test_indicator_files)
  
  ## Combine main dataset and test indicators
  main_dataset <- bind_rows(main_dataset, test_indicator_dataset)
}


## Read in older data ----
# note: most indicators will have their own separate data file. 
# However, some indicators which haven't been updated in years sit in a file called 'All Data for Shiny.csv' 
# from when there was a different process for creating indicator data. Once they have been prepared via the new process
# (i.e. using the analysis functions from the indicator production repository), this step will no longer be required
old_data <- read_csv(paste0(shiny_files, "/All Data for Shiny.csv")) |>
  filter(!(ind_id %in% unique(main_dataset$ind_id)))


## Combine new and old data ----
main_dataset <- bind_rows(main_dataset, old_data)


## Attach metadata from the technical doc lookup ----
main_dataset <- left_join(x = main_dataset, y = technical_doc, by = "ind_id")


## Attach geography info from geography lookup--------
main_dataset <- main_dataset |> 
  # # temporarily removing this indicator at HSC locality level as still uses old codes
  filter(!(indicator_name == "Children in low income families" &
             substr(code, 1, 3) == "S99")
  ) |>
  replace_old_geography_codes(col_name = "code") |>
  left_join(geography_lookup, by = "code")



## Apply suppression where required ---------
main_dataset <- main_dataset |>
  apply_suppressions()



## convert some cols to numeric and round digits
main_dataset <- main_dataset |>
  mutate(
    across(
      c("numerator", "measure", "upci", "lowci"),
      ~ round(as.numeric(.), digits = 1)
    )
  )


# some indicators have years missing from their dataset (e.g. if no data was collected that year due to covid)
# To ensure that the data in the trend tab doesn't drop to 0 for those years, we create data for those missing years
# and populate the measure with 'NA' instead - this creates a gap in the trend chart, instead of an incorrect drop to 0
# Note: This might make more sense to eventually add to the indicator production functions instead?
main_dataset <- main_dataset |>
  rbind(
    # child dental health pri 1
    create_gap_year(
      dataset = main_dataset,
      indicator_id = 21005, 
      gap_year = 2020, 
      base_year = 2019, 
      gap_trend_axis = "2020/21"
    ), 
    
    # Population within 500 metres of a derelict site
    create_gap_year(
      dataset = main_dataset,
      indicator_id = 20901, 
      gap_year = 2020, 
      base_year = 2019, 
      gap_trend_axis = "2020"
    ) 
  )


# Remove some indicators from IZ level to reduce risk of secondary disclosure
# Data is generated to allow ScotPHO analysts to review numbers and release to internal staff on request
# However not likely to be able to satisfy statistical disclosure signed off to include in main tool
main_dataset <- main_dataset |>
  filter(
    !(ind_id %in% c(
      "20205", # drug-related hospital stays
      "20403", # deaths from suicide
      "20204", # alcohol-related deaths
      "20402", # psychiatric hospital admissions
      "20301", # cancer registrations
      "20401", # teenage pregnancies
      "21001", # Population prescribed drugs for anxiety/depression/psychosis
      "21002" # mothers smoking during pregnancy
    ) & substr(code, 1, 3) == "S02")) |>
    rename(indicator = indicator_name)

# make dataset available in global environment for validation tests
main_dataset_validation <<- main_dataset 

# remove columns not required within shiny app
main_dataset <- main_dataset |>
  select(-c(supression, supress_less_than, type_id, file_name, label_inequality))


# create a new column which contains the full geography path
# most geographies have 2 parts to their path i.e. 'Health Board/NHS Ayrshire & Arran'
# with the exception of IZs/HSC Localities where a parent area is also included i.e. 'HSC Locality/Edinburgh City/Edinburgh North-East'
main_dataset <- create_geography_path_column(main_dataset)


# make available in global environment for viewing what will be sent to shiny app
main_dataset <<- main_dataset

## save final file to local repo
write_parquet(main_dataset, "shiny_app/data/main_dataset", compression = "zstd")

## Optional: Create backup of from local repo -----
## Usually would only want to create a backup if you intend to update live tool
## This file will be stored in backups folder and could be used to roll back app to a particular date
if (create_backup == TRUE) {
  
  file.copy(
    "shiny_app/data/main_dataset", 
    paste0(backups, "main_dataset_", Sys.Date()), 
    overwrite = TRUE
  )
} 




}





