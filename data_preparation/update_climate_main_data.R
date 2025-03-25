## FUNCTION: update_main_data.R ----


## PARAMETERS
# load_test_indicators : (default is FALSE) option to set to TRUE which reads indicators from the test shiny folder
# create_backup :  (default is FALSE ) option to set to TRUE when creating a distinct backup version desired e.g. when planning to deploy the app

update_climate_main_data <- function(load_test_indicators = FALSE, create_backup = FALSE) {
  
  
  ## Find all separate indicator data files in the shiny data folder -----
  indicator_data_files <- list.files(
    path = "/PHI_conf/ScotPHO/Profiles/Data/Climate Data", 
    pattern = "*_shiny.parquet", 
    full.names = TRUE
  )
  
  
  ## Combine into one dataset  -----
  climate_main_dataset <- combine_files(indicator_data_files)
  
  
  ## If test indicators are to be included then list files & combine files from test shiny folder
  if (load_test_indicators == TRUE){
    
    ## Find new indicators data files in the test shiny data folder -----
    test_indicator_files <- list.files(
      path = test_shiny_files, 
      pattern = "*_shiny.parquet", 
      full.names = TRUE
    )
    
    ## Combine into one dataset  -----
    test_indicator_dataset <- combine_files(test_indicator_files)
    
    ## Combine main dataset and test indicators
    main_dataset <- bind_rows(main_dataset, test_indicator_dataset)
  }
  
  
  
  
  ## Attach metadata from the technical doc lookup ----
  climate_main_dataset <- left_join(x = climate_main_dataset, y = climate_technical_doc, by = "ind_id")
  
  
  ## Attach geography info from geography lookup--------
  climate_main_dataset <- climate_main_dataset |> 
    replace_old_geography_codes(col_name = "code") |>
    left_join(geography_lookup, by = "code")
  
  
  
  ## Apply suppression where required ---------
  climate_main_dataset <- climate_main_dataset |>
    apply_suppressions()
  
  
  
  ## convert some cols to numeric and round digits
  climate_main_dataset <- climate_main_dataset |>
    mutate(
      across(
        c("numerator", "measure", "upci", "lowci"),
        ~ round(as.numeric(.), digits = 1)
      )
    )
  
  
  # make dataset available in global environment for validation tests
  main_dataset_validation <<- climate_main_dataset 
  
  # remove columns not required within shiny app
  climate_main_dataset <- climate_main_dataset |>
    select(-c(supression, supress_less_than, type_id, file_name, label_inequality))
  
  
  # create a new column which contains the full geography path
  # most geographies have 2 parts to their path i.e. 'Health Board/NHS Ayrshire & Arran'
  # with the exception of IZs/HSC Localities where a parent area is also included i.e. 'HSC Locality/Edinburgh City/Edinburgh North-East'
  climate_main_dataset <- create_geography_path_column(climate_main_dataset)
  
  
  # make available in global environment for viewing what will be sent to shiny app
  climate_main_dataset <<- climate_main_dataset
  
  ## save final file to local repo
  write_parquet(climate_main_dataset, "shiny_app/data/climate_main_dataset", compression = "zstd")
  
  ## Optional: Create backup of from local repo -----
  ## Usually would only want to create a backup if you intend to update live tool
  ## This file will be stored in backups folder and could be used to roll back app to a particular date
  if (create_backup == TRUE) {
    
    file.copy(
      "shiny_app/data/climate_main_dataset", 
      paste0(backups, "climate_main_dataset_", Sys.Date()), 
      overwrite = TRUE
    )
  } 
  
  
  
  
}





