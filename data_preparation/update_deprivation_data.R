## FUNCTION: update_deprivation_data.R ----



## PARAMETERS
# load_test_indicators : (default is FALSE) option to set to TRUE which reads indicators from the test shiny folder
# create_backup :  (default is FALSE ) option to set to TRUE when creating a distinct backup version desired e.g. when planning to deploy the app


update_deprivation_data <- function(load_test_indicators = FALSE, create_backup = FALSE) {
  
  ## generate list deprivation data files ----
  deprivation_files <- list.files(
    path = shiny_files, 
    pattern = "*_ineq.rds", 
    full.names = TRUE
  )
  
  ## combine into one deprivation dataset
  deprivation_dataset <- combine_files(deprivation_files) #call to one of the generic functions that combines files from list
  
  ## If test indicators are to be included then list files & combine files from test shiny folder
  if (load_test_indicators == TRUE){
    ## Find new indicators data files in the test shiny data folder -----
    test_deprivation_files <- list.files(
      path = test_shiny_files, 
      pattern = "*_ineq.rds", # was .csv previously: changed to match the non-test dataset. Is this OK?
      full.names = TRUE
    )
    
    ## Combine into one dataset  -----
    test_deprivation_dataset <- combine_files(test_deprivation_files)
    
    ## Combine main dataset and test indicators
    deprivation_dataset <- bind_rows(deprivation_dataset, test_deprivation_dataset)
  } #close test indicator function
  
  # replace NAs in sex column with Total
  # these NAs are introduced when combining files where sex column not present and so auto-filled with NAs
  deprivation_dataset$sex[is.na(deprivation_dataset$sex)] <- "Total"
  
  # for now skip loading andy pulford inequality indicators
  # attach technical document info ----
  deprivation_dataset <- left_join(x = deprivation_dataset, y = technical_doc, by = "ind_id")
  
  # attach geography info ----
  deprivation_dataset <- deprivation_dataset |>
    replace_old_geography_codes(col_name = "code") |>
    left_join(geography_lookup, by = "code")
  
  # formatting numbers ----
  deprivation_dataset <- deprivation_dataset |>
    mutate(quintile = recode(quintile,"1" = "1 - most deprived","5" = "5 - least deprived")) |>
    mutate_at(c("numerator", "measure", "lowci", "upci", "rii", "upci_rii",
                "lowci_rii", "sii", "lowci_sii", "upci_sii", "par", "abs_range",
                "rel_range", "rii_int", "lowci_rii_int", "upci_rii_int"),round, 1)
  
  ## apply suppression function ----
  deprivation_dataset <- deprivation_dataset |>
    apply_suppressions()
  
  ## create new fields ----
  deprivation_dataset <- deprivation_dataset |>
    group_by(ind_id, year, quint_type, code) |>
    # label if par, sii or rii  positive or negative (helps with health inequality dynamic summary text)
    mutate(across(c(sii, rii, par),
             ~ case_when(. > 0 ~ "positive", . < 0 ~ "negative",  . == 0 ~ "zero"),
             .names = "{.col}_gradient")) |>
    mutate( # added logic for combos with NA values 
      qmax = ifelse(sum(is.na(measure))==5, as.character(NA), quintile[which.max(measure)]), # which quintile contains highest rate/value
      qmin = ifelse(sum(is.na(measure))==5, as.character(NA), quintile[which.min(measure)]) # which quintile contains lowest rate/value
    ) |>
    ungroup() |>
    rename(indicator = indicator_name)
  
  
  # create a new column which contains the full geography path
  # most geographies have 2 parts to their path i.e. 'Health Board/NHS Ayrshire & Arran'
  deprivation_dataset <- create_geography_path_column(deprivation_dataset)
  
  # make dataset available in global environment for validation tests
   deprivation_dataset_validation <<- deprivation_dataset 

  # Make dataset visible outside of function
  deprivation_dataset <<- deprivation_dataset %>%
    select(-c("file_name", "parent_area", # deselect columns not required in shiny app
              #"areaname_full" #areaname_full required to make validation test work
              "supression", "supress_less_than")) # additional columns need to be removed to make the validation test work for ineq data
  
 
  #write parquet file to shiny app data folder
  write_parquet(deprivation_dataset, "shiny_app/data/deprivation_dataset", compression = "zstd")
  
  
  ## Optional: Create backup of from local repo -----
  ## Usually would only want to create a backup if you intend to update live tool
  ## This file will be stored in backups folder and could be used to roll back app to a particular date
  
  if (create_backup == TRUE) {
    
    file.copy(
      "shiny_app/data/deprivation_dataset", 
      paste0(backups, "deprivation_dataset_", Sys.Date()), 
      overwrite = TRUE
    )
  } 

}
