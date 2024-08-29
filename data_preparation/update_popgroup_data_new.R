## FUNCTION: update_popgroup_data.R ----

## Creates a single RDS data file within local shiny app data folder that contains latest versions of indicators which populate
## population group tab in shiny app (ie indicators split by variables such as age/sex/disability etc)

## Caution: a number of these indicators may not have complete information i.e. some are available at Scotland level but not NHS board or LA.
## Some indicators may cover different time periods for different geography levels e.g. annual figures at Scotland level but rolling year averages for sub national geographies
## The data preparation script should accept these variances but if issues arise consider if the data structure could be a problem.

## THIS VERSION ACCEPTS COMBINED POPGROUP AND SIMD FILES.
## INPUT FILE SUFFIX IS _shiny_popgrp_newsplitvals.csv
## The file has split_value and split_value2 columns. If SIMD + another split is included, the SIMD quintile (including total) is in split_value2.
## SIMD only data could be split_value = "Total" and split_value2 = SIMD quintile
## File written out is popgroup_dataset2

## PARAMETERS
# load_test_indicators : (default is FALSE) option to set to TRUE which reads indicators from the test shiny folder
# create_backup :  (default is FALSE ) option to set to TRUE when creating a distinct backup version desired e.g. when planning to deploy the app

update_popgroup_data <- function(load_test_indicators = FALSE, create_backup = FALSE) {

  ## Find all separate indicator data files in the shiny data folder -----
  popgroup_data_files <- list.files(
    path = shiny_files, 
    pattern = "*_shiny_popgrp_newsplitvals.csv", 
    full.names = TRUE
  )
  
  ## Combine into one dataset  -----
  popgroup_dataset <- combine_files(popgroup_data_files)
  
  
  ## If test indicators are to be included then list files & combine files from test shiny folder
  if (load_test_indicators == TRUE){
    
    ## Find new indicators data files in the test shiny data folder -----
  test_popgroup_data_files <- list.files(
      path = test_shiny_files, 
      pattern = "*_shiny_popgrp_newsplitvals.csv", 
      full.names = TRUE
    )
    
    
    ## Combine into one dataset  -----
    test_popgroup_dataset  <- combine_files(test_popgroup_data_files)
    
    ## Combine main dataset and test indicators
    popgroup_dataset<- bind_rows(popgroup_dataset, test_popgroup_dataset)
  }
  
  
  ## Attach metadata from the technical doc lookup ----
  popgroup_dataset <- left_join(x = popgroup_dataset, y = technical_doc, by = "ind_id")
  
  # ## Attach geography info from geography lookup--------
  # popgroup_dataset<- popgroup_dataset |> 
  #   left_join(geography_lookup, by = "code")

    ## Apply suppression where required ---------
  popgroup_dataset <- popgroup_dataset |>
    apply_suppressions()
  
  # ## convert some cols to numeric and round digits
  # popgroup_dataset <- popgroup_dataset |>
  #   mutate(
  #     across(
  #       c("numerator", "measure", "upci", "lowci"),
  #       ~ round(as.numeric(.), digits = 1)
  #     )
  #   )
  
  # attach geography info ----
  popgroup_dataset <- popgroup_dataset |>
    replace_old_geography_codes(col_name = "code") |>
    left_join(geography_lookup, by = "code")
  
  # formatting numbers ----
  popgroup_dataset <- popgroup_dataset |>
    mutate(split_value2 = recode(split_value2, 
                             "1" = "1 - most deprived", 
                             "5" = "5 - least deprived")) |>
    mutate_at(c("numerator", "measure", "lowci", "upci", "rii", "upci_rii",
                "lowci_rii", "sii", "lowci_sii", "upci_sii", "par", "abs_range",
                "rel_range", "rii_int", "lowci_rii_int", "upci_rii_int"),round, 1)
  
  ## create new fields ----
  popgroup_dataset <- popgroup_dataset |>
    group_by(ind_id, year, quint_type, code) |>
    # label if par, sii or rii  positive or negative (helps with health inequality dynamic summary text)
    mutate(across(c(sii, rii, par),
                  ~ case_when(. > 0 ~ "positive", . < 0 ~ "negative",  . == 0 ~ "zero"),
                  .names = "{.col}_gradient")) |>
    mutate( # added logic for combos with NA values 
      qmax = ifelse(sum(is.na(measure))==5, as.character(NA), split_value2[which.max(measure)]), # which quintile contains highest rate/value # NEEDS GENERALISING, IN CASE OTHER DATA PUT IN SPLIT_VALUE2
      qmin = ifelse(sum(is.na(measure))==5, as.character(NA), split_value2[which.min(measure)]) # which quintile contains lowest rate/value
    ) |>
    ungroup() |>
    rename(indicator = indicator_name)
  
  #rename and select columns required for shiny file
  popgroup_dataset <- popgroup_dataset |>
 # rename(indicator = indicator_name) |>
    select(-c(supression, supress_less_than, type_id, file_name, label_inequality))
  
  # create a new column which contains the full geography path
  # most geographies have 2 parts to their path i.e. 'Health Board/NHS Ayrshire & Arran'
  # with the exception of IZs/HSC Localities where a parent area is also included i.e. 'HSC Locality/Edinburgh City/Edinburgh North-East'
  popgroup_dataset <- create_geography_path_column(popgroup_dataset)
  
  
  # make available in global environment for viewing what will be sent to shiny app
  popgroup_dataset <<- popgroup_dataset
  
  ## save final file to local repo
  write_parquet(popgroup_dataset, "shiny_app/data/popgroup_dataset2")
  
  ## Optional: Create backup of from local repo -----
  ## Usually would only want to create a backup if you intend to update live tool
  ## This file will be stored in backups folder and could be used to roll back app to a particular date
  if (create_backup == TRUE) {
    
    file.copy(
      "shiny_app/data/popgroup_dataset2", 
      paste0(backups, "popgroup_dataset2", Sys.Date()), 
      overwrite = TRUE
    )
  } 
  
  
  
    
} #close function

#END.