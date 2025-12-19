#' `create_dataset` prepares the indicator datasets used in the shiny app
#'
#' @param folder folder where 'Shiny Data', 'Shiny Test Data' and 'Lookups' sub-folders are within  
#' @param techdoc_name Unique name given to the technical document where profile indicators metadata found
#' @param file_suffix pattern in filename of files to read in, including file extension.
#' @param dataset one of `main`, `popgroup` or `deprivation`
#' @param test_indicators whether to read in indicators from 'Shiny Test Data' folder. Set to `FALSE` by default.
#' @param profile_filter 3-letter profile abbreviation to filter indicators according to their profile abbrev. in the techdoc. Set to `NULL` by default
#' @param file_snapshot whether to take snapshot info about files in folder, and compare against prev snapshot. Set to `FALSE` by default 
#'
#' @returns dataframe
#'
#' @examples
# data <- create_dataset(
#   folder = "/PHI_conf/ScotPHO/Profiles/Data",
#   file_suffix = "_shiny.csv",
#   dataset = "main",
#   test_indicators = FALSE,
#   profile_filter = NULL
#   )


create_dataset <- function(folder,
                           techdoc_name,
                           file_suffix, 
                           dataset = c("main", "deprivation", "popgroup"),
                           profile_filter = NULL,
                           test_indicators = FALSE,
                           file_snapshot = FALSE) {
  
  
  # check folder exists
  if (!dir.exists(folder)){
    cli_abort("{.val {folder}} does not exist.")
  }
  
  
  
  # check shiny data folder exists
  shiny_folder <- file.path(folder, "Shiny Data")
  
  if (!dir.exists(shiny_folder)){
    cli_abort(
      c(
        "x" = "{.val {shiny_folder}} does not exist.",
        "i" = "All finalised data ready to go live should be in this sub-folder."
      )
    )
  }
  
  
  
  # check test shiny data folder exists (if applicable)
  test_shiny_folder <- file.path(folder, "Test Shiny Data")
  
  if(test_indicators){
    if (!dir.exists(test_shiny_folder)){
      cli_abort(
        c(
          "x" = "{.val {test_shiny_folder}} does not exist.",
          "i" = "All test data not ready to go live should be in this sub-folder."
        )
      )
    }
  }
  
  
  
  # check profile filter is 3-letter capitalised abbreviation (if applicable)
  if (!is.null(profile_filter)){
    if (!grepl(pattern = "[A-Z]{3}", x = profile_filter)){
      cli_abort(
        c(
          "x" = "Invalid {.var {profile_filter}}",
          "i" = "Should be a 3-letter abbreviation matching assignment in technical document e.g. {.val CWB}"
        )
      )
    }
  }
  
  
  
  # check geography lookup exists in shiny_app/data sub-folder of this project
  geo_lookup_path <- "shiny_app/data/profiles_geo_lookup.rds"
  
  
  if (!file.exists(geo_lookup_path)){
    cli_abort(
      c(
        "x" = "{.val profiles_geo_lookup.rds} not found in {.val shiny_app/data} folder",
        "i" = "Run {.val update_geography_lookups.R} script first to generate lookup."
      )
    )
  }
  
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Combine indicator files ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # combine all files in the shiny folder ending in file suffix
  data <- combine_indicator_files(
    dataset = dataset,
    folder = shiny_folder,
    file_suffix = file_suffix
  )
  
  
  # if applicable: combine data from test folder and add on data from shiny folder
  if (test_indicators){
    
    test_files <- list.files(path = test_shiny_folder, pattern = file_suffix)
    
    if (length(test_files) == 0L){
      
      cli_alert_warning("No test files found ending in {.val {file_suffix}}")
      
    } else {
      
      test_data <- combine_indicator_files(
        dataset = dataset,
        folder = test_shiny_folder,
        file_suffix = file_suffix
      )
      
      data <- rbind(data, test_data)
      
    }
  }
  
  
  # Read in some older data
  # note: most indicators will have their own separate data file. 
  # However, some indicators which haven't been updated in years sit in a file called 'All Data for Shiny.csv' 
  # from when there was a different process for creating indicator data. Once they have been prepared via the new process
  # (i.e. using the analysis functions from the indicator production repository), this step will no longer be required
  if (dataset == "main" & folder == "/PHI_conf/ScotPHO/Profiles/Data"){
    
    old_data <- data.table::fread(file.path(shiny_folder, "All Data for Shiny.csv")) |>
      filter(!(ind_id %in% unique(data$ind_id))) |>
      mutate(
        file_name = "All data for Shiny.csv", 
        folder_name = folder
      )
    
    
    data <- rbind(data, old_data)
    
    
  }
  
  
  
  # if the dataset is deprivation, add 2 additional columns and recode the quintile values
  if (dataset == "deprivation"){
    data <- data |>
      mutate(
        qmax = ifelse(sum(is.na(measure))==5, as.character(NA), quintile[which.max(measure)]), # which quintile contains highest rate/value
        qmin = ifelse(sum(is.na(measure))==5, as.character(NA), quintile[which.min(measure)]) # which quintile contains lowest rate/value
      ) |>
      dplyr::mutate(
        quintile = dplyr::recode(
          quintile,
          "1" = "1 - most deprived", 
          "5" = "5 - least deprived"
        )
      )
  }
  
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 2. Add metadata ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # get metadata 
  techdoc <- prepare_techdoc(
    filepath = file.path(folder, "Lookups", techdoc_name), 
    test_indicators = ifelse(test_indicators, TRUE, FALSE)
  )
  
  
  # select cols
  techdoc <- techdoc |>
    select(
      # joining col between indicator datasets and metadata
      ind_id,
      active,
      # 4 x columns added to indicator datasets
      indicator = indicator_name,
      profile_domain,
      interpret,
      type_definition,
      # these 3 cols only used for applying suppression
      # and will be removed from dataset after suppression applied
      type_id,
      supression,
      supress_less_than
    )
  
  
  
  # join data and metadata
  data <- left_join(data, techdoc, by = "ind_id")
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Filter by profile -----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # filter dataset by profile (only if applicable)
  # if profile_filter set to the default of 'NULL' then all indicators are included
  
  if (!is.null(profile_filter)){
    data <- data |>
      filter(grepl(pattern = profile_filter, x = profile_domain))
    
    if(nrow(data) == 0){
      cli_abort(
        c(
          "x" = "No rows returned after filtering by profile.",
          "i" = "Ensure {.val {profile_filter}} matches profile abbreviation assigned to indicators in 'profile_domain' column of techdoc"
        )
      )
    } else {
      cli_alert_info("Filtered on {.val {length(unique(data$ind_id))}} {.val {profile_filter}} indicators")
    }
  }
  
  
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 2. add geography cols -----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # replace any invalid geography codes
  data <- replace_old_geography_codes(data, col_name = "code")
  
  
  # read in geography lookup from 'shiny_app/data' project in this folder
  geo_lookup <- readRDS(geo_lookup_path)
  
  
  # ensure there's no other invalid/inactive geography codes
  invalid_code_files <- data |>
    filter(!code %in% geo_lookup$code) |>
    pull(file_name)
  
  
  # return error if invalid geography codes identified
  if (length(invalid_code_files) > 0L){
    cli_abort("Invalid geography codes found in: {.val {invalid_code_files}}")
  }
  
  
  # join with geography lookup
  data <- data |>
    left_join(geo_lookup, by = "code")
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3. apply suppression -----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  data <- data |>
    apply_suppressions() |>
    select(-c(supression, supress_less_than, type_id))
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check no missing files/no extra files ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # These checks can currently only be done on the main dataset as should always be a file for every indicator
  # To do: Add popgroup and deprivation cols to the techdoc that are filled with 'Y' or 'N'
  # so can identify what additional files we're expecting for each indicator
  
  # This step only done when test indicators are not included and when profile
  # filter not in use as most applicable when preparing data for deployment
  
  if(dataset == "main" & isFALSE(test_indicators) & is.null(profile_filter)){
    
    # get unique ind_ids and the filename/folder that data came from 
    x <- data |>
      select(ind_id, file_name, folder_name) |>
      unique()
    
    # get ind_ids and their status from techdoc (i.e. active/inactive/test/archived)
    y <- techdoc |>
      select(ind_id, indicator, active)
    
    
    # combine rows from both 
    join <- full_join(x, y, by = c("ind_id"))
    
    
    # indicators found in techdoc but not found in folder 
    missing_inds <- join |>
      filter(is.na(file_name)) |>
      pull(indicator)
    
    if (length(missing_inds) > 0L) {
      cli_alert_warning(
        "Active/Archived indicator{?s} missing from `Shiny Data` folder: {.val {missing_inds}}"
      )
    }
    
    
    # indicators found in folder but not in techdoc
    extra_files <- join |>
      filter(is.na(indicator)) |>
      pull(file_name)
    
    
    if (length(extra_files) > 0L) {
      cli_alert_warning(
        "inactive/test/unknown indicator data from file{?s} found in `Shiny Data` folder and added to dataset: {.val {extra_files}}"
      )
    }
    
    
  }
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove foldername and filename columns ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  data <- data |>
    select(-c(folder_name, file_name))
  
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4. file snapshot -----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if (file_snapshot){
    if (test_indicators){
      cli_abort(
        c(
          "x" = "Test indicators are being included",
          "i" = "Snapshots should only be taken when deploying the profiles tool. {.val test_indicators} argument should be {.val FALSE}"
        )
      )
    } else {
      cli_alert_info("Snapshot in progress...")
      
      file_snapshot(
        folder = shiny_folder,
        file_suffix = file_suffix,
        dataset = dataset
      )
      
    }
  }
  
  
  
  # Remove some indicators from IZ level to reduce risk of secondary disclosure
  # Data is generated to allow ScotPHO analysts to review numbers and release to internal staff on request
  # However not likely to be able to satisfy statistical disclosure signed off to include in main tool
  data <- data |>
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
      ) & substr(code, 1, 3) == "S02"))
  
  
  
  
  # return dataset
  return(data)
  
  
  
  
}



