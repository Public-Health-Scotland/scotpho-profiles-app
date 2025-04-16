## FUNCTION: prepare_techdoc.R (technical document) ----


## PARAMETERS
# load_test_data : (default is FALSE) option to set to TRUE which reads indicators where the column called 'active' in techdoc is either (A) active or (T) test 
# create_backup :  (default is FALSE ) option to set to TRUE when creating a distinct backup version desired e.g. when planning to deploy the app

update_techdoc <- function(load_test_indicators=FALSE, create_backup=FALSE) {
  
  # read in technical document source of all meta data about individual indicators, located in network directory
  technical_doc_raw <- read.xlsx(
    xlsxFile = paste0(lookups, "Technical_Document.xlsx"), 
    sheet = "Raw", 
    sep = " ") |>
    clean_names()
  
  
  # Saving a backup of techdoc (maybe be useful if live version is ever corrupted or we need to roll back app)
  # Save back needs to occur before column selections/data manipulations applied. 
  if (create_backup==TRUE){
    ## Optional - generate a techdoc backup - suggested to run only when deploying live shiny app otherwise this line can be skipped
    write_parquet(technical_doc_raw, paste0(backups, "techdoc-", Sys.Date()), compression = "zstd") # version for backups folder
  } 
  
  
  # filter the technical document based on parameter 'load_test_indicators' either include or exclude test indicator datasets
  # technical document coontains a column called "active" which can be set as 'A' active, 'N' not active - no data, 'T' test and 'AR' archived - historic data
    if (load_test_indicators==TRUE){
    technical_doc_raw <- technical_doc_raw |>
      filter(active %in% c("A","T", "AR")) # filter for active (A), test (T) and archived (AR) indicators
  } else { #default parameter is false so only active indicators read in
    technical_doc_raw <- technical_doc_raw |>
      filter(active %in% c("A", "AR")) # filter for active and archived indicators only
  }
  
    ## Clean date columns -----
  technical_doc <- technical_doc_raw |>
    mutate( #create columns used in the online tool metadata page informing users of last update dates
      last_updated_temp = suppressWarnings(convertToDate(last_updated)), 
      days_since_update = difftime(Sys.Date(), last_updated_temp)) |>
    mutate(
      across(
        .cols = c("last_updated", "next_update", "source_next_update"),
        .fns = ~ suppressWarnings(
          case_when(!is.na(as.numeric(.)) ~ strftime(convertToDate(.), "%b-%Y"), TRUE ~ .)))) |>
    select(-c(last_updated_temp, data_request_needed, if_so_who, r_script_name)) #remove unnecessary columns
  
  
  # some columns contain links and we format them like an rmarkdown link in the techdoc e.g. [scotpho website](https://www.scotpho.org.uk/)
  # for the shiny app to recognise it as a link the [] and () brackets can't have space between them - this code removes any space.
  technical_doc <- technical_doc |>
    mutate(across(c("scotpho_web_link", "related_publications", "supporting_information"), ~ gsub("\\]\\ \\(", "](", .)))
  
  
  
  ## Save file -----
  write_parquet(technical_doc, "shiny_app/data/techdoc", compression = "zstd") # version for local shiny app
  
  
  ## DO WE USE PROFILE LOOKUP ANYWHERE? -COULD IT SAVE processing in the shiny app?
  # profile_lookup <<- technical_doc_raw |>
  #   select(contains("profile_domain")) |> #select only columns containing 'profile'
  #   pivot_longer(cols = everything(), values_to = "value") |>
  #   filter(!is.na(value))|> #filterout any NA
  #   transmute(profile = substr(value, 1, 3),
  #             domain = substr(value, 5, nchar(value))) |>
  #   distinct()
  
  #saveRDS(profile_lookup, "shiny_app/data/profile_lookup.rds") # commented out as a test to see if we really need this file
  
  
  ## Select columns which are required by main indicator dataset -----
  #' <<- makes dataframe available outside of function
  technical_doc <<- technical_doc |>
    select(
      ind_id, indicator_name, type_id, interpret, supression,
      supress_less_than, type_definition, 
      label_inequality, profile_domain
    )
  
  
}