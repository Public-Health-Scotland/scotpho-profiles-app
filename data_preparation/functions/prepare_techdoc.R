#' `prepare_techdoc`reads in a technical document and formats it.
#'
#'@param filepath full filepath of techdoc to read in
#'@param test_indicators one of `TRUE` or `FALSE`. Whether to include indicators marked as 'T' in the active col of techdoc. (`FALSE` by default)
#'
#'@returns dataframe
#'
#'@Example:
# x <- prepare_techdoc(
#   filepath  = "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Technical_Document.xlsx",
#   test_indicators = TRUE
#   )


prepare_techdoc <- function(filepath, 
                            test_indicators = FALSE
                            ){
  
  # get folder and filename from full filepath
  folder <- dirname(filepath)
  filename <- basename(filepath)
  
  
  # check folder exists
  if (!dir.exists(folder)){
    cli_abort(c("x" = "folder {.val {folder}} does not exist"))
  }
  
  
  # check file exists
  if (!file.exists(filepath)){
    cli_abort(c("x" = "file {.val {filename}} not found"))
  }
  
  
  # check sheet named 'Raw' exists
  if (!"Raw" %in% openxlsx::getSheetNames(filepath)){
    cli_abort(
      c(
        "x" = "Sheet named {.val Raw} not found in excel workbook.",
        "i" = "Amend sheet name in techdoc and re-run."
      )
    )
  }
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read in techdoc file -----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  techdoc <- openxlsx::read.xlsx(xlsxFile = filepath, sheet = "Raw", sep = " ")
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Filter indicators ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # filter on A (active) and AR (archived) indicators
  # also filter on T (test) indicators if 'test_indicators' 
  # parameter set to 'TRUE'
  techdoc <- techdoc |>
    filter(active %in% c("A", "AR", if(test_indicators) "T"))
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Select columns -----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  techdoc <- techdoc |>
    # clean column names
    janitor::clean_names() |>
    
    # select cols
    select(
      
      # indicator id, name and definition
      ind_id, 
      indicator_name,
      indicator_definition,
      
      # profile(s) and domain(s) indicator belongs to
      profile_domain,
      
      # data source details (why 2 cols?)
      data_source,
      source_details,
      
      # methodology
      inclusion_rationale,
      diagnostic_code_position,
      numerator,
      denominator,
      type_definition,
      disclosure_control,
      rounding,
      age_group,
      sex,
      confidence_interval_method,
      available_geographies,
      interpret,
      interpretation,
      
      # time periods
      year_type,
      trends_from,
      aggregation,
      
      # update schedule
      update_frequency,
      last_updated,
      next_update,
      active,
      
      # notes/caveats and related links 
      notes_caveats,
      related_publications,
      supporting_information,
      scotpho_web_link,
      
      
      # only used for applying suppression to some indicators during data prep
      # e.g. supression thresholds (supress_less_than) not included in final
      # version of techdoc file used in dashboard
      supression,
      supress_less_than, 
      type_id
    )
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Format columns ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # numeric cols 
  num_cols <- c("ind_id", "supress_less_than")
  
  # date cols 
  date_cols <- c("last_updated", "next_update")
  
  # character cols 
  char_cols <- setdiff(colnames(techdoc), c(num_cols, date_cols))
  
  # cols containing links 
  links_cols <- c("notes_caveats", "scotpho_web_link", "related_publications", "supporting_information")
  
  
  techdoc <- techdoc |>
    mutate(
      # convert columns to correct class
      across(all_of(num_cols), ~ as.numeric(.)),
      across(all_of(char_cols), ~ as.character(.)),
      across(all_of(date_cols), ~ suppressWarnings(strftime(openxlsx::convertToDate(.), "%b-%Y"))),
      # remove any space in link cols between text to display and link e.g [scotpho website](https://www.scotpho.org.uk/)
      across(all_of(links_cols), ~ gsub("\\]\\ \\(", "](", .)) 
    )
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add techdoc filepath as column ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  techdoc <- techdoc |>
    mutate(techdoc_path = filepath)

  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check `profile_domain` column -----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # check profile_domain col and pull out indicators that don't follow required pattern
  # of 3-letter upper-case abbreviation with domain e.g. "HWB-Life expectancy;'
  # If not populated properly, they will not appear under any profile in the dashboard!
  
  unassigned_inds <- techdoc |>
    # exceptions: 3 indicators not currently assigned to a profile but may be in future
    filter(
      !indicator_name %in% c(
        "Dying in hospital",
        "Food insecurity",
        "Gender balance in organisations"
      )
    ) |>
    # filter indicators not matching required pattern
    # (excluding any archived indicators)
    filter(
      !grepl(pattern = "^([A-Z]{3}-([a-zA-Z*[[:blank:]]]*)*)", x = profile_domain) &
        active != "AR"
    ) |>
    pull(indicator_name)
  
  
  # print error if profile_domain col not populated or formatted properly
  if (length(unassigned_inds) > 0L){
    cli_abort(
      c(
        "x" = "indicator{?s} missing or contains invalid {.emph `profile_domain`} entry in techdoc: {.val {unassigned_inds}}",
        "i" = "Should be 3-letter capitalised profile abbreviation followed by domain name e.g. {.val HWB-Life expectancy;}"
      )
    )
    
  }
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return clean metadata ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  return(techdoc)
  
  
}