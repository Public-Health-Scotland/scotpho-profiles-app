
#' `combine_files` reads and combines multiple indicator data files to create one dataset.
#'
#' @param folder folder to read files from 
#' @param file_suffix pattern in filename of files to read in, including file extension.
#' @param dataset one of `main`, `popgroup` or `deprivation` (for checking correct columns exist before combining)
#'
#' @returns dataframe
#'
#' @examples
# data <- combine_indicator_files(
#   folder = "/PHI_conf/ScotPHO/Profiles/Data/Shiny Data",
#   file_suffix = "_ineq.rds",
#   dataset = "deprivation"
#   )


combine_indicator_files <- function(folder,
                                    file_suffix, 
                                    dataset = c("main", "popgroup", "deprivation")
){
  
  # check dataset argument is valid
  dataset <- rlang::arg_match(dataset)
  
  
  # check folder exists
  if (!dir.exists(folder)) {
    cli_abort(c("x" = "{.val {folder}} does not exist"))
  }
  
  
  # valid file extensions
  valid_ext <- c("csv", "rds", "parquet")
  
  
  # check file extension is included in file suffix argument
  # required as some indicator files are saved in multiple formats in same folder
  # so wan't want to read in and combine different formats of identical indicator data!
  if (!grepl(pattern = paste(valid_ext, collapse = "|"), x = file_suffix)){
    cli_abort(
      c(
        "x" = "Valid file extension not supplied",
        "i" = "Should be included within {.val file_suffix} argument and be one of {.val {valid_ext}}, e.g. {.val {paste0(file_suffix, '.csv')}}"
      )
    )
  }
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Search for files to read in -----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # get list of filepaths from folder containing file suffix
  filepaths_list <- list.files(
    path = folder, 
    pattern = file_suffix, 
    full.names = TRUE
  )
  
  
  # set filename as the name of each element in list
  names(filepaths_list) <- basename(filepaths_list) 
  
  
  # check files ending in file_suffix exist in folder
  if (length(filepaths_list) == 0L) {
    cli_abort(c("x" = "No files ending in {.val {file_suffix}} found in {.val {folder}}"))
  }
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read indicator files into a list of dataframes -----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # go through each filepath and perform the following steps:
  data_list <- purrr::imap(filepaths_list, function(filepath, filename) {
    
    
    ## Step 1: read in file ----
    # switch function to use depending on the file extension
    data <- switch(
      EXPR = tools::file_ext(filepath),
      "rds" = readRDS(filepath),
      "csv" = data.table::fread(filepath),
      "parquet" = nanoparquet::read_parquet(filepath)
    )
    
    
    
    ## Step 2: check required columns exist -----
    
    # required cols (character)
    char_cols <- c(
      "code", "def_period", "trend_axis",
      if (dataset == "popgroup") 
        c("split_name", "split_value"),
      if (dataset == "deprivation") 
        c("quintile", "quint_type", "sex")
    )
    
    # required cols (numeric)
    num_cols <- c(
      "ind_id", "year", "numerator", "measure", "upci", "lowci",
      if (dataset == "deprivation")
        c("denominator", "sii", "lowci_sii", "upci_sii", "rii",
          "lowci_rii", "upci_rii", "rii_int", "lowci_rii_int",
          "upci_rii_int", "par", "abs_range", "rel_range"
          # cols in some deprivation indicator files not used in dashboard:
          # "total_pop", "proportion_pop", "most_rate", "least_rate",
          # "par_rr", "count", "overall_rate"
        )
    )
    
    
    
    # Temporary step:
    # add 'sex' col if dataset is deprivation (if doesn't already exists)
    # Required as sex field is new (2025) and some indicator data files may not
    # yet contain it depending on when they were last run
    if (dataset == "deprivation" & (!"sex" %in% colnames(data))) {
      data <- data |>
        mutate(sex = "Total")
      
    }
    
    
    
    # if measure column is instead called 'rate' then rename it
    # to do: amend column name produced in indicator production
    # so this step can eventually be removed?
    if ("rate" %in% colnames(data)){
      data <- data |>
        rename("measure" = "rate")
    }
    
    
    
    
    # return names of any missing cols
    missing_cols <- setdiff(
      x = c(char_cols, num_cols), # expected cols 
      y = colnames(data) # existing cols
    )
    
    
    # return error if any cols missing
    if (length(missing_cols) > 0L) {
      cli_abort(
        c(
          "x" = "{.val filename}} is missing column{?s}: {.val {missing_cols}}", 
          "i" = "Amend file and re-run."
        )
      )
    }
    
    
    
    ## Step 3: select and format columns ----
    
    data <- data |>
      # select cols
      select(all_of(c(char_cols, num_cols))) |>
      mutate(
        # convert some cols to class character
        across(all_of(char_cols), ~ as.character(.)),
        # convert some cols to class numeric (+ round decimal places) - suppressing warning about NAs
        across(all_of(num_cols), ~ round(suppressWarnings(as.numeric(.)), digits = 1)), 
        # add filename and foldername cols 
        file_name = filename,
        folder_name = dirname(filepath)
      )
    
    
    
    ## Step 4: check for duplicate ind_ids ----
    
    
    # unique ind_ids in file
    unique_ind_ids <- unique(data$ind_id)
    
    
    # if there is more than 1 return error message
    if (length(unique_ind_ids) > 1) {
      cli_abort(
        c(
          "x" = "{.val {filename}} contains more than 1 ind_id: {.val {unique_ind_ids}}", 
          "i" = "Amend file and re-run."
        )
      )
    }
    
    
    
    ## Step 5: return clean indicator data ----
    return(data)
    
  })
  
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Checks no two files contain same ind_id ------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # get ind_id and filename from each df in data_list
  ind_ids <- purrr::map_dfr(data_list, ~ data.frame(ind_id = unique(.x$ind_id), filename = unique(.x$file_name)))
  
  
  # check if any 2 dfs contain the same ind_id before combining the list of dataframes into 1 large dataframe
  duplicates <- ind_ids |>
    group_by(ind_id) |>
    filter(n() > 1) |>
    summarise(
      n_files = n(),
      files = paste(filename, collapse = ", "), 
      .groups = "drop"
    )
  
  
  # abort if duplicates found
  if (nrow(duplicates) > 0) {
    cli_abort(c(
      "The same ind_id was found in more than 1 file",
      "x" = sprintf("ind_id {.val %s} found in %s files: %s", duplicates$ind_id, duplicates$n_files, duplicates$files),
      "i" = "There should only be 1 file for each indicator"
    ))
    
  }
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Combine indicator files ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # combine list of indicator dataframes into one large df
  data_combined <- purrr::list_rbind(data_list)
  
  # print message when completed
  cli_alert_info(
    "Combined {.emph {length(filepaths_list)}} {.val {dataset}} files ending in {.val {file_suffix}} from {.val {folder}}"
  )
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return combined dataset -----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  return(data_combined)
  
}

