#' `files_snapshot` takes snapshot of files in a folder matching a particular pattern and saves as RDS file
#' It then compares that snapshot to the previous one taken, and returns information about what files have been added/removed/updated
#' This can be used each time the dashboard is deployed, to help keep track of what's been updated.
#'
#'@param folder folder to read files in from 
#'@param file_suffix pattern in files to read in
#'@param dataset one of `main`, `popgroup` or `deprivation` - used for filename when saving snapshot file
#'
#'@Example:
# files_snapshot(folder = "/PHI_conf/ScotPHO/Profiles/Data/Data to be checked",
# file_suffix = "_shiny.csv",
# dataset = "main")



file_snapshot <- function(folder, 
                          file_suffix, 
                          dataset = c("main", "popgroup", "deprivation")
){
  
  
  # take snapshot 
  snapshot <- utils::fileSnapshot(
    path = folder,
    pattern = file_suffix,
    recursive = TRUE,
    file.info = TRUE,
    full.names = TRUE,
    md5sum = TRUE,
    ignore.case = FALSE 
  )
  
  
  # path to snapshot sub-folder 
  snapshot_folder <- file.path(folder, "snapshots")
  
  
  # if snapshot sub-folder doesn't exist then create one
  if(!dir.exists(snapshot_folder)){
    dir.create(snapshot_folder)
  }
  
  
  # save snapshot file 
  filename <- paste0(dataset, "_snapshot_", format(Sys.time(), "%Y-%m-%d_%H.%M"), ".rds")
  saveRDS(snapshot, file.path(snapshot_folder, filename))
  
  
  current_time <- format(Sys.time(), "%d %B %Y %H:%M")
  cli::cli_alert_info("New shapshot taken at {.val {current_time}}")
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compare file changes -----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # get list of snapshot files
  snapshot_files <- list.files(
    path = file.path(folder, "snapshots"),
    pattern = dataset,
    full.names = TRUE
  )
  
  
  
  # find the two latest files
  files_info <- file.info(snapshot_files)
  latest_snapshots <- head(snapshot_files[order(files_info$ctime, decreasing = TRUE)], 2)
  
  
  
  # return warning if no previous snapshot to compare with 
  if(length(snapshot_files) < 2){
    
    cli::cli_alert_warning("No previous snapshot file available to compare")
    
  } else {
    
    
    # read in two latest files
    snap_curr <- readRDS(latest_snapshots[1])
    snap_prev <- readRDS(latest_snapshots[2])
    
    # compare snapshots 
    comparison <- utils::changedFiles(before = snap_prev, after = snap_curr)
    
    
    # filter on variables of interest (i.e. files that have been added/changed/deleted)
    comparison <- comparison[names(comparison) %in% c("added", "changed", "deleted")]
    
    
    
    # print info about what's been added/changed/deleted
    prev_snapshot_date <- format(file.info(latest_snapshots[2])$mtime, "%d %B %Y %H:%M")
    cli::cli_text("Changes detected since: {.val {prev_snapshot_date}}")
    
    
    # go through each of the 3 comparison variables
    # and return a message in the console for each of them:
    purrr::iwalk(comparison,  ~ {
      
      # no. of files detected that are different to previous snapshot 
      n_files <- length(.x) 
      
      cli::cli_text("{.val {n_files}} files {.val {.y}}")
      
      if(!is.null(.x)){
        cli::cli_ul(basename(.x))
      }
      
    })
  }
  
  
}

