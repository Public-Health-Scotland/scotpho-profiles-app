######################################################################################################################################.
# Functions are called during running of ScotPHO profiles tool data preparation scripts.
# Functions saved in distinct script as they are called by more than one part of data prep script
######################################################################################################################################.



#########################################################.
## Combine data files ----
# Purpose: read in and combine individual indicator datasets to create one dataset (this is used for main indicator set and deprivation(SIMD) indicators)
# note: will stop process and throw an error if there is a dataset that has missing columns


combine_files <- function(file_list) {
  
  # Determine the file type in list of files passed to function
  file_type <- if (grepl("\\.csv$", file_list[1])) 'csv' else 'rds'
  
  
  # Define expected columns 
  expected_cols <- if (file_type == 'csv') {
    
    # opt data mandatory columns 
    c("code", "ind_id", "year", "numerator", "rate", 
      "upci", "lowci", "def_period", "trend_axis")
  } else {
    
    # inequalities data mandatory columns 
    c("year",  "code",  "quintile",  "quint_type", 
      "numerator", "denominator", "rate", "lowci",
      "upci", "sii" , "lowci_sii" , "upci_sii" , "rii", 
      "lowci_rii", "upci_rii", "rii_int", "lowci_rii_int", 
      "upci_rii_int", "par", "abs_range", "rel_range", "ind_id"   
    )
  }
  
  check_columns <- function(data, expected_cols, file_name) {
    
    missing_cols <- setdiff(expected_cols, 
                            tolower(names(data)))
    
    if (length(missing_cols) > 0) {
      stop("Missing columns in file: ", file_name, ": ", 
           paste(missing_cols, collapse = ", "), "\n")
    }
  }
  
  
  combined_data <- rbindlist(lapply(file_list, function(x) {
    
    # read in datafile
    data <- if (file_type == 'rds') readRDS(x) else fread(x)
    
    # check columns 
    check_columns(data, expected_cols, x)
    
    # add column that includes filename
    data$file_name <- basename(x)
    
    # make numerator column class numeric
    data$numerator <- as.numeric(data$numerator)
    
    # make ind id column class numeric 
    data$ind_id <- as.numeric(data$ind_id)
    
    #rename column
    colnames(data)[colnames(data) == 'rate'] <- 'measure'
    
    # clean column names 
    clean_names(data)
    
  }), fill = TRUE)
  
  return(combined_data)
}




#########################################################.
## Re-coding geography codes function ----
# Purpose: replace any old/inactive geography codes with their new one
# Note: sometimes geography codes are made 'inactive' and replaced with a different one
# any changes are usually published here: https://www.opendata.nhs.scot/dataset/geography-codes-and-labels
# can be linked with boundary changes where postcodes/IZ change organistion affiliation and new standard geography codes are released.

replace_old_geography_codes <- function(data, col_name) {
  data |> 
    mutate(!!col_name := recode(!!sym(col_name), 
                                # Council area code changes
                                "S12000015" = 'S12000047', # Fife
                                "S12000024" = 'S12000048', # Perth and Kinross
                                "S12000046" = 'S12000049', # Glasgow City
                                "S12000044" = 'S12000050', # North Lanarkshire
                                # Health board code changes
                                "S08000018" = 'S08000029', # NHS Fife
                                "S08000027" = 'S08000030', # NHS Tayside
                                "S08000021" ='S08000031', # NHS Greater Glasgow & Clyde
                                "S08000023" = 'S08000032', # NHS Lanarkshire
                                # HSCP code changes
                                "S37000014" ='S37000032', # Fife
                                "S37000023" ='S37000033', # Perth and Kinross
                                "S37000015" ='S37000034', # Glasgow City
                                "S37000021" ='S37000035')) # North Lanarkshire))
}




#########################################################.
## Suppression function ------
# Purpose: applies statistical disclosure control (SDC) to any indicators where it is required (see technical document)
# Note: only some indicators require SDC 
# and they each have different thresholds for what needs suppressed (i.e. figures less than 5, less then 10 etc.)



apply_suppressions <- function(dataset) {
  
  if ("Y" %in% unique(dataset$supression)) { #don't run this chunk unless supression==Y, because otherwise cases without supression==Y will crash this function
    
  dataset <- dataset |>
    # suppress numerator if required (for crude rates, %'s and standardised rates)
    mutate(numerator = case_when(supression == "Y" & 
                                   type_id %in% c('cr', '%', 'sr') 
                                 & numerator < supress_less_than 
                                 ~ NA_real_, TRUE ~ numerator),
           # additionally, suppress measure, upper ci and lower ci if it's a crude rate or % (to avoid backwards calculating)     
           across(.cols = c('measure', 'upci', 'lowci'),
                  ~ case_when(supression == "Y" & 
                                type_id %in% c('cr', '%') & 
                                numerator < supress_less_than ~ NA_real_, TRUE ~ .x))
    )
  }
  
  return(dataset)
  
}


#########################################################.
## Gap year function ----
# Purpose: Generates 'NA' data for indicators where there are years missing from their data file
# e.g. if a data collection was paused due to the pandemic, or that years data is deemed to be of poor data quality
# It ensures that when users are looking at trends in the profiles tool, the missing years still appear on the x-axis
# and instead of dropping to 0, a gap will appear in the trend chart to signify missing data

create_gap_year <- function(dataset,
                            indicator_id, # ind_id
                            gap_year, # the missing year
                            base_year, # year to take a copy of to create data for missing year
                            gap_trend_axis) { # axis label for missing year
  
  # take a copy of a years data for that indicator
  base_year_data <- dataset |> 
    filter(ind_id == indicator_id & year == base_year)
  
  if(!(gap_year %in% unique(base_year_data$year))){
    
    # generate data for the missing year
    gap_year_data <- base_year_data |>
      mutate(year = gap_year, # overwrite the year column with missing year
             across(c("def_period", "trend_axis"), ~ gsub(unique(trend_axis),gap_trend_axis,.)), # overwrite the axis label with missing years axis label
             across(c("numerator","measure","lowci","upci"), ~ NA)) # populate numeric columns with NA
    
    return(gap_year_data) 
    
  }  else {
    
    warning(paste0("ABORT!!: ", gap_year," is already contained in the data for indicator ",indicator_id))
  }
  
}


#########################################################.
## Create geography path column function ------
# Purpose: creates a column in the dataset containing full details of a geography, including areatype, parent area (if IZ/HSCL selected) and areaname
# E.g: 'Intermediate zone/Aberdeen City/Aberdeen North'
# This column is used to filter data in the data table tab of the app, where users can select multiple geographies, which sometimes share the same name
# hence why entire details of the geography are required to ensure filtering on the correct selection
create_geography_path_column <- function(dataset) {
  dataset <- dataset %>%
    mutate(
      # create column called 'geo_path' by 
      # a. pasting areatype with parent area if IZ/HSC Locality selected (i.e. Intermediate zone/Aberdeen City)
      # otherwise paste areatype with areaname (i.e. Health board/NHS Lothian)
      geo_path = paste(
        areatype,
        case_when(
          areatype %in% c("Intermediate zone", "HSC locality") ~ parent_area,
          TRUE ~ areaname
        ),
        # b. further pasting areaname on if IZ /HSC locality was selected, otherwise paste NA (as areaname would already have been pasted in step above)
        case_when(
          areatype %in% c("Intermediate zone", "HSC locality") ~ areaname,
          TRUE ~ NA_character_
        ),
        # c. separating pasted values with a "/" symbol
        sep = "/"
      ),
      geo_path = sub("/NA$", "", geo_path)
    )
  
  return(dataset)
}



#########################################################.
## Create geography nodes function ------
# Purpose: creates geography lists to be used in the hierarchical geography filter for the data table tab of the profiles tool
# this function is lifted from the documentation for the 'jsTreeR' package (which is the package used to create the geography filter in the data table tab)
# see examples here: https://www.rdocumentation.org/packages/jsTreeR/versions/1.1.0/topics/jstree-shiny 

create_geography_nodes <- function(leaves){
  dfs <- lapply(strsplit(leaves, "/"), function(s){
    item <-
      Reduce(function(a,b) paste0(a,"/",b), s[-1], s[1], accumulate = TRUE)
    data.frame(
      item = item,
      parent = c("root", item[-length(item)]),
      stringsAsFactors = FALSE
    )
  })
  
  #dat <- dfs[[1]]
  # for(i in 2:length(dfs)){
  #   dat <- merge(dat, dfs[[i]], all = TRUE)
  # }
  
  # amending function using 2 lines below to use rbind instead of merge 
  # This allows parent nodes to be ordered based on how data is arranged before being passed to this function
  # instead of alphabetically
  dat <- do.call(rbind, dfs)
  dat <- dat[!duplicated(dat), ]
  
  f <- function(parent){
    i <- match(parent, dat$item)
    item <- dat$item[i]
    children <- dat$item[dat$parent==item]
    label <- tail(strsplit(item, "/")[[1]], 1)
    if(length(children)){
      list(
        text = label,
        children = lapply(children, f),
        icon =FALSE,
        state = list(selected = FALSE, opened = FALSE )
      )
    }else{
      list(text = label, type = "child",icon = FALSE,
           state = list(selected = FALSE,opened = FALSE ))
    }
  }
  lapply(dat$item[dat$parent == "root"], f)
}






#END