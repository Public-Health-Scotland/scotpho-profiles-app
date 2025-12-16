#' `replace_old_geography_codes` replaces 2011 Health board/Council area codes with 2019 equivalents
#'
#' @param data the dataframe to replace codes in
#' @param col_name the column name containing geography codes

#' @returns dataframe
#'
#' @examples
# data <- replace_old_geography_codes(data = df,col_name = "code")




replace_old_geography_codes <- function(data, col_name) {
  
  data <- data |> 
    mutate(!!col_name := recode(!!rlang::sym(col_name), 
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
                                "S37000021" ='S37000035')) # North Lanarkshire
  
  
  return(data)
}






