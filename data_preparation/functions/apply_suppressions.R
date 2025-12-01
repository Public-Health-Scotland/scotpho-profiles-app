#' `apply_suppression` applies suppression to indicators where required, using information originally sourced from the metadata about what indicators
#' # require suppression, and what the threshold for suppression is (i.e <5, <10 etc.)
#'
#'@param dataset name of dataframe to apply suppression to 
#'@Example:
# x <- apply_suppressions(dataset = df)


apply_suppressions <- function(data) {
  

  
  if(!all(c("supression", "supress_less_than") %in% colnames(data))){
    cli_abort("{.val {dataset}} does not contain columns `supression` and `supress_less_than`.")
  }
  
  
  # get indicators requiring supression
  suppress_inds <- unique(data$indicator[data$supression == "Y"])


  if (length(suppress_inds) == 0L) {
    
    cli_alert_info("No indicators identified requiring suppression.")
    
    return(data)
    
    } else {
    
    # suppress numerator if required (for crude rates, %'s and standardised rates)
    data_suppressed <- data |>
      mutate(
        numerator = case_when(
          supression == "Y" &
            type_id %in% c('cr', '%', 'sr') &
            numerator < supress_less_than ~ NA_real_,
          TRUE ~ numerator
        ),
        # additionally, suppress measure, upper ci and lower ci if it's a crude rate or % (to avoid backwards calculating)     
        across(
          .cols = c('measure', 'upci', 'lowci'),
          ~ case_when(
            supression == "Y" &
              type_id %in% c('cr', '%') &
              numerator < supress_less_than ~ NA_real_,
            TRUE ~ .x
          )
        )
      )

    # check difference with data before/after suppression applied and return number of rows changed
    suppressed_rows <- anti_join(data, data_suppressed, by = colnames(data))

    cli_alert_info("Suppression applied to {.val {nrow(suppressed_rows)}} rows across {.val {length(unique(suppressed_rows$indicator))}} indicator{?s}.")
    
    
    return(data_suppressed)
    
  }

  
}
