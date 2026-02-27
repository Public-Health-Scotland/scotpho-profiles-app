#' Data table Module
#'
#' A Shiny module that creates a data table using **reactable** that is updated
#' in response to a reactive dataset
#'


#' Data Table mod UI
#'
#' Creates a UI container for the table
#'
#' @param id Character string.
#'
#' @return A `ReactableOutput` element
#'
#' @examples
#' # In UI:
#' data_table_mod_UI("table")
#'

data_table_mod_UI <- function(id) {
  ns <- NS(id)
  tagList(
  reactableOutput(ns("tbl"))
  )
}



#' Data table Module Server
#'
#' Server logic for rendering and updating a reactable data table.
#'

#' @param id Character string. The module's namespace ID (should match the UI function).
#'
#' @param r_data A **reactive** dataset to be displayed
#' @param cols A vector of columns to display. The rest will be hidden.
#'
#' @return None. This server function is called for its side-effects
#'   (it renders and updates a reactable in the UI).
#'
#' @examples
#' # In server:
#' data_table_mod_Server(
#'   id = "table",
#'   r_data = data,
#'   cols = c("year", "measure")
#' )
#' 
#' 
#' 

data_table_mod_Server <- function(id, r_data, cols) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      output$tbl <- renderReactable({
        
        # identify columns in dataset that have not been passed
        # to the cols argument - these are to be hidden from the table
        hide_cols <- setdiff(colnames(r_data()), cols)
        
        reactable(
          data = r_data(),
          defaultExpanded = TRUE,
          defaultPageSize = nrow(r_data()),
          columns = purrr::map(hide_cols, ~ reactable::colDef(show = FALSE)) |> set_names(hide_cols)
          )
      })
      
      
      
    }
  )
}

