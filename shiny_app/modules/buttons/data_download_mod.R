#' download_data_btns module
#'
#' @description 3 download buttons that when clicked on, download a reactive dataset
#' 
#' This function creates the UI for the Shiny module. It is a drop-down menu with 3 download buttons to 
#' download data in different formats - csv, rds and .json 
#'
#' @param id A unique identifier for the module.
#' @param size Size of button. Optional argument set to small by default. Can also be "lg" or "xs"

download_data_btns_ui <- function(id, size = "sm") {
  ns <- NS(id)
  shinyWidgets::dropdownButton(
    label = "Download data", icon = icon("download"), circle = FALSE, status = 'download', size = size,
    shiny::downloadLink(ns("downloadCSV"), label = "as CSV"),
    shiny::downloadLink(ns("downloadRDS"), label = "as RDS"),
    shiny::downloadLink(ns("downloadJSON"), label = "as JSON")
  )
}


#' download_data_btns UI function
#'
#' This function creates the Server function for the module. It triggers the data to download when the buttons are clicked
#'
#' @param id A unique identifier for the module.
#' @param input, output, session Standard Shiny server arguments.
#' @param data The name of the reactive dataframe to be downloaded
#' @param file_name The filename of the data download
#' @param selected_columns a vector of column names to include in the data download. Optional argument set to NULL to include all by default.
#'
download_data_btns_server <- function(id, data, selected_columns = NULL, file_name) {
  moduleServer(id, function(input, output, session) {
    
    dataset <- reactive({
      
      # Convert data.table to data.frame
      if ("data.table" %in% class(data())) {
        data <- as.data.frame(data())
      } else {
        data <- data()
      }
      
      
      # Select (and rename columns) if selected_columns arg is in use
      # otherwise download entire df
      if(!is.null(selected_columns)){
        data <- data |>
          select(all_of(selected_columns))
      } else {
        data
      }
    })
    
    # download as csv
    output$downloadCSV <- downloadHandler(
      filename = paste0(file_name, "_", Sys.Date(), ".csv"),
      content = function(file) {
        write.csv(as.data.frame(dataset()), file, row.names = FALSE)
      }
    )
    
    # download as rds
    output$downloadRDS <- downloadHandler(
      filename = paste0(file_name, "_", Sys.Date(), ".rds"),
      content = function(file) {
        saveRDS(dataset(), file)
      }
    )
    
    # download as json
    output$downloadJSON <- downloadHandler(
      filename = paste0(file_name, "_", Sys.Date(), ".json"),
      content = function(file) {
        jsonlite::write_json(as.list(dataset()), file)
      }
    )
    
  })
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example ----
# run the functions above and un-comment the code below for example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# library(shiny)
# library(shinyWidgets)
# library(jsonlite)
# library(dplyr)
# 
# 
# ui <- fluidPage(
#   download_data_btns_ui(id = "example")
# )
# 
# server <- function(input, output, session) {
# download_data_btns_server(id = "example", data = reactive({mtcars}), file_name = "mtcars_download")
# }
# 
# shinyApp(ui, server)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How do add to app ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# If adding to main UI and server scripts, use the below and replace placeholders - 
# add to main UI script:
#download_data_btns_ui(id = "placeholder")

# add to main Server script:
# download_data_btns_server(id = "placeholder", data = placeholder, file_name = "placeholder")

# If nesting inside another module, use the below and replace placeholders - 

# add to modules UI function:
#download_data_btns_ui(id = ns("placeholder"))

# add to modules Server function:
# download_data_btns_server(id = "placeholder", data = placeholder, file_name = "placeholder")
