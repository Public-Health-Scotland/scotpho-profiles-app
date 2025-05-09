# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# navigate_data_download_mod.R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#'@description an action link that when click on, navigates to the data download page

#' navigate_data_download_UI Function
#'
#' @param id unique id 
#' @param size size of the dropdown button 

# UI function 
navigate_data_download_UI <- function(id, size = "sm") {
  ns <- NS(id) #namespace
  shinyWidgets::dropdownButton(
    label = "More download options", icon =icon("download"), circle = FALSE, status = "download", size = size,
    p(
      "Data for multiple areas or indicators can be downloaded in the", #inline text
    shiny::actionLink( #action link UI
      inputId = ns("go_to_data_download"), #link ID
      label = "data download tab"), ".")
  )
}



#' navigate_data_download_Server Function
#'
#' @param id unique id 
#' @param target_id the id of the data download page 
#' @param parent_session the main server session - not the nested modules


# Server function
navigate_data_download_Server <- function(id, target_id, parent_session){
  shiny::moduleServer(id, function(input, output, session){
    shiny::observeEvent(input$go_to_data_download, {
      
      # navigate to the tab
      bslib::nav_select(id="nav",
                        selected = target_id,
                        session = parent_session)
      
    }) 
  }) #close module server
} #close server



# ~~~~~~~~~~~~~~~~~
# example ----
# ~~~~~~~~~~~~~~~~~

library(shiny) #for base shiny functions
library(bslib) #for layout e.g. page_navbar, nav_panel


ui <- page_navbar(id = "nav",
                  title = "Click to download data in bulk",
                  nav_panel(title = "tab 1",
                            value = "t1",
                            p("Tab number 1"),
                            navigate_data_download_UI(id = "tab1_to_tab2")),
                  nav_panel(title = "tab 2",
                            value = "t2",
                            p("Tab number 2"))
)

server <- function(input, output, session) {
  navigate_data_download_Server(id = "tab1_to_tab2", target_id="t2", parent_session = session)

}

shinyApp(ui, server)


