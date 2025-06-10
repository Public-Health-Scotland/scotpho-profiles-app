# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# filter_metadata_mod.R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @description filters dataframe storing metadata (i.e. the techdoc) by indicator and returns results as a reactive dataframe


#' metadata_scroll_button_UI Function
#'
#' @param id unique id 
#' @param metadata name of dataframe storing metadata (set to techdoc by default)
#' @param r_indicator name of reactive object storing selected indicator 
#' 
#' 

filter_metadata_Server <- function(id, metadata = techdoc, r_indicator) {
  moduleServer(
    id,
    function(input, output, session) {
      return(
        reactive({
          metadata[metadata$indicator_name == r_indicator(), ]
        })
      )
    }
  )
}


# To be copied in the server
# indicator_metadata<- filter_metadata_Server(id = "placeholder", r_indicator = placeholder)

# ~~~~~~~~~~~~~~~~~~
# Example ----
# ~~~~~~~~~~~~~~~~~~

# library(shiny)
# 
# techdoc <- data.frame(
#   indicator_name = c("Active travel to work", "Active travel to school"),
#   age_group = c("Working age adults", "Primary and secondary school children")
# )
# 
# ui <- fluidPage(
#   selectizeInput("indicator_filter", label = NULL, choices = c("Active travel to school", "Active travel to work")),
#   tableOutput("results")
# )
# 
# server <- function(input, output, session) {
# 
#   selected_indicator <- reactive({input$indicator_filter})
#   indicator_metadata <- filter_metadata_Server(id = "metadata", r_indicator = selected_indicator)
#   output$results <- renderTable({indicator_metadata()})
# }
# 
# shinyApp(ui, server)
