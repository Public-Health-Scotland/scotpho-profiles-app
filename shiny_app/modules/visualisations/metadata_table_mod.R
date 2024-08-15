#####################################
# metadata_table_mod.R
# This module creates the table displaying metadata for a selected indicator
# This table is nested inside other larger layout modules and is displayed when the user selects the 'metadata' tab on a multi-tab card
# and is found across most sub-tabs that display data for a single indicator (i.e. trends, rank, pop groups)

# id = unique id 
metadata_table_mod_UI <- function(id) {
  ns <- NS(id)
  tagList(
    reactableOutput(ns("metadata_table"))
  )
}


# id = unique id matching that assigned when using ui function
# techdoc = the technical document containing indicator metadata
# selected_indicator = the reactive value storing the selected indicator
metadata_table_mod_Server <- function(id, selected_indicator, technical_doc = techdoc) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      # filter the technical document by the selected indicator and format the metadata
      metadata <- reactive({
        req(selected_indicator())
        
        technical_doc |>
          filter(indicator_name == selected_indicator()) |>
          select(indicator_definition,data_source, notes_caveats, interpretation, numerator, denominator, disclosure_control) |>
          pivot_longer(cols = everything(), names_to = "Item", values_to = "Description") |>
          mutate(Item = gsub("_", " ", Item))
      })
      
      
      
      # display the formatted metadata in a table
      output$metadata_table <- renderReactable({
        reactable(data = metadata(),
                  defaultExpanded = TRUE,
                  defaultPageSize = nrow(metadata()),
                  columns = list(
                    Description = colDef(minWidth = 200)))
      })
      
      
    }
  )
}


####################################
# EXAMPLE USAGE ----
# before uncommenting and running the below you should run the main shiny app first so that techdoc 
# containing the indicator metadata saves in your global environment
# or alternatively read the techdoc in first
####################################


# library(shiny)
# 
# ui <- fluidPage(
#   # indicator filter
#   selectizeInput(
#     inputId = "indicator_filter", 
#     label = "select indicator:", 
#     choices = c("Alcohol-related hospital admissions","Active travel to school")
#   ),
#   
#   # metdata table module 
#   metadata_table_mod_UI(id = "metadata")
# )
# 
# server <- function(input, output, session) {
#   
#   # call metadata table module, passing what was selected from the filter
#   metadata_table_mod_Server(id = "metadata", selected_indicator = reactive({input$indicator_filter}))
# }
# 
# shinyApp(ui, server)

