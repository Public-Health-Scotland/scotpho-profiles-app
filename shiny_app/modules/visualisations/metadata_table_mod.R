# ~~~~~~~~~~~~~~~~~~~~~
# metadata_panel_mod.R
# ~~~~~~~~~~~~~~~~~~~~~

#' @description filters metadata by selected indicator and displays it in displays a table inside an accordion


#' metadata_panel_UI Function
#'
#' @param id unique id 
#' 
#' 
metadata_panel_UI <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(
      open = FALSE,
      id = ns("metadata_accordion"),
      accordion_panel(
        open = FALSE,
        value = "metadata_panel",
        title = "Indicator metadata and useful links",
        reactableOutput(ns("table"))
      )
    )
  )
}

#' metadata_panel_Server Function
#' 
#' @param id unique id 
#' @r_event name of object storing the event which should trigger the panel to open 
#' @r_metadata name of reactive dataframe storing filtered metadata 
#' @parent_session always set to session
#' 
# open accordion panel when event triggered and display table of results 
metadata_panel_Server <- function(id, r_event, r_metadata, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # open panel if event triggered (i.e. button clicked )
      observeEvent(r_event(), {
        bslib::accordion_panel_open(
          id = ns("metadata_accordion"),
          values = "metadata_panel",
          session = parent_session)
      }, ignoreNULL = TRUE)
      
      # select columns/format metadata 
      formatted_metadata <- reactive({
        r_metadata() |>
          select(
            Definition = indicator_definition, 
            Source = data_source, 
            Notes = notes_caveats, 
            Interpretation = interpretation, 
            Numerator = numerator, 
            Denominator = denominator, 
            Suppression = disclosure_control,
            `ScotPHO website link` = scotpho_web_link,
            `Related publications`= related_publications,
            `Supporting information` = supporting_information
          ) |>
          tidyr::pivot_longer(cols = everything(), names_to = "Item", values_to = "Description")
      })
      
      
      # display formatted metadata in table 
      output$table <- renderReactable({
        reactable(
          data = formatted_metadata(),
          defaultExpanded = TRUE,
          defaultPageSize = nrow(formatted_metadata()),
          columns = list(
            Item = colDef(maxWidth = 150),
            # apply markdown() function to the 'Description' column of the table to convert any text and links to html
            # e.g. "Information on the [scotpho website](https://www.scotpho.org.uk/)"
            # when passed to the markdown() function would create <p>Information on the <a href="https://www.scotpho.org.uk/">scotpho website</a></p>
            # then set html to TRUE to make sure it's rendered as html in the table - this will make the hyperlinks clickable
            Description = colDef(
              html = TRUE, 
              cell = function(values){
                shiny::markdown(values)
              }
            )
          )
        )
      })
      
      
    }
  )
}

## To be copied in the UI
# metadata_panel_UI(id = "placeholder")

## To be copied in the Server
# metadata_panel_Server(id = "placeholder", r_event = placeholder, r_metadata = placeholder, parent_session = session)



# # ~~~~~~~~~~~~~~~~~
# # example ----
# # ~~~~~~~~~~~~~~~~~
# 
# # example techdoc
# techdoc <- data.frame(
#   indicator_name = c("Alcohol-related hospital admissions"),
#   indicator_definition = c("General acute inpatient and day case stays with diagnosis of alcohol misuse in any position."),
#   data_source = c("Public Health Scotland (SMR01)"),
#   notes_caveats = c(" Rates have been standardised to the European Standard Population 2013."),
#   interpretation = c("Higher numbers are worse"),
#   numerator = c("Number of alcohol-related hospital admissions"),
#   denominator = c("Total population"),
#   disclosure_control = c("None"),
#   scotpho_web_link = c("[Alcohol section](https://www.scotpho.org.uk/risk-factors/alcohol/)"),
#   related_publications = c("[Alcohol related hospital statistics](https://publichealthscotland.scot/publications/alcohol-related-hospital-statistics/)"),
#   supporting_information = c("Public Health Scotland provide an overview of [key statistics on alcohol misuse, related harms and actions to reduce them] (https://publichealthscotland.scot/population-health/improving-scotlands-health/alcohol/overview-of-alcohol-related-harm/).")
#   )
# 
# 
# 
# ui <- fluidPage(
#   theme = bs_theme(version = 5), # BS version 5 required for bslib accordions 
#   actionLink("open_panel", "click here to open accordion panel"),
#   metadata_panel_UI("metadata_panel")
# )
# 
# server <- function(input, output, session) {
#   
#   
#   btn_status <- reactive({input$open_panel}) # to track if button clicked 
#   metadata <- reactive({techdoc}) # reactive metadata
#   metadata_panel_Server(id = "metadata_panel", r_event = btn_status, r_metadata = metadata, parent_session = session)
#   
# }
# 
# shinyApp(ui, server)
# 
# 

