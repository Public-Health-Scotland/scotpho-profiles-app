# Indicator_definition_btn_module ----

# Shiny action button linked to a modal dialog box that on click opens to display full indicator definition (sourced from technical document) of the selected indicator

 
###UI

indicator_definition_btn_ui <- function(id, class= NULL) {
  ns <- NS(id)
  tagList(
      shiny::actionButton(ns("indicator_definition"),
                          label = "Definition",
                          icon= icon('info'),
                          class=class))
  }

####SERVER

indicator_definition_btn_server <- function(id, selected_indicator) {
  moduleServer(
    id,
    function(input, output, session) {

      #filter techdoc by selected indicator
      filtered_indicator <- reactive({
        techdoc |>
          filter(indicator_name == selected_indicator())})

      
      #content of ModalDialog box
        observeEvent(input$indicator_definition, {
        showModal(
          modalDialog(
           title = "Definition:",
            #insert indicator name (formatted in bold & underlined) followed by indicator definition
            HTML(paste(sprintf("<b><u>%s</b></u> <br> %s ", selected_indicator(),
                              filtered_indicator()$indicator_definition, collapse = "<br><br>"))),
            easyClose = TRUE,fade=FALSE,footer = modalButton("Close (Esc)") #easyclose allows you to use 'Esc' button to close modal 
          )
        )
      })


         }) #close module server
} #closer server


