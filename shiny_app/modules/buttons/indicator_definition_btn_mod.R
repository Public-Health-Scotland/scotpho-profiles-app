# modal dialog box opens and displays definition of selected indicator

 
###UI

indicator_definition_btn_ui <- function(id) {
  ns <- NS(id)
  tagList(
      actionButton(ns("indicator_definition"), label = "Definition",icon= icon('info'))
    )

}


####SERVER

indicator_definition_btn_server <- function(id, selected_indicator) {
  moduleServer(
    id,
    function(input, output, session) {

        observeEvent(input$indicator_definition, {
        showModal(
          modalDialog(
            title = selected_indicator
          )
        )
      })

      
     })


} #closer server

