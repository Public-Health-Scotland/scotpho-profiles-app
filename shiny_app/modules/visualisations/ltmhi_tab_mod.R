ltmhi_UI <- function(id, ltmhi_dataset) {
  ns <- NS(id)
  tagList(

    # header and description
    div(
      class = "p-2 mb-2",
      h1("Long-term Monitoring of Health Inequalities in Scotland"),
      p("Description")
    ),

    # horizonal line
    hr(),


    # sidebar navigaton menu with sub-sections
    navset_pill_list(
      widths = c(3,9),


      # Key points section
      nav_panel(
        title = "Key points",
        class = "container",
        p("placeholder")
      ),


      # Summary table section
      nav_panel(
        title = "Summary table",
        class = "container",
        card(reactableOutput(ns("summary_tbl")))
      ),


      # Explore indicators section
      nav_panel(
        title = "Explore indicators",
        class = "container",
        # indicator filter 
        selectizeInput(
          inputId = ns("ind_filter"),
          label = NULL,
          choices = split(ltmhi_info$domain, ltmhi_info$indicator)
        )
      ),


      # About section
      nav_panel(
        title = "About",
        class = "container",
        p("placeholder")
      ),


      # Report link
      nav_menu(
        title = "Read full report",
        nav_item(tags$a("Report link", href = "placeholder", target = "_blank"))
      )

    ) # close navset_pill_list

  )
}

ltmhi_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Summary table server code ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      
      # icon to use depending on direction of SII/RII trend 
      direction_icon <- function(value) {
        switch(
          value, 
          "Widening" =  bsicons::bs_icon("arrow-up", size = "1.5em", class = "text-red"),
          "Narrowing" = bsicons::bs_icon("arrow-down", size = "1.5em", class = "text-green"),
          "Unchanged" = bsicons::bs_icon("dash", size = "1.5em", class= "text-black")
        )
      }
      

      
      
      # render the summary table 
      output$summary_tbl <- renderReactable({
        
        reactable(
          data = ltmhi_lookup,
          highlight = TRUE,
          pagination = FALSE,
          columns = list(
            
            # hidden columns
            sii_trend_period = colDef(show = FALSE),
            rii_trend_period = colDef(show = FALSE),
            comparison_period = colDef(show = FALSE),
            description = colDef(show = FALSE),
            
            
            # domain column
            domain = colDef(
              name = "Domain",
              cell = function(value, index) {
                if (index > 1 && shi_lookup$domain[index] == shi_lookup$domain[index - 1]) {
                  div(style = "visibility:hidden", value)
                } else value
              }
            ),
            
            
            # comparison column
            comparison = colDef(
              name = "Most vs. least deprived quintile",
              html = TRUE,
              cell = function(value, index){
                as.character(tooltip(
                  div(class = "badge rounded-pill ms-auto", style = "background-color: #adb5bd", value),
                  paste(value, "in the least deprived quintile in", shi_lookup$comparison_period[index])
                ))
              }
            ),
            
            
            # indicator column
            indicator = colDef(
              name = "Indicator",
              minWidth = 270,
              html = TRUE,
              cell = function(value){
                tags$a(
                  href = "#section-3",
                  value,
                  # when link is clicked, update input$indicator_clicked with the name of the indicator
                  # which will trigger an observeEvent() that is spying on this input run in order to
                  # go to  'explore indicators' section and select indicator from filter
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', %s, {priority:'event'});",
                    input$indicator_clicked,
                    jsonlite::toJSON(value, auto_unbox = TRUE)
                  )
                  
                )
              }
            ),
            
            
            # absolute inequalities direction column
            sii_trend = colDef(
              align = 'center',
              html = TRUE,
              name = "Absolute Inequalities Trend",
              cell = function(value, index){
                as.character(tooltip(
                  direction_icon(value),
                  paste(value, "since", shi_lookup$rii_trend_period[index])
                ))
              }
            ),
            
            # relative inequalities direction column
            rii_trend = colDef(
              align = 'center',
              html = TRUE,
              name = "Relative Inequalities Trend",
              cell = function(value, index){
                as.character(tooltip(
                  direction_icon(value),
                  paste(value, "since", shi_lookup$rii_trend_period[index])
                ))
              }
            ),
            
            
            # Overall trend column
            overall_trend = colDef(
              align = 'center',
              name = "Overall Direction of Inequalities",
              cell = function(value){
                
                colour <- switch(
                  value,
                  "Widening" = "red",
                  "Narrowing" = "green",
                  "Unchanged" = "black"
                )
                
                div(class = paste0("badge rounded-pill ms-auto bg-", colour), value)
              }
            )
          )
        )
      })
      
      
      
      # update selection from indicator filter in 'explore indicators' section 
      # whenever link clicked in summary table (i.e. when input$indicator_clicked changes)
      observeEvent(input$indicator_clicked, {
        updateSelectizeInput(
          session = session,
          inputId = "ind_filter",
          selected = input$indicator_clicked
        )
      })
      
      
      
      
    }
  )
}





# For testing:


# shinyApp(
#   ui = page_navbar(
#     nav_panel(
#       title = "National profile",
#       ltmhi_UI("example")
#       )
#     ),
#   server = function(input, output, session){
#     ltmhi_Server("example")
#   }
# )



