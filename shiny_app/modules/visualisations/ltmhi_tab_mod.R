ltmhi_UI <- function(id, ltmhi_dataset) {
  ns <- NS(id)
 tagList(

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Page header  ----
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
    div(
      class = "p-2 mb-3", # add padding and bottom margin 
      h1("Long-term Monitoring of Health Inequalities in Scotland"),
      # MM revisit: final link to report to be added
      p("Access the", tags$a("full report with narrative", href = "placeholder", target = "_blank"), "on the Public Health Scotland website."),
      bookmarkButton(class = "btn-sm")
    ),

    # horizonal line
    hr(),
   
   
   # ~~~~~~~~~~~~~~~~~~~~~~
   # Page contents ------
   # ~~~~~~~~~~~~~~~~~~~~~~

    # sidebar navigaton menu with sub-sections
    layout_columns(
      col_widths = c(3,9),
      
   
    # ~~~~~~~~~~~~~~~~~~~~~
    ## Contents Menu -----
    # ~~~~~~~~~~~~~~~~~~~~~
    # left:
      div(
        class = "sticky-top p-4", # sticky-top prevents menu from scrolling out of view p-4
        h4("Contents"),
        tags$ul(
          class = "list-group gap-3 list-unstyled",
          tags$li(a(href = "#headline-indicators", "Headline indicators")),
          tags$li(a(href = "#summary-table", "Summary table")),
          tags$li(a(href = "#explore-indicators", "Explore Indicators")),
          tags$li(a(href = "#about", "About"))
        ),
        hr()
      ), # close menu 
      
    
     # ~~~~~~~~~~~~~~~~~~~~~
     # Contents -----
     # ~~~~~~~~~~~~~~~~~~~~~
     # right:
      div(
        class = "pt-4", # add top padding
     
      # Section 1: Headline indicators --------------
      div(
        class = "mb-5", # add bottom margin
        id = "headline-indicators",
        h4("Headline indicators", class = "mb-4"),
        layout_column_wrap(
          width = "20rem",
          heights_equal = "row",
          card("placeholder", full_screen = TRUE, height = 300),
          card("placeholder", full_screen = TRUE, height = 300),
          card("placeholder", full_screen = TRUE, height = 300),
          card("placeholder", full_screen = TRUE, height = 300)
        )
      ), # close key points section 
      
      # horizontal line 
      hr(),

      # Section 2: Summary table ----------------
      div(
        id = "summary-table",
        class = "mb-5", # add bottom margin
        div(
          class = "mb-4",
          h4("Summary table"),
          p("Recent trends in key indicators of health inequalities.")
          ),
        card(reactableOutput(ns("summary_tbl")))
        ), # close summary table section 
      
      
      # horizontal line
      hr(),


      # Section 3: Explore indicators section ----------------
      div(
        id = "explore-indicators",
        class = "mb-5", # add bottom margin
        
        # panel with indicators
        # sticky-top prevents panel from moving when scrolling
        # bg-white makes background white (to prevent seeing content scrolling past underneath the filters)
        # py-2 and my-2 = top and bottom padding and margins
        div(
          class = "sticky-top bg-white py-2 my-2", 
          h3("Explore indicators", class = "mb-4"),
          
          div(
            id = ns("filter_body"),
            class = "p-2",
            layout_columns(
              # indicator filter 
              selectizeInput(
                inputId = ns("ind_filter"),
                label = NULL,
                choices = split(ltmhi_lookup$indicator, ltmhi_lookup$domain),
                width = "100%"
              ),
              # type filter
              radioButtons(
                inputId = ns("type_filter"),
                choiceNames = c("Quintiles", "Deciles"),
                choiceValues = c("sc_quin", "sc_decile"),
                label = NULL,
                inline = TRUE,
                selected = "sc_quin"
              ) |>
                # temporary code: disable sc_decile option for now
                # first iteration will only present data by quintiles
                tagAppendChild(
                  tags$script(
                    sprintf(
                      "$(function(){ $('#%s input[value=\"sc_decile\"]').prop('disabled', true); });",
                      ns("type_filter")
                    )
                  )
                )
              )
            ), #c lose filter_body div
          
          # button for opening/closing filters
          actionButton(
            inputId = ns("toggle_filters"),
            class = "btn-sm position-absolute",
            style = "right: 0.75rem; bottom: 0.5rem; z-index: 10;",
            label = "Hide filters"
          )
        ), # close sticky filter panel
        
        
        # container for cards with charts
        div(
          class = "m-1",
          
          # Patterns of inequality charts
          h5("Patterns of inequality", class = "mb-4"),
          
          layout_column_wrap(
            width = "20rem",
            
            # SIMD bar chart 
            navset_card_tab(
              height = 450,
              id = ns("simd_bar_card"),
              full_screen = TRUE,
              nav_panel(title = "Chart", "placeholder"),
              nav_panel(title = "Table", "placeholder"),
              footer = card_footer(class = "d-flex justify-content-start", "placeholder")
            ),
            
            # SIMD trend chart 
            navset_card_tab(
              height = 450,
              id = ns("simd_trend_card"),
              full_screen = TRUE,
              nav_panel(title = "Chart", "placeholder"),
              nav_panel(title = "Table", "placeholder"),
              footer = card_footer(class = "d-flex justify-content-start", "placeholder")
            )
          ),
          
          
          br(),
          
          
          # Inequality gap charts
          h5("Inequality gap", class = "mb-4"),
          
          layout_column_wrap(
            width = "20rem",
            
            # SII trend chart 
            navset_card_tab(
              height = 450,
              id = ns("sii_card"),
              full_screen = TRUE,
              nav_panel(title = "Chart", "placeholder"),
              nav_panel(title = "Table", "placeholder"),
              footer = card_footer(class = "d-flex justify-content-start", "placeholder")
            ),
            
            # RII trend chart 
            navset_card_tab(
              height = 450,
              id = ns("rii_card"),
              full_screen = TRUE,
              nav_panel(title = "Chart", "placeholder"),
              nav_panel(title = "Table", "placeholder"),
              footer = card_footer(class = "d-flex justify-content-start", "placeholder")
            )
          ),
          
          
          br(),
          
          
          # Potential for imrpovement charts
          h5("Potential for improvement", class = "mb-4"),
          
          layout_column_wrap(
            width = "20rem",
            
            # SII trend chart 
            navset_card_tab(
              height = 450,
              id = ns("par_bar_card"),
              full_screen = TRUE,
              nav_panel(title = "Chart", "placeholder"),
              nav_panel(title = "Table", "placeholder"),
              footer = card_footer(class = "d-flex justify-content-start", "placeholder")
            ),
            
            # RII trend chart 
            navset_card_tab(
              height = 450,
              id = ns("par_trend_card"),
              full_screen = TRUE,
              nav_panel(title = "Chart", "placeholder"),
              nav_panel(title = "Table", "placeholder"),
              footer = card_footer(class = "d-flex justify-content-start", "placeholder")
            )
          )
        ) # close container for charts
      ), # close explore indicators section
      
      
      # horizontal line
      hr(),
      
      
      # About section -----------------------
      div(
        id = "about",
        class = "mb-5",
        h4("About", class = "mb-4"),
        div(style = "height:800px", "Placeholder")
        )
      
      ) # close right hand side section
    ) # close layout_colums for left-menu and right-content
 ) # close tagList

} # close function 



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
                if (index > 1 && ltmhi_lookup$domain[index] == ltmhi_lookup$domain[index - 1]) {
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
                  paste(value, "in the least deprived quintile in", ltmhi_lookup$comparison_period[index])
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
                  href = "#explore-indicators",
                  value,
                  # when link is clicked, update input$indicator_clicked with the name of the indicator
                  # which will trigger an observeEvent() that is spying on this input run in order to
                  # go to  'explore indicators' section and select indicator from filter
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', %s, {priority:'event'});",
                    session$ns("indicator_clicked"),
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
                  paste(value, "since", ltmhi_lookup$rii_trend_period[index])
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
                  paste(value, "since", ltmhi_lookup$rii_trend_period[index])
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
      
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Explore indicators server code ------
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # open/close panel with filters when show/hide filter
      # button is clicked and update buttons label accordingly
      observeEvent(input$toggle_filters, {
        shinyjs::toggle(id = "filter_body")
        
        if(input$toggle_filters %% 2 != 0){
          updateActionButton(inputId = "toggle_filters", label = "Show filters")
        } else {
          updateActionButton(inputId = "toggle_filters", label = "Hide filters")
        }
        
      })
      
      
      
    }
  )
}





# For testing:

# shinyApp(
#   ui = page_navbar(
#     fillable = FALSE,
#     header = useShinyjs(),
#     nav_panel(
#       class = "container-xxl",
#       title = "National profile",
#       ltmhi_UI("example")
#       )
#     ),
#   server = function(input, output, session){
#     ltmhi_Server("example")
#   }
# )



