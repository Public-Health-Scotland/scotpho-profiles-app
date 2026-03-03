##########################################################################.
# MODULE: Long Term Monitioring of Health Inequalities (LTMHI) module ---- 
# Prepares nav panel which displays LTMHI information .
##########################################################################.


#######################################################.
## MODULE UI ----
#######################################################.


ltmhi_UI <- function(id, ltmhi_dataset) {
  ns <- NS(id)
  
  # chart for LE/HLE headline indicator cards
  le_chart <- function(most_males, most_females, least_males, least_females){
    highchart() |>
      hc_chart(type = "bar") |>
      hc_xAxis(categories = c("Males", "Females"), title = list(text = NULL), lineWidth = 0,tickLength = 0) |>
      hc_add_series(name = "most deprived", data = c(most_males, most_females), dataLabels = list(enabled = TRUE)) |>
      hc_add_series(name = "least deprived", data = c(least_males, least_females), dataLabels = list(enabled = TRUE)) |>
      hc_yAxis(title = list(text = ""), labels = list(enabled = FALSE), gridLineWidth = 0) |>
      hc_colors(c(phs_colors("phs-magenta"), phs_colors("phs-blue")))
  }
  
  
  
  
 tagList(
   div(class = "container-xxl",

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Page header  ----
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
    div(
      class = "p-2 mb-3", # add padding and bottom margin 
      h1("Long-term Monitoring of Health Inequalities in Scotland", style = "font-size:2.5rem"),
      p("This page provides interactive charts and summary data for indicators featured in the latest Long‑Term Monitoring of Health Inequalities in Scotland report. 
      Read the accompanying narrative in the full report", tags$a("here", icon("arrow-up-right-from-square"), href = "placeholder", target = "_blank"), "."),
      bookmarkButton(class = "btn-sm", label = "Bookmark page")
    ),

    # space
    br(),
   
   # Notice about feedback
   div(
     class = "p-3", style = "background-color:#ECECEC",
     p(class = "m-0", "We welcome any feedback on the contents and design of this page at ",  tags$a("phs.scotpho@phs.scot", href = "mailto:phs.scotpho@phs.scot", target = "_blank"), ".")
   ),
   
   
   

   # ~~~~~~~~~~~~~~~~~~~~~~
   # Page contents ------
   # ~~~~~~~~~~~~~~~~~~~~~~

    # sidebar navigaton menu with sub-sections
    layout_columns(
      gap = "0.5rem",
      col_widths = c(3,9),
      
   
    # ~~~~~~~~~~~~~~~~~~~~~
    ## Contents Menu -----
    # ~~~~~~~~~~~~~~~~~~~~~
    # left:
      div(
        class = "sticky-top p-4", # sticky-top prevents menu from scrolling out of view
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
          h2("Headline indicators", class = "mb-4"),
          layout_column_wrap(
            width = "20rem",
            heights_equal = "row",
            card(
              class = "py-3",
              height = 450,
              full_screen = TRUE,
              card_body(
                class = "ms-4",
                h3("Life expectancy", style = "font-size: 1.2rem;"),
                markdown("In 2022-2024, females living in the most deprived areas were expected to live **9 years less**, and males **11 years less**, than those in the least deprived areas."),
                le_chart(most_males = 70, most_females = 76, least_males = 82, least_females = 85)
              )
            ),

            card(
              class = "py-3",
              height = 450,
              full_screen = TRUE,
              card_body(
                class = "ms-4",
                h3("Healthy life expectancy", style = "font-size: 1.2rem;"),
                markdown("In 2021-2023, females in the most deprived areas were expected to spend **22 fewer years** in good health, and males **23 fewer years**, than those in the least deprived areas."),
                le_chart(most_males = 48, most_females = 50, least_males = 71, least_females = 71)
              )
            )
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
          h2("Summary table"),
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
          h2("Explore indicators", class = "mb-4"),
          
          div(
            id = ns("filter_body"),
            class = "p-2",
            layout_columns(
              # indicator filter 
              selectizeInput(
                inputId = ns("ind_filter"),
                label = NULL,
                choices = split(ltmhi_lookup$indicator, ltmhi_lookup$domain),
                width = "100%",
                selected = "Life expectancy, males"
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
            ), #close filter_body div
          
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
          h3("Patterns of inequality", class = "mb-4"),
          
          layout_column_wrap(
            width = "20rem",
            
            # SIMD bar chart 
            navset_card_tab(
              height = 500,
              id = ns("simd_bar_card"),
              full_screen = TRUE,
              nav_panel(
                title = "Chart",
                div(
                  class = "p-2",
                h4(textOutput(ns("simd_bar_title"), container = span), class = "chart-header"),
                textOutput(ns("simd_bar_subtitle"))
                ),
              bar_chart_mod_UI(ns("simd_bar_chart"))
                ),
              nav_panel(title = "Table", data_table_mod_UI(ns("simd_bar_tbl"))),
              nav_spacer(),
              chart_controls_mod_UI(ns("simd_bar_controls"), controls = c(ci_switch = FALSE)),
              footer = card_footer(
                class = "d-flex justify-content-start", 
                share_button_mod_UI(ns("simd_bar_share"))
                )
            ), # close simd bar card
            
            
            # SIMD trend chart 
            navset_card_tab(
              height = 500,
              id = ns("simd_trend_card"),
              full_screen = TRUE,
              nav_panel(
                title = "Chart", 
                div(
                h4(textOutput(ns("simd_trend_title"), container = span), class = "chart-header"),
                textOutput(ns("simd_trend_subtitle"))
                ),
                # to be added
                multi_trend_chart_mod_UI(ns("simd_trend_chart"))
                ),
              nav_panel(title = "Table", data_table_mod_UI(ns("simd_trend_tbl"))),
              nav_spacer(),
              chart_controls_mod_UI(ns("simd_trend_controls"), controls = c(ci_switch = FALSE, avg_switch = TRUE, zero_yaxis_switch = TRUE)),
              footer = card_footer(
                class = "d-flex justify-content-start", 
                share_button_mod_UI(ns("simd_trend_share"))
                )
          )# close simd trend card
          ),  # close layout_column_wrap
          
          # add space
          br(),
          
          
          # Inequality gap charts
          h3("Inequality gap", class = "mb-4"),
          
          layout_column_wrap(
            width = "20rem",
            
            # SII trend chart 
            navset_card_tab(
              height = 600,
              id = ns("sii_card"),
              full_screen = TRUE,
              nav_panel(
                title = "Chart",
                div(
                  h4("Inequalities over time: absolute differences", class = "chart-header"),                
                  p(textOutput(ns("sii_subtitle"))),
                  p("An increasing trend suggests the gap between the most and least deprived areas is growing ", actionLink(ns("sii_info_btn"), label = "learn more"))
                ),
                trend_chart_mod_UI(ns("sii_chart"))
                ),
              nav_panel(title = "Table", data_table_mod_UI(ns("sii_tbl"))),
              nav_panel(title = "Help", about_sii),
              nav_spacer(),
              chart_controls_mod_UI(ns("sii_controls"), controls = c(ci_switch = FALSE, zero_yaxis_switch = TRUE)),
              footer = card_footer(
                class = "d-flex justify-content-start", 
                share_button_mod_UI(ns("sii_share"))
                )
            ),
            
            # RII trend chart 
            navset_card_tab(
              height = 600,
              id = ns("rii_card"),
              full_screen = TRUE,
              nav_panel(
                title = "Chart", 
                div(
                  h4("Inequalities over time: relative differences", class = "chart-header"),
                p("The differences between the most disadvantaged area and the overall average for Scotland (expressed as a percentage)."),
                p("An increasing trend suggests that the gap between the most disadvantaged area and the average is growing ", actionLink(ns("rii_info_btn"), label = "learn more"))
                ),
                trend_chart_mod_UI(ns("rii_chart"))
                ),
              nav_panel(title = "Table", data_table_mod_UI(ns("rii_tbl"))),
              nav_panel(title = "Help", about_rii),
              nav_spacer(),
              chart_controls_mod_UI(ns("rii_controls"), controls = c(ci_switch = FALSE, zero_yaxis_switch = TRUE)),
              footer = card_footer(
                class = "d-flex justify-content-start", 
                share_button_mod_UI(ns("rii_share"))
                )
            )
            
          ), # close layout_column_wrap
          
          
          br(),
          
          
          # Potential for imrpovement charts
          h3("Potential for improvement", class = "mb-4"),
          
          layout_column_wrap(
            width = "20rem",
            
            # PAR bar chart 
            navset_card_tab(
              height = 550,
              id = ns("par_bar_card"),
              full_screen = TRUE,
              nav_panel(title = "Chart", 
                        div(
                        h4(textOutput(ns("par_bar_title")), class = "chart-header"),
                        p(textOutput(ns("par_bar_subtitle"), inline = TRUE), actionLink(ns("par_bar_info_btn"), label = "learn more"))
                        ),
                        stacked_bar_chart_mod_UI(ns("par_bar_chart"))
                        ),
              nav_panel(title = "Table", "placeholder"),
              nav_panel(title = "Help", about_par),
              footer = card_footer(
                class = "d-flex justify-content-start", 
                share_button_mod_UI(ns("par_bar_share"))
                )
            ),
            
            # PAR trend chart 
            navset_card_tab(
              height = 550,
              id = ns("par_trend_card"),
              full_screen = TRUE,
              nav_panel(
                title = "Chart", 
                div(
                  h4("Potential for improvement", class = "chart-header"),
                p(textOutput(ns("par_trend_subtitle"), inline = TRUE), actionLink(ns("par_trend_info_btn"), label = "learn more"))
                ),
                trend_chart_mod_UI(ns("par_trend_chart"))
                ),
              nav_panel(title = "Table", "placeholder"),
              nav_panel(title = "Help", about_par_trend),
              nav_spacer(),
              chart_controls_mod_UI(ns("par_trend_controls"), controls = c(zero_yaxis_switch = TRUE)),
              footer = card_footer(
                class = "d-flex justify-content-start", 
                share_button_mod_UI(ns("par_trend_share"))
                )
            )
          ) # close layout_column_wrap
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
   ) # close container
 ) # close tagList

} # close function 


#######################################################.
## MODULE SERVER ----
#######################################################.

ltmhi_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      

      ns <- session$ns

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
          defaultExpanded = TRUE,
          defaultPageSize = nrow(ltmhi_lookup),
          # centre the contents inside each cell
          defaultColDef = colDef(vAlign = "center"), 
          columns = list(
            
            # hidden columns
            ind_id = colDef(show = FALSE),
            sii_trend_period = colDef(show = FALSE),
            rii_trend_period = colDef(show = FALSE),
            comparison_period = colDef(show = FALSE),

            
            # domain column
            domain = colDef(
              name = "Domain",
              minWidth = 110,
              cell = function(value, index) {
                if (index > 1 && ltmhi_lookup$domain[index] == ltmhi_lookup$domain[index - 1]) {
                  div(style = "visibility:hidden", value)
                } else div(class = "fw-bold", value)
              }
            ),
            
            
            # comparison column
            comparison = colDef(
              name = "Most vs. least deprived quintile",
              minWidth = 120,
              html = TRUE,
              cell = function(value, index){
                as.character(tooltip(
                  div(value),
                  #div(class = "badge rounded-pill ms-auto", style = "background-color: #adb5bd", value),
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
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # reactive datasets ------
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # filter simd dataset by selected indicator (and transpose values so higher always worse)
      r_ind_data <- reactive({
        simd_dataset |>
          filter(indicator == input$ind_filter & areaname == "Scotland" & sex == "Total" & quint_type == "sc_quin") |>
          mutate(across(.cols=sii:abs_range,.fns=abs)) |>
          arrange(year)
      })
      
      # further filter by totals only (no simd splits - for SII/RII/PAR trend charts)
      r_ind_totals <- reactive({
        r_ind_data() |>
          filter(quintile == "Total")
      })
      
      # further filter by quintiles only (no totals - for SIMD trend chart)
      r_ind_quintiles <- reactive({
        r_ind_data() |>
          group_by(year) |>
          mutate(total = measure[quintile == "Total"]) |>
          ungroup() |>
          filter(quintile != "Total")
      })
      
      # further filter by quintiles for latest year only (for SIMD bar chart)
      r_ind_quintiles_max <- reactive({
        r_ind_data() |>
          filter(quintile != "Total") |>
          filter(year == max(year))
      })
      
      # further filter by quintiles for latest year only and reshape data (for par bar chart)
      r_ind_quintiles_par <- reactive({
        r_ind_data() |>
        filter(quintile != "Total") |>
          filter(year == max(year)) |>
        mutate(highest_measure= case_when(quintile==qmax ~ measure, TRUE ~ 0),
               lowest_measure= case_when(quintile==qmin ~ measure, TRUE ~0),
               highest=max(highest_measure),
               lowest=max(lowest_measure),
               baseline=case_when(interpret=="L" ~ lowest,
                                  interpret=="H" ~ measure),
               `attributable to inequality`=case_when(interpret=="L" ~ abs(measure-baseline),
                                                      (interpret=="H" ~ abs(measure-highest)))) |>
        pivot_longer(cols = c("attributable to inequality", "baseline"), names_to = "measure_name")
      })
      
      

      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #  chart controls ------
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # these objects store user selection from each cards chart controls
      # as reactiveValues which can then be used to update the charts accordingly
      # each one contains 3 values. If the input wasn't used in the UI the returned value
      # will be NULL, e.g. for the SIMD bar chart when the app initially loads simd_bar_controls will be:
      # simd_bar_controls$ci_switch = FALSE
      # simd_bar_controls$avg_switch = NULL
      # simd_bar_controls$zero_yaxis_switch = TRUE
      
      simd_bar_controls <- chart_controls_mod_server("simd_bar_controls")
      simd_trend_controls <- chart_controls_mod_server("simd_trend_controls")
      sii_controls <- chart_controls_mod_server("sii_controls")
      rii_controls <- chart_controls_mod_server("rii_controls")
      par_trend_controls <- chart_controls_mod_server("par_trend_controls")
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Chart titles ---------------
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # dynamic titles
      r_titles <- reactive({
        max_year <- r_ind_quintiles_max()$trend_axis[1]
        min_year <- r_ind_totals()$trend_axis[1]
        
        list(
          simd_bar = paste(input$ind_filter, "by SIMD quintile,", max_year),
          simd_trend = paste(input$ind_filter, "by SIMD quintile,", min_year, "-", max_year),
          par_bar = paste("Attributable to inequality,", max_year)
        )
      })
      
      
      # dynamic subtitles
      r_subtitles <- reactive({
        
        measure <- r_ind_data()$type_definition[1]
        indicator <- input$ind_filter
        
        list(
          simd_bar = measure,
          simd_trend = measure,
          sii = paste0("The difference between most and least deprived areas (expressed as ", measure, ")."),
          par_bar = paste("The portion of", indicator, "which could be attributed to socioeconomic inequalities."),
          par_trend = paste("How much (%)", indicator, "could be improved if the levels of the 
          least deprived area were experienced across the whole population.")
        )
        
      })

      
      # render chart titles
      output$simd_bar_title <- renderText({r_titles()$simd_bar})
      output$simd_trend_title <- renderText({r_titles()$simd_trend})
      output$par_bar_title <- renderText({r_titles()$par_bar})
      
      
      # render chart subtitles
      output$simd_bar_subtitle <- renderText({r_subtitles()$simd_bar})
      output$simd_trend_subtitle <- renderText({r_subtitles()$simd_trend})
      output$sii_subtitle <- renderText({r_subtitles()$sii})
      output$par_bar_subtitle <- renderText({r_subtitles()$par_bar})
      output$par_trend_subtitle <- renderText({r_subtitles()$par_trend})

      
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~
      # charts -----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # SIMD snapshot chart
      bar_chart_mod_Server(id = "simd_bar_chart",
                           r_data = r_ind_quintiles_max,
                           r_chart_controls = simd_bar_controls,
                           x_col = "quintile", y_col = "measure", lowci_col = "lowci", upci_col = "upci"
                           )
      

      # SIMD trend chart
      multi_trend_chart_mod_Server(id = "simd_trend_chart", r_data = r_ind_quintiles,
                               r_chart_controls = simd_trend_controls,
                               x_col = "trend_axis", y_col = "measure", lowci_col = "lowci", upci_col = "upci",
                               group_col = "quintile", avg_col = "total")



      # Relative Index of Inequality (RII) trend chart
      trend_chart_mod_Server(id = "rii_chart",
                         r_data = r_ind_totals,
                         r_chart_controls = rii_controls,
                         x_col = "trend_axis", y_col = "rii_int", lowci_col = "lowci_rii_int", upci_col = "upci_rii_int"
      )


      # Slope Index of Inequality (SII) trend chart
      trend_chart_mod_Server(id = "sii_chart",
                         r_data = r_ind_totals,
                         r_chart_controls = sii_controls,
                         x_col = "trend_axis", y_col = "sii", lowci_col = "lowci_sii", upci_col = "upci_sii"
      )



      # Population attributable Risk (PAR) bar chart
      stacked_bar_chart_mod_Server(id = "par_bar_chart", r_data = r_ind_quintiles_par,
                                   x_col = "quintile", y_col = "value", group_col = "measure_name")


      # Population Attributable Risk (PAR) trend chart
      trend_chart_mod_Server(id = "par_trend_chart",
                             r_data = r_ind_totals,
                             r_chart_controls = par_trend_controls,
                             x_col = "trend_axis", y_col = "par"
                             )
      
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Share card buttons ------
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      share_button_mod_Server(id = "simd_bar_share", card_id = ns("simd_bar_card"))
      share_button_mod_Server(id = "simd_trend_share", card_id = ns("simd_trend_card"))
      share_button_mod_Server(id = "sii_share", card_id = ns("sii_card"))
      share_button_mod_Server(id = "rii_share", card_id = ns("rii_card"))
      share_button_mod_Server(id = "par_bar_share", card_id = ns("par_bar_card"))
      share_button_mod_Server(id = "par_trend_share", card_id = ns("par_trend_card"))
      
      

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Data tables ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      data_table_mod_Server(id = "simd_bar_tbl", r_data = r_ind_quintiles_max, cols = c("quintile", "measure"))
      data_table_mod_Server(id = "simd_trend_tbl", r_data = r_ind_quintiles, cols = c("year", "quintile", "measure"))
      data_table_mod_Server(id = "sii_tbl", r_data = r_ind_totals, cols = c("year", "sii"))
      data_table_mod_Server(id = "rii_tbl", r_data = r_ind_totals, cols = c("year", "rii_int"))
      data_table_mod_Server(id = "par_bar_tbl", r_data = r_ind_totals, cols = c("year", "sii"))
      data_table_mod_Server(id = "par_trend_tbl", r_data = r_ind_totals, cols = c("year", "rii_int"))
      

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Bookmarking exclusions -----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Excluding some inputs created within this module (such as cards, buttons) 
      # from appearing in bookmarked URLs. Note that any nested modules used within this
      # module will have bookmark exclusions applied in their source code so won't be listed here
      # The exclusions here are only inputs created within this module.
      setBookmarkExclude(
          # exclude card inputs
          c("simd_bar_card", "simd_trend_card", "sii_card", "rii_card", "par_bar_card", "par_trend_card",
          # exclude buttons 
          "toggle_filters", "sii_info_btn", "rii_info_btn", "par_bar_info_btn", "par_trend_info_btn",
          # temporary - exclude quint_type filter until enabled
          "type_filter",
          # exclude inputs automatically created by reactable
          "summary_tbl__reactable__page", "summary_tbl__reactable__pageSize", "summary_tbl__reactable__pages",
          "summary_tbl__reactable__sorted", "summary_tbl__reactable__selected"
            ))
                         

    }
  )
}





# #For testing:
# shinyApp(
#   enableBookmarking = "url",
#  ui = function(request){
#    page_navbar(
#    theme = phs_theme,
#    navbar_options = list(bg = "#3F3685"),
#    fillable = FALSE,
#    header = useShinyjs(),
#    nav_panel(
#      class = "container-xxl",
#      title = "National profile",
#      ltmhi_UI("example")
#      )
#    )
#    },
#  server = function(input, output, session){
#    ltmhi_Server("example")
#  }
# )




